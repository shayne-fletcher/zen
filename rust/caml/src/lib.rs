#![allow(unused_imports)]

pub use crate::block::{Block, BlockBuilder};
pub use crate::error::FromError;
pub use crate::value::{Value, Value_};
pub use arena::Arena;
pub use block::{CLOSURE_TAG, CUSTOM_TAG, DOUBLE_TAG, NO_SCAN_TAG, STRING_TAG};
pub use bumpalo::Bump;
pub use cache::MemoizationCache;
pub use impls::{bytes_to_ocamlrep, str_to_ocamlrep};

// ---

mod error {

    use std::error::Error;
    use std::fmt;
    use std::num::TryFromIntError;
    use std::str::Utf8Error;

    pub enum FromError {
        BadUtf8(Utf8Error),
        BlockTagOutOfRange { max: u8, actual: u8 },
        ErrorInField(usize, Box<FromError>),
        ExpectedBlock(isize),
        ExpectedBlockTag { expected: u8, actual: u8 },
        ExpectedBool(isize),
        ExpectedChar(isize),
        ExpectedImmediate(usize),
        ExpectedUnit(isize),
        ExpectedZeroTag(u8),
        IntOutOfRange(TryFromIntError),
        NullaryVariantTagOutOfRange { max: usize, actual: isize },
        WrongBlockSize { expected: usize, actual: usize },
        UnexpectedCustomOps { expected: usize, actual: usize },
    }

    impl std::convert::From<TryFromIntError> for FromError {
        fn from(error: TryFromIntError) -> Self {
            FromError::IntOutOfRange(error)
        }
    }

    impl std::convert::From<Utf8Error> for FromError {
        fn from(error: Utf8Error) -> Self {
            FromError::BadUtf8(error)
        }
    }

    impl fmt::Display for FromError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            use FromError::*;
            match self {
                BadUtf8(_) => write!(f, "Invalid UTF-8"),
                BlockTagOutOfRange { max, actual } => {
                    write!(f, "Expected tag value <= {}, bug got {}", max, actual)
                }
                ErrorInField(idx, _) => write!(f, "Failed to convert field {}", idx),
                ExpectedBlock(x) => write!(f, "Expected block, but got immediate value {}", x),
                ExpectedBlockTag { expected, actual } => write!(
                    f,
                    "Expected block with tag {}, but got {}",
                    expected, actual
                ),
                ExpectedBool(x) => write!(f, "Expected bool, but got {}", x),
                ExpectedChar(x) => write!(f, "Expected char, but got {}", x),
                ExpectedImmediate(x) => {
                    write!(f, "Expected immediate value, but got block pointer {:p}", x)
                }
                ExpectedUnit(x) => write!(f, "Expected (), but got {}", x),
                ExpectedZeroTag(x) => write!(
                 f,
                 "Expected block with tag 0 (tuple, record, cons cell, etc), but got tag value {}",
                 x
             ),
                IntOutOfRange(_) => write!(f, "Integer value out of range"),
                NullaryVariantTagOutOfRange { max, actual } => write!(
                    f,
                    "Expected nullary variant tag, where 0 <= tag <= {}, but got {}",
                    max, actual
                ),
                WrongBlockSize { expected, actual } => write!(
                    f,
                    "Expected block of size {}, but got size {}",
                    expected, actual
                ),
                UnexpectedCustomOps { expected, actual } => write!(
                    f,
                    "Expected custom operations struct address 0x{:x}, but got address 0x{:x}",
                    expected, actual
                ),
            }
        }
    }
} // mod error

// ---

pub trait ToOcamlRep {
    fn to_ocamlrep<'a, A: Allocator>(&'a self, alloc: &'a A) -> Value_<'a>;
}

pub trait FromOcamlRep: Sized {
    fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError>;

    unsafe fn from_ocaml(value: usize) -> Result<Self, FromError> {
        Self::from_ocamlrep(Value::from_bits(value))
    }
}

pub trait FromOcamlRepIn<'a>: Sized {
    fn from_ocamlrep_in(value: Value<'_>, arena: &'a Bump) -> Result<Self, FromError>;
}

// ---

pub trait Allocator: Sized {
    fn generation(&self) -> usize;

    fn block_with_size_and_tag(&self, size: usize, tag: u8) -> BlockBuilder<'_>;
    fn block_with_size(&self, size: usize) -> BlockBuilder<'_> {
        self.block_with_size_and_tag(size, 0u8)
    }

    fn set_field<'a>(&self, block: &mut BlockBuilder<'a>, index: usize, value: Value_<'a>);

    unsafe fn block_ptr_mut<'a>(&self, block: &mut BlockBuilder<'a>) -> *mut Value_<'a>;

    fn add<'a, T: ToOcamlRep + ?Sized>(&'a self, value: &'a T) -> Value_<'a> {
        value.to_ocamlrep(self)
    }
    fn add_copy<'a, T: ToOcamlRep + Copy + 'static>(&'a self, value: T) -> Value_<'a> {
        let value_ref = &value;
        self.add(unsafe { std::mem::transmute::<&'_ T, &'a T>(value_ref) })
    }

    fn memoized<'a>(
        &'a self,
        ptr: usize,
        size_in_bytes: usize,
        f: impl FnOnce(&'a Self) -> Value_<'a>,
    ) -> Value_<'a>;

    fn add_root<'a, T: ToOcamlRep + ?Sized>(&'a self, value: &'a T) -> Value_<'a>;
}

// ---

mod cache {
    use nohash_hasher::IntMap;
    use std::cell::RefCell;

    pub struct MemoizationCache {
        // Maps from input address -> size in bytes -> output
        cache: RefCell<Option<IntMap<usize, IntMap<usize, usize>>>>,
    }

    impl MemoizationCache {
        pub fn new() -> Self {
            Self {
                cache: RefCell::new(None),
            }
        }

        pub fn with_cache<T>(&self, f: impl FnOnce() -> T) -> T {
            let prev_value = self.cache.replace(Some(IntMap::default()));
            if prev_value.is_some() {
                panic!("Attempted to re-enter `MemoizationCache::with_cache`")
            }
            let result = f();
            self.cache.replace(None);
            result
        }

        pub fn memoized(
            &self,
            input: usize,
            size_in_bytes: usize,
            f: impl FnOnce() -> usize,
        ) -> usize {
            if size_in_bytes == 0 {
                return f();
            }

            let cell_cont: &Option<IntMap<_, _>> = &self.cache.borrow();
            let r: Option<&IntMap<usize, IntMap<usize, usize>>> = cell_cont.as_ref();
            let ff/*: FnOnce(&IntMap<usize, IntMap<usize, usize>>) -> Option<usize>*/ =
                |cache: &IntMap<usize, IntMap<usize, usize>>| cache.get(&input).and_then(|m| m.get(&size_in_bytes).copied());
            let fmap_r_ff: Option<Option<usize>> = r.map(ff);
            let memoized_output = match fmap_r_ff {
                None => return f(),
                Some(output) => output,
            };
            match memoized_output {
                Some(output) => output,
                None => {
                    let output = f();
                    let mut cache = self.cache.borrow_mut();
                    let by_size = cache.as_mut().unwrap().entry(input).or_default();
                    by_size.insert(size_in_bytes, output);
                    output
                }
            }
        }
    }
}

// ---

mod arena {
    use crate::{
        block::Header, Allocator, BlockBuilder, MemoizationCache, ToOcamlRep, Value, Value_,
    };
    use std::cell::RefCell;
    use std::cmp::max;
    use std::sync::atomic::{AtomicUsize, Ordering};

    struct Chunk {
        data: Box<[Value_<'static>]>,
        index: usize,
        #[allow(dead_code)] // field never read (only written)
        prev: Option<Box<Chunk>>, // ptr to the prev arena segment
    }

    impl Chunk {
        // capacity is given in words
        fn with_capacity(capacity: usize) -> Self {
            Self {
                index: 0,
                data: vec![Value_::int(0); capacity].into_boxed_slice(),
                prev: None,
            }
        }

        fn capacity(&self) -> usize {
            self.data.len()
        }

        fn can_fit(&self, requested_size: usize) -> bool {
            self.index + requested_size <= self.data.len()
        }

        pub fn alloc(&mut self, requested_size: usize) -> &mut [Value_<'static>] {
            let previous_index = self.index;
            self.index += requested_size;
            &mut self.data[previous_index..self.index]
        }
    } //impl Chunk

    // The generation number is used solely to identify which arean a
    // cached value belongs to in `RcOc`.
    static NEXT_GENERATION: AtomicUsize = AtomicUsize::new(usize::max_value() / 2);

    pub struct Arena {
        generation: usize,
        current_chunk: RefCell<Chunk>,
        cache: MemoizationCache,
    }

    impl Arena {
        pub fn new() -> Self {
            Self::with_capacity(1024 * 4)
        }

        pub fn with_capacity(capacity_in_bytes: usize) -> Self {
            let generation = NEXT_GENERATION.fetch_add(1, Ordering::SeqCst);
            let capacity_in_words = max(2, capacity_in_bytes / std::mem::size_of::<Value_<'_>>());
            Self {
                generation,
                current_chunk: RefCell::new(Chunk::with_capacity(capacity_in_words)),
                cache: MemoizationCache::new(),
            }
        }

        fn alloc<'a>(&'a self, requested_size: usize) -> &'a mut [Value_<'a>] {
            if !self.current_chunk.borrow().can_fit(requested_size) {
                let prev_chunk_capacity = self.current_chunk.borrow().capacity();
                let prev_chunk = self.current_chunk.replace(Chunk::with_capacity(max(
                    requested_size * 2,
                    prev_chunk_capacity,
                )));
                self.current_chunk.borrow_mut().prev = Some(Box::new(prev_chunk))
            }
            let mut chunk = self.current_chunk.borrow_mut();
            let slice = chunk.alloc(requested_size);
            unsafe { std::mem::transmute::<&'_ mut [Value_<'static>], &'a mut [Value_<'a>]>(slice) }
        }

        // Must be used only with values allocated by an `Arena`
        pub unsafe fn make_transparent(value: Value_<'_>) -> Value<'_> {
            Value::from_bits(value.to_bits())
        }

        pub fn add<'a, T: ToOcamlRep + ?Sized>(&'a self, value: &'a T) -> Value<'a> {
            unsafe { Self::make_transparent(value.to_ocamlrep(self)) }
        }

        pub fn add_root<'a, T: ToOcamlRep + ?Sized>(&'a self, value: &'a T) -> Value<'a> {
            unsafe { Self::make_transparent(Allocator::add_root(self, value)) }
        }
    } //impl Arena

    impl Allocator for Arena {
        fn generation(&self) -> usize {
            self.generation
        }

        fn block_with_size_and_tag(&self, size: usize, tag: u8) -> BlockBuilder<'_> {
            let block = self.alloc(size + 1);
            let header = Header::new(size, tag);
            block[0] = unsafe { Value_::from_bits(header.to_bits()) };
            let slice = &mut block[1..];
            BlockBuilder::new(slice.as_ptr() as usize, slice.len())
        }

        fn set_field<'a>(&self, block: &mut BlockBuilder<'a>, index: usize, value: Value_<'a>) {
            unsafe { *self.block_ptr_mut(block).offset(index as isize) = value }
        }

        unsafe fn block_ptr_mut<'a>(&self, block: &mut BlockBuilder<'a>) -> *mut Value_<'a> {
            block.address() as *mut _
        }

        fn memoized<'a>(
            &'a self,
            ptr: usize,
            size: usize,
            f: impl FnOnce(&'a Self) -> Value_<'a>,
        ) -> Value_<'a> {
            let bits = self.cache.memoized(ptr, size, || f(self).to_bits());
            unsafe { Value_::from_bits(bits) }
        }

        fn add_root<'a, T: ToOcamlRep + ?Sized>(&'a self, value: &'a T) -> Value_<'a> {
            self.cache.with_cache(|| value.to_ocamlrep(self))
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        use std::time::Instant;

        #[test]
        fn test_alloc_block_of_three_fields() {
            let arena = Arena::with_capacity(1024);
            let mut block = arena.block_with_size(3);
            arena.set_field(&mut block, 0, Value_::int(1));
            arena.set_field(&mut block, 1, Value_::int(2));
            arena.set_field(&mut block, 2, Value_::int(3));
            let block = block.build();
            let block = unsafe { Arena::make_transparent(block) }
                .as_block()
                .unwrap();
            assert_eq!(block.size(), 3);
            assert_eq!(block[0].as_int().unwrap(), 1);
            assert_eq!(block[1].as_int().unwrap(), 2);
            assert_eq!(block[2].as_int().unwrap(), 3);
        }

        #[test]
        fn test_large_allocs() {
            let arena = Arena::with_capacity(1000);
            let alloc_block = |size| {
                unsafe { Arena::make_transparent(arena.block_with_size(size).build()) }
                    .as_block()
                    .unwrap()
            };
            let max = alloc_block(1000);
            assert_eq!(max.size(), 1000);
            let two_thousand = alloc_block(2000);
            assert_eq!(two_thousand.size(), 2000);
            let four_thousand = alloc_block(4000);
            assert_eq!(four_thousand.size(), 4000);
        }

        #[test]
        fn perf_test() {
            let arena = Arena::with_capacity(10_000);

            let alloc_block = |size| {
                unsafe { Arena::make_transparent(arena.block_with_size(size).build()) }
                    .as_block()
                    .unwrap()
            };

            println!("Benchmarks for allocating [1] 200,000 times");
            let now = Instant::now();
            for _ in 0..200_000 {
                vec![0; 1].into_boxed_slice();
            }
            println!("Alloc: {:?}", now.elapsed());

            let now = Instant::now();
            for _ in 0..200_000 {
                alloc_block(1);
            }
            println!("Arena: {:?}", now.elapsed());

            println!("Benchmarks for allocating [5] 200,000 times");
            let now = Instant::now();
            for _ in 0..200_000 {
                vec![0; 5].into_boxed_slice();
            }
            println!("Alloc: {:?}", now.elapsed());

            let now = Instant::now();
            for _ in 0..200_000 {
                alloc_block(5);
            }
            println!("Arena: {:?}", now.elapsed());

            println!("Benchmarks for allocating [10] 200,000 times");
            let now = Instant::now();
            for _ in 0..200_000 {
                vec![0; 10].into_boxed_slice();
            }
            println!("Alloc: {:?}", now.elapsed());

            let now = Instant::now();
            for _ in 0..200_000 {
                alloc_block(10);
            }
            println!("Arena: {:?}", now.elapsed());
        }
    }
} //mod arena

// ---

pub const fn is_ocaml_int(value: usize) -> bool {
    value & 1 == 1
}

pub const fn isize_to_ocaml_int(value: isize) -> usize {
    ((value as usize) << 1) | 1
}

pub const fn ocaml_int_to_isize(value: usize) -> isize {
    (value as isize) >> 1
}

mod value {

    use crate::block::{self, Block, Header};
    use crate::{is_ocaml_int, isize_to_ocaml_int, ocaml_int_to_isize};
    use std::borrow::Cow;
    use std::fmt::{self, Debug};
    use std::marker::PhantomData;

    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct Value<'a>(pub(crate) usize, PhantomData<&'a ()>);

    impl<'a> Value<'a> {
        pub fn int(value: isize) -> Value<'static> {
            Value(isize_to_ocaml_int(value), PhantomData)
        }

        pub fn is_immediate(self) -> bool {
            is_ocaml_int(self.0)
        }

        pub fn as_int(self) -> Option<isize> {
            if self.is_immediate() {
                Some(ocaml_int_to_isize(self.0))
            } else {
                None
            }
        }

        pub fn as_block(self) -> Option<Block<'a>> {
            if self.is_immediate() {
                return None;
            }
            let block = unsafe {
                let ptr = self.0 as *const Value<'_>;
                let header = ptr.offset(-1);
                let size = Header::from_bits((*header).to_bits()).size() + 1;
                std::slice::from_raw_parts(header, size)
            };
            Some(Block(block))
        }

        pub fn as_float(self) -> Option<f64> {
            let block = self.as_block()?;
            if block.tag() != block::DOUBLE_TAG {
                return None;
            }
            Some(f64::from_bits(block[0].0 as u64))
        }

        pub fn as_byte_string(self) -> Option<&'a [u8]> {
            let block = self.as_block()?;
            if block.tag() != block::STRING_TAG {
                return None;
            }
            let slice = unsafe {
                let size = block.size() * std::mem::size_of::<Value<'_>>();
                let ptr = self.0 as *mut u8;
                let last_byte = ptr.offset(size as isize - 1);
                let padding = *last_byte;
                let size = size - padding as usize - 1;
                std::slice::from_raw_parts(ptr, size)
            };
            Some(slice)
        }

        pub fn as_str(self) -> Option<Cow<'a, str>> {
            let slice = self.as_byte_string()?;
            Some(String::from_utf8_lossy(slice))
        }

        pub fn to_bits(self) -> usize {
            self.0
        }

        // used by slab
        pub(crate) unsafe fn _add_ptr_offset(&mut self, diff: isize) {
            if !self.is_immediate() {
                self.0 = (self.0 as isize + diff) as usize;
            }
        }

        pub unsafe fn from_ptr(value: *const Value<'a>) -> Value<'a> {
            Value(value as usize, PhantomData)
        }

        pub unsafe fn from_bits(value: usize) -> Value<'a> {
            Value(value, PhantomData)
        }
    }

    impl Debug for Value<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.as_block() {
                None => write!(f, "{}", self.as_int().unwrap()),
                Some(block) => write!(f, "{:?}", block),
            }
        }
    }

    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct Value_<'a>(usize, PhantomData<&'a ()>);

    impl<'a> Value_<'a> {
        pub fn int(value: isize) -> Value_<'static> {
            Value_(isize_to_ocaml_int(value), PhantomData)
        }

        pub unsafe fn from_bits(value: usize) -> Value_<'a> {
            Value_(value, PhantomData)
        }

        pub fn to_bits(self) -> usize {
            self.0
        }
    }
}

mod block {

    use crate::value::{Value, Value_};
    use std::fmt::{self, Debug};
    use std::marker::PhantomData;
    use std::ops::Index;

    pub const CLOSURE_TAG: u8 = 247;
    pub const NO_SCAN_TAG: u8 = 251;
    pub const STRING_TAG: u8 = 252;
    pub const DOUBLE_TAG: u8 = 253;
    pub const CUSTOM_TAG: u8 = 255;

    // ---
    // struct BlockBuilder<'a>

    pub struct BlockBuilder<'a> {
        address: usize,
        size: usize,
        _phantom: PhantomData<&'a mut [Value_<'a>]>,
    }

    impl<'a> BlockBuilder<'a> {
        pub fn new(address: usize, size: usize) -> Self {
            if size == 0 {
                panic!()
            }
            Self {
                address,
                size,
                _phantom: PhantomData,
            }
        }

        pub fn address(&self) -> usize {
            self.address
        }

        pub fn size(&self) -> usize {
            self.size
        }

        pub fn build(self) -> Value_<'a> {
            unsafe { Value_::from_bits(self.address) }
        }
    } // impl BlockBuilder<'a>

    // ---
    // struct Block<'a>

    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct Block<'arena>(pub(crate) &'arena [Value<'arena>]);

    impl<'a> Block<'a> {
        pub(crate) fn header(&self) -> Header {
            Header(self.0[0].0)
        }

        pub fn size(&self) -> usize {
            self.header().size()
        }

        pub fn tag(&self) -> u8 {
            self.header().tag()
        }

        pub fn as_value(&self) -> Value<'_> {
            unsafe { Value::from_ptr(&self.0[1]) }
        }
    }

    impl<'a> Index<usize> for Block<'a> {
        type Output = Value<'a>;

        fn index(&self, index: usize) -> &Self::Output {
            &self.0[index + 1]
        }
    }

    impl Debug for Block<'_> {
        fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Ok(())
        }
    }

    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct Header(usize);

    impl Header {
        pub(crate) fn new(size: usize, tag: u8) -> Self {
            let bits = size << 10 | (tag as usize);
            Header(bits)
        }

        pub fn size(self) -> usize {
            self.0 >> 10
        }

        pub fn tag(self) -> u8 {
            self.0 as u8
        }

        pub(crate) fn from_bits(bits: usize) -> Self {
            Header(bits)
        }

        pub(crate) fn to_bits(self) -> usize {
            self.0
        }
    }

    impl Debug for Header {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("Header")
                .field("size", &self.size())
                .field("tag", &self.tag())
                .finish()
        }
    }
}

// ---
//
mod from {
    use crate::{Block, FromError, FromOcamlRep, Value};
    use std::convert::TryInto;

    pub fn expect_int(value: Value<'_>) -> Result<isize, FromError> {
        match value.as_int() {
            Some(value) => Ok(value),
            None => Err(FromError::ExpectedImmediate(value.to_bits())),
        }
    }

    pub fn expect_block<'a>(value: Value<'a>) -> Result<Block<'a>, FromError> {
        match value.as_block() {
            Some(block) => Ok(block),
            None => Err(FromError::ExpectedBlock(value.as_int().unwrap())),
        }
    }

    pub fn expect_block_size(block: Block<'_>, size: usize) -> Result<(), FromError> {
        if block.size() != size {
            return Err(FromError::WrongBlockSize {
                expected: size,
                actual: block.size(),
            });
        }
        Ok(())
    }

    pub fn expect_block_tag(block: Block<'_>, tag: u8) -> Result<(), FromError> {
        if block.tag() != tag {
            return Err(FromError::ExpectedBlockTag {
                expected: tag,
                actual: block.tag(),
            });
        }
        Ok(())
    }

    pub fn expect_block_with_size_and_tag(
        value: Value<'_>,
        size: usize,
        tag: u8,
    ) -> Result<Block<'_>, FromError> {
        let block = expect_block(value)?;
        expect_block_size(block, size)?;
        expect_block_tag(block, tag)?;
        Ok(block)
    }

    pub fn expect_nullary_variant(value: Value<'_>, max: usize) -> Result<isize, FromError> {
        let value = expect_int(value)?;
        let max_as_isize: isize = max.try_into().unwrap();
        if 0 <= value && value <= max_as_isize {
            Ok(value)
        } else {
            Err(FromError::NullaryVariantTagOutOfRange { max, actual: value })
        }
    }

    pub fn field<T: FromOcamlRep>(block: Block<'_>, field: usize) -> Result<T, FromError> {
        T::from_ocamlrep(block[field]).map_err(|e| FromError::ErrorInField(field, Box::new(e)))
    }
}

// ---
//

mod impls {
    use crate::from;
    use crate::{
        block, Allocator, FromError, FromOcamlRep, FromOcamlRepIn, ToOcamlRep, Value, Value_,
    };
    use bumpalo::Bump;
    use std::convert::TryInto;

    macro_rules! trivial_from_in_impl {
        ($ty:ty) => {
            impl<'a> FromOcamlRepIn<'a> for $ty {
                fn from_ocamlrep_in(value: Value<'_>, _alloc: &'a Bump) -> Result<Self, FromError> {
                    Self::from_ocamlrep(value)
                }
            }
        };
    }

    const WORD_SIZE: usize = std::mem::size_of::<Value<'_>>();

    // unit

    impl ToOcamlRep for () {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, _alloc: &'a A) -> Value_<'a> {
            Value_::int(0)
        }
    }

    impl FromOcamlRep for () {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            match from::expect_int(value)? {
                0 => Ok(()),
                i => Err(FromError::ExpectedUnit(i)),
            }
        }
    }

    trivial_from_in_impl!(());

    // isize

    impl ToOcamlRep for isize {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, _alloc: &'a A) -> Value_<'a> {
            Value_::int(*self)
        }
    }

    impl FromOcamlRep for isize {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            from::expect_int(value)
        }
    }

    trivial_from_in_impl!(isize);

    // usize

    impl ToOcamlRep for usize {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, _alloc: &'a A) -> Value_<'a> {
            Value_::int((*self).try_into().unwrap())
        }
    }

    impl FromOcamlRep for usize {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            Ok(from::expect_int(value)?.try_into()?)
        }
    }

    trivial_from_in_impl!(usize);

    // i64, u64, i32, u32

    // ...

    // bool

    impl ToOcamlRep for bool {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, _alloc: &'a A) -> Value_<'a> {
            Value_::int((*self).into())
        }
    }

    impl FromOcamlRep for bool {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            match from::expect_int(value)? {
                0 => Ok(false),
                1 => Ok(true),
                i => Err(FromError::ExpectedBool(i)),
            }
        }
    }

    trivial_from_in_impl!(bool);

    // ---
    //

    impl ToOcamlRep for char {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, _alloc: &'a A) -> Value_<'a> {
            assert!(*self as u32 <= 255, "char out of range: {}", self);
            Value_::int(*self as isize)
        }
    }

    impl FromOcamlRep for char {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            let c = from::expect_int(value)?;
            if (0..=255).contains(&c) {
                Ok(c as u8 as char)
            } else {
                Err(FromError::ExpectedChar(c))
            }
        }
    }

    trivial_from_in_impl!(char);

    // ---
    //

    impl ToOcamlRep for f64 {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, alloc: &'a A) -> Value_<'a> {
            let mut block = alloc.block_with_size_and_tag(1, block::DOUBLE_TAG);
            alloc.set_field(&mut block, 0, unsafe {
                Value_::from_bits(self.to_bits() as usize)
            });
            block.build()
        }
    }

    impl FromOcamlRep for f64 {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            let block = from::expect_block_with_size_and_tag(value, 1, block::DOUBLE_TAG)?;
            Ok(f64::from_bits(block[0].0 as u64))
        }
    }

    trivial_from_in_impl!(f64);

    // ---

    impl<T: ToOcamlRep> ToOcamlRep for Box<T> {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, alloc: &'a A) -> Value_<'a> {
            alloc.add(&**self)
        }
    }

    impl<T: FromOcamlRep> FromOcamlRep for Box<T> {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            Ok(Box::new(T::from_ocamlrep(value)?))
        }
    }

    // ---
    //

    // &T, Rc<T>, Arc<T>, RefCell<T>, Cell<T>

    // ---
    //

    impl<T: ToOcamlRep> ToOcamlRep for Option<T> {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, alloc: &'a A) -> Value_<'a> {
            match self {
                None => Value_::int(0),
                Some(val) => {
                    let mut block = alloc.block_with_size(1);
                    alloc.set_field(&mut block, 0, alloc.add(val));
                    block.build()
                }
            }
        }
    }

    impl<T: FromOcamlRep> FromOcamlRep for Option<T> {
        fn from_ocamlrep(value: Value<'_>) -> Result<Self, FromError> {
            if value.is_immediate() {
                let _ = from::expect_nullary_variant(value, 0)?;
                Ok(None)
            } else {
                let block = from::expect_block_with_size_and_tag(value, 1, 0)?;
                Ok(Some(from::field(block, 0)?))
            }
        }
    }

    // ----
    // Result<T, E>, [T]

    // ---

    impl<T: ToOcamlRep> ToOcamlRep for [T] {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, alloc: &'a A) -> Value_<'a> {
            let mut hd = alloc.add(&());
            for val in self.iter().rev() {
                let mut block = alloc.block_with_size(2);
                alloc.set_field(&mut block, 0, alloc.add(val));
                alloc.set_field(&mut block, 1, hd);
                hd = block.build();
            }
            hd
        }
    }

    // ---
    // &'_ [T], Vec<T>, BTreeMap<K, V>, BTreeSet<T>

    // ---

    pub fn bytes_to_ocamlrep<'a, A: Allocator>(s: &[u8], alloc: &'a A) -> Value_<'a> {
        let words = (s.len() + 1 /* null-ending */ + (WORD_SIZE - 1)/* rounding */) / WORD_SIZE;
        let length = words * WORD_SIZE;
        let mut block = alloc.block_with_size_and_tag(words, block::STRING_TAG);
        let block_contents_as_slice: &mut [u8] = unsafe {
            let block = alloc.block_ptr_mut(&mut block);
            *block.add(words - 1) = Value_::from_bits(0);
            let block_bytes = block as *mut u8;
            *block_bytes.add(length - 1) = (length - s.len() - 1) as u8;
            std::slice::from_raw_parts_mut(block_bytes, s.len())
        };
        block_contents_as_slice.copy_from_slice(s);
        block.build()
    }

    pub fn str_to_ocamlrep<'a, A: Allocator>(s: &str, alloc: &'a A) -> Value_<'a> {
        bytes_to_ocamlrep(s.as_bytes(), alloc)
    }

    impl ToOcamlRep for str {
        fn to_ocamlrep<'a, A: Allocator>(&'a self, alloc: &'a A) -> Value_<'a> {
            str_to_ocamlrep(self, alloc)
        }
    }

    // ---
    // Vec<u8>, &'_ str, String, Cow<_, str>, PathBuf, Path, &'a Path, &'_ Path, Path, OsString, &'a OsStr, &_ OsStr, BStr, [u8], &'a [u8]

    // ---
    // (T0, T1), (T0, T1, T2) , ....
}
