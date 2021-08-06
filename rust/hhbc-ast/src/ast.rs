#![allow(non_camel_case_types)]
#![allow(dead_code)]

use std::cmp::Ordering;

mod ffi {
    use std::cmp::Ordering;
    use std::hash::{Hash, Hasher};

    #[derive(Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
    #[repr(C)]
    pub enum Maybe<T> {
        Just(T),
        Nothing,
    }
    use self::Maybe::*;
    impl<T: Clone> Clone for Maybe<T> {
        #[inline]
        fn clone(&self) -> Self {
            match self {
                Just(x) => Just(x.clone()),
                Nothing => Nothing,
            }
        }
        #[inline]
        fn clone_from(&mut self, source: &Self) {
            match (self, source) {
                (Just(to), Just(from)) => to.clone_from(from),
                (to, from) => *to = from.clone(),
            }
        }
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(C)]
    pub struct Pair<U, V>(pub U, pub V);

    #[derive(Debug)]
    #[repr(C)]
    pub struct SliceMut<'a, T> {
        pub data: *mut T,
        pub len: usize,
        pub alloc: usize, // *const bumpalo::Bump
        pub marker: std::marker::PhantomData<&'a ()>,
    }
    impl<'a, T> SliceMut<'a, T> {
        pub fn new(alloc: &'a bumpalo::Bump, t: &'a mut [T]) -> Self {
            SliceMut {
                data: t.as_mut_ptr(),
                len: t.len(),
                alloc: alloc as *const bumpalo::Bump as usize,
                marker: std::marker::PhantomData,
            }
        }
    }
    impl<'a, T> AsRef<[T]> for SliceMut<'a, T> {
        fn as_ref<'r>(&'r self) -> &'r [T] {
            unsafe { std::slice::from_raw_parts(self.data, self.len) }
        }
    }
    impl<'a, T> AsMut<[T]> for SliceMut<'a, T> {
        fn as_mut<'r>(&'r mut self) -> &'r mut [T] {
            unsafe { std::slice::from_raw_parts_mut(self.data, self.len) }
        }
    }
    impl<'a, T: 'a + Clone> Clone for SliceMut<'a, T> {
        fn clone(&self) -> Self {
            let alloc: &'a bumpalo::Bump =
                unsafe { (self.alloc as *const bumpalo::Bump).as_ref().unwrap() };
            SliceMut::new(alloc, alloc.alloc_slice_clone(self.as_ref()))
        }
    }

    #[derive(Clone, Copy, Debug)]
    #[repr(C)]
    pub struct Slice<'arena, T> {
        pub data: *const T,
        pub len: usize,
        pub marker: std::marker::PhantomData<&'arena ()>,
    }
    impl<'a, T: 'a> Slice<'a, T> {
        fn new(t: &'a [T]) -> Slice<'a, T> {
            Slice {
                data: t.as_ptr(),
                len: t.len(),
                marker: std::marker::PhantomData,
            }
        }
    }
    impl<'a, T> AsRef<[T]> for Slice<'a, T> {
        fn as_ref(&self) -> &[T] {
            unsafe { std::slice::from_raw_parts(self.data, self.len) }
        }
    }
    impl<'arena, T: PartialEq> PartialEq for Slice<'arena, T> {
        fn eq(&self, other: &Self) -> bool {
            unsafe {
                let left = std::slice::from_raw_parts(self.data, self.len);
                let right = std::slice::from_raw_parts(other.data, other.len);
                left.eq(right)
            }
        }
    }
    impl<'arena, T: Eq> Eq for Slice<'arena, T> {}
    impl<'arena, T: Hash> Hash for Slice<'arena, T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            unsafe {
                let me = std::slice::from_raw_parts(self.data, self.len);
                me.hash(state);
            }
        }
    }
    impl<'arena, T: Ord> Ord for Slice<'arena, T> {
        fn cmp(&self, other: &Self) -> Ordering {
            unsafe {
                let left = std::slice::from_raw_parts(self.data, self.len);
                let right = std::slice::from_raw_parts(other.data, other.len);
                left.cmp(right)
            }
        }
    }
    impl<'arena, T: PartialOrd> PartialOrd for Slice<'arena, T> {
        fn partial_cmp(&self, other: &Self) -> std::option::Option<Ordering> {
            unsafe {
                let left = std::slice::from_raw_parts(self.data, self.len);
                let right = std::slice::from_raw_parts(other.data, other.len);
                left.partial_cmp(right)
            }
        }
    }

    pub type Str<'arena> = Slice<'arena, u8>;
    // C++:
    // std::string slice_to_string(Str s) {
    //    std::string {s.data, s.data + s.len}
    // }
}

use ffi::{Maybe, Pair, Slice, SliceMut, Str};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum CheckStarted {
    IgnoreStarted,
    CheckStarted,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum FreeIterator {
    IgnoreIter,
    FreeIter,
}

pub type RepoAuthType<'arena> = Str<'arena>;

#[derive(Clone, Debug)]
#[repr(C)]
pub enum ParamId<'arena> {
    ParamUnnamed(isize),
    ParamNamed(Str<'arena>),
}

pub type ParamNum = isize;
pub type StackIndex = isize;
pub type RecordNum = isize;
pub type TypedefNum = isize;
pub type ClassNum = isize;
pub type ConstNum = isize;

mod hhbc_by_ref_id {
    pub mod class {
        use super::super::Str;
        //hhbc_id::class::Type<'arena>
        #[derive(Copy, Clone, Debug)]
        #[repr(C)]
        pub struct Type<'arena>(pub Str<'arena>);
    }
    pub mod function {
        use super::super::Str;
        //hhbc_id::function::Type<'arena>
        #[derive(Copy, Clone, Debug)]
        #[repr(C)]
        pub struct Type<'arena>(pub Str<'arena>);
    }
    pub mod method {
        use super::super::Str;
        //hhbc_id::method::Type<'arena>
        #[derive(Copy, Clone, Debug)]
        #[repr(C)]
        pub struct Type<'arena>(pub Str<'arena>);
    }
    pub mod prop {
        use super::super::Str;
        //hhbc_id::prop::Type<'arena>
        #[derive(Copy, Clone, Debug)]
        #[repr(C)]
        pub struct Type<'arena>(pub Str<'arena>);
    }
    pub mod r#const {
        use super::super::Str;
        //hhbc_id::r#const::Type<'arena>
        #[derive(Copy, Clone, Debug)]
        #[repr(C)]
        pub struct Type<'arena>(pub Str<'arena>);
    }
    pub mod record {
        use super::super::Str;
        //hhbc_id::record::Type<'arena>
        #[derive(Copy, Clone, Debug)]
        #[repr(C)]
        pub struct Type<'arena>(pub Str<'arena>);
    }
}

pub type ClassId<'arena> = hhbc_by_ref_id::class::Type<'arena>;
pub type FunctionId<'arena> = hhbc_by_ref_id::function::Type<'arena>;
pub type MethodId<'arena> = hhbc_by_ref_id::method::Type<'arena>;
pub type ConstId<'arena> = hhbc_by_ref_id::method::Type<'arena>;
pub type PropId<'arena> = hhbc_by_ref_id::prop::Type<'arena>;

pub type NumParams = usize;

pub type ByRefs<'arena> = Slice<'arena, bool>;

bitflags::bitflags! {
    #[repr(C)]
    pub struct FcallFlags: u8 {
        const HAS_UNPACK =                  0b0001;
        const HAS_GENERICS =                0b0010;
        const LOCK_WHILE_UNWINDING =        0b0100;
    }
}

mod hhbc_by_ref_label {

    pub type Id = usize;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, std::cmp::Ord, std::cmp::PartialOrd)]
    #[repr(C)]
    pub enum Label {
        Regular(Id),
        DefaultArg(Id),
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct FcallArgs<'arena>(
    pub FcallFlags,
    pub NumParams,
    pub NumParams,
    pub ByRefs<'arena>,
    pub Maybe<hhbc_by_ref_label::Label>,
    pub Maybe<Str<'arena>>,
);

mod iterator {
    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub struct Id(pub usize);
}

mod local {
    pub type Id = usize;

    use super::Str;

    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub enum Type<'arena> {
        Unnamed(Id),
        Named(Str<'arena>),
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct IterArgs<'arena> {
    pub iter_id: iterator::Id,
    pub key_id: Maybe<local::Type<'arena>>,
    pub val_id: local::Type<'arena>,
}

pub type ClassrefId = isize;
/// Conventionally this is "A_" followed by an integer
pub type AdataId<'arena> = Str<'arena>; //&'arena str;
pub type ParamLocations<'arena> = Slice<'arena, isize>; //&'arena [isize];

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum SpecialClsRef {
    Static,
    Self_,
    Parent,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum MemberOpMode {
    ModeNone,
    Warn,
    Define,
    Unset,
    InOut,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum QueryOp {
    CGet,
    CGetQuiet,
    Isset,
    InOut,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum CollectionType {
    Vector,
    Map,
    Set,
    Pair,
    ImmVector,
    ImmMap,
    ImmSet,
    Dict,
    Array,
    Keyset,
    Vec,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum FatalOp {
    Parse,
    Runtime,
    RuntimeOmitFrame,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub enum MemberKey<'arena> {
    EC(StackIndex, ReadOnlyOp),
    EL(local::Type<'arena>, ReadOnlyOp),
    ET(Str<'arena>, ReadOnlyOp),
    EI(i64, ReadOnlyOp),
    PC(StackIndex, ReadOnlyOp),
    PL(local::Type<'arena>, ReadOnlyOp),
    PT(PropId<'arena>, ReadOnlyOp),
    QT(PropId<'arena>, ReadOnlyOp),
    W,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum InstructBasic {
    Nop,
    EntryNop,
    PopC,
    PopU,
    Dup,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum TypestructResolveOp {
    Resolve,
    DontResolve,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum ReadOnlyOp {
    ReadOnly,
    Mutable,
    Any,
    CheckROCOW,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum HasGenericsOp {
    NoGenerics,
    MaybeGenerics,
    HasGenerics,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum IsLogAsDynamicCallOp {
    LogAsDynamicCall,
    DontLogAsDynamicCall,
}

mod hhbc_by_ref_runtime {
    mod float {
        #[derive(Clone, Copy, Debug, Hash, PartialOrd, Ord, PartialEq, Eq)]
        #[repr(C)]
        pub struct F64([u8; 8]);
    }

    use super::Pair;
    use super::Slice;
    use super::Str;

    #[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
    #[repr(C)]
    pub enum TypedValue<'arena> {
        /// Used for fields that are initialized in the 86pinit method
        Uninit,
        /// Hack/PHP integers are 64-bit
        Int(i64),
        Bool(bool),
        /// Both Hack/PHP and Caml floats are IEEE754 64-bit
        Float(float::F64),
        String(Str<'arena>),
        LazyClass(Str<'arena>),
        Null,
        // Classic PHP arrays with explicit (key,value) entries
        HhasAdata(Str<'arena>),
        // Hack arrays: vectors, keysets, and dictionaries
        Vec(Slice<'arena, TypedValue<'arena>>),
        Keyset(Slice<'arena, TypedValue<'arena>>),
        Dict(Slice<'arena, Pair<TypedValue<'arena>, TypedValue<'arena>>>),
    }
}

#[derive(Debug)]
#[repr(C)]
pub enum InstrSeq<'a> {
    List(SliceMut<'a, Instruct<'a>>),
    Concat(SliceMut<'a, InstrSeq<'a>>),
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructLitConst<'arena> {
    Null,
    True,
    False,
    NullUninit,
    Int(i64),
    Double(Str<'arena>),
    String(Str<'arena>),
    LazyClass(ClassId<'arena>),
    /// Pseudo instruction that will get translated into appropraite
    /// literal bytecode, with possible reference to .adata *)
    TypedValue(hhbc_by_ref_runtime::TypedValue<'arena>),
    Vec(AdataId<'arena>),
    Dict(AdataId<'arena>),
    Keyset(AdataId<'arena>),
    /// capacity hint
    NewDictArray(isize),
    NewStructDict(Slice<'arena, Str<'arena>>),
    NewVec(isize),
    NewKeysetArray(isize),
    NewPair,
    NewRecord(ClassId<'arena>, Slice<'arena, Str<'arena>>),
    AddElemC,
    AddNewElemC,
    NewCol(CollectionType),
    ColFromArray(CollectionType),
    CnsE(ConstId<'arena>),
    ClsCns(ConstId<'arena>),
    ClsCnsD(ConstId<'arena>, ClassId<'arena>),
    ClsCnsL(local::Type<'arena>),
    File,
    Dir,
    Method,
    FuncCred,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructOperator<'arena> {
    Concat,
    ConcatN(isize),
    Add,
    Sub,
    Mul,
    AddO,
    SubO,
    MulO,
    Div,
    Mod,
    Pow,
    Not,
    Same,
    NSame,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Cmp,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    Shl,
    Shr,
    CastBool,
    CastInt,
    CastDouble,
    CastString,
    CastVec,
    CastDict,
    CastKeyset,
    InstanceOf,
    InstanceOfD(ClassId<'arena>),
    IsLateBoundCls,
    IsTypeStructC(TypestructResolveOp),
    ThrowAsTypeStructException,
    CombineAndResolveTypeStruct(isize),
    Print,
    Clone,
    Exit,
    Fatal(FatalOp),
    ResolveFunc(FunctionId<'arena>),
    ResolveRFunc(FunctionId<'arena>),
    ResolveMethCaller(FunctionId<'arena>),
    ResolveObjMethod,
    ResolveClsMethod(MethodId<'arena>),
    ResolveClsMethodD(ClassId<'arena>, MethodId<'arena>),
    ResolveClsMethodS(SpecialClsRef, MethodId<'arena>),
    ResolveRClsMethod(MethodId<'arena>),
    ResolveRClsMethodD(ClassId<'arena>, MethodId<'arena>),
    ResolveRClsMethodS(SpecialClsRef, MethodId<'arena>),
    ResolveClass(ClassId<'arena>),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum Switchkind {
    Bounded,
    Unbounded,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructControlFlow<'arena> {
    Jmp(hhbc_by_ref_label::Label),
    JmpNS(hhbc_by_ref_label::Label),
    JmpZ(hhbc_by_ref_label::Label),
    JmpNZ(hhbc_by_ref_label::Label),
    /// bounded, base, offset vector
    Switch(
        Switchkind,
        isize,
        SliceMut<'arena, hhbc_by_ref_label::Label>, //bumpalo::collections::Vec<'arena, hhbc_by_ref_label::Label>,
    ),
    /// litstr id / offset vector
    SSwitch(SliceMut<'arena, Pair<Str<'arena>, hhbc_by_ref_label::Label>>), //bumpalo::collections::Vec<'arena, (&'arena str, hhbc_by_ref_label::Label)>),
    RetC,
    RetCSuspended,
    RetM(NumParams),
    Throw,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructSpecialFlow<'arena> {
    Continue(isize),
    Break(isize),
    Goto(Str<'arena>),
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructGet<'arena> {
    CGetL(local::Type<'arena>),
    CGetQuietL(local::Type<'arena>),
    CGetL2(local::Type<'arena>),
    CUGetL(local::Type<'arena>),
    PushL(local::Type<'arena>),
    CGetG,
    CGetS(ReadOnlyOp),
    ClassGetC,
    ClassGetTS,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum IstypeOp {
    OpNull,
    OpBool,
    OpInt,
    OpDbl,
    OpStr,
    OpObj,
    OpRes,
    OpScalar,
    /// Int or Dbl or Str or Bool
    OpKeyset,
    OpDict,
    OpVec,
    OpArrLike,
    /// Arr or Vec or Dict or Keyset *)
    OpClsMeth,
    OpFunc,
    OpLegacyArrLike,
    OpClass,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructIsset<'arena> {
    IssetC,
    IssetL(local::Type<'arena>),
    IssetG,
    IssetS,
    IsUnsetL(local::Type<'arena>),
    IsTypeC(IstypeOp),
    IsTypeL(local::Type<'arena>, IstypeOp),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum SetrangeOp {
    Forward,
    Reverse,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum EqOp {
    PlusEqual,
    MinusEqual,
    MulEqual,
    ConcatEqual,
    DivEqual,
    PowEqual,
    ModEqual,
    AndEqual,
    OrEqual,
    XorEqual,
    SlEqual,
    SrEqual,
    PlusEqualO,
    MinusEqualO,
    MulEqualO,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum IncdecOp {
    PreInc,
    PostInc,
    PreDec,
    PostDec,
    PreIncO,
    PostIncO,
    PreDecO,
    PostDecO,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum InitpropOp {
    Static,
    NonStatic,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructMutator<'arena> {
    SetL(local::Type<'arena>),
    /// PopL is put in mutators since it behaves as SetL + PopC
    PopL(local::Type<'arena>),
    SetG,
    SetS(ReadOnlyOp),
    SetOpL(local::Type<'arena>, EqOp),
    SetOpG(EqOp),
    SetOpS(EqOp),
    IncDecL(local::Type<'arena>, IncdecOp),
    IncDecG(IncdecOp),
    IncDecS(IncdecOp),
    UnsetL(local::Type<'arena>),
    UnsetG,
    CheckProp(PropId<'arena>),
    InitProp(PropId<'arena>, InitpropOp),
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum ObjNullFlavor {
    NullThrows,
    NullSafe,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructCall<'arena> {
    NewObj,
    NewObjR,
    NewObjD(ClassId<'arena>),
    NewObjRD(ClassId<'arena>),
    NewObjS(SpecialClsRef),
    FCall(FcallArgs<'arena>),
    FCallClsMethod(FcallArgs<'arena>, IsLogAsDynamicCallOp),
    FCallClsMethodD(FcallArgs<'arena>, ClassId<'arena>, MethodId<'arena>),
    FCallClsMethodS(FcallArgs<'arena>, SpecialClsRef),
    FCallClsMethodSD(FcallArgs<'arena>, SpecialClsRef, MethodId<'arena>),
    FCallCtor(FcallArgs<'arena>),
    FCallFunc(FcallArgs<'arena>),
    FCallFuncD(FcallArgs<'arena>, FunctionId<'arena>),
    FCallObjMethod(FcallArgs<'arena>, ObjNullFlavor),
    FCallObjMethodD(FcallArgs<'arena>, ObjNullFlavor, MethodId<'arena>),
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructBase<'arena> {
    BaseGC(StackIndex, MemberOpMode),
    BaseGL(local::Type<'arena>, MemberOpMode),
    BaseSC(StackIndex, StackIndex, MemberOpMode, ReadOnlyOp),
    BaseL(local::Type<'arena>, MemberOpMode),
    BaseC(StackIndex, MemberOpMode),
    BaseH,
    Dim(MemberOpMode, MemberKey<'arena>),
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructFinal<'arena> {
    QueryM(NumParams, QueryOp, MemberKey<'arena>),
    SetM(NumParams, MemberKey<'arena>),
    IncDecM(NumParams, IncdecOp, MemberKey<'arena>),
    SetOpM(NumParams, EqOp, MemberKey<'arena>),
    UnsetM(NumParams, MemberKey<'arena>),
    SetRangeM(NumParams, isize, SetrangeOp),
}

mod hhbc_by_ref_iterator {
    #[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
    #[repr(C)]
    pub struct Id(pub usize);
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructIterator<'arena> {
    IterInit(IterArgs<'arena>, hhbc_by_ref_label::Label),
    IterNext(IterArgs<'arena>, hhbc_by_ref_label::Label),
    IterFree(hhbc_by_ref_iterator::Id),
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructIncludeEvalDefine {
    Incl,
    InclOnce,
    Req,
    ReqOnce,
    ReqDoc,
    Eval,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum BareThisOp {
    Notice,
    NoNotice,
    NeverNull,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum ClassKind {
    Class,
    Interface,
    Trait,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum OpSilence {
    Start,
    End,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum InstructMisc<'arena> {
    This,
    BareThis(BareThisOp),
    CheckThis,
    FuncNumArgs,
    ChainFaults,
    OODeclExists(ClassKind),
    VerifyParamType(ParamId<'arena>),
    VerifyParamTypeTS(ParamId<'arena>),
    VerifyOutType(ParamId<'arena>),
    VerifyRetTypeC,
    VerifyRetTypeTS,
    Self_,
    Parent,
    LateBoundCls,
    ClassName,
    LazyClassFromClass,
    RecordReifiedGeneric,
    CheckReifiedGenericMismatch,
    NativeImpl,
    AKExists,
    CreateCl(NumParams, ClassNum),
    Idx,
    ArrayIdx,
    ArrayMarkLegacy,
    ArrayUnmarkLegacy,
    AssertRATL(local::Type<'arena>, RepoAuthType<'arena>),
    AssertRATStk(StackIndex, RepoAuthType<'arena>),
    BreakTraceHint,
    Silence(local::Type<'arena>, OpSilence),
    GetMemoKeyL(local::Type<'arena>),
    CGetCUNop,
    UGetCUNop,
    MemoGet(
        hhbc_by_ref_label::Label,
        Maybe<Pair<local::Type<'arena>, isize>>,
    ),
    MemoGetEager(
        hhbc_by_ref_label::Label,
        hhbc_by_ref_label::Label,
        Maybe<Pair<local::Type<'arena>, isize>>,
    ),
    MemoSet(Maybe<Pair<local::Type<'arena>, isize>>),
    MemoSetEager(Maybe<Pair<local::Type<'arena>, isize>>),
    LockObj,
    ThrowNonExhaustiveSwitch,
    RaiseClassStringConversionWarning,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum GenCreationExecution {
    CreateCont,
    ContEnter,
    ContRaise,
    Yield,
    YieldK,
    ContCheck(CheckStarted),
    ContValid,
    ContKey,
    ContGetReturn,
    ContCurrent,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum AsyncFunctions<'arena> {
    WHResult,
    Await,
    AwaitAll(Maybe<Pair<local::Type<'arena>, isize>>),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(C)]
pub enum InstructTry {
    TryCatchBegin,
    TryCatchMiddle,
    TryCatchEnd,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct Srcloc {
    pub line_begin: isize,
    pub col_begin: isize,
    pub line_end: isize,
    pub col_end: isize,
}

#[derive(Clone, Debug)]
#[repr(C)]
pub enum Instruct<'arena> {
    IBasic(InstructBasic),
    IIterator(InstructIterator<'arena>),
    ILitConst(InstructLitConst<'arena>),
    IOp(InstructOperator<'arena>),
    IContFlow(InstructControlFlow<'arena>),
    ISpecialFlow(InstructSpecialFlow<'arena>),
    ICall(InstructCall<'arena>),
    IMisc(InstructMisc<'arena>),
    IGet(InstructGet<'arena>),
    IMutator(InstructMutator<'arena>),
    IIsset(InstructIsset<'arena>),
    IBase(InstructBase<'arena>),
    IFinal(InstructFinal<'arena>),
    ILabel(hhbc_by_ref_label::Label),
    ITry(InstructTry),
    IComment(Str<'arena>),
    ISrcLoc(Srcloc),
    IAsync(AsyncFunctions<'arena>),
    IGenerator(GenCreationExecution),
    IIncludeEvalDefine(InstructIncludeEvalDefine),
}

// --

#[no_mangle]
pub unsafe extern "C" fn foo_07<'a, 'arena>(
    _: FcallFlags,
    _: CheckStarted,
    _: FreeIterator,
    _: Str<'arena>,
    _: RepoAuthType<'arena>,
    _: ParamId<'arena>,
    _: ParamNum,
    _: StackIndex,
    _: RecordNum,
    _: TypedefNum,
    _: ClassNum,
    _: ClassId<'arena>,
    _: FunctionId<'arena>,
    _: MethodId<'arena>,
    _: ConstId<'arena>,
    _: PropId<'arena>,
    _: NumParams,
    _: ByRefs<'arena>,
    _: hhbc_by_ref_label::Label,
    _: FcallArgs<'arena>,
    _: iterator::Id,
    _: local::Type<'arena>,
    _: IterArgs<'arena>,
    _: ClassrefId,
    _: AdataId<'arena>,
    _: ParamLocations<'arena>,
    _: FatalOp,
    _: CollectionType,
    _: QueryOp,
    _: MemberOpMode,
    _: SpecialClsRef,
    _: MemberKey<'arena>,
    _: InstructBasic,
    _: TypestructResolveOp,
    _: ReadOnlyOp,
    _: HasGenericsOp,
    _: IsLogAsDynamicCallOp,
    _: hhbc_by_ref_runtime::TypedValue<'arena>,
    _: InstructLitConst<'arena>,
    _: InstructOperator<'arena>,
    _: Switchkind,
    _: InstructControlFlow<'arena>,
    _: InstructSpecialFlow<'arena>,
    _: InstructGet<'arena>,
    _: IstypeOp,
    _: InstructIsset<'arena>,
    _: SetrangeOp,
    _: EqOp,
    _: InitpropOp,
    _: InstructMutator<'arena>,
    _: ObjNullFlavor,
    _: InstructCall<'arena>,
    _: InstructBase<'arena>,
    _: InstructFinal<'arena>,
    _: hhbc_by_ref_iterator::Id,
    _: InstructIterator<'arena>,
    _: BareThisOp,
    _: ClassKind,
    _: OpSilence,
    _: InstructMisc<'arena>,
    _: GenCreationExecution,
    _: AsyncFunctions<'arena>,
    _: InstructTry,
    _: Srcloc,
    _: Instruct<'arena>,
    _: InstrSeq<'arena>,
) {
    unimplemented!()
}

// --

#[derive(Debug)]
pub struct DocComment(pub String); // pub struct DocComment(pub Rc<Pstring>);

#[derive(Debug)]
pub struct HhasAdata<'arena> {
    pub id: String,
    pub value: hhbc_by_ref_runtime::TypedValue<'arena>,
}

mod hhas_pos {
    /// Span, emitted as prefix to classes and functions
    #[derive(Clone, Copy, Debug, Default)]
    pub struct Span(pub usize, pub usize);
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ctx {
    Defaults,
    // Shared
    WriteThisProps,
    WriteProps,
    // Rx hierarchy
    RxLocal,
    RxShallow,
    Rx,
    // Policied hierarchy
    PoliciedOfLocal,
    PoliciedOfShallow,
    PoliciedOf,
    PoliciedLocal,
    PoliciedShallow,
    Policied,
    Controlled,
    ReadGlobals,
    Globals,
    // Pure
    Pure,
}

#[derive(Debug)]
pub struct HhasCtxConstant {
    pub name: String,
    pub coeffects: Vec<Ctx>,
    pub is_abstract: bool,
}

#[derive(Clone, Debug, Default)]
pub struct HhasCoeffects {
    static_coeffects: Vec<Ctx>,
    unenforced_static_coeffects: Vec<String>,
    fun_param: Vec<usize>,
    cc_param: Vec<(usize, String)>,
    cc_this: Vec<Vec<String>>,
    cc_reified: Vec<(bool, usize, Vec<String>)>,
    closure_parent_scope: bool,
    generator_this: bool,
    caller: bool,
}

#[derive(Default, Debug)]
pub struct HhasBodyEnv {
    pub is_namespaced: bool,
    //pub class_info: Option<(ast_defs::ClassKind, String)>,
    pub parent_name: Option<String>,
}

mod hhas_type {
    #[derive(Clone, Debug)]
    pub struct Info {
        pub user_type: Option<String>,
        pub type_constraint: constraint::Type,
    }
    pub mod constraint {
        use bitflags::bitflags;

        #[derive(Clone, Default, Debug)]
        pub struct Type {
            pub name: Option<String>,
            pub flags: Flags,
        }

        bitflags! {
            #[derive(Default)]
            pub struct Flags: u8 {
                const NULLABLE =         0b0000_0001;
                const EXTENDED_HINT =    0b0000_0100;
                const TYPE_VAR =         0b0000_1000;
                const SOFT =             0b0001_0000;
                const TYPE_CONSTANT =    0b0010_0000;
                const DISPLAY_NULLABLE = 0b0100_0000;
                const UPPERBOUND =       0b1000_0000;
            }
        }
    }
}

#[derive(Debug)] //Cannot be Default...
pub struct HhasBody<'arena> {
    pub body_instrs: InstrSeq<'arena>, //... because InstrSeq not Default.
    pub decl_vars: Vec<String>,
    pub num_iters: usize,
    pub num_closures: u32,
    pub is_memoize_wrapper: bool,
    pub is_memoize_wrapper_lsb: bool,
    pub upper_bounds: Vec<(String, Vec<hhas_type::Info>)>,
    pub shadowed_tparams: Vec<String>,
    pub params: Vec<HhasParam<'arena>>,
    pub return_type_info: Option<hhas_type::Info>,
    pub doc_comment: Option<DocComment>,
    pub env: Option<HhasBodyEnv>,
}

mod hhas_function {
    bitflags::bitflags! {
        pub struct Flags: u8 {
            const ASYNC =          1 << 1;
            const GENERATOR =      1 << 2;
            const PAIR_GENERATOR = 1 << 3;
            const NO_INJECTION =   1 << 4;
            const INTERCEPTABLE =  1 << 5;
            const MEMOIZE_IMPL =   1 << 6;
            const RX_DISABLED =    1 << 7;
        }
    }
}

#[derive(Debug)]
pub struct HhasFunction<'arena> {
    pub attributes: Vec<HhasAttribute<'arena>>,
    pub name: hhbc_by_ref_id::function::Type<'arena>,
    pub body: HhasBody<'arena>,
    pub span: hhas_pos::Span,
    pub coeffects: HhasCoeffects,
    pub flags: hhas_function::Flags,
}

#[derive(Debug)]
pub enum TraitReqKind {
    MustExtend,
    MustImplement,
}

mod hhas_property {
    bitflags::bitflags! {
        pub struct HhasPropertyFlags: u16 {
            const IS_ABSTRACT = 1 << 0;
            const IS_STATIC = 1 << 1;
            const IS_DEEP_INIT = 1 << 2;
            const IS_CONST = 1 << 3;
            const IS_LSB = 1 << 4;
            const IS_NO_BAD_REDECLARE = 1 << 5;
            const HAS_SYSTEM_INITIAL = 1 << 6;
            const NO_IMPLICIT_NULL = 1 << 7;
            const INITIAL_SATISFIES_TC = 1 << 8;
            const IS_LATE_INIT = 1 << 9;
            const IS_READONLY = 1 << 10;
        }
    }
}

#[derive(Debug)]
pub struct HhasProperty<'arena> {
    pub name: hhbc_by_ref_id::prop::Type<'arena>,
    pub flags: hhas_property::HhasPropertyFlags,
    pub attributes: Vec<HhasAttribute<'arena>>,
    //pub visibility: Visibility,
    pub initial_value: Option<hhbc_by_ref_runtime::TypedValue<'arena>>,
    pub initializer_instrs: Option<InstrSeq<'arena>>,
    pub type_info: hhas_type::Info,
    pub doc_comment: Option<DocComment>,
}

#[derive(Debug)]
pub struct HhasMethod<'arena> {
    pub attributes: Vec<HhasAttribute<'arena>>,
    //pub visibility: Visibility,
    pub name: hhbc_by_ref_id::method::Type<'arena>,
    pub body: HhasBody<'arena>,
    pub span: hhas_pos::Span,
    pub coeffects: HhasCoeffects,
    pub flags: hhas_method::HhasMethodFlags,
}

mod hhas_method {
    bitflags::bitflags! {
        pub struct HhasMethodFlags: u16 {
            const IS_STATIC = 1 << 1;
            const IS_FINAL = 1 << 2;
            const IS_ABSTRACT = 1 << 3;
            const IS_ASYNC = 1 << 4;
            const IS_GENERATOR = 1 << 5;
            const IS_PAIR_GENERATOR = 1 << 6;
            const IS_CLOSURE_BODY = 1 << 7;
            const IS_INTERCEPTABLE = 1 << 8;
            const IS_MEMOIZE_IMPL = 1 << 9;
            const RX_DISABLED = 1 << 10;
            const NO_INJECTION = 1 << 11;
        }
    }
}

#[derive(Debug)]
pub struct HhasTypeConstant<'arena> {
    pub name: String,
    pub initializer: Option<hhbc_by_ref_runtime::TypedValue<'arena>>,
    pub is_abstract: bool,
}

#[derive(Debug)]
pub struct HhasClass<'a, 'arena> {
    pub attributes: Vec<HhasAttribute<'arena>>,
    pub base: Option<hhbc_by_ref_id::class::Type<'arena>>,
    pub implements: Vec<hhbc_by_ref_id::class::Type<'arena>>,
    pub enum_includes: Vec<hhbc_by_ref_id::class::Type<'arena>>,
    pub name: hhbc_by_ref_id::class::Type<'arena>,
    pub span: hhas_pos::Span,
    pub uses: Vec<&'a str>,
    // Deprecated - kill please
    pub use_aliases: Vec<(
        Option<hhbc_by_ref_id::class::Type<'arena>>,
        hhbc_by_ref_id::class::Type<'arena>,
        Option<hhbc_by_ref_id::class::Type<'arena>>,
        //&'a Vec<tast::UseAsVisibility>,
    )>,
    // Deprecated - kill please
    pub use_precedences: Vec<(
        hhbc_by_ref_id::class::Type<'arena>,
        hhbc_by_ref_id::class::Type<'arena>,
        Vec<hhbc_by_ref_id::class::Type<'arena>>,
    )>,
    pub enum_type: Option<hhas_type::Info>,
    pub methods: Vec<HhasMethod<'arena>>,
    pub properties: Vec<HhasProperty<'arena>>,
    pub constants: Vec<HhasConstant<'arena>>,
    pub type_constants: Vec<HhasTypeConstant<'arena>>,
    pub ctx_constants: Vec<HhasCtxConstant>,
    pub requirements: Vec<(hhbc_by_ref_id::class::Type<'arena>, TraitReqKind)>,
    pub upper_bounds: Vec<(String, Vec<hhas_type::Info>)>,
    pub doc_comment: Option<DocComment>,
    pub flags: HhasClassFlags,
}

bitflags::bitflags! {
    pub struct HhasClassFlags: u16 {
        const IS_FINAL = 1 << 1;
        const IS_SEALED = 1 << 2;
        const IS_ABSTRACT = 1 << 3;
        const IS_INTERFACE = 1 << 4;
        const IS_TRAIT = 1 << 5;
        const IS_XHP = 1 << 6;
        const IS_CONST = 1 << 7;
        const NO_DYNAMIC_PROPS = 1 << 8;
        const NEEDS_NO_REIFIEDINIT = 1 << 9;
    }
}

#[derive(Debug)]
pub struct Field<'a, 'arena>(
    pub &'a str,
    pub hhas_type::Info,
    pub Option<hhbc_by_ref_runtime::TypedValue<'arena>>,
);

#[derive(Debug)]
pub struct HhasRecord<'a, 'arena> {
    pub name: hhbc_by_ref_id::record::Type<'arena>,
    pub is_abstract: bool,
    pub base: Option<hhbc_by_ref_id::record::Type<'arena>>,
    pub fields: Vec<Field<'a, 'arena>>,
    pub span: hhas_pos::Span,
}

#[derive(Clone, Debug)]
pub struct HhasParam<'arena> {
    pub name: String,
    pub is_variadic: bool,
    pub is_inout: bool,
    pub user_attributes: Vec<HhasAttribute<'arena>>,
    pub type_info: Option<hhas_type::Info>,
    // I think about the best we can do is send a pretty-print of the
    // expression here.
    //pub default_value: Option<(Label, tast::Expr)>,
}

#[derive(Debug)]
pub struct Typedef<'arena> {
    pub name: hhbc_by_ref_id::class::Type<'arena>,
    pub attributes: Vec<HhasAttribute<'arena>>,
    pub type_info: hhas_type::Info,
    pub type_structure: hhbc_by_ref_runtime::TypedValue<'arena>,
    pub span: hhas_pos::Span,
}

#[derive(Clone, Debug)]
pub struct HhasAttribute<'arena> {
    pub name: String,
    pub arguments: Vec<hhbc_by_ref_runtime::TypedValue<'arena>>,
}

/// Data structure for keeping track of symbols (and includes) we encounter in
///the course of emitting bytecode for an AST. We split them into these four
/// categories for the sake of HHVM, which has lookup function corresponding to each.
#[derive(Clone, Debug, Default)]
pub struct HhasSymbolRefs {
    pub includes: IncludePathSet,
    pub constants: SSet,
    pub functions: SSet,
    pub classes: SSet,
}

/// NOTE(hrust): order matters (hhbc_hhas write includes in sorted order)
pub type IncludePathSet = std::collections::BTreeSet<IncludePath>;

type SSet = std::collections::BTreeSet<String>;

#[derive(Clone, Debug, Eq)]
pub enum IncludePath {
    Absolute(String),                    // /foo/bar/baz.php
    SearchPathRelative(String),          // foo/bar/baz.php
    IncludeRootRelative(String, String), // $_SERVER['PHP_ROOT'] . "foo/bar/baz.php"
    DocRootRelative(String),
}
impl IncludePath {
    fn extract_str(&self) -> (&str, &str) {
        use IncludePath::*;
        match self {
            Absolute(s) | SearchPathRelative(s) | DocRootRelative(s) => (s, ""),
            IncludeRootRelative(s1, s2) => (s1, s2),
        }
    }
}
impl Ord for IncludePath {
    fn cmp(&self, other: &Self) -> Ordering {
        self.extract_str().cmp(&other.extract_str())
    }
}
impl PartialOrd for IncludePath {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialEq for IncludePath {
    fn eq(&self, other: &Self) -> bool {
        self.extract_str().eq(&other.extract_str())
    }
}

#[derive(Debug)]
pub struct HhasConstant<'arena> {
    pub name: hhbc_by_ref_id::r#const::Type<'arena>,
    pub value: Option<hhbc_by_ref_runtime::TypedValue<'arena>>,
    pub initializer_instrs: Option<InstrSeq<'arena>>,
    pub is_abstract: bool,
}

#[derive(Debug)]
pub enum Pos {
    Pos,
} // oxidized::pos::Pos

#[derive(Default, Debug)]
//#[repr(C)]
pub struct HhasProgram<'a, 'arena> {
    pub adata: Vec<HhasAdata<'arena>>,
    pub functions: Vec<HhasFunction<'arena>>,
    pub classes: Vec<HhasClass<'a, 'arena>>,
    pub record_defs: Vec<HhasRecord<'a, 'arena>>,
    pub typedefs: Vec<Typedef<'arena>>,
    pub file_attributes: Vec<HhasAttribute<'arena>>,
    pub symbol_refs: HhasSymbolRefs,
    pub constants: Vec<HhasConstant<'arena>>,
    pub fatal: Option<(FatalOp, Pos, String)>,
}

/*
#[derive(Debug)]
#[repr(C)]
pub struct HhasProgram<'arena> {
    pub adata: Slice<'arena, HhasAdata<'arena>>,
    pub functions: Slice<'arena, HhasFunction<'arena>>,
    pub classes: Slice<'arena, HhasClass<'arena>>,
    pub record_defs: Slice<'arena, HhasRecord<'arena>>,
    pub typedefs: Slice<'arena, Typedef<'arena>>,
    pub file_attributes: Slice<'arena, HhasAttribute<'arena>>,
    pub symbol_refs: HhasSymbolRefs<'arena>,
    pub constants: Slice<'arena, HhasConstant<'arena>>,
    pub fatal: Maybe<(FatalOp, Pos, Str<'arena>)>,
}
*/
