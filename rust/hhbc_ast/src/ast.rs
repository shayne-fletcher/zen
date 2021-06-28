#![allow(non_camel_case_types)]
#![allow(dead_code)]

mod hhbc_ffi {
    use std::cmp::Ordering;
    use std::hash::{Hash, Hasher};
    use std::slice::from_raw_parts;
    use std::slice::from_raw_parts_mut;

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
    pub struct SliceMut<'arena, T> {
        pub data: *mut T,
        pub len: usize,
        pub alloc: &'arena bumpalo::Bump,
    }
    impl<'arena, T: Clone> Clone for SliceMut<'arena, T> {
        fn clone(&self) -> Self {
            unsafe {
                let alloc: &'arena bumpalo::Bump = self.alloc;
                let mut vec = bumpalo::collections::Vec::from_iter_in(
                    from_raw_parts_mut(self.data, self.len).iter().cloned(),
                    alloc,
                );
                let slice = vec.as_mut_slice();
                SliceMut {
                    data: slice.as_mut_ptr(),
                    len: slice.len(),
                    alloc: self.alloc,
                }
            }
        }
    }
    impl<'arena, T: Copy> Copy for SliceMut<'arena, T> {}

    #[derive(Clone, Copy, Debug)]
    #[repr(C)]
    pub struct Slice<'arena, T> {
        pub data: *const T,
        pub len: usize,
        pub marker: std::marker::PhantomData<&'arena ()>,
    }
    impl<'arena, T: PartialEq> PartialEq for Slice<'arena, T> {
        fn eq(&self, other: &Self) -> bool {
            unsafe {
                let left = from_raw_parts(self.data, self.len);
                let right = from_raw_parts(other.data, other.len);
                left.eq(right)
            }
        }
    }
    impl<'arena, T: Eq> Eq for Slice<'arena, T> {}
    impl<'arena, T: Hash> Hash for Slice<'arena, T> {
        fn hash<H: Hasher>(&self, state: &mut H) {
            unsafe {
                let me = from_raw_parts(self.data, self.len);
                me.hash(state);
            }
        }
    }
    impl<'arena, T: Ord> Ord for Slice<'arena, T> {
        fn cmp(&self, other: &Self) -> Ordering {
            unsafe {
                let left = from_raw_parts(self.data, self.len);
                let right = from_raw_parts(other.data, other.len);
                left.cmp(right)
            }
        }
    }
    impl<'arena, T: PartialOrd> PartialOrd for Slice<'arena, T> {
        fn partial_cmp(&self, other: &Self) -> std::option::Option<Ordering> {
            unsafe {
                let left = from_raw_parts(self.data, self.len);
                let right = from_raw_parts(other.data, other.len);
                left.partial_cmp(right)
            }
        }
    }

    fn to_slice<'a, T>(t: &'a [T]) -> Slice<'a, T> {
        Slice {
            data: t.as_ptr(),
            len: t.len(),
            marker: std::marker::PhantomData,
        }
    }

    pub type Str<'arena> = Slice<'arena, u8>;
    // C++:
    // std::string slice_to_string(Str s) {
    //    std::string {s.data, s.data + s.len}
    // }
}

use hhbc_ffi::Maybe;
use hhbc_ffi::Pair;
use hhbc_ffi::Slice;
use hhbc_ffi::SliceMut;
use hhbc_ffi::Str;

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
pub unsafe extern "C" fn foo_07<'arena>(
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
) {
    unimplemented!()
}
