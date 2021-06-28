#![allow(non_camel_case_types)]
#![allow(dead_code)]

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::slice::from_raw_parts;

#[derive(Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
#[repr(C)]
pub enum option_t<T> {
    Just(T),
    Nothing,
}
use option_t::*;
impl<T: Clone> Clone for option_t<T> {
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
pub struct pair<U, V>(pub U, pub V);

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct slice_t<'arena, T> {
    pub data: *const T,
    pub len: usize,
    pub marker: std::marker::PhantomData<&'arena ()>,
}
impl<'arena, T: PartialEq> PartialEq for slice_t<'arena, T> {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            let left = from_raw_parts(self.data, self.len);
            let right = from_raw_parts(other.data, other.len);
            left.eq(right)
        }
    }
}
impl<'arena, T: Eq> Eq for slice_t<'arena, T> {}
impl<'arena, T: Hash> Hash for slice_t<'arena, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe {
            let me = from_raw_parts(self.data, self.len);
            me.hash(state);
        }
    }
}
impl<'arena, T: Ord> Ord for slice_t<'arena, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        unsafe {
            let left = from_raw_parts(self.data, self.len);
            let right = from_raw_parts(other.data, other.len);
            left.cmp(right)
        }
    }
}
impl<'arena, T: PartialOrd> PartialOrd for slice_t<'arena, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unsafe {
            let left = from_raw_parts(self.data, self.len);
            let right = from_raw_parts(other.data, other.len);
            left.partial_cmp(right)
        }
    }
}

fn to_slice_t<'a, T>(t: &'a [T]) -> slice_t<'a, T> {
    slice_t {
        data: t.as_ptr(),
        len: t.len(),
        marker: std::marker::PhantomData,
    }
}

pub type Str<'arena> = slice_t<'arena, u8>;
// C++:
// std::string slice_to_string(Str s) {
//    std::string {s.data, s.data + s.len}
// }

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

mod class {
    use super::Str;
    //hhbc_id::class::Type<'arena>
    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub struct Type<'arena>(pub Str<'arena>);
}
mod function {
    use super::Str;
    //hhbc_id::function::Type<'arena>
    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub struct Type<'arena>(pub Str<'arena>);
}
mod method {
    use super::Str;
    //hhbc_id::method::Type<'arena>
    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub struct Type<'arena>(pub Str<'arena>);
}
mod prop {
    use super::Str;
    //hhbc_id::prop::Type<'arena>
    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub struct Type<'arena>(pub Str<'arena>);
}
mod r#const {
    use super::Str;
    //hhbc_id::r#const::Type<'arena>
    #[derive(Copy, Clone, Debug)]
    #[repr(C)]
    pub struct Type<'arena>(pub Str<'arena>);
}
pub type ClassId<'arena> = class::Type<'arena>;
pub type FunctionId<'arena> = function::Type<'arena>;
pub type MethodId<'arena> = method::Type<'arena>;
pub type ConstId<'arena> = method::Type<'arena>;
pub type PropId<'arena> = prop::Type<'arena>;

pub type NumParams = usize;

pub type ByRefs<'arena> = slice_t<'arena, bool>;

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
    pub option_t<hhbc_by_ref_label::Label>,
    pub option_t<Str<'arena>>,
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
    pub key_id: option_t<local::Type<'arena>>,
    pub val_id: local::Type<'arena>,
}

pub type ClassrefId = isize;
/// Conventionally this is "A_" followed by an integer
pub type AdataId<'arena> = Str<'arena>; //&'arena str;
pub type ParamLocations<'arena> = slice_t<'arena, isize>; //&'arena [isize];

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

    use super::pair;
    use super::slice_t;
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
        Vec(slice_t<'arena, TypedValue<'arena>>),
        Keyset(slice_t<'arena, TypedValue<'arena>>),
        Dict(slice_t<'arena, pair<TypedValue<'arena>, TypedValue<'arena>>>),
    }
}

pub type Str_slice<'arena> = slice_t<'arena, Str<'arena>>; //&'arena [&'arena str];

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
    NewStructDict(Str_slice<'arena>),
    NewVec(isize),
    NewKeysetArray(isize),
    NewPair,
    NewRecord(ClassId<'arena>, Str_slice<'arena>),
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

pub type Label_slice<'arena> = slice_t<'arena, hhbc_by_ref_label::Label>;
pub type Str_Label_pair_slice<'arena> =
    slice_t<'arena, pair<Str<'arena>, hhbc_by_ref_label::Label>>;

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
        Label_slice<'arena>, //bumpalo::collections::Vec<'arena, hhbc_by_ref_label::Label>,
    ),
    /// litstr id / offset vector
    SSwitch(Str_Label_pair_slice<'arena>), //bumpalo::collections::Vec<'arena, (&'arena str, hhbc_by_ref_label::Label)>),
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
        option_t<pair<local::Type<'arena>, isize>>,
    ),
    MemoGetEager(
        hhbc_by_ref_label::Label,
        hhbc_by_ref_label::Label,
        option_t<pair<local::Type<'arena>, isize>>,
    ),
    MemoSet(option_t<pair<local::Type<'arena>, isize>>),
    MemoSetEager(option_t<pair<local::Type<'arena>, isize>>),
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
) {
    unimplemented!()
}
