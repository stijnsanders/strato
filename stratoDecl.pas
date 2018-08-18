unit stratoDecl;

interface

{$D-}
{$L-}

const
  StratoSphereFileVersion=$00000300;//0.3.0

type
  xValue=type cardinal;
  xItem=type xValue;
  xName=type xValue;
  xSrcPos=type xValue;
  PxValue=^xValue;

//  xTypeNr=(
  xTypeNr=type xValue;
const
  //node identification codes (see also NodeTypeToStr)
                                //SNNN: size; number
    n_BinaryData                = 1000; //use vSizeDiv4 when iterating
    n_NameData                  = 4000;

    nNameSpace                  = 5000;
    nType                       = 6000;
    nLiteral                    = 3001;
    nConstant                   = 6002; //constant: iValue -> nLiteral;nConstant
    nRecord                     = 6003;
    nArray                      = 7004;
    nEnum                       = 5005; //enumeration: lItems -> nConstant
    nSignature                  = 7006;
    nSigArg                     = 6007;
    nSigArgByRef                = 5008;
    nMember                     = 4009;
    nOverload                   = 6010;
    nPointer                    = 6011;
    nTypeAlias                  = 5012;

    nGlobal                     = 4013;

    nClass                      = 7020;
    nClassRef                   = 5021; //class reference: iTarget -> nClass
    nCtors                      = 3022; //constructors: lItems -> nCtor
    nCtor                       = 6023;
    nDtor                       = 4024;
    nPropGet                    = 6025;
    nPropSet                    = 6026;
    //TODO: nPropPtr?
    nInterface                  = 6027;

    nCodeBlock                  = 7028;
    nVar                        = 7029;
    nVarByRef                   = 6030;
    nVarReadOnly                = 7031;
    nThis                       = 4032;

    nSCall                      = 5033;//system call: iTarget -> nLiteral
    nFCall                      = 5034;//function call: iTarget -> nOverload
    nVCall                      = 6035;//virtual/dynamic call
    nICall                      = 6036;//inherited call
    nCallArg                    = 5037;//call argument

    nCast                       = 5040;
    nAddressOf                  = 5041;
    nDereference                = 5042;
    nArrayIndex                 = 6043;
    nField                      = 5044;//field 'subject.target'

    nAssign                     = 6045;
    nUnaryOp                    = 6046;
    nBinaryOp                   = 7047;
    nSelection                  = 7048;
    nIteration                  = 6049;
    nIterPostEval               = 6050;
    nRange                      = 7051;
    nRangeIndex                 = 5052;

    nTry                        = 3055;
    nThrow                      = 4056;//raise/throw/except: iSubject -> nVCall on nCtor of exception object
    nDefer                      = 4057;
    nCatch                      = 6058;//catch: all OR lTypes -> nClass OR iTarget -> nVarReadOnly with iType nClass

    //see xTypeDef below
    n_TypeNr_Base               = 1000;//used with div to get size from value
    n_TypeNr_Low                = 3000;//used for xTypeDefX below
    n_TypeNr_High               = 7060;//used for xTypeDefX below

  //field identification codes (see also NodeFieldToStr)
  //  (see xTypeDef below)
  
    iName            = 100;
    iParent          = 101;
    iNext            = 102;
    iType            = 103;
    iSubject         = 104;
    iTarget          = 105;
    iSignature       = 106;
    iValue           = 107;
    iReturnType      = 108;
    iInheritsFrom    = 109;

    iBody            = 120;
    iLeft            = 121;
    iRight           = 122;
    iFirstArgVar     = 123;
    iPredicate       = 124;
    iDoTrue          = 125;
    iDoFalse         = 126;

    lItems           = 200;
    lArguments       = 201;
    lTypes           = 202;
    lLocals          = 203;

    vTypeNr          = 000;
    vSrcPos          = 001;
    vByteSize        = 002;
    vLength          = 002;//only used with n_NameData
    vLocalsSize      = 002;//only used with nCodeBlock
    vOffset          = 003;
    vOperator        = 004;
    vKey             = 005;

    f_FieldsMax      = 209;

    //keep these in equal sequence to xSourceFile fields below
    //use with "xxr(SrcIndex * StratoSphereBlockBase)"
    iSourceFile_FileName            = 300;
    vSourceFile_FileSize            = 301;
    vSourceFile_FileHash            = 302;
    vSourceFile_SrcPosLineIndex     = 303;
    vSourceFile_BlockIndex          = 304;
    lSourceFile_NameSpaces          = 305;
    iSourceFile_Local               = 306;
    lSourceFile_Globals             = 307;
    lSourceFile_Dictionary          = 308;
    iSourceFile__Reserved1          = 309;
    iSourceFile_InitializationBlock = 310;
    iSourceFile_FinalizationBlock   = 311;
    //= 312;
    //= 313;

type
  xSourceFile=record
    FileName:xItem;//n_BinaryData
    FileSize:cardinal;
    FileHash:cardinal; //TODO: file hash, timestamp
    SrcPosLineIndex:cardinal;
    BlockIndex:xItem;
    NameSpaces:xItem;
    Local:xItem;
    Globals:xItem;
    Dictionary:xItem;
    _Reserved1:xItem;
    InitializationBlock:xItem;
    FinalizationBlock:xItem;
  end;
  PxSourceFile=^xSourceFile;

  TStratoIntrinsicType=(
    itVoid,
    itType,
    itPointer,
    itBoolean,
    itNumber,//native integer
    itObject,//allow only one "object()={}"
    itString
  );

const
  xTypeDefLen=278;
  xTypeDef:array[0..xTypeDefLen] of xTypeNr=(

    //
    //  ATTENTION
    //  when changing these, also update stratoDebug's StratoDumpThing !!!
    //

    //n_BinaryData,            vSizeDiv4,{pData*,}
    n_NameData,                iParent,iNext,vKey,lItems,

    nNameSpace,nEnum,          iParent,iNext,vSrcPos,iName,lItems,
    nType,nRecord,             iParent,iNext,vSrcPos,iName,lItems,vByteSize, //TODO: iPointerTo?
    nArray,                    iParent,iNext,vSrcPos,iName,lItems,iType,vByteSize,
      //TODO: total/elem size (vByteSize= size element * count, for now)
      //TODO: multi-demensional
    nLiteral,                  vSrcPos,iType,iValue,
    nConstant,                 iParent,iNext,vSrcPos,iName,iType,iValue,
    nSignature,                iParent,iNext,vSrcPos,iName,iSubject,lArguments,iReturnType,
    nSigArg,                   iParent,iNext,vSrcPos,iName,iType,iValue,
    nSigArgByRef,              iParent,iNext,vSrcPos,iName,iType,
    nMember,                   iParent,iNext,iName,lItems,
    nOverload,                 iParent,iNext,vSrcPos,iSignature,iFirstArgVar,iBody,
    nPointer,                  iParent,iNext,vSrcPos,iName,lItems,iTarget,
    nTypeAlias,                iParent,iNext,vSrcPos,iName,iTarget,

    nGlobal,                   iParent,iNext,vSrcPos,iTarget,

    nClass,                    iParent,iNext,vSrcPos,iName,lItems,vByteSize,iInheritsFrom,
    nClassRef,                 iParent,iNext,vSrcPos,iName,iTarget,
    nCtors,                    iParent,iNext,lItems,
    nCtor,nPropGet,nPropSet,   iParent,iNext,vSrcPos,iSignature,iFirstArgVar,iBody,
    nDtor,                     iParent,iNext,vSrcPos,iBody,
    nInterface,                iParent,iNext,vSrcPos,iName,lItems,iInheritsFrom,

    nCodeBlock,                iParent,iNext,vSrcPos,lLocals,vLocalsSize,lItems,iReturnType,
    nVar,nVarReadOnly,         iParent,iNext,vSrcPos,iName,vOffset,iType,iValue,
    nVarByRef,                 iParent,iNext,vSrcPos,iName,vOffset,iType,
    nThis,                     iParent,iNext,vOffset,iType,

    nSCall,nFCall,             iParent,iNext,vSrcPos,lArguments,iTarget,
    nVCall,nICall,             iParent,iNext,vSrcPos,lArguments,iTarget,iSubject,
    nCallArg,                  iParent,iNext,vSrcPos,iValue,iType,

    nCast,                     iParent,iNext,vSrcPos,iSubject,iType,
    nAddressOf,nDereference,   iParent,iNext,vSrcPos,iSubject,iReturnType,
    nArrayIndex,               iParent,iNext,vSrcPos,iSubject,lArguments,iType,
    nField,                    iParent,iNext,vSrcPos,iSubject,iTarget,

    nAssign,                   iParent,iNext,vSrcPos,vOperator,iTarget,iValue,
    nUnaryOp,                  iParent,iNext,vSrcPos,vOperator,iRight,iReturnType,
    nBinaryOp,                 iParent,iNext,vSrcPos,vOperator,iLeft,iRight,iReturnType,
    nSelection,                iParent,iNext,vSrcPos,iPredicate,iDoTrue,iDoFalse,iReturnType,
    nIteration,nIterPostEval,  iParent,iNext,vSrcPos,iPredicate,iBody,iReturnType,
    nRange,                    iParent,iNext,vSrcPos,iName,iLeft,iRight,iReturnType,
    nRangeIndex,               iParent,iNext,vSrcPos,iLeft,iRight,

    nTry,                      iParent,iNext,vSrcPos,
    nThrow,                    iParent,iNext,vSrcPos,iSubject,
    nDefer,                    iParent,iNext,vSrcPos,lItems,
    nCatch,                    iParent,iNext,vSrcPos,lTypes,iTarget,iBody,

    n_TypeNr_Low);

var
  xTypeDefX:array[n_TypeNr_Low..n_TypeNr_High] of cardinal;

const
  StratoSphereFileMarker=$00727453;//'Str'#0 see pHeader below

type
  TStratoStoreHeader=record
    FileMarker:cardinal;
    FileVersion:cardinal;
    SourceFilesCount:cardinal;
    BlocksCount:cardinal;
  end;

implementation

uses SysUtils;

{$IFDEF DEBUG}
procedure StratoCheckTypeNrs;
var
  i,j,k:cardinal;
begin
  i:=0;
  while i<xTypeDefLen do
   begin
    j:=i+1;
    while (j<xTypeDefLen) and (xTypeDef[j]>f_FieldsMax) do inc(j);
    k:=j+1;
    while (k<xTypeDefLen) and (xTypeDef[k]<f_FieldsMax) do inc(k);
    while (i<>j) do
     begin
      if (xTypeDef[i] div n_TypeNr_Base)<>(k-j) then
        raise Exception.CreateFmt('Type Length mismatch %d:%d',
          [xTypeDef[i],k-j]);
      inc(i);
     end;
    i:=k;
   end;
end;
{$ENDIF}

procedure StratoBuildTypeDefX;
var
  i,j:cardinal;
begin
  {$IFDEF DEBUG}
  StratoCheckTypeNrs;
  {$ENDIF}
  i:=0;
  while i<xTypeDefLen do
   begin
    j:=i+1;
    while (j<xTypeDefLen) and (xTypeDef[j]>f_FieldsMax) do inc(j);
    while (i<>j) do
     begin
      xTypeDefX[xTypeDef[i]]:=j;
      inc(i);
     end;
    while (j<xTypeDefLen) and (xTypeDef[j]<f_FieldsMax) do inc(j);
    i:=j;
   end;
end;

initialization
  StratoBuildTypeDefX;
end.

