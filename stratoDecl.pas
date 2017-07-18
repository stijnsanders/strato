unit stratoDecl;

interface

type
  xValue=type cardinal;
  xItem=type xValue;
  xName=type xValue;
  xSrcPos=type xValue;
  xTypeNr=type xValue;

  PxValue=^xValue;

  xPtr=cardinal;//used for pointer arithmetic

const
  xValueSize=SizeOf(xValue);

                               //SFFFXNN size, field bits, master, number
    n_BinaryData               =$0000001; //use SizeDiv4 when iterating
    //n_Item                     =$1C00100;
    //n_Chainable                =$2600200;
    //n_Named                    =$3600300;

    n_Item_Low                 =$2600000;//used by xTypeDefX below
    n_Item_High                =$7800000;

    nSourceFile                =$943C000;
    
    nNameSpace                 =$4680300;
    nImport                    =$4680308;
    nGlobal                    =$4700000;
    nPrivate                   =$2600201;

    nTypeDecl                  =$4600400;
    nSignature                 =$66E0400;
    nArgument                  =$56C0300;
    nArgByRef                  =$56C0308;
    nVarDecl                   =$66C0300;
    nVarByRef                  =$66C0308;
    nLiteral                   =$3700100;
    nConstant                  =$56C0301;
    nTypeAlias                 =$3700208;

    nMember                    =$4680301;
    nOverload                  =$57C0200;

    nArray                     =$5640400;
    nRecord                    =$5640401;
    nEnumeration               =$4680402;

    nClass                     =$66C0400;
    nClassRef                  =$4680408;
    nInterface                 =$56C0401;
    nConstructors              =$4680304;
    nConstructor               =$57C0203;
    nDestructor                =$57C0204;
    nPropertyGet               =$57C0201;
    nPropertySet               =$57C0202;

    nCodeBlock                 =$6760200;
    nSysCall                   =$3600201;
    nFCall                     =$4780210;//function call
    nSCall                     =$57C0211;//static call
    nVCall                     =$57C0212;//virtual call (dynamic)
    nThis                      =$66C0202;
    nPointer                   =$3700200;
    nAddressOf                 =$4780208;
    nDereference               =$4780209;
    nField                     =$4780200;
    nArrayIndex                =$4780201;
    nCast                      =$4780202;

    nAssign                    =$56C0204;
    nUnaryOp                   =$56C0205;
    nBinaryOp                  =$66E0201;
    nSelection                 =$67E0201;
    nIteration                 =$77F0200;
    nIterPostEval              =$77F0201;

    nTry                       =$3700203;
    nThrow                     =$3700204;
    nDeferred                  =$3700205;
    nCatchAll                  =$3700206;
    nCatchTypes                =$4780203;
    nCatchNamed                =$56C0200;

    n_Invalid                  =$0000000;

    //field indexes

    f_Unknown     =100;
    fParent       =101;//n_Item
    fNext         =102;//n_Chainable
      // ATTENTION: list pointers point to last element in list,
      // fNext value of last element points to first item in list
    fItems        =103;
    fValue        =104;
    fSubject      =105;
    fTarget       =106;

    fTypeDecl     =107; //nTypeDecl
    fSignature    =108;
    fArguments    =109; //nArgument,nArgByRef
    fFirstArgVar  =110;
    fReturnType   =111; //nTypeDecl
    fVarDecl      =112; //nVarDecl
    fInheritsFrom =113;
    fVarDecls     =114;

    fDoFirst      =120;
    fDoIf         =121;
    fDoThen       =122;
    fDoElse       =123;
    fBody         =124;
    fLeft         =125;
    fRight        =126;
    fExceptionConstructor =127;

    //see also xSourcefile below
    fSourceFile_NameSpaces          =150;
    fSourceFile_Globals             =151;
    fSourceFile_InitializationBlock =152;
    fSourceFile_FinalizationBlock   =153;

    v_IntegerFields =180;

    vTypeNr         =181;
    vSrcPos         =182;
    vName           =183;
    vByteSize       =184;
    vOffset         =185;
    vOperator       =186;
    vExName         =187;

    f_FieldsMax     =199;

type
  xSourceFile=record
    TypeNr:xTypeNr;
    SrcPosLineIndex:cardinal;
    FileName:xItem;//xBinaryData
    FileSize:cardinal;
    Hash:cardinal;
    Reserved1:cardinal;
    Reserved2:cardinal;
    //TODO: hash, timestamp
    NameSpaces:xItem;
    Globals:xItem;
    InitializationBlock:xItem;
    FinalizationBlock:xItem;
  end;
  PxSourceFile=^xSourceFile;

  (*
    n_BinaryData, vTypeNr,vBytesize,{pData,}
    n_Item,       vTypeNr,vSrcPos,fParent
    n_Chainable,  vTypeNr,vSrcPos,fParent,fNext,
    n_Named,      vTypeNr,vSrcPos,fParent,fNext,vName,

    nSourceFile,  vTypeNr,vSrcPosLineIndex,fFileName{:n_BinaryData},vFileSize,vFileHash,
                  vReserved1,vReserved2,lNameSpaces,lGlobals,lInitializationBlock,lFinalizationBlock,
  *)

const
  xTypeDefLen=195;
  xTypeDef:array[0..xTypeDefLen-1] of xTypeNr=(

    //list of uses of fields (all have vTypeNr,vSrcPos,fParent in common)

    nNameSpace,                 fNext,vName,fItems,
    nImport,                    fNext,vName,fSubject{nNameSpace},
    nTypeDecl,                  fNext,vName,vByteSize, //TODO:fPointerTo{nTypeDecl}
    nSignature,                 fNext,vName,fSubject{nClass,nRecord},fArguments,fReturnType,
    nArgument,                  fNext,vName,fTypeDecl,fValue,
      //Parent is nSignature: default value (nConstant,nLiteral)
      //Parent is n*Call: arg var (nVarDecl,nVarByRef)

    nArgByRef,                  fNext,vName,fTypeDecl,fValue, //same as above
    nVarDecl,nVarByRef,nThis,   fNext,vName,fTypeDecl,fValue,vOffset,
    nGlobal,                    fNext,fVarDecl,vByteSize,
    nPrivate,                   fNext,
    nLiteral,                   fValue,fTypeDecl,
    nConstant,                  fNext,vName,fValue,fTypeDecl,
    nTypeAlias,                 fNext,fTypeDecl,
    nMember,                    fNext,vName,fItems{nOverload,nPropertyGet,nPropertySet},
    nConstructors,              fNext,vName,fItems{nConstructor},
    nOverload,                  fNext,fSignature,fFirstArgVar,fBody,
    nConstructor,nDestructor,   fNext,fSignature,fFirstArgVar,fBody,
    nPropertyGet,nPropertySet,  fNext,fSignature,fFirstArgVar,fBody,
    nArray,                     fNext,vName,vByteSize,fTypeDecl,
      //TODO: size/extent/dimension: Array.ByteSize div ByteSize(Array.ElementType) for now
      //TODO: multi-dimensional arrays
    nRecord,                    fNext,vName,vByteSize,fItems,
    nEnumeration,               fNext,vName,fItems,
    nClass,                     fNext,vName,fInheritsFrom{nClass},fItems,vByteSize,
    nClassRef,                  fNext,vName,fSubject{nClass},
    nInterface,                 fNext,vName,fInheritsFrom{nInterface},fItems,

    nCodeBlock,                 fNext,fVarDecls,vByteSize,fItems,fReturnType,
    nSysCall,                   fNext,vOffset,
    nFCall,                     fNext,fTarget,fArguments,
    nSCall,nVCall,              fNext,fSubject,fTarget,fArguments,
    //TODO: nPGetCall,nPSetCall

    nPointer,                   fNext,fSubject{nTypeDecl,nPointer},
    nAddressOf,nDereference,    fNext,fSubject,fReturnType,

    nField,                     fNext,fSubject,fTarget, //subject.target
    nArrayIndex,                fNext,fSubject,fItems, //subject[item(s)]
    nCast,                      fNext,fSubject,fTypeDecl,
    nAssign,                    fNext,vOperator,fTarget,fValue,
    nUnaryOp,                   fNext,vOperator,fRight,fReturnType,
    nBinaryOp,                  fNext,vOperator,fLeft,fRight,fReturnType,
    nSelection,                 fNext,fDoIf,fDoThen,fDoElse,fReturnType,
    nIteration,nIterPostEval,   fNext,fDoFirst,fDoIf,fDoThen,fBody,fReturnType,//TODO: iterate over range always
    nThrow,                     fNext,fExceptionConstructor,
    nDeferred,                  fNext,fItems,
    nCatchAll,                  fNext,fBody,
    nCatchTypes,                fNext,fItems{nTypeAlias},fBody,
    nCatchNamed,                fNext,vExName,fVarDecl,fBody,

    n_Item_Low //closing entry
  );

var
  xTypeDefX:array[n_Item_Low..n_Item_High] of integer;

const
  StratoSphereFileMarker=$00727453;//'Str'#0 see pHeader below

type
  TStratoFileHeader=record
    FileMarker:cardinal;
    FileVersion:cardinal;
    BlockCount:cardinal;
    Reserved1:cardinal;
  end;
  TStratoBlockHeader=record
    Reserved1:cardinal;
    ItemCount:cardinal;
    SourceFile:xItem;
    ContinueBlockIdx:cardinal;
  end;

implementation

uses SysUtils;

{$D-}
{$L-}

procedure StratoCheckTypeNrs;
var
  i,j,k,n,m:cardinal;
begin
  i:=0;
  while i<xTypeDefLen do
   begin
    j:=i+1;
    while (j<xTypeDefLen) and (xTypeDef[j]>f_FieldsMax) do inc(j);
    k:=j;
    n:=$0200;
    m:=$0400;
    while (k<xTypeDefLen) and (xTypeDef[k]<f_FieldsMax) do
     begin
      if xTypeDef[k]<v_IntegerFields then m:=m or n;
      n:=n shr 1;
      inc(k);
     end;
    m:=m or ((k-j+1) shl 12);

    if ((cardinal(xTypeDef[i]) shr 12)<>m) and (i<>xTypeDefLen-1) then
      raise Exception.CreateFmt('Flags mismatch: %.7x %.4x',[cardinal(xTypeDef[i]),m]);
      
    inc(i);
    if i=j then
     begin
      while (j<xTypeDefLen) and (xTypeDef[j]<f_FieldsMax) do inc(j);
      i:=j;
     end;
   end;
end;

procedure StratoBuildTypeDefX;
var
  i,j:integer;
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

