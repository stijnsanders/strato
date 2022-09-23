unit stratoDecl;

interface

{$D-}
{$L-}

const
  StratoSphereFileVersion=$000400;//0.4.0

type
  xValue=type cardinal;
  xRef=type xValue;
  xName=type xValue;
  xSrcPos=type xValue;

  xKey=( //=type xValue?

    xUnassigned                     = 0,

    xBinaryData                     = 1000, //i: running length (in bytes)
                                            //data is stored in subsequent records

    xListEntry                      = 2000,

    xDictionary_Entry               = 2001, //i:fraction v:down
    xDictionary_Tail                = 2002, //i:index v:parent

    n_Max1                          = 95,
    n_Max                           = 100,

    nSphere                         = 99,
    iSphere_FileName                = 300,
    vSphere_FileSize                = 301,
    vSphere_FileHash                = 302, //reserved
    vSphere_SrcPosLineIndex         = 303,
    lSphere_Errors                  = 304, //when inlining errors only
    //lSphere_Imports                 = 305,
    //lSphere_NameSpaces              = 306,
    iSphere_Local                   = 307,
    lSphere_Globals                 = 308,
    lSphere_Dictionary              = 309,
    iSphere_InitializationBlock     = 310,
    iSphere_FinalizationBlock       = 311,

{
    nImport                         = 98,
    vImport_SphereIndex             = 330,
    vImport_SphereRef               = 331,
}

    nNameSpace                      = 1,
    nType                           = 2,
    nLiteral                        = 3,
    nConstant                       = 4,
    nArray                          = 5,
    nEnum                           = 6,
    nRecord                         = 7,
    nPointer                        = 8,
    nTypeAlias                      = 9,
    nSignature                      = 10,
    nSigArg                         = 11,
    nSigArgByRef                    = 12,

    nOverload                       = 15,

    nClass                          = 20,
    nClassRef                       = 21,
    nCtor                           = 22, //constructor
    nDtor                           = 23, //destructor
    nPropGet                        = 24, //property getter
    nPropSet                        = 25, //property setter
    nInterface                      = 26,

    nCodeBlock                      = 30,
    lCodeBlock_Statements           = 200,
    lCodeBlock_Locals               = 201,
    vCodeBlock_LocalsSize           = 202,

    nVar                            = 31,
    nVarByRef                       = 32,
    nVarReadOnly                    = 33,
    nThis                           = 34,

    nSCall                          = 35, //system call: iTarget -> nLiteral
    nFCall                          = 36, //function call: iTarget -> nOverload
    nVCall                          = 37, //virtual/dynamic call
    nICall                          = 38, //inherited call
    nCallArg                        = 39, //call argument

    nCast                           = 40,
    nAddressOf                      = 41,
    nDereference                    = 42,
    nArrayIndex                     = 43,
    nField                          = 44, //field 'subject.target'

    nAssign                         = 45,
    nUnaryOp                        = 46,
    nBinaryOp                       = 47,
    nSelection                      = 50,
    nIteration                      = 51,
    nIterPostEval                   = 52,
    nRange                          = 53,
    nRangeIndex                     = 54,

    nTry                            = 55,
    nThrow                          = 56, //raise/throw/except: iSubject -> nVCall on nCtor of exception object
    nDefer                          = 57,
    nCatch                          = 58,  //catch: all
    lCatch_Types                    = 209, //  OR lTypes -> nClass
                                           //  OR iTarget -> nVarReadOnly with iType nClass

    xStageList                      = 97, //used by stratoFn.pas

    //generic field keys
    iParent                         = 101,
    vSrcPos                         = 102, // source line * sphere's vSphere_SrcPosLineIndex value + source position
    dName                           = 103,
    iType                           = 104,
    vByteSize                       = 105,
    lChildren                       = 106,
    vOffset                         = 107,
    vOperator                       = 108,

    iSubject                        = 109,
    iTarget                         = 110,
    iSignature                      = 111,
    iValue                          = 112,
    iReturnType                     = 113,
    iInheritsFrom                   = 114,

    iBody                           = 120,
    iLeft                           = 121,
    iRight                          = 122,
    iPredicate                      = 123,
    iDoTrue                         = 124,
    iDoFalse                        = 125,

    iArgVar                         = 126,
    lArguments                      = 127,

    x_Invalid                       = 9999
  );

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
  StratoSphereFileMarker=$00727453;//'Str'#0 see pHeader below

type
  TStratoStoreHeader=record
    FileMarker:cardinal;
    FileVersion:cardinal;
    SpheresCount:cardinal;
    Reserved_1:cardinal;
  end;

  TStratoSphereHeader=record
    Items:cardinal;
    Reserved_1:cardinal;
    Reserved_2:cardinal;
    Reserved_3:cardinal;
  end;

const
  xSCall_inc     = 1;
  xSCall_dec     = 2;
  xSCall_malloc  = 100;

  xSCall_writeln     = 200;
  xSCall_filetostr   = 201;
  xSCall_strtofile   = 202;
  xSCall_filetomem   = 203;
  xSCall_memtofile   = 204;
  xSCall_commandline = 205;

implementation

end.

