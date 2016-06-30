unit stratoDecl;

interface

type
  TStratoThingType=type cardinal;
  TStratoIndex=type cardinal;
  TStratoField=type cardinal;
  TStratoName=type cardinal;

const
  //About TStratoThing:
  //not all ThingType's use all of the TStratoThing fields,
  //also to save space, some overlap
  //use below guide to determine which fields
  //are in use for each ThingType

  ttFileMarker   = $00727453;//'Str'#0;
    //(node 0: use PStratoHeader)

  ttSourceFile   = $0100;
    //(use PStratoSourceFile)
    //FileSize
    //FileName (ttBinaryData)
    //SrcPosLineIndex
    //InitializationCode (0,ttCodeBlock)
    //FinalizationCode (0,ttCodeBlock
    //TODO: FileCRC32,FileDate

  ttBinaryData   = $0200;
    //(use PStratoBinaryData)
    //DataLength: data in (DataLength+8 div 32) node(s)
    //DataStart: start of inlined data

  tt__Resolvable = $0010;
    //[bitmask: things that use FirstItem]
  tt__Typed      = $0020;
    //[bitmask: things that use EvaluateTo]
  tt__IsType     = $0040;
    //[bitmask: things allowed as EvaluatesTo]

  //TODO: tt__Named?

  ttNameSpace    = $0011;
    //(use PStratoNameSpace)
    //FirstItem	(0,*)
    //SourceFile (0,ttSourceFile)
    //FirstInitialization (0,ttCodeBlock)
    //FirstFinalization (0,ttCodeBlock)

  ttTypeDecl     = $0050;
    //FirstItem (0,*)
    //ByteSize: memory used by var of type

  ttRecord       = $0051;
    //ByteSize
    //FirstItem (0,*)

  ttEnumeration  = $0052;
    //ByteSize
    //FirstItem (0,ttConstant)

  ttLiteral      = $0021;
    //EvaluatesTo (ttTypeDecl)
    //InitialValue (ttBinary)

  ttVar          = $0022;
    //Offset: memory position relative to parent runtime location
    //EvaluatesTo (ttTypeDecl)
    //InitialValue (*)

  ttConstant     = $0023;
    //EvaluatesTo (ttTypeDecl)
    //InitialValue (ttLiteral)

  ttCodeBlock    = $0131;
    //ByteSize: memory for local variables
    //EvaluatesTo: end with expression
    //FirstItem (ttVar): local variables
    //FirstStatement (*)

  tt__Directed   = $0080;
    //[bitmask: things that use Target]

  ttImport       = $0081;
    //Target (ttNameSpace)

  ttAlias        = $0082;
    //Target (*)

  ttGlobal       = $0083;
    //Target (ttVar)

  ttPrivate      = $0091;
    //SourceFile (ttSourceFile)
    //FirstItem (*)
    //Target (ttNameSpace)

    
  ttSignature    = $00E0;
    //Target (ttTypeDecl): call subject (this)
    //EvaluatesTo (0,ttTypeDecl): return value
    //FirstArgument (*): first argument

  ttMember       = $0001;
    //Name:
    //FirstItem (0,ttOverload,ttPropertyGet,ttPropertySet):

  ttOverload     = $0002;
    //SourceFile (ttSourceFile)
    //FirstArgument (*): first argument value in overload body
    //Target (ttSignature): call signature
    //Body (ttCodeBlock): overload body

  ttFnCall       = $0084;
    //FirstArgument (0,ttArgument)
    //Target (ttField,ttOverload,ttConstructor,ttDestructor):
    //EvaluatesTo (0,*): return value type
    //Name=0 when constructor and calling inherited constructor 

  ttArgument     = $00A0;
    //InitialValue (ttLiteral): default value (signature only)
    //Target (*): argument value [ATTENTION! Target and not ValueFrom!]
    //EvaluatesTo (ttTypeDecl): argument type (used to find suitable signature)

  ttAssign       = $0003;
    //Op
    //ValueFrom (*)
    //AssignTo (ttVar,ttArrayIndex,ttField,ttCast)

  ttUnaryOp      = $0025;
    //Op
    //EvaluatesTo (ttTypeDecl)
    //Right (*): [ATTENTION! Right and not Target!]

  ttBinaryOp     = $0026;
    //Op
    //EvaluatesTo (ttTypeDecl)
    //Left (*)
    //Right (*)

  ttCast         = $00A1;
    //Target (*)
    //EvaluatesTo (ttTypeDecl)

  ttSelection    = $0123;
    //DoIf: criterium
    //EvaluatesTo (ttTypeDecl)
    //DoThen: to perform when criterium evaluates to true
    //DoElse: to perform when criterium evaluates to false

  ttIteration    = $0101;
  ttIterationPE  = $0102;//post evaluation
    //DoIf: criterium
    //DoElse: initialization (e.g. i=0) [should be DoFirst but saving a label]
    //DoThen: do between body and criterium (e.g. i++)
    //Body

  ttTry          = $0004;
    //Target: (ttDeferred,ttCatch)

  ttThrow        = $0008;
    //Target: object (creator) to throw
    //TODO: (EvaluatesTo?

  ttDeferred     = $0005;
    //Target: deferred command

  ttCatch        = $0006;
    //Target (0,ttTypeDecl): exception object mask
    //FirstArgument (ttVar): exception object reference
    //Body (*) exception handling command(s)

  ttSysCall      = $0007;
    //Op : internal value (see PerformSysCall)

  ttArray        = $0041;
    //ByteSize: total array memory size
    //Subject: element type (ttTypeDecl)
    //TODO: multi-dimensional arrays

  ttArrayIndex   = $0024;
    //Target: x in x[y]
    //FirstArgument: y in x[y]
    //EvaluateTo (0,ttTypeDecl)

  ttField        = $0027;
    //Subject (ttVar): x in x.y
    //Target: y in x.y
    //EvaluateTo (0,ttTypeDecl)

  ttThis         = $0028;
    //Offset
    //EvaluatesTo (ttTypeDecl)

  ttPointer      = $00C0;
    //ByteSize: SystemWordSize
    //EvaluatesTo (ttTypeDecl)

  ttAddressOf    = $002A;
    //EvaluatesTo (ttTypeDecl)
    //ValueFrom (ttVar)

  ttDereference  = $002B;
    //EvaluatesTo (ttTypeDecl): from pointer
    //ValueFrom (ttVar): of type pointer

  ttArgByRef     = $00A2;
    //EvaluatesTo (ttTypeDecl)

  ttVarByRef     = $00A3;
    //Offset
    //EvaluatesTo (ttTypeDecl)

  ttClass        = $00D0;
    //FirstItem (0, ttVar, ttMember, ttConstructors)
    //ByteSize: size of data, not value since that's a pointer
    //InheritsFrom (0, ttClass)
    //Target (ttClassInfo)

  ttConstructors = $000A;
    // (like ttMember)
    //FirstItem (0,ttConstructor): first constructor

  ttConstructor  = $000B;
    // (like ttOverload)
    //SourceFile (ttSourceFile)
    //FirstArgument (*): first argument value in overload body
    //Target (ttSignature): call signature
    //Body (ttCodeBlock): overload body

  ttDestructor   = $000C;
    //Target (ttSignature)
    //Body (ttCodeBlock)

  ttClassRef     = $00C1;
    //ByteSize: SystemWordSize
    //EvaluatesTo (ttClass)

  ttInterface    = $00D1;
    //ByteSize: SystemWordSize (since it's a pointer!)
    //FirstItem (ttVar, ttMember)
    //InheritsFrom (ttRecord)

  ttPropertyGet  = $000D;
    // (like ttOverload)
    //SourceFile (ttSourceFile)
    //FirstArgument (*): first argument value in overload body
    //Target (ttSignature): call signature
    //Body (ttCodeBlock): overload body

  ttPropertySet  = $000E;
    // (like ttOverload)
    //SourceFile (ttSourceFile)
    //FirstArgument (*): first argument value in overload body
    //Target (ttSignature): call signature
    //Body (ttCodeBlock): overload body

  ttPropCall     = $0085;
    //Op: 0 with ttPropertyGet, stAssign* with ttPropertySet
    //FirstArgument (0,ttArgument)
    //Target (ttPropertyGet,ttPropertySet):
    //EvaluatesTo (0,*): ttPropertySet: value to set

const
  //fields
  tfThingType      =$00;
  tfParent         =$01;
  tfNext           =$02;
  tfName          =$103;//TStratoName
  tfFirstItem      =$04;
  tfByteSize      =$105;//v
  tfInheritsFrom   =$06;
  tfSrcPos        =$107;//v
  tfFirstStatement =$13;
  tfInitialValue   =$14;
  tfOffset        =$115;//v
  tfEvaluatesTo    =$16;
  tfSubject        =$23;
  tfValueFrom      =$24;
  tfAssignTo       =$25;
  tfOperator      =$133;//TStratoToken (stOp*)
  tfLeft           =$34;
  tfRight          =$35;
  tfSourceFile     =$43;
  tfFirstArgument  =$44;
  tfSignature      =$45;
  tfBody           =$46;
  tfTarget         =$75;
  tfDoIf           =$53;
  tfDoThen         =$54;
  tfDoElse         =$55;
  tfDoFirst        =$65;


{$IFDEF DEBUG}
function rx(tt:TStratoThingType;f:TStratoField):cardinal;
procedure rxc(tt:TStratoThingType;f:TStratoField;tc:TStratoThingType);
{$ENDIF}

type
  TStratoThing=array[0..7] of cardinal;
  PStratoThing=^TStratoThing;//only used by TStratoSphere internally

  TStratoHeader=record
    FileMarker:TStratoThingType;
    ThingCount,
    Version:cardinal;
    FirstNameSpace,
    FirstGlobalVar:TStratoIndex;
    GlobalByteSize:cardinal;
    FirstInitialization,
    FirstFinalization:TStratoIndex;
  end;
  PStratoHeader=^TStratoHeader;

  TStratoBlockHeader=record
    FirstIndex,
    ThingCount,
    xReserved1:cardinal;
    xReserved2,
    xReserved3:TStratoIndex;
    xReserved4:cardinal;
    xReserved5,
    xReserved6:TStratoIndex;
  end;

  TStratoSourceFile=record
    ThingType:TStratoThingType;//ttSourceFile
    FileName:TStratoName;
    FileSize:cardinal;
    SrcPosLineIndex:cardinal;
    xReserved1,//FileCRC32?
    xReserved2,//FileDate?
    InitializationCode,
    FinalizationCode:TStratoIndex;
  end;
  PStratoSourceFile=^TStratoSourceFile;

  TStratoBinaryData=record
    ThingType:TStratoThingType;//ttBinaryData
    DataLength,
    DataStart,
    xPadding1,
    xPadding2,
    xPadding3,
    xPadding4,
    xPadding5:cardinal;
  end;
  PStratoBinaryData=^TStratoBinaryData;

  TStratoNameSpaceData=record
    ThingType:TStratoThingType;//ttNameSpace
    Parent,
    Next:TStratoIndex;
    Name:TStratoName;
    FirstItem,
    SourceFile:TStratoIndex;
    FirstInitialization,
    FirstFinalization:TStratoIndex;
  end;
  PStratoNameSpaceData=^TStratoNameSpaceData;

implementation

uses SysUtils,Classes;

{$IFDEF DEBUG}

{xx$D-}
{xx$L-}

function rx(tt:TStratoThingType;f:TStratoField):cardinal;
begin
  if f=tfParent then Result:=1 else
  if f=tfNext then Result:=2 else
  if f=tfSrcPos then Result:=7 else
   begin
    Result:=0;//default
    case tt of
      ttNameSpace://use PStratoNameSpaceData
        case f of
          tfName:Result:=3;
          tfFirstItem:Result:=4;
          //tfSourceFile:Result:=5;//!!! use PStratoNameSpaceData
          //tfFirstInitialization:Result:=6;//!!! use PStratoNameSpaceData
          //tfFirstFinalization:Result:=7;//!!! use PStratoNameSpaceData
        end;
      ttTypeDecl,ttRecord,ttEnumeration:
        case f of
          tfName:Result:=3;
          tfFirstItem:Result:=4;
          tfByteSize:Result:=5;
        end;
      ttLiteral:
        case f of
          tfInitialValue:Result:=4;
          tfEvaluatesTo:Result:=6;
        end;
      ttConstant:
        case f of
          tfName:Result:=3;
          tfInitialValue:Result:=4;
          tfEvaluatesTo:Result:=6;
        end;
      ttVar:
        case f of
          tfName:Result:=3;
          tfInitialValue:Result:=4;
          tfOffset:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttCodeBlock:
        case f of
          tfFirstStatement:Result:=3;
          tfFirstItem:Result:=4;
          tfByteSize:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttImport,ttAlias,ttGlobal:
        case f of
          tfTarget:Result:=5;
        end;
      ttPrivate:
        case f of
          tfSourceFile:Result:=3;
          tfFirstItem:Result:=4;
          tfTarget:Result:=5;
        end;
      ttSignature:
        case f of
          tfName:Result:=3;
          tfFirstArgument:Result:=4;
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttMember:
        case f of
          tfName:Result:=3;
          tfFirstItem:Result:=4;
        end;
      ttOverload,ttConstructor,ttPropertyGet,ttPropertySet:
        case f of
          tfSourceFile:Result:=3;
          tfFirstArgument:Result:=4;
          tfSignature:Result:=5;
          tfBody:Result:=6;
        end;
      ttFnCall:
        case f of
          tfName:Result:=3;
          tfFirstArgument:Result:=4;
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttArgument:
        case f of
          tfName:Result:=3;
          tfInitialValue:Result:=4;
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttAssign:
        case f of
          tfOperator:Result:=3;
          tfValueFrom:Result:=4;
          tfAssignTo:Result:=5;
        end;
      ttUnaryOp:
        case f of
          tfOperator:Result:=3;
          tfRight:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttBinaryOp:
        case f of
          tfOperator:Result:=3;
          tfLeft:Result:=4;
          tfRight:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttCast:
        case f of
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttSelection:
        case f of
          tfDoIf:Result:=3;
          tfDoThen:Result:=4;
          tfDoElse:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttIteration,ttIterationPE:
        case f of
          tfDoIf:Result:=3;
          tfDoThen:Result:=4;
          tfDoFirst:Result:=5;
          tfBody:Result:=6;
        end;
      ttTry,ttThrow,ttDeferred:
        case f of
          tfTarget:Result:=5;
        end;
      ttCatch:
        case f of
          tfFirstArgument:Result:=4;
          tfTarget:Result:=5;
          tfBody:Result:=6;
        end;
      ttSysCall:
        case f of
          tfOperator:Result:=3;
        end;
      ttArray:
        case f of
          tfSubject:Result:=3;
          //TODO: multi-dimensional arrays
          tfByteSize:Result:=5;
        end;
      ttArrayIndex:
        case f of
          tfFirstArgument:Result:=4;
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttField:
        case f of
          tfSubject:Result:=3;
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttThis:
        case f of
          tfName:Result:=3;//'@@'
          tfOffset:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttPointer:
        case f of
          tfByteSize:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttAddressOf,ttDereference:
        case f of
          tfValueFrom:Result:=4;
          tfEvaluatesTo:Result:=6;
        end;
      ttArgByRef:
        case f of
          tfEvaluatesTo:Result:=6;
        end;
      ttVarByRef:
        case f of
          tfOffset:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttClass:
        case f of
          tfName:Result:=3;
          tfFirstItem:Result:=4;
          tfByteSize:Result:=5;
          tfInheritsFrom:Result:=6;
          //tfTarget:Result:=;//TODO!
        end;
      ttConstructors:
        case f of
          tfName:Result:=3;//always 0!
          tfFirstItem:Result:=4;
        end;
      ttDestructor:
        case f of
          tfName:Result:=3;
          tfSignature:Result:=5;
          tfBody:Result:=6;
        end;
      ttClassRef:
        case f of
          tfName:Result:=3;
          tfByteSize:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
      ttInterface:
        case f of
          tfFirstItem:Result:=4;
          tfByteSize:Result:=5;
          tfInheritsFrom:Result:=6;
        end;
      ttPropCall:
        case f of
          tfOperator:Result:=3;
          tfFirstArgument:Result:=4;
          tfTarget:Result:=5;
          tfEvaluatesTo:Result:=6;
        end;
    end;
    if Result=0 then
      raise Exception.CreateFmt('Field not available for thing type %.4x[%.3x]',
        [tt,f]);
   end;
end;

procedure rxc(tt:TStratoThingType;f:TStratoField;tc:TStratoThingType);
var
  ok:boolean;
  procedure anydecl;
  begin
    ok:=tc in [ttTypeDecl,ttRecord,ttEnumeration,ttClass,ttInterface,
      ttVar,ttVarByRef,ttConstant,ttSignature,ttMember,ttDestructor,
      ttClassRef];
  end;
  procedure anyval;
  begin
    ok:=((tc and tt__Typed)<>0) or (tc in [ttFnCall,ttPropCall]);
  end;
begin
  ok:=false;//default;
  //TODO
  case tt of
    ttNameSpace:
      case f of
        tfFirstItem,tfNext:if tc=ttNameSpace then ok:=true else anydecl;
      end;
    ttTypeDecl:
      case f of
        tfParent:ok:=tc=ttNameSpace;
        tfNext:anydecl;
      end;
    ttRecord,ttInterface:
      case f of
        tfParent:ok:=tc=ttNameSpace;
        tfNext:anydecl;
        tfFirstItem:ok:=tc in [ttVar,ttMember,ttPropertyGet,ttPropertySet];
      end;
    ttEnumeration:
      case f of
        tfParent:ok:=tc=ttNameSpace;
        tfNext:anydecl;
        tfFirstItem:ok:=tc=ttConstant;
      end;
    ttLiteral:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfInitialValue:ok:=tc=ttBinaryData;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttVar:
      case f of
        tfParent:
          case tc of
            ttNameSpace,ttClass,ttCodeBlock:ok:=true;
          end;
        tfNext:anydecl;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttConstant:
      case f of
        tfParent:
          case tc of
            ttNameSpace,ttClass,ttCodeBlock:ok:=true;
          end;
        tfNext:anydecl;
        tfInitialValue:ok:=tc=ttLiteral;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttCodeBlock:
      case f of
        tfParent:
          case tc of
            ttNameSpace,ttCodeBlock,ttOverload,ttConstructor,ttDestructor,
            ttPropertyGet,ttPropertySet:
              ok:=true;
          end;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfFirstItem:ok:=tc in [ttVar,ttThis];
        tfFirstStatement:ok:=(tc and tt__IsType)=0;
      end;
    ttImport:
      case f of
        tfParent,tfTarget:ok:=tc=ttNameSpace;
      end;
    ttAlias:
      case f of
        tfParent:
          case tc of
            ttCodeBlock,ttNameSpace:ok:=true;
          end;
        tfTarget:
          case tc of
            ttMember,ttCodeBlock:ok:=true;
          end;
      end;
    ttGlobal:
      case f of
        tfTarget:ok:=tc=ttVar;
      end;
    ttSignature:
      case f of
        tfParent:ok:=tc in [ttNameSpace,ttClass,ttRecord,ttOverload];
        tfFirstArgument:ok:=tc in [ttArgument,ttArgByRef];
        tfTarget:ok:=tc in [ttClass,ttRecord];
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttMember:
      case f of
        tfParent:ok:=tc in [ttNameSpace,ttClass,ttRecord];
        tfNext:anydecl;
        tfFirstItem:ok:=tc in [ttOverload,ttPropertyGet,ttPropertySet];
      end;
    ttOverload,ttPropertyGet,ttPropertySet:
      case f of
        tfParent:ok:=tc=ttMember;
        tfNext:ok:=tc in [ttOverload,ttPropertyGet,ttPropertySet];
        tfSourceFile:ok:=tc=ttSourceFile;
        tfFirstArgument:ok:=tc in [ttVar,ttVarByRef];
        tfSignature:ok:=tc=ttSignature;
        tfBody:ok:=tc=ttCodeBlock;//ttSysCall?
      end;
    ttFnCall:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfFirstArgument:ok:=(tc and tt__Typed)<>0;
        tfTarget:ok:=tc in [ttMember,ttOverload,
          ttClass,//only when parsing, to resolve later
          ttField,ttCast,ttVar,ttPointer,//TODO: resolve more?
          ttConstructor,ttDestructor];
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttArgument:
      case f of
        tfParent:ok:=tc in [ttSignature,ttFnCall];
        tfNext:ok:=tc=ttArgument;
        tfInitialValue,tfTarget:ok:=(tc and tt__Typed)<>0;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttAssign:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfAssignTo:ok:=(tc and tt__Typed)<>0;
        tfValueFrom:anyval;
      end;
    ttUnaryOp:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfRight:anyval;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttBinaryOp:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfLeft,tfRight:anyval;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttCast:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfEvaluatesTo:ok:=((tc and tt__IsType)<>0) or (tc=ttFnCall);
        tfTarget:ok:=((tc and tt__Typed)<>0) or (tc=ttFnCall);
      end;
    ttSelection:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfDoIf,tfDoThen,tfDoElse:ok:=(tc and tt__IsType)=0;
      end;
    ttIteration,ttIterationPE:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfDoFirst,tfDoIf,tfDoThen,tfBody:ok:=(tc and tt__IsType)=0;
      end;
    ttTry:;//TODO:
    ttThrow:;
    ttDeferred:;
    ttCatch:;
    ttSysCall:
      case f of
        tfParent:
          case tc of
            ttOverload,ttCodeBlock:ok:=true;
          end;
      end;
    ttArray:;//TODO:
    ttArrayIndex:;
    ttField:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfSubject:ok:=(tc and tt__Typed)<>0;
        tfTarget:ok:=tc in [ttVar,
          ttMember,//only when parsing, to resolve later
          ttOverload,ttConstructor,ttDestructor,ttPropertyGet,ttPropertySet];
        tfEvaluatesTo:ok:=((tc and tt__Typed)<>0) or ((tc and tt__IsType)<>0);
      end;
    ttThis:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=tc=ttVar;
        tfEvaluatesTo:ok:=tc in [ttClass,ttRecord];
      end;
    ttPointer:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfEvaluatesTo:ok:=tc in [ttClass,ttTypeDecl];
      end;
    ttAddressOf:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfValueFrom:ok:=tc in [ttThis,ttVar];
        tfEvaluatesTo:ok:=tc=ttPointer;
      end;
    ttDereference:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfEvaluatesTo:ok:=tc in [ttPointer,ttField];
        tfValueFrom:ok:=tc=ttVar;
      end;
    ttArgByRef:
      case f of
        tfParent:ok:=tc=ttSignature;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttVarByRef:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttClass:
      case f of
        tfParent:ok:=tc=ttNameSpace;
        tfNext:anydecl;
        tfFirstItem:ok:=tc in [ttVar,ttMember,ttConstructors,ttDestructor];
        tfInheritsFrom:ok:=tc=ttClass;
      end;
    ttConstructors:
      case f of
        tfParent:ok:=tc=ttClass;
        tfNext:ok:=tc in [ttVar,ttMember,ttDestructor];
        tfFirstItem:ok:=tc=ttConstructor;
      end;
    ttConstructor:
      case f of
        tfParent:ok:=tc=ttConstructors;
        tfNext:ok:=tc=ttConstructor;
        tfSourceFile:ok:=tc=ttSourceFile;
        tfSignature:ok:=tc=ttSignature;
        tfBody:ok:=tc=ttCodeBlock;
        tfFirstArgument:ok:=tc in [ttVar,ttVarByRef];
      end;
    ttDestructor:
      case f of
        tfParent:ok:=tc=ttClass;
        tfNext:ok:=tc in [ttVar,ttMember,ttDestructor];
        tfSignature:ok:=tc=ttSignature;
        tfBody:ok:=tc=ttCodeBlock;
        //no tfFirstArgument!
      end;
    ttClassRef:
      case f of
        tfParent:ok:=tc in [ttClass,ttNameSpace];
        tfNext:anydecl;
        tfEvaluatesTo:ok:=tc=ttClass;
      end;
    ttPropCall:
      case f of
        tfParent:ok:=tc=ttCodeBlock;
        tfNext:ok:=(tc and tt__IsType)=0;
        tfFirstArgument:ok:=(tc and tt__Typed)<>0;
        tfTarget:ok:=tc in [ttMember,ttOverload,
          ttClass,//only when parsing, to resolve later
          ttField,ttCast,ttVar,ttPointer,//TODO: resolve more?
          ttConstructor,ttDestructor];
        tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
      end;
    ttPrivate:
      case f of
        tfParent:ok:=tc=ttNameSpace;
        tfNext:anydecl;
        tfSourceFile:ok:=tc=ttSourceFile;
        tfTarget:ok:=tc=ttNameSpace;
      end;
  end;
  if not ok then
    raise Exception.CreateFmt('Child not allowed for field %.4x[%.3x]=%.4x',
      [tt,f,tc]);
end;
{$ENDIF}


end.
