unit stratoDecl;

interface

type
  TStratoThingType=type cardinal;
  TStratoIndex=type cardinal;
  TStratoField=type cardinal;
  TStratoName=type cardinal;

  TStratoThing=array[0..7] of cardinal;
  PStratoThing=^TStratoThing;//only used by TStratoSphere internally
  
const
  //About TStratoThing:
  //not all ThingType's use all of the TStratoThing fields,
  //also to save space, some overlap
  //function "rx" is used to govern which fields
  //are in use for each ThingType
  //constant values here are carefully chosen to be binary relevant (see bitmasks) and not to overlap

  ttFileMarker   = $00727453;//'Str'#0;
  ttSourceFile   = $0100;
  ttBinaryData   = $0200;

  tt__Resolvable = $0010;   //bitmask: things that use FirstItem
  tt__Typed      = $0020;   //bitmask: things that use EvaluateTo
  tt__IsType     = $0040;   //bitmask: things allowed as EvaluatesTo
  //TODO: tt__Named?

  ttNameSpace    = $0011;
  ttTypeDecl     = $0050;
  ttRecord       = $0051;
  ttEnumeration  = $0052;
  ttLiteral      = $0021;
  ttVar          = $0022;
  ttConstant     = $0023;
  ttCodeBlock    = $0131;

  tt__Directed   = $0080;   //bitmask: things that use Target

  ttImport       = $0081;
  ttAlias        = $0082;
  ttGlobal       = $0083;   //see pHeader tf_FirstGlobalVar,tf_GlobalByteSize
  ttPrivate      = $0091;
  ttSignature    = $00E0;
  ttMember       = $0001;
  ttOverload     = $0002;
  ttFnCall       = $0084;
  ttArgument     = $00A0;
  ttAssign       = $0003;
  ttUnaryOp      = $0025;
  ttBinaryOp     = $0026;
  ttCast         = $00A1;
  ttSelection    = $0123;
  ttIteration    = $0101;
  ttIterationPE  = $0102;//post evaluation
  ttTry          = $0004;
  ttThrow        = $0008;
  ttDeferred     = $0005;
  ttCatch        = $0006;
  ttSysCall      = $0007;
  ttArray        = $0041;
  ttArrayIndex   = $0024;
  ttField        = $0027;
  ttThis         = $0028;
  ttPointer      = $00C0;
  ttAddressOf    = $002A;
  ttDereference  = $002B;
  ttArgByRef     = $00A2;
  ttVarByRef     = $00A3;
  ttClass        = $00D0;
  ttConstructors = $000A;
  ttConstructor  = $000B;
  ttDestructor   = $000C;
  ttClassRef     = $00C1;
  ttInterface    = $00D1;
  ttPropertyGet  = $000D;
  ttPropertySet  = $000E;
  ttPropCall     = $0085;

  //TStratoThing fields:
  tf__FieldIndex   = $800; //bitmask: prevents overlap with tt*
  tf__IsValue      = $400; //bitmask: field contains plain cardinal, not TStratoIndex
  tfThingType      = $C00;//use Sphere.t
  tfParent         = $801;
  tfNext           = $802;
  tfName           = $C03;//TStratoName
  tfFirstItem      = $804;
  tfByteSize       = $C05;
  tfInheritsFrom   = $806;
  tfSrcPos         = $C07;
  tfFirstStatement = $813;
  tfInitialValue   = $814;
  tfOffset         = $C15;
  tfEvaluatesTo    = $816;
  tfSubject        = $823;
  tfValueFrom      = $824;
  tfAssignTo       = $825;
  tfOperator       = $C33;//TStratoToken (stOp*)
  tfLeft           = $834;
  tfRight          = $835;
  tfSourceFile     = $843;
  tfFirstArgument  = $844;
  tfSignature      = $845;
  tfBody           = $846;
  tfTarget         = $875;
  tfDoIf           = $853;
  tfDoThen         = $854;
  tfDoElse         = $855;
  tfDoFirst        = $865;

  //header (ttFileMarker)
  pHeader=cardinal(-5);//see TStratoSphere.GetNode
  tf_ThingCount          =$D01;
  tf_Version             =$D02;
  tf_FirstNameSpace      =$903;
  tf_FirstGlobalVar      =$904;
  tf_GlobalByteSize      =$D05;
  tf_FirstInitialization =$906;
  tf_FirstFinalization   =$907;

  //ttSourceFile
  tf_SourceFile_FileName           =$A01;
  tf_SourceFile_FileSize           =$E02;
  tf_SourceFile_SrcPosLineIndex    =$E03;
  tf_SourceFile_PartOfModule       =$A05;
  tf_SourceFile_InitializationCode =$A06;
  tf_SourceFile_FinalizationCode   =$A07;

  //ttNameSpace
  tf_NameSpace_SourceFile          =$B05;

{$IFDEF DEBUG}
type
  rxt=function(p:TStratoIndex):TStratoThingType of object;

function rx(tt:TStratoThingType;f:TStratoField;t:rxt;q:TStratoIndex):cardinal;
{$ENDIF}

type
  TStratoBlockHeader=record
    FirstIndex,
    ThingCount,
    xReserved1:cardinal;
    xReserved2,
    Module:TStratoIndex;
    xReserved4:cardinal;
    xReserved5,
    xReserved6:TStratoIndex;
  end;

implementation

uses SysUtils;

{$IFDEF DEBUG}

{xx$D-}
{xx$L-}

function rx(tt:TStratoThingType;f:TStratoField;t:rxt;q:TStratoIndex):cardinal;
var
  ok:boolean;
  tc:TStratoThingType;
  procedure anydecl;
  begin
    ok:=tc in [0,ttTypeDecl,ttRecord,ttEnumeration,ttClass,ttInterface,
      ttVar,ttVarByRef,ttConstant,ttSignature,ttMember,ttDestructor,
      ttClassRef];
  end;
  procedure anyval;
  begin
    ok:=((tc and tt__Typed)<>0) or (tc in [ttFnCall,ttPropCall]);
  end;
begin
  if ((f and tf__IsValue)=0) and (q<>0) and (@t<>nil) then tc:=t(q) else tc:=0;
  if (f=tfSrcPos) and (tt<>ttSourceFile) then Result:=7 else
   begin
    ok:=false;//default
    case tt of
      ttFileMarker:
        case f of
          tf_ThingCount,
          tf_Version:ok:=true;
          tf_FirstNameSpace:ok:=(q=0) or (tc=ttNameSpace);
          tf_FirstGlobalVar:ok:=(q=0) or (tc=ttGlobal);
          tf_GlobalByteSize:ok:=true;
          tf_FirstInitialization,
          tf_FirstFinalization:ok:=(q=0) or (tc=ttCodeBlock);
        end;
      ttSourceFile:
        case f of
          tf_SourceFile_FileName:ok:=tc=ttBinaryData;
          tf_SourceFile_FileSize:ok:=true;
          tf_SourceFile_SrcPosLineIndex:ok:=true;
          tf_SourceFile_PartOfModule:ok:=(q=0) or (tc=ttNameSpace);
          tf_SourceFile_InitializationCode,
          tf_SourceFile_FinalizationCode:ok:=(q=0) or (tc=ttCodeBlock);
          //TODO: FileCRC32,FileDate
        end;
      ttNameSpace:
        case f of
          tfParent:ok:=(q=0) or (tc=ttNameSpace);
          tfName:ok:=true;
          tfFirstItem,tfNext:if tc=ttNameSpace then ok:=true else anydecl;
          tf_NameSpace_SourceFile:ok:=(q=0) or (tc=ttSourceFile);
        end;
      ttTypeDecl,ttRecord,ttEnumeration:
        case f of
          tfParent:ok:=tc=ttNameSpace;
          tfNext:anydecl;
          tfName:ok:=true;
          tfFirstItem:if q=0 then ok:=true else
            case tt of
              ttRecord:ok:=tc in [ttVar,ttMember,ttPropertyGet,ttPropertySet];
              ttEnumeration:ok:=tc=ttConstant;
            end;
          tfByteSize:ok:=true;
        end;
      ttLiteral:
        case f of
          tfParent:ok:=(q=0) or (tc=ttCodeBlock);
          tfNext:anydecl;
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
          tfName:ok:=true;
          tfInitialValue:ok:=tc in [0,ttLiteral,ttConstant];
          tfOffset:ok:=true;
          tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
        end;
      ttConstant:
        case f of
          tfParent:
            case tc of
              ttNameSpace,ttClass,ttCodeBlock:ok:=true;
            end;
          tfNext:anydecl;
          tfName:ok:=true;
          tfInitialValue:ok:=tc in [ttLiteral,ttConstant];
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
          tfFirstItem:ok:=tc in [0,ttVar,ttThis];
          tfFirstStatement:ok:=(tc and tt__IsType)=0;
          tfByteSize:ok:=true;
          tfEvaluatesTo:ok:=(q=0) or ((tc and tt__IsType)<>0);
        end;
      ttImport:
        case f of
          tfParent,tfTarget:ok:=tc=ttNameSpace;
          tfName:ok:=true;
        end;
      ttAlias:
        case f of
          tfParent:
            case tc of
              ttCodeBlock,ttNameSpace:ok:=true;
            end;
          tfNext:ok:=tc=ttAlias;
          tfTarget:
            case tc of
              ttMember,ttCodeBlock:ok:=true;
            end;
        end;
      ttGlobal:
        case f of
          tfNext:ok:=tc in [0,ttGlobal];
          tfTarget:ok:=tc=ttVar;
        end;
      ttPrivate:
        case f of
          tfParent:ok:=tc=ttNameSpace;
          tfNext:anydecl;
          tfSourceFile:ok:=tc=ttSourceFile;
          tfFirstItem:anydecl;
          tfTarget:ok:=tc=ttNameSpace;
        end;
      ttSignature:
        case f of
          tfParent:ok:=tc in [ttNameSpace,ttClass,ttRecord,ttOverload];
          tfNext:anydecl;
          tfName:ok:=true;
          tfFirstArgument:ok:=tc in [0,ttArgument,ttArgByRef];
          tfTarget:ok:=tc in [0,ttClass,ttRecord]; //call subject (this)
          tfEvaluatesTo:ok:=(q=0) or ((tc and tt__IsType)<>0); //return value type
        end;
      ttMember:
        case f of
          tfParent:ok:=tc in [ttNameSpace,ttClass,ttRecord];
          tfNext:anydecl;
          tfName:ok:=true;
          tfFirstItem:ok:=tc in [0,ttOverload,ttPropertyGet,ttPropertySet];
        end;
      ttOverload,ttPropertyGet,ttPropertySet:
        case f of
          tfParent:ok:=tc=ttMember;
          tfNext:ok:=tc in [0,ttOverload,ttPropertyGet,ttPropertySet];
          tfSourceFile:ok:=tc=ttSourceFile;
          tfFirstArgument:ok:=tc in [0,ttVar,ttVarByRef];
          tfSignature:ok:=tc=ttSignature;
          tfBody:ok:=tc=ttCodeBlock;//ttSysCall?
        end;
      ttFnCall:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfName:ok:=true; //=0 when constructor and calling inherited constructor
          tfFirstArgument:ok:=(q=0) or ((tc and tt__Typed)<>0);
          tfTarget:
            ok:=tc in [ttMember,ttOverload,
              ttClass,//only when parsing, to resolve later
              ttField,ttCast,ttVar,ttPointer,//TODO: resolve more?
              ttConstructor,ttDestructor];
          tfEvaluatesTo:ok:=(q=0) or ((tc and tt__IsType)<>0);
        end;
      ttArgument://TODO: split parent ttSignature or ttFnCall
        case f of
          tfParent:ok:=tc in [ttSignature,ttFnCall];
          tfNext:ok:=tc in [0,ttArgument,ttArgByRef];
          tfName:ok:=true;
          tfInitialValue: //default value (signature only)
            ok:=(q=0) or ((tc and tt__Typed)<>0);
          tfTarget: //argument value [ATTENTION! Target and not ValueFrom!]
            //0 when parent ttSignature, non-0 when parent ttFnCall
            ok:=(q=0) or ((tc and tt__Typed)<>0);
          tfEvaluatesTo://argument type (used to find suitable signature)
            ok:=(tc and tt__IsType)<>0;
        end;
      ttAssign:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfOperator:ok:=true;
          tfAssignTo:ok:=tc in [ttVar,ttArrayIndex,ttField,ttCast];
          tfValueFrom:anyval;
        end;
      ttUnaryOp:
        case f of
          tfThingType:ok:=q=ttFnCall;//only allow upgrade to destructor call
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfOperator:ok:=true;
          tfRight:anyval; //[ATTENTION! Right and not Target!]
          tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
        end;
      ttBinaryOp:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfOperator:ok:=true;
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
          tfEvaluatesTo:ok:=(q=0) or ((tc and tt__IsType)<>0);
        end;
      ttIteration,ttIterationPE:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfDoFirst,tfDoIf,tfDoThen,tfBody:ok:=(tc and tt__IsType)=0;
        end;
      ttTry:
        case f of
          tfTarget:ok:=tc in [ttDeferred,ttCatch];
        end;
      ttThrow:
        case f of
          tfTarget:ok:=tc=ttFnCall; //exception object constructor //TODO
        end;
      ttDeferred:
        case f of
          tfTarget:ok:=(tc and tt__IsType)<>0;//TODO
        end;
      ttCatch:
        case f of
          tfFirstArgument:ok:=tc=ttVar; //exception object reference
          tfTarget:ok:=(tc and tt__IsType)<>0; //exception object mask
          tfBody:ok:=tc=ttCodeBlock;
        end;
      ttSysCall:
        case f of
          tfParent:
            case tc of
              ttOverload,ttCodeBlock:ok:=true;
            end;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfOperator:ok:=true; //internal value (see PerformSysCall)
        end;
      ttArray:
        case f of
          tfParent:
            case tc of
              ttNameSpace,ttClass,ttCodeBlock:ok:=true;
            end;
          tfNext:anydecl;
          tfName:ok:=true;
          tfSubject:ok:=(tc and tt__Istype)<>0; //array element type (ttTypeDecl)
          //TODO: multi-dimensional arrays
          tfByteSize:ok:=true; //total array memory size
        end;
      ttArrayIndex:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfTarget:ok:=tc in [ttArray,ttField]; //x in x[y]
          tfFirstArgument:ok:=(tc and tt__IsType)<>0; //y in x[y]
          tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
        end;
      ttField:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfSubject:ok:=(tc and tt__Typed)<>0; //x in x.y
          tfTarget: //y in x.y
            ok:=tc in [ttVar,
              ttMember,//only when parsing, to resolve later
              ttOverload,ttConstructor,ttDestructor,ttPropertyGet,ttPropertySet];
          tfEvaluatesTo:ok:=(q=0) or //0 when as ttFnCall's tfTarget
            ((tc and tt__Typed)<>0) or ((tc and tt__IsType)<>0);
        end;
      ttThis:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(q=0) or (tc=ttVar);
          tfName:ok:=true;//'@@'
          tfOffset:ok:=true;
          tfEvaluatesTo:ok:=tc in [ttClass,ttRecord];
        end;
      ttPointer:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfByteSize:ok:=true;//ok:=q=SystemWordSize;
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
          tfNext:ok:=tc in [0,ttArgument,ttArgByRef];
          tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
        end;
      ttVarByRef:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfOffset:ok:=true;
          tfEvaluatesTo:ok:=(tc and tt__IsType)<>0;
        end;
      ttClass:
        case f of
          tfParent:ok:=tc=ttNameSpace;
          tfNext:anydecl;
          tfName:ok:=true;
          tfFirstItem:ok:=tc in [0,ttVar,ttMember,ttConstructors,ttDestructor];
          tfByteSize:ok:=true; //size of data, not value since that's a pointer
          tfInheritsFrom:ok:=(q=0) or (tc=ttClass);
        end;
      ttConstructors:
        case f of
          tfParent:ok:=tc=ttClass;
          tfNext:ok:=tc in [0,ttVar,ttMember,ttDestructor];
          tfName:ok:=true;//always 0!
          tfFirstItem:ok:=(q=0) or (tc=ttConstructor);
        end;
      ttConstructor: // (like ttOverload)
        case f of
          tfParent:ok:=tc=ttConstructors;
          tfNext:ok:=(q=0) or (tc=ttConstructor);
          tfSourceFile:ok:=tc=ttSourceFile;
          tfSignature:ok:=tc=ttSignature;
          tfBody:ok:=tc=ttCodeBlock;
          tfFirstArgument:ok:=tc in [0,ttVar,ttVarByRef];
        end;
      ttDestructor:
        case f of
          tfParent:ok:=tc=ttClass;
          tfNext:ok:=tc in [0,ttVar,ttMember,ttDestructor];
          tfName:ok:=true;
          tfSignature:ok:=tc=ttSignature;
          tfBody:ok:=tc=ttCodeBlock;
          //no tfFirstArgument!
        end;
      ttClassRef:
        case f of
          tfParent:
            case tc of
              ttClass,ttNameSpace,ttCodeBlock:ok:=true;
            end;
          tfNext:anydecl;
          tfName:ok:=true;
          tfByteSize:ok:=true;//ok:=q=SystemWordSize;
          tfEvaluatesTo:ok:=tc=ttClass;
        end;
      ttInterface:
        case f of
          tfFirstItem:ok:=tc=ttMember;
          tfByteSize:ok:=true;//ok:=q=SystemWordSize;//since it's a pointer
          tfInheritsFrom:ok:=tc=ttInterface;
        end;
      ttPropCall:
        case f of
          tfParent:ok:=tc=ttCodeBlock;
          tfNext:ok:=(tc and tt__IsType)=0;
          tfOperator:ok:=true; //0 with ttPropertyGet, stAssign* with ttPropertySet
          tfFirstArgument:ok:=(tc and tt__Typed)<>0;
          tfTarget:ok:=tc in [ttMember,ttOverload,
            ttClass,//only when parsing, to resolve later
            ttField,ttCast,ttVar,ttPointer,//TODO: resolve more?
            ttConstructor,ttDestructor];
          tfEvaluatesTo:ok:=(tc and tt__IsType)<>0; //ttPropertySet: value to set
        end;
    end;
    if ok then Result:=f and $07 else
      if tc=0 then
        raise Exception.CreateFmt(
          'Field not available for thing type %.4x[%.3x] (%d)',[tt,f,q])
      else
        raise Exception.CreateFmt(
          'Child not allowed for field %.4x[%.3x]=%.4x (%d)',[tt,f,tc,q]);
   end;
end;
{$ENDIF}

end.
