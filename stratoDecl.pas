unit stratoDecl;

{

stratoDecl

  declares TStratoThing and related types

TStratoThingType constant values are declared here,
they determine which fields of the TStratoThing record
are used to hold which value, this way the total size
of the TStratoThing record is held low at 32 bytes,
and enables faster random access into arrays of
TStratoThing records as used by TStratoSphere.

}

interface

type
  TStratoThingType=type cardinal;
  TStratoIndex=type cardinal;
  TStratoName=type cardinal;

const
  //About TStratoThing:
  //not all ThingType's use all of the TStratoThing fields,
  //also to save space some overlap,
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


type
  TStratoThing=record
    ThingType:TStratoThingType;
    Parent:TStratoIndex;
    Next:TStratoIndex;
    case cardinal of
      //ATTENTION!
      //  Below identifiers carefully chosen and ordered to correctly overlap
      //  in accorance with their meaning given by the value of ThingType.
      ttTypeDecl:(
        Name:TStratoName;
        FirstItem:TStratoIndex;
        ByteSize:cardinal;
        InheritsFrom:TStratoIndex;
        SrcPos:cardinal;
      );
      ttVar:(
        FirstStatement:TStratoIndex;//ttCodeBlock
        InitialValue:TStratoIndex;
        Offset:cardinal;
        EvaluatesTo:TStratoIndex;
      );
      ttAssign:(
        Subject,
        ValueFrom,
        AssignTo:TStratoIndex;
      );
      ttBinaryOp:(
        Op:cardinal;
        Left,
        Right:TStratoIndex;
      );
      ttOverload:(
        SourceFile:TStratoIndex;//ttNameSpace,ttOverload,ttConstructor only!
        FirstArgument,
        Target,
        Body:TStratoIndex;
      );
      ttSelection:(
        DoIf,
        DoThen,
        DoElse:TStratoIndex;//ttIteration:DoFirst
      );
  end;
  PStratoThing=^TStratoThing;

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

const
{$IFDEF DEBUG}
  IndexStep1=999999901;
  IndexStep2=999999902;
  IndexStep3=999999903;
  IndexStep4=999999904;
  IndexStep5=999999905;
  IndexStep6=999999906;
  IndexStep7=999999907;
{$ELSE}
  IndexStep1={TStratoIndex}cardinal(-$E);
  IndexStep2={TStratoIndex}cardinal(-$D);
  IndexStep3={TStratoIndex}cardinal(-$C);
  IndexStep4={TStratoIndex}cardinal(-$B);
  IndexStep5={TStratoIndex}cardinal(-$A);
  IndexStep6={TStratoIndex}cardinal(-$9);
  IndexStep7={TStratoIndex}cardinal(-$8);
{$ENDIF}
  
implementation

end.
