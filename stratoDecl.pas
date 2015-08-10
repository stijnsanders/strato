unit stratoDecl;

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
    //Source (0,ttSourceFile)
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

  ttFunction     = $0001;
    //FirstItem (0,ttOverload): first overload

  ttOverload     = $0002;
    //SourceFile (ttSourceFile)
    //FirstArgument (*): first argument value in overload body
    //Target (ttSignature): call signature
    //Body (ttCodeBlock): overload body

  ttFnCall       = $0084;
    //FirstArgument (0,ttArgument)
    //Target (ttVarIndex,ttOverload,ttConstructor,ttDestructor):
    //EvaluatesTo (ttClass): only with ttConstructor, in case of class without own constructor 

  ttArgument     = $00A0;
    //InitialValue (ttLiteral): default value (signature only)
    //Target (*): argument value [ATTENTION! Target and not ValueFrom!]
    //EvaluatesTo (ttTypeDecl): argument type (used to find suitable signature)

  ttAssign       = $0003;
    //Op
    //ValueFrom (*)
    //AssignTo (ttVar,ttVarIndex,ttCast)

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
    //DoIf (0,ttTypeDecl): exception object mask
    //FirstArgument (ttVar): exception object reference
    //Body (*) exception handling command(s)

  ttSysCall      = $0007;
    //Op : internal value (see PerformSysCall)

  ttArray        = $0041;
    //ByteSize: total array memory size
    //ElementType (ttTypeDecl)

  ttVarIndex     = $0027;
    //Parent (ttVar)
    //Target
    //EvaluateTo
    //FirstArgument: index value

  ttThis         = $0028;
    //Offset
    //EvaluatesTo (ttTypeDecl)

  ttInherited    = $0029;
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
    //FirstItem (0, ttVar, ttFunction, ttProperty, ttConstructor)
    //ByteSize: size of data, not value since that's a pointer
    //InheritsFrom (0, ttClass)
    //Target (ttClassInfo)

  ttConstructor  = $000A;
    //FirstArgument (*): first argument value in overload body
    //Target (ttSignature)
    //Body (ttCodeBlock): first overload body

  ttDestructor   = $000B;
    //Target (ttSignature)
    //Body (ttCodeBlock)

  ttInterface    = $00D1;
    //ByteSize: SystemWordSize (since it's a pointer!)
    //FirstItem (ttVar, ttFunction, ttProperty)
    //InheritsFrom (ttRecord)

  ttProperty     = $002C;
    //EvaluatesTo (ttTypeDecl)
    //ValueFrom (ttOverload)
    //AssignTo (ttOverload)

  ttClassRef     = $00C1;
    //ByteSize: SystemWordSize
    //EvaluatesTo (ttClass)

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
        Op:cardinal;
        ValueFrom,
        AssignTo:TStratoIndex;
      );
      ttBinaryOp:(
        SourceFile:TStratoIndex;//ttOverload only!
        Left,
        Right:TStratoIndex;
      );
      ttOverload:(
        ElementType:cardinal;//ttArray only!
        FirstArgument,
        Target:TStratoIndex;
      );
      ttSelection:(
        DoIf,
        DoThen,
        DoElse,//ttSelection:DoElse,ttIteration:DoFirst
        Body:TStratoIndex;
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

implementation

end.
