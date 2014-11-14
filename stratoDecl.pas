unit stratoDecl;

interface

type
  TStratoIndex=type cardinal;
  TStratoName=type cardinal;

const
  //About TStratoThing:
  //not all ThingType's use all of the TStratoThing fields,
  //also to save space some overlap,
  //use below guide to determine which fields
  //are in use for each ThingType

  ttHeader       = $00727453;//'Str'#0;
    //SrcIndexLineMultiplier: see stratoTokenizer
    //Version: see TStratoSphere.Create

  ttSourceFile   = $0100;
    //FileSize
    //FileName (ttBinaryData)
    //TODO: FileCRC32,FileDate

  ttBinaryData   = $0200;
    //[Name]:size of data
    //[Parent]:start of data


  tt__Resolvable = $0010;
    //[bitmask: things that use FirstItem]
  tt__Typed      = $0020;
    //[bitmask: things that use EvaluateTo]
  tt__IsType     = $0040;
    //[bitmask: things allowed as EvaluatesTo]

  ttNameSpace    = $0011;
    //FirstInitialization (ttCodeBlock)
    //FirstFinalization (ttCodeBlock)
    //FirstItem (*)

  ttTypeDecl     = $0050;
    //ByteSize: memory used by var of type
    //FirstItem (*)
    //InheritsFrom (ttTypeDecl)

  ttRecord       = $0051;
    //ByteSize
    //FirstItem (*)
    //InheritsFrom (ttRecord)

  ttEnumeration  = $0052;
    //ByteSize
    //FirstItem (ttConstant)

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
    //[bitmask: things that use Subject]

  ttImport       = $0081;
    //Subject

  ttAlias        = $0082;
    //Subject

  ttGlobal       = $0083;
    //Subject (ttVar)


  ttSignature    = $00E0;
    //Subject (ttTypeDecl): call subject (this)
    //EvaluatesTo (ttTypeDecl): return value
    //FirstArgument (*): first argument

  ttFunction     = $0001;
    //FirstItem (ttOverload): first overload

  ttOverload     = $0002;
    //Signature (ttSignature): call signature
    //Body (ttCodeBlock): overload body
    //FirstArgument (*): first argument value in overload body

  ttFnCall       = $0084;
    //Subject (ttOverload, ttVarIndex)
    //FirstArgument (*)
    //Signature (ttSignature): matching signature
    //Body (ttCodeBlock): body from overload with matching signature (see ttOverload)

  ttArgument     = $00A0;
    //Subject (*): argument value (not ValueFrom!)
    //EvaluatesTo (ttTypeDecl): argument type (used to find suitable signature)
    //InitialValue (ttLiteral): default value (signature only)

  ttAssign       = $0024;
    //Op
    //EvaluatesTo (ttTypeDecl)
    //ValueFrom (*)
    //AssignTo (ttVar)

  ttUnaryOp      = $0025;
    //Op
    //EvaluatesTo (ttTypeDecl)
    //Right (*): [ATTENTION! Right and not Subject!]

  ttBinaryOp     = $0026;
    //Op
    //EvaluatesTo (ttTypeDecl)
    //Left (*)
    //Right (*)

  ttCast         = $00A1;
    //Subject (*)
    //EvaluatesTo (ttTypeDecl)

  ttSelection    = $0123;
    //DoIf: criterium
    //EvaluatesTo (ttTypeDecl)
    //DoThen: to perform when criterium evaluates to true
    //DoElse: to perform when criterium evaluates to false

  ttIteration    = $0101;
  ttIterationPE  = $0102;//post evaluation
    //DoIf: criterium
    //DoFirst: initialization (e.g. i=0)
    //DoThen: do between body and criterium (e.g. i++)
    //Body

  ttTry          = $0003;
    //Subject: (ttDeffered or ttCatch)

  ttThrow        = $0004;
    //Subject: object (creator) to throw
    //TODO: (EvaluatesTo?

  ttDeferred     = $0005;
    //Subject: deferred command

  ttCatch        = $0006;
    //Subject: exception handling command(s)
    //ItemType: exception object mask
    //FirstItem (ttVar): exception object reference

  ttSysCall      = $0007;
    //Op : internal value (see PerformSysCall)


  ttArray        = $0041;
    //ByteSize
    //ItemType (ttTypeDecl)

  ttVarIndex     = $0027;
    //Parent (ttVar)
    //Subject: struct member
    //EvaluateTo
    //FirstArgument: index value

  ttThis         = $0028;
    //Offset
    //EvaluatesTo (ttTypeDecl)

  ttPointer      = $00C0;
    //ByteSize: SystemWordSize
    //EvaluatesTo (ttTypeDecl)

  ttAddressOf    = $0029;
    //EvaluatesTo (ttTypeDecl)
    //ValueFrom (ttVar)

  ttDereference  = $002A;
    //EvaluatesTo (ttTypeDecl): from pointer
    //ValueFrom (ttVar): of type pointer

  ttArgByRef     = $00A2;
    //EvaluatesTo (ttTypeDecl)

  ttVarByRef     = $00A3;
    //Offset
    //EvaluatesTo (ttTypeDecl)

  ttClass        = $00D0;
    //ByteSize (of data, not value since that's a pointer)
    //FirstConstructor (ttConstructor)
    //FirstItem (ttVar, ttFunction, ttProperty)
    //InheritsFrom (ttClass)

  ttConstructor  = $000A;
    //Signature (ttSignature): first overload signature
    //Body (ttCodeBlock): first overload body
    //FirstArgument (*): first argument value in overload body

  ttDestructor   = $000B;
    //Body (ttCodeBlock)

  ttInterface    = $00D1;
    //ByteSize: SystemWordSize (since it's a pointer!)
    //FirstItem (ttVar, ttFunction, ttProperty)
    //InheritsFrom (ttRecord)

  ttProperty     = $002B;
    //EvaluatesTo (ttTypeDecl)
    //ValueFrom (ttOverload)
    //AssignTo (ttOverload)

type
  TStratoThing=record
    ThingType:cardinal;
    Name:TStratoName;
    Parent:TStratoIndex;
    Next:TStratoIndex; //TODO: something better than linked list for look-up
    case cardinal of
      //ATTENTION!
      //  Below identifiers carefully chosen and ordered to correctly overlap
      //  in accorance with their meaning given by the value of ThingType.
      ttCodeBlock,ttTypeDecl:(
        ByteSize:cardinal;
        ItemType,FirstItem,FirstStatement:TStratoIndex;
        Source:TStratoIndex;
        SrcPos,
        x_Reserved1,x_Reserved2:cardinal;
      );
      ttVar:(
        Offset:cardinal;
        EvaluatesTo,InitialValue,InheritsFrom:TStratoIndex;
      );
      ttSignature,ttOverload,ttFnCall:(
        Subject,Signature,FirstArgument,Body:TStratoIndex;
      );
      ttBinaryOp:(
        Op:cardinal;
        FirstConstructor,//ttClass
        Left,Right:TStratoIndex;
      );
      ttNameSpace,ttAssign:(
        FirstInitialization,
        FirstFinalization,
        ValueFrom,AssignTo:TStratoIndex;
      );
      ttSelection,ttIteration:(
        DoIf,DoFirst,DoThen,DoElse:TStratoIndex;
      );
  end;
  PStratoThing=^TStratoThing;

  TStratoHeader=record
    ThingType:cardinal;//ttHeader
    Name:cardinal;
    FirstInitialization,FirstFinalization:TStratoIndex;
    ThingCount:cardinal;
    SrcIndexLineMultiplier:cardinal;
    FirstNameSpace,FirstGlobalVar:TStratoIndex;
    GlobalByteSize:cardinal;
    x_Reserved4,x_Reserved5:TStratoIndex;
    Version:cardinal;
  end;
  PStratoHeader=^TStratoHeader;

  TStratoSourceFile=record
    ThingType:cardinal;
    Name:TStratoName;
    InitializationCode,FinalizationCode:TStratoIndex;
    FileSize:cardinal;
    NameSpace,FileName:TStratoIndex;
    //FileCRC32,FileDate:cardinal;
  end;
  PStratoSourceFile=^TStratoSourceFile;

implementation

end.
