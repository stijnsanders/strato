unit stratoRunTime;

interface

uses stratoSphere, stratoDecl, stratoLogic, stratoFn;

const
  SystemWordSize=4;//32-bits? 64-bits?

  stratoSysCall_xinc=1;
  stratoSysCall_xdec=2;
  stratoSysCall_malloc=$100;
  stratoSysCall_realloc=$101;
  stratoSysCall_mfree=$102;
  stratoSysCall_writeln=$200;

procedure DefaultTypes(Store:TStratoStore;InlineErrors:boolean;
  var ErrorCount:integer);

var
  TypeDecl_void,TypeDecl_type,TypeDecl_number,TypeDecl_intLast,
  TypeDecl_pointer,TypeDecl_bool,TypeDecl_string,TypeDecl_variant,
  TypeDecl_object,TypeDecl_hash:TStratoIndex;
  Name_Inherited:TStratoName;

implementation

uses SysUtils, stratoSource, stratoParse;

procedure DefaultTypes(Store:TStratoStore;InlineErrors:boolean;
  var ErrorCount:integer);
var
  Source:TStratoSource;
  Sphere:TStratoSphere;
  ns,nn:TStratoIndex;

  function A(const Name:UTF8String;s:integer): TStratoIndex;
  begin
    Result:=Sphere.Add(ttTypeDecl,
      [tfName,Store.Dict.StrIdx(Name)
      ,tfParent,ns
      ,tfByteSize,s
      ]);
    if nn=0 then
      Sphere.s(ns,tfFirstItem,Result)
    else
      Sphere.s(nn,tfNext,Result);
    nn:=Result;
  end;

  const
    C_ByRef=$10000;

  procedure C(const FunctionName,SignatureName:UTF8String;Op:cardinal;
    const Arguments:array of TStratoIndex;ReturnType:TStratoIndex);
  var
    i,bs:integer;
    n,n1,p1,p2,p3,s:TStratoIndex;
  begin
    n1:=Sphere.Add(ttMember,
      [tfName,Store.Dict.StrIdx(FunctionName)
      ,tfParent,ns
      ]);
    n:=Sphere.Add(ttOverload,
      [tfParent,n1
      ]);
    Sphere.s(n1,tfFirstItem,n);
    Sphere.s(nn,tfNext,n1);
    nn:=n1;

    s:=Sphere.Add(ttSignature,
      [tfName,Store.Dict.StrIdx(SignatureName)
      ,tfParent,n
      ,tfEvaluatesTo,ReturnType
      ]);
    Sphere.s(n,tfSignature,s);

    p2:=0;
    for i:=0 to Length(Arguments)-1 do
     begin
      if (Arguments[i] and C_ByRef)=0 then
        p1:=Sphere.Add(ttArgument,
          [tfParent,s
          ,tfEvaluatesTo,Arguments[i]
          ])
      else
        p1:=Sphere.Add(ttArgByRef,
          [tfParent,s
          ,tfEvaluatesTo,Arguments[i] xor C_ByRef
          ]);
      if p2=0 then Sphere.s(s,tfFirstArgument,p1) else Sphere.s(p2,tfNext,p1);
      p2:=p1;
     end;

    bs:=0;
    p1:=Sphere.Add(ttCodeBlock,
      [tfParent,n
      ]);
    Sphere.s(n,tfBody,p1);
    p2:=Sphere.Add(ttSysCall,
      [tfParent,n
      ,tfOperator,Op
      ]);
    Sphere.s(p1,tfFirstStatement,p2);

    if ReturnType<>0 then
     begin
      p2:=Sphere.Add(ttVar,
        [tfName,Store.Dict.StrIdx(FunctionName)
        ,tfParent,p1
        ,tfEvaluatesTo,ReturnType
        ,tfOffset,bs
        ]);
      inc(bs,ByteSize(Sphere,ReturnType));
      Sphere.s(p1,tfFirstItem,p2);
     end
    else
      p2:=0;

    for i:=0 to Length(Arguments)-1 do
     begin
      if (Arguments[i] and C_ByRef)=0 then
       begin
        p3:=Sphere.Add(ttVar,
           [tfParent,p1
           ,tfOffset,bs
           ,tfEvaluatesTo,Arguments[i]
           ]);
        inc(bs,Sphere.v(Arguments[i],tfByteSize));
       end
      else
       begin
        p3:=Sphere.Add(ttVarByRef,
           [tfParent,p1
           ,tfOffset,bs
           ,tfEvaluatesTo,Arguments[i] xor C_ByRef
           ]);
        inc(bs,SystemWordSize);
       end;
      if p2=0 then
        Sphere.s(p1,tfFirstItem,p3)
      else
        Sphere.s(p2,tfNext,p3);
      p2:=p3;
      if i=0 then Sphere.s(n,tfFirstArgument,p3);
     end;
    Sphere.s(p1,tfByteSize,bs);

  end;

var
  p:TStratoIndex;
  function pp(const Name:UTF8String):TStratoIndex;
  begin
    Result:=Sphere.Lookup(p,tfFirstItem,Store.Dict.StrIdx(Name));
    if Result=0 then
      raise Exception.Create('Run-time library missing "'+Name+'"');
  end;
begin
  Source:=TStratoSource.Create;//pro-forma
  Sphere:=TStratoSphere.Create(Store,Source);
  try
    {
    Source.LoadFromFile(Store.ResolvePath('$compiler\Strato.xs'));
    p:=StratoParseSource(Store,Source,InlineErrors);
    inc(ErrorCount,Source.ErrorCount);
    }

    ns:=Sphere.Add(ttNameSpace,
      [tfName,Store.Dict.StrIdx('Strato')
      ]);
    Sphere.s(Sphere.Module,tf_Module_FirstNameSpace,ns);

    nn:=0;
    TypeDecl_void:=A('void',0);
    TypeDecl_type:=A('type',SystemWordSize);
    TypeDecl_number:=A('number',SystemWordSize);//TODO: other numerics inherit, (auto)casting
    TypeDecl_pointer:=A('pointer',SystemWordSize);
    TypeDecl_bool:=A('bool',SystemWordSize);
    TypeDecl_string:=A('string',SystemWordSize);
    TypeDecl_variant:=A('variant',16);//TODO: OLE compatible variants
    A('i8',1);
    A('i16',2);
    A('i32',4);
    A('i64',8);
    A('u8',1);
    A('u16',2);
    A('u32',4);
    TypeDecl_intLast:=A('u64',8);
    A('f32',4);//TODO: floating-point support
    A('f64',8);
    TypeDecl_object:=0;//see StratoParseSource: allow only one object()={}
    TypeDecl_hash:=A('hash',SystemWordSize);//TODO:

    C('__xinc','__xinc(^number)',
      stratoSysCall_xinc,[TypeDecl_number or C_ByRef],TypeDecl_number);
    C('__xdec','__xdec(^number)',
      stratoSysCall_xdec,[TypeDecl_number or C_ByRef],TypeDecl_number);
    C('__writeln','__writeln(string)',
      stratoSysCall_writeln,[TypeDecl_string],0);
    C('__malloc','__malloc(number)',
      stratoSysCall_malloc,[TypeDecl_number],TypeDecl_pointer);
    C('__realloc','__realloc(pointer,number)',
      stratoSysCall_realloc,[TypeDecl_pointer,TypeDecl_number],TypeDecl_pointer);
    C('__mfree','__mfree(pointer)',
      stratoSysCall_mfree,[TypeDecl_pointer],0);

    //TODO: exitcode (and all system things)
    //TODO: objects with reference counting
    //TODO: strings with reference counting, copy-on-write, etc
  finally
    Sphere.Free;
    Source.Free;
  end;
end;

end.
