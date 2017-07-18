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

procedure DefaultTypes(Store:TStratoStore);

var
  TypeDecl_void,TypeDecl_type,TypeDecl_bool,TypeDecl_string,
  TypeDecl_number,TypeDecl_intLast,TypeDecl_variant,TypeDecl_pointer,
  TypeDecl_hash,TypeDecl_object:xItem;

implementation

uses SysUtils;

procedure DefaultTypes(Store:TStratoStore);
var
  Sphere:TStratoSphere;
  ns:xItem;

  function A(const Name:UTF8String;s:integer):xItem;
  begin
    Result:=Sphere.Add(ns,fItems,nTypeDecl,ns,0,
      [vName,Store.Dict.StrIdx(Name)
      ,vByteSize,s
      ]);
  end;

  procedure C(const FunctionName,SignatureName:UTF8String;Op:xValue;
    const Arguments:array of xItem;ReturnType:xItem);
  var
    m,o,s:xItem;
    i,l:integer;
    cb,v1:xItem;
    bs:PxValue;
  begin
    m:=Sphere.Add(ns,fItems,nMember,ns,0,[vName,Store.Dict.StrIdx(FunctionName)]);

    o:=Sphere.Add(nOverload,m,0,[]);
    Sphere.Append(m,fItems,o);

    s:=Sphere.Add(nSignature,o,0,
      [vName,Store.Dict.StrIdx(SignatureName)
      ,fReturnType,ReturnType
      ]);
    Sphere.n(o,fSignature)^:=s;

    i:=0;
    l:=Length(Arguments);
    while i<l do
     begin
      if Arguments[i]<>0 then //nil prefix for arg-by-ref?
        Sphere.Add(s,fArguments,nArgument,s,0,[fTypeDecl,Arguments[i]])
      else
       begin
        inc(i);
        Sphere.Add(s,fArguments,nArgByRef,s,0,[fTypeDecl,Arguments[i]]);
       end;
      inc(i);
     end;

    cb:=Sphere.Add(nCodeBlock,o,0,[fReturnType,ReturnType]);
    bs:=Sphere.n(cb,vByteSize);
    Sphere.n(o,fBody)^:=cb;
    Sphere.Add(cb,fItems,nSysCall,cb,0,[vOffset,Op]);

    if ReturnType<>0 then
     begin
      Sphere.Add(cb,fVarDecls,nVarDecl,cb,0,
        [fTypeDecl,ReturnType
        ,vOffset,bs^
        ]);
      inc(bs^,ByteSize(Sphere,ReturnType));
     end;

    i:=0;
    l:=Length(Arguments);
    while i<l do
     begin
      if Arguments[i]<>0 then //nil prefix for arg-by-ref?
       begin
        v1:=Sphere.Add(cb,fVarDecls,nVarDecl,cb,0,
          [fTypeDecl,Arguments[i]
          ,vOffset,bs^
          ]);
        inc(bs^,ByteSize(Sphere,Arguments[i]));
       end
      else
       begin
        inc(i);
        v1:=Sphere.Add(cb,fVarDecls,nVarByRef,cb,0,
          [fTypeDecl,Arguments[i]
          ,vOffset,bs^
          ]);
        inc(bs^,SystemWordSize);
       end;
      if i=0 then Sphere.n(o,fFirstArgVar)^:=v1;
      inc(i);
     end;
  end;
  
begin
  Sphere:=TStratoSphere.Create(Store,nil);
  try
    ns:=Sphere.Add(Sphere.SourceFile,fSourceFile_NameSpaces,nNameSpace,0,0,
      [vName,Store.Dict.StrIdx('Strato')]);

    TypeDecl_void:=A('void',0);
    TypeDecl_type:=A('type',SystemWordSize);
    TypeDecl_bool:=A('bool',SystemWordSize);
    TypeDecl_string:=A('string',SystemWordSize);
    TypeDecl_variant:=A('variant',16);//TODO: OLE compatible variants
    TypeDecl_number:=A('number',SystemWordSize);//TODO: other numerics inherit, (auto)casting
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
    TypeDecl_pointer:=A('pointer',SystemWordSize);

    C('__xinc','__xinc(^number)',
      stratoSysCall_xinc,[0,TypeDecl_number],TypeDecl_number);
    C('__xdec','__xdec(^number)',
      stratoSysCall_xdec,[0,TypeDecl_number],TypeDecl_number);
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
  end;
end;

end.
