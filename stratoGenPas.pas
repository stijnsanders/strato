unit stratoGenPas;

interface

uses stratoSphere;

type
  TStratoGenPascal=class(TObject)
  public
    procedure GenerateSource(const FilePath:string);
  end;

implementation

uses SysUtils, Classes, stratoDecl, stratoLogic, stratoGenTools;

{ TStratoGenPascal }

procedure TStratoGenPascal.GenerateSource(const FilePath: string);
type
  TCodeType=(
    ctPending,

    ctInterface,
    ctInterfaceType,
    ctInterfaceConst,
    ctInterfaceVar,
    ctInterface_Any,

    ctImplementation,
    ctImplementationType,
    ctImplementationConst,
    ctImplementationVar,
    ctImplementation_Any,

    ctInterfaceUses,
    ctImplementationUses,

    ctInitialization,
    ctFinalization);

var
  c:array of record
    p:rItem;
    t:TCodeType;
    l:cardinal;
    c:AnsiString;
   end;
  ci,cl:cardinal;
const
  cGrowStep=$1000;

  function Add(p:rItem;level:cardinal):rItem;
  var
    i:cardinal;
  begin
    if p.x<>0 then
     begin
      i:=0;
      while (i<ci) and (c[i].p.x<>p.x) do inc(i);
      if i=ci then
       begin
        if ci=cl then
         begin
          //grow
          inc(cl,cGrowStep);
          SetLength(c,cl);
         end;
        c[i].p:=p;
        c[i].t:=ctPending;
        c[i].l:=level;
        c[i].c:='';
        inc(ci);
       end
      else
       begin
        if c[i].l<level then
          c[i].l:=level;
       end;
     end;
    Result:=p;
  end;

  procedure AddCode(p:rItem;t:TCodeType;level:cardinal;const code:AnsiString);
  var
    i:cardinal;
  begin
    if p.x=0 then i:=ci else i:=0;
    while (i<ci) and (c[i].p.x<>p.x) do inc(i);
    if i=ci then
     begin
      if ci=cl then
       begin
        //grow
        inc(cl,cGrowStep);
        SetLength(c,cl);
       end;
      c[i].p:=p;
      inc(ci);
     end
    else
     begin
      if c[i].t<>ctPending then raise Exception.Create('Duplicate Generate Detected');
     end;
    c[i].t:=t;
    c[i].l:=level;
    c[i].c:=code;
  end;

  function Signature(p:rItem;named:boolean;level:cardinal):AnsiString;
  var
    p1,p2,q0,q1:rItem;
    src:AnsiString;
    b:boolean;
  begin
    case p.NodeType of
      nSignature,nDtor:
       begin
        p1:=p;
        p2.x:=0;
       end;
      nCtor:
       begin
        p1:=p.r(iSignature);
        p2.x:=IntrinsicTypes[itPointer];
        Add(p2,level+1);
       end;
      else
       begin
        p1:=p.r(iSignature);
        p2:=Add(p1.r(iReturnType),level);
       end;
    end;
    if p2.x=0 then src:='procedure' else src:='function';
    if named then src:=src+' '+rs(p);
    b:=true;

    if p.NodeType=nDtor then
     begin
      b:=false;
      src:=src+'(nThis: pointer';
     end
    else
     begin
      q1:=Add(p1.r(iSubject),level);
      if q1.x<>0 then
       begin
        b:=false;
        //TODO nThis
        //src:=src+'(var nThis: '+rs(q1);
        src:=src+'(nThis: pointer';
       end;

      ListFirst(p1,lArguments,q1,q0);
      while q1.x<>0 do
       begin
        if b then
         begin
          src:=src+'(';
          b:=false;
         end
        else
          src:=src+';';
        if q1.NodeType=nSigArgByRef then src:=src+'var ';
        src:=src+rs(q1)+': '+rs(Add(q1.r(iType),level));
        ListNext(q1,q0);
       end;
     end;

    if not(b) then src:=src+')';
    if p2.x<>0 then src:=src+': '+rs(p2);
    src:=src+';';
    Result:=src;
  end;

  function CodeBlock(cb:rItem;level:cardinal):AnsiString;
  var
    p,p0:rItem;
    src:AnsiString;
  begin
    src:='';

    //
    ListFirst(cb,lLocals,p,p0);
    if p.x<>0 then
     begin
      src:=src+'var'#13#10;
      while p.x<>0 do
       begin
        //
        src:=src+'  '+rs(p)+': '+rs(Add(p.r(iType),level))+';'#13#10;
        ListNext(p,p0);
       end;
     end;

    src:=src+'begin //'+rs(cb)+#13#10;

    ListFirst(cb,lItems,p,p0);
    while p.x<>0 do
     begin
      //
      src:=src+'  '+rs(p)+';//'+AnsiString(NodeTypeToStr(p.NodeType))+#13#10;

      //TODO: case p.NodeType of


      ListNext(p,p0);
     end;

    //
    src:=src+'end';
    //src:=src+'end;'#13#10#13#10;
    Result:=src;
  end;

  procedure AddFuncProc(p:rItem;level:cardinal;const suffix:AnsiString);
  var
    sig:AnsiString;
  begin
    sig:=Signature(p,true,level+1);
    if suffix<>'' then sig:=sig+' //'+suffix;
    AddCode(p,ctInterface,level,sig);
    AddCode(xx0,ctImplementation,level,sig+#13#10+
      CodeBlock(p.r(iBody),level)+';'#13#10);
  end;

  procedure pType1(tt:TStratoIntrinsicType;const pName:AnsiString);
  var
    p:rItem;
    n:UTF8String;
  begin
    p.x:=IntrinsicTypes[tt];
    if p.x<>0 then
     begin
      n:=GetName(p.v(iName));
      AddCode(p,ctInterfaceType,0,rs(p)+' = '+pName+'; //'+AnsiString(n));
     end;
  end;

  procedure pType2(pp:rItem;const sName:UTF8String;const pName:AnsiString);
  var
    p,p0:rItem;
    n:UTF8String;
  begin
    ListFirst(pp,lItems,p,p0);
    while (p.x<>0) do
     begin
      n:=GetName(p.v(iName));
      if n=sName then break;
      ListNext(p,p0);
     end;
    if p.x<>0 then
      AddCode(p,ctInterfaceType,0,rs(p)+' = '+pName+'; //'+AnsiString(n));
  end;

var
  i,j:cardinal;
  p,p0,p1,p2,q,q0,q1,q2:rItem;
  f:TFileStream;
  src,src1,src2,n:AnsiString;
  ct:TCodeType;
  level:cardinal;
  ii:integer;

const
  CodeTypeName:array[TCodeType] of string=(
    'pend_',
    'intf_','intfT','intfC','intfV','intfX',
    'impl_','implT','implC','implV','implX',
    'intfU','implU','init0','final');

begin
  ci:=0;
  cl:=0;

  //defaults
  AddCode(xxr(0),ctInterfaceUses,1,'Windows, SysUtils');//Variants?

  //intrinsic types
  pType1(itPointer,'pointer');
  pType1(itBoolean,'boolean');
  pType1(itNumber,'integer');//NativeInt?
  //itObject, see below
  pType1(itString,'string');

  //'Strato'?
  ListFirst(xxr(0 * StratoSphereBlockBase),lSourceFile_NameSpaces,p,p0);

  pType2(p,'i8','ShortInt');
  pType2(p,'i16','SmallInt');
  pType2(p,'i32','integer');
  pType2(p,'i64','int64');
  pType2(p,'u8','byte');
  pType2(p,'u16','word');
  pType2(p,'u32','cardinal');
  pType2(p,'u64','Uint64');

  pType2(p,'f32','single');
  pType2(p,'f64','double');
  pType2(p,'f80','extended');

  //global variables
  for i:=0 to SourceFilesCount-1 do
   begin
    ListFirst(xxr(i * StratoSphereBlockBase),lSourceFile_Globals,p,p0);
    while p.x<>0 do
     begin
      p1:=p.r(iTarget);
      AddCode(p1,ctInterfaceVar,1,rs(p1)+': '+rs(Add(p1.r(iType),1))+'; //global '+rs(p));
      p2:=p1.r(iValue);
      if p2.x<>0 then
       begin
        AddCode(p2,ctInitialization,1,rs(p1)+':='+
          //TODO:
          rs(p2)+';');
       end;

      //Add(p);
      ListNext(p,p0);
     end;
   end;

  //initialization
  for i:=0 to SourceFilesCount-1 do
   begin
    p.x:=i * StratoSphereBlockBase;
    p:=p.r(iSourceFile_InitializationBlock);
    if p.x<>0 then
     begin
      AddCode(p,ctImplementation,1,'procedure '+rs(p)+';'#13#10+
        CodeBlock(p,1)+';'#13#10);
      AddCode(xx0,ctInitialization,1,rs(p)+';');
     end;
   end;

  //finalization
  for i:=SourceFilesCount-1 downto 0 do
   begin
    p.x:=i * StratoSphereBlockBase;
    p:=p.r(iSourceFile_FinalizationBlock);
    if p.x<>0 then
     begin
      AddCode(p,ctImplementation,1,'procedure '+rs(p)+';'#13#10+
        CodeBlock(p,1)+';'#13#10);
      AddCode(xx0,ctFinalization,1,rs(p)+';');
     end;
   end;

  //expand any listed
  level:=1;
  i:=0;
  while i<ci do
   begin
    if c[i].t=ctPending then
     begin
      if c[i].l>level then
        level:=c[i].l
      else
       begin
        c[i].l:=level;
        inc(level);
       end;
      ct:=ctInterface; //default
      src:='';
      p:=c[i].p;
      case p.NodeType of

        nArray:
         begin
          q:=Add(p.r(iType),level);
          ct:=ctInterfaceType;
          src:=rs(p)+' = array [0..'+AnsiString(IntToStr(
            p.v(vByteSize) div ByteSize(q)))+'-1] of '+rs(q)+';';
         end;

        nRecord:
         begin
          ct:=ctInterfaceType;
          src:=rs(p)+' = record'#13#10;
          ListFirst(p,lItems,p1,p0);
          while p1.x<>0 do
           begin
            case p1.NodeType of

              nVar:
               begin
                ii:=p1.v(vOffset);
                if ii<0 then
                  src:=src+'    //'+rs(p1)+' offset '+AnsiString(IntToStr(ii))+#13#10
                else
                 begin
                  src:=src+'    '+rs(p1)+': '+rs(Add(p1.r(iType),level))+';// offset '+
                    AnsiString(IntToStr(ii))+#13#10;
                  //TODO: p1.v(vOffset)
                 end;
               end;

              nMember:
               begin
                ListFirst(p1,lItems,q1,q0);
                while q1.x<>0 do
                 begin
                  AddFuncProc(q1,level+1,'');
                  ListNext(q1,q0);
                 end;
               end;

              else //TODO: raise?
                src:=src+'    //'+rs(p1)+' '+AnsiString(
                  NodeTypeToStr(p1.NodeType))+#13#10;
            end;
            ListNext(p1,p0);
           end;
          src:=src+'  end;';
         end;

        nClass:
         begin
          ct:=ctInterfaceType;
          //TODO?
          src:=rs(p)+' = record //class'#13#10;
          if p.x=IntrinsicTypes[itObject] then
            src:=rs(p)+'_d = procedure(nThis: pointer);'#13#10'  '+src;
          src2:=rs(p)+'_v : record //vtable'#13#10+
            '    xx_dtor:'+rs(xxr(IntrinsicTypes[itObject]))+'_d;'#13#10;

          //destructor pointer
          ListFirst(p,lItems,p1,p0);
          while (p1.x<>0) and (p1.NodeType<>nDtor) do ListNext(p1,p0);
          if p1.x=0 then
            AddCode(xxr(0),ctInitialization,level,rs(p)+'_v.xx_dtor:=nil')
          else
            AddCode(xxr(0),ctInitialization,level,rs(p)+'_v.xx_dtor:=@'+rs(p1));

          ListFirst(p,lItems,p1,p0);
          while p1.x<>0 do
           begin
            case p1.NodeType of

              nVar:
               begin
                ii:=p1.v(vOffset);
                if ii<0 then
                  src:=src+'    //'+rs(p1)+' offset '+AnsiString(IntToStr(ii))+#13#10
                else
                 begin
                  src:=src+'    '+rs(p1)+': '+rs(Add(p1.r(iType),level))+';// offset '+
                    AnsiString(IntToStr(ii))+#13#10;
                  //TODO: p1.v(vOffset)
                 end;
               end;

              nMember:
               begin
                ListFirst(p1,lItems,q1,q0);
                while q1.x<>0 do
                 begin
                  q2:=q1.r(iSignature);
                  AddCode(q2,ctInterfaceType,level+2,rs(q2)+' = '+Signature(q2,false,level+2));
                  n:=AnsiString(GetName(p1.v(iName)));
                  src2:=src2+'    '+n+':'+rs(q2)+';'#13#10;
                  AddCode(xxr(0),ctInitialization,level,rs(p)+'_v.'+n+':=@'+rs(q1)+';');
                  AddFuncProc(q1,level+1,'');
                  ListNext(q1,q0);
                 end;
               end;

              nCtors:
               begin
                ListFirst(p1,lItems,q1,q0);
                while q1.x<>0 do
                 begin
                  Add(q1,level);
                  ListNext(q1,q0);
                 end;
               end;

              nDtor:
                AddFuncProc(p1,level,'dtor');

              else //TODO: raise?
                src:=src+'    //'+rs(p1)+' '+AnsiString(
                  NodeTypeToStr(p1.NodeType))+#13#10;
            end;
            ListNext(p1,p0);
           end;
          src:=src+'  end;';
          AddCode(xxr(0),ctInterfaceVar,level,src2);
         end;

        nCtor:
         begin
          src:=Signature(p,true,level+1)+' //ctor';
          AddCode(xx0,ctImplementation,level,src+#13#10+
            CodeBlock(p.r(iBody),level)+';'#13#10);
         end;

        nClassRef:
         begin
          ct:=ctInterfaceType;
          src:=rs(p)+' = pointer; //cref '+rs(p.r(iTarget))+';';
         end;

        else
          src:='//'+rs(c[i].p)+' '+
            AnsiString(NodeTypeToStr(p.NodeType));//TODO: error?
      end;
      c[i].t:=ct;
      c[i].c:=src;
      //c[i].l see above
     end;
    inc(i);
   end;

  //now generate source code
  i:=Length(FilePath);
  while (i<>0) and (FilePath[i]<>'.') do dec(i);
  j:=i;
  while (j<>0) and (FilePath[j]<>PathDelim) do dec(j);
  //src:='unit '?
  src:='program '+AnsiString(Copy(FilePath,j,i-j-1))+
    ';'#13#10#13#10'interface'#13#10#13#10;

  if false then//if debug
   begin
    src:=src+#13#10'{'#13#10;
    for i:=0 to ci-1 do
      src:=src+AnsiString(Format('%s %s %d %d'#13#10,
        [rs(c[i].p),CodeTypeName[c[i].t],c[i].l,Length(c[i].c)]));
    src:=src+#13#10'}'#13#10;
   end;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctInterfaceUses then
     begin
      if j=0 then
       begin
        src:=src+'uses ';
        j:=1;
       end
      else
        src:=src+', ';
      src:=src+c[i].c;
     end;
  if j<>0 then src:=src+';'#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if (c[i].t in [ctInterface..ctInterface_Any]) and (c[i].l>j) then
      j:=c[i].l;

  ct:=ctInterface;
  while j<>0 do
   begin
    for i:=ci-1 downto 0 do
      if (c[i].l=j) and (c[i].t in [ctInterface..ctInterface_Any]) and (c[i].c<>'') then
       begin
        case c[i].t of
          ctInterfaceType:
            if ct<>ctInterfaceType then
              src:=src+#13#10'type'#13#10;
          ctInterfaceConst:
            if ct<>ctInterfaceConst then
              src:=src+#13#10'const'#13#10;
          ctInterfaceVar:
            if ct<>ctInterfaceVar then
              src:=src+#13#10'var'#13#10;
        end;
        //src:=src+#13#10;
        ct:=c[i].t;
        if c[i].t<>ctInterface then src:=src+'  ';
        src:=src+c[i].c+#13#10;
       end;
    dec(j);
   end;

  src:=src+#13#10'implementation'#13#10#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctImplementationUses then
     begin
      if j=0 then
       begin
        src:=src+'uses ';
        j:=1;
       end
      else
        src:=src+', ';
      src:=src+c[i].c;
     end;
  if j<>0 then src:=src+';'#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if (c[i].t in [ctImplementation..ctImplementation_Any]) and (c[i].l>j) then
      j:=c[i].l;

  ct:=ctInterface;
  while j<>0 do
   begin
    for i:=ci-1 downto 0 do
      if (c[i].l=j) and (c[i].t in [ctImplementation..ctImplementation_Any]) and (c[i].c<>'') then
       begin
        case c[i].t of
          ctImplementationType:
            if ct<>ctImplementationType then
              src:=src+#13#10'type'#13#10;
          ctImplementationConst:
            if ct<>ctImplementationConst then
              src:=src+#13#10'const'#13#10;
          ctImplementationVar:
            if ct<>ctImplementationVar then
              src:=src+#13#10'var'#13#10;
        end;
        //src:=src+#13#10;
        ct:=c[i].t;
        if c[i].t<>ctImplementation then src:=src+'  ';
        src:=src+c[i].c+#13#10;
       end;
    dec(j);
   end;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctInitialization then
     begin
      if j=0 then
       begin
        src:=src+#13#10'initialization'#13#10;
        j:=1;
       end;
      src:=src+'  '+c[i].c+#13#10;
     end;
  if j<>1 then src:=src+#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctFinalization then
     begin
      if j=0 then
       begin
        src:=src+#13#10'finalization'#13#10;
        j:=1;
       end;
      src:=src+'  '+c[i].c+#13#10;
     end;
  if j<>1 then src:=src+#13#10;

  src:=src+#13#10'end.'#13#10;

  //write code to file
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    f.Write(src[1],Length(src));
  finally
    f.Free;
  end;
end;

end.
