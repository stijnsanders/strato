unit stratoDebug;

interface

uses stratoTokenizer, stratoSphere, stratoDecl;

procedure StratoDumpTokens(const t:TStratoSourceTokenList);
function StratoDumpThing(s:TStratoSphere;i:TStratoIndex;p:PStratoThing):string;
function StratoGetSourceFile(s:TStratoSphere;p:TStratoIndex;
  var q:TStratoIndex;var LineIndex:cardinal):boolean;
procedure StratoDumpSphereData(s:TStratoSphere; const fn:string);

implementation

uses SysUtils, Classes;

const
  TokenName:array[TStratoToken] of string=(
    'id','string','numeric','',
    ';',',','.',':','=','@','^','?','&',
    '(',')','{','}','[',']',
    '---',':::','???','!!!','@@','@@@','??',
    ':=','+=','-=','*=','/=','%=','||=','&&=',
    '=','<>','<','<=','>','>=','&&','||','!','|!',
    '+','-','*','/','%','++','--','<<','>>',
    '~','<<<','>>>','@?','?=',
    '');

procedure StratoDumpTokens(const t:TStratoSourceTokenList);
var
  i,l:cardinal;
begin
  l:=Length(t);
  //Writeln(l,' token(s):');
  for i:=0 to l-1 do Writeln(Format('%.8d %.6d %.4d %s',
    [t[i].SrcPos,t[i].Index,t[i].Length,TokenName[t[i].Token]]));
end;

function StratoDumpThing(s:TStratoSphere;i:TStratoIndex;p:PStratoThing):string;
begin
  if p=nil then Result:='' else
  case p.ThingType of
    ttSourceFile:
      Result:=Format('src  ini=%d fin=%d fn=%d fs=%d',
        [PStratoSourceFile(p).InitializationCode
        ,PStratoSourceFile(p).FinalizationCode
        ,PStratoSourceFile(p).FileName
        ,PStratoSourceFile(p).FileSize
        ]);
    ttBinaryData:
      Result:=Format('"%s"',[s.GetBinaryData(i)]);
    ttNameSpace:
      Result:=Format('ns   %s  ->%d  src=%d ini=%d fin=%d',
        [s.FQN(i)
        ,PStratoNameSpaceData(p).FirstItem
        ,PStratoNameSpaceData(p).SourceFile
        ,PStratoNameSpaceData(p).FirstInitialization
        ,PStratoNameSpaceData(p).FirstFinalization
        ]);
    ttTypeDecl:
      Result:=Format('type %s  #%d ->%d',
        [s.FQN(i),p.ByteSize,p.FirstItem]);
    ttRecord:
      Result:=Format('rec  %s  #%d ->%d',
        [s.FQN(i),p.ByteSize,p.FirstItem]);
    ttEnumeration:
      Result:=Format('enum %s  ->%d',
        [s.FQN(i),p.FirstItem]);
    ttLiteral:
      Result:=Format('lit  t=%d v=%d',
        [p.EvaluatesTo,p.InitialValue]);
    ttVar:
      Result:=Format('var  %s  @%d t=%d v=%d',
        [s.FQN(i),integer(p.Offset),p.EvaluatesTo,p.InitialValue]);
    ttConstant:
      Result:=Format('cons %s  t=%d v=%d',
        [s.FQN(i),p.EvaluatesTo,p.InitialValue]);
    ttCodeBlock:
      if p.EvaluatesTo=0 then
        Result:=Format('{}   #%d var->%d cmd->%d',
          [p.ByteSize,p.FirstItem,p.FirstStatement])
      else
        Result:=Format('{}:  #%d var->%d cmd->%d t=%d',
          [p.ByteSize,p.FirstItem,p.FirstStatement,p.EvaluatesTo]);
    ttImport:
      Result:=Format('<<<  %s  %d',
        [s.FQN(i),p.Target]);
    ttAlias:
      Result:=Format('---> %d',
        [p.Target]);
    ttGlobal:
      Result:=Format('===> %d  %s',
        [p.Target,s.FQN(p.Target)]);
    ttSignature:
      Result:=Format('sig  %s  %d.(%d):%d',
        [s.FQN(i),p.Target,p.FirstArgument,p.EvaluatesTo]);
    ttMember:
      Result:=Format('mem  %s  ->%d',
        [s.FQN(i),p.FirstItem]);
    ttOverload:
      Result:=Format('fn   %s  %d(%d){%d}',
        [s.FQN(i),p.Target,p.FirstArgument,p.Body]);
    ttFnCall:
      if p.EvaluatesTo<>0 then
        Result:=Format('call %s  %d(%d):%d',
          [s.Dict[p.Name]
          ,p.Target,p.FirstArgument,p.EvaluatesTo])
      else
        Result:=Format('call %s  %d(%d)',
          [s.Dict[p.Name]//s.FQN(i)
          ,p.Target,p.FirstArgument]);
    ttArgument:
      Result:=Format('arg  %s  t=%d d=%d v=%d',
        [s.FQN(i),p.EvaluatesTo,p.InitialValue,p.Target]);//not ValueFrom!
    ttAssign:
      Result:=Format(':=   %d %s %d',
        [p.AssignTo,TokenName[TStratoToken(p.Op)],p.ValueFrom]);
    ttUnaryOp:
      Result:=Format('_x   %s %d  t=%d',
        [TokenName[TStratoToken(p.Op)],p.Right,p.EvaluatesTo]);
    ttBinaryOp:
      Result:=Format('x_x  %d %s %d  t=%d',
        [p.Left,TokenName[TStratoToken(p.Op)],p.Right,p.EvaluatesTo]);
    ttCast:
      Result:=Format('cast %d into %d',
        [p.Target,p.EvaluatesTo]);
    ttSelection:
      Result:=Format('if   (%d){%d}{%d} t=%d',
        [p.DoIf,p.DoThen,p.DoElse,p.EvaluatesTo]);
    ttIteration:
      Result:=Format('for  &({%d}%d{%d}){%d}',
        [p.DoElse,p.DoIf,p.DoThen,p.Body]);
    ttIterationPE:
      Result:=Format('loop &{%d}({%d}%d{%d})',
        [p.Body,p.DoElse,p.DoIf,p.DoThen]);
    ttTry:
      Result:=Format(':::  ->%d',[p.Target]);
    ttThrow:
      Result:=Format('!!!  ->%d',[p.Target]);
    ttDeferred:
      Result:=Format('>>>  ->%d',[p.Target]);
    ttCatch:
      Result:=Format('???  t=%d v=%d ->%d',
        [p.Target,p.FirstArgument,p.Body]);
    ttSysCall:
      Result:=Format('sys  %.4x',[p.Op]);
    ttArray:
      Result:=Format('arr  %s  #%d t=%d',
        [s.FQN(i),p.ByteSize,p.Subject]);
    ttArrayIndex:
      Result:=Format('x[]  %d[%d]  t=%d',
        [p.Target,p.FirstArgument,p.EvaluatesTo]);
    ttField:
      Result:=Format('x.y  %d.%d  t=%d',
        [p.Subject,p.Target,p.EvaluatesTo]);
    ttThis:
      Result:=Format('this @%d t=%d',
        [p.Offset,p.EvaluatesTo]);
    ttPointer:
      Result:=Format('ptr  %s  t=%d',
        [s.FQN(i),p.EvaluatesTo]);
    ttAddressOf:
      Result:=Format('addr %d t=%d',
        [p.ValueFrom,p.EvaluatesTo]);
    ttDereference:
      Result:=Format('dref %d t=%d',
        [p.ValueFrom,p.EvaluatesTo]);
    ttClass:
      Result:=Format('cls  %s  #%d ->%d <-%d',
        [s.FQN(i),p.ByteSize,p.FirstItem,p.InheritsFrom]);
    ttConstructors:
      Result:=Format('ctor ->%d',
        [p.FirstItem]);
    ttConstructor:
      Result:=Format('ctor %d(%d){%d}',
        [p.Target,p.FirstArgument,p.Body]);
    ttDestructor:
      Result:=Format('dtor %d: %d(){%d}',
        [p.Parent,p.Target,p.Body]);
    ttInterface:
      Result:=Format('intf %s  ->%d <-%d',
        [s.FQN(i),p.FirstItem,p.InheritsFrom]);
    ttArgByRef:
      Result:=Format('^arg %s  t=%d',
        [s.FQN(i),p.EvaluatesTo]);
    ttVarByRef:
      Result:=Format('^var %s  @%d t=%d',
        [s.FQN(i),integer(p.Offset),p.EvaluatesTo]);
    ttClassRef:
      Result:=Format('cref %s  t=%d',
        [s.FQN(i),p.EvaluatesTo]);
    ttPropertyGet:
      Result:=Format('pget %s  %d[%d]{%d}',
        [s.FQN(i),p.Target,p.FirstArgument,p.Body]);
    ttPropertySet:
      Result:=Format('pset %s  %d[%d]{%d}',
        [s.FQN(i),p.Target,p.FirstArgument,p.Body]);
    ttPropCall:
      if p.Op=0 then
        Result:=Format('prop %s  %d[%d]',
          [s.FQN(p.Target),p.Target,p.FirstArgument])
      else
        Result:=Format('prop %s  %d[%d] %s %d',
          [s.FQN(p.Target),p.Target
          ,p.FirstArgument,TokenName[TStratoToken(p.Op)],p.EvaluatesTo]);
    else
      Result:=Format('?    "%s" (%.4x) %d,%d,%d,%d,%d',
        [s.FQN(i),p.ThingType,p.Name,p.FirstItem
        ,p.ByteSize,p.EvaluatesTo,p.SrcPos]);
  end;
end;

function StratoGetSourceFile(s:TStratoSphere;p:TStratoIndex;
  var q:TStratoIndex;var LineIndex:cardinal):boolean;
begin
  //assert p<>0
  //assert Sphere[p].SrcPos<>0
  //assert not Sphere[p].ThingType in [ttHeader,ttSourceFile,ttBinaryData]
  q:=p;
  while (q<>0) and not(s[q].ThingType in [ttNameSpace,ttOverload,ttConstructor]) do
    q:=s[q].Parent;
  if q<>0 then
    case s[q].ThingType of
      ttNameSpace:q:=PStratoNameSpaceData(s[q]).SourceFile;
      ttOverload,ttConstructor:q:=s[q].SourceFile;
      else q:=0;//raise?
    end;
  if q=0 then LineIndex:=1 else
   begin
    LineIndex:=PStratoSourceFile(s[q]).SrcPosLineIndex;
    if LineIndex=0 then LineIndex:=1;
   end;
  Result:=q<>0;
end;

procedure StratoDumpSphereData(s:TStratoSphere; const fn:string);
var
  f:TFileStream;
  px:PStratoThing;
  p,q:TStratoIndex;
  l:cardinal;
  x:string;
  xx:AnsiString;
  procedure xn(zz,z:cardinal);
  begin
    while (z<>0) and (zz<>0) do
     begin
      x[zz]:=char($30+z mod 10);
      z:=z div 10;
      dec(zz);
     end;
  end;
begin
  f:=TFileStream.Create(fn,fmCreate);
  try
    px:=PStratoThing(s.Header);
    xx:=Format(
      'Strato v=%.8x ini=%d fin=%d ns=%d global=%d #%d'#13#10,
      [PStratoHeader(px).Version
      ,PStratoHeader(px).FirstInitialization
      ,PStratoHeader(px).FirstFinalization
      ,PStratoHeader(px).FirstNameSpace
      ,PStratoHeader(px).FirstGlobalVar
      ,PStratoHeader(px).GlobalByteSize
      ])+
      'index   parent  next    source  line :col what info'#13#10;
    f.Write(xx[1],Length(xx));

    p:=0;
    while s.NextIndex(p) do
     begin
      px:=s[p];
      case px.ThingType of
        ttBinaryData:
         begin
          x:=Format('%7d "%s"',[p,s.GetBinaryData(p)]);
          inc(p,((PStratoBinaryData(px).DataLength+8-1) div SizeOf(TStratoThing)));
         end;
        ttSourceFile:
          x:=Format('%7d %33s ',[p,''])+
            StratoDumpThing(s,p,px);
        else
          try
            x:=Format('%7d %33s ',[p,'']);
            if px.ThingType=ttNameSpace then
             begin
              xn(15,px.Parent);
              xn(23,px.Next);
              xn(31,PStratoNameSpaceData(px).SourceFile);
             end
            else
            if (px.SrcPos=0) or not(StratoGetSourceFile(s,p,q,l)) then
             begin
              xn(15,px.Parent);
              xn(23,px.Next);
             end
            else
             begin
              xn(15,px.Parent);
              xn(23,px.Next);
              xn(31,q);
              xn(37,px.SrcPos div l);
              x[38]:=':';
              xn(41,px.SrcPos mod l);
             end;
            x:=x+StratoDumpThing(s,p,px);
          except
            on e:Exception do
              x:=Format('%7d ! %s',[p,e.Message]);
          end;
      end;
      xx:=AnsiString(x+#13#10);
      f.Write(xx[1],Length(xx));
     end;
  finally
    f.Free;
  end;
end;

end.
