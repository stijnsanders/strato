unit stratoDebug;

interface

uses stratoTokenizer, stratoSphere, stratoDecl;

procedure StratoDumpTokens(const t:TStratoSourceTokenList);
function StratoDumpThing(s:TStratoSphere; i:cardinal; p:PStratoThing):string;
procedure StratoDumpSphereData(s:TStratoSphere; const fn:string);

implementation

uses SysUtils, Classes;

const
  TokenName:array[TStratoToken] of string=(
    'id','string','numeric','',
    ';',',','.',':','@','^','?','&','(',')','{','}','[',']',
    '<<<','>>>',':::','???','!!!','@@','@@@','??',
    ':=','+=','-=','*=','/=','%=','||=','&&=',
    '=','<>','<','<=','>','>=','&&','||','!','|!',
    '+','-','*','/','%','++','--','<<','>>','@?','?=',
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

function StratoDumpThing(s:TStratoSphere; i:cardinal; p:PStratoThing):string;
begin
  case p.ThingType of
    ttSourceFile:
      Result:=Format('src  fn=%d ini=%d fin=%d',
        [PStratoSourceFile(p).FileName
        ,PStratoSourceFile(p).InitializationCode
        ,PStratoSourceFile(p).FinalizationCode]);
    ttNameSpace:
      Result:=Format('ns   %s  ->%d  ini=%d fin=%d',
        [s.FQN(i),p.FirstItem
        ,p.FirstInitialization,p.FirstFinalization]);
    ttBinaryData:
      Result:=Format('"%s"',[s.GetBinaryData(i)]);
    ttImport:
      Result:=Format('<<<  %s  %d',
        [s.FQN(i),p.Subject]);
    ttTypeDecl:
      Result:=Format('type %s  #%d ->%d',
        [s.FQN(i),p.ByteSize,p.FirstItem]);
    ttRecord:
      Result:=Format('rec  %s  #%d ->%d',
        [s.FQN(i),p.ByteSize,p.FirstItem]);
    ttEnumeration:
      Result:=Format('enum %s  ->%d',
        [s.FQN(i),p.FirstItem]);
    ttArray:
      Result:=Format('arr  %s  #%d t=%d',
        [s.FQN(i),p.ByteSize,p.ItemType]);
    ttAlias:
      Result:=Format('---> %d',
        [p.Subject]);
    ttGlobal:
      Result:=Format('===> %d  %s',
        [p.Subject,s.FQN(p.Subject)]);
    ttVar:
      Result:=Format('var  %s  @%d t=%d v=%d',
        [s.FQN(i),p.Offset,p.EvaluatesTo,p.InitialValue]);
    ttConstant:
      Result:=Format('cons %s  t=%d v=%d',
        [s.FQN(i),p.EvaluatesTo,p.InitialValue]);
    ttLiteral:
      Result:=Format('lit  t=%d v=%d',
        [p.EvaluatesTo,p.InitialValue]);
    ttSignature:
      Result:=Format('sig  %s  "%d.(%d):%d"',
        [s.FQN(i),p.Subject,p.FirstArgument,p.EvaluatesTo]);
    ttFunction:
      Result:=Format('fn:  %s  ->%d',
        [s.FQN(i),p.FirstItem]);
    ttOverload:
      Result:=Format('fn   %s  %d(%d){%d}',
        [s.FQN(i),p.Signature,p.FirstArgument,p.Body]);
    ttFnCall:
      Result:=Format('call %s  fn=%d  %d(%d){%d}',
        [s.Dict[p.Name]{s.FQN(i)}
        ,p.Subject,p.Signature,p.FirstArgument,p.Body]);
    ttArgument:
      Result:=Format('arg  %s  t=%d d=%d v=%d',
        [s.FQN(i),p.EvaluatesTo,p.InitialValue,p.Subject]);
    ttThis:
      Result:=Format('this @%d t=%d',
        [p.Offset,p.EvaluatesTo]);
    ttVarIndex:
      Result:=Format('.[]  %d.%d[%d]  t=%d',
        [p.Parent,p.Subject,p.FirstArgument,p.EvaluatesTo]);
    ttCodeBlock:
      Result:=Format('{}   #%d ->%d,%d t=%d',
        [p.ByteSize,p.FirstItem,p.FirstStatement,p.EvaluatesTo]);
    ttAssign:
      Result:=Format(':=   %d %s %d  t=%d',
        [p.AssignTo,TokenName[TStratoToken(p.Op)],p.ValueFrom,p.EvaluatesTo]);
    ttUnaryOp:
      Result:=Format('_x   %s %d  t=%d',
        [TokenName[TStratoToken(p.Op)],p.Right,p.EvaluatesTo]);
    ttBinaryOp:
      Result:=Format('x_x  %d %s %d  t=%d',
        [p.Left,TokenName[TStratoToken(p.Op)],p.Right,p.EvaluatesTo]);
    ttCast:
      Result:=Format('cast %d into %d',
        [p.Subject,p.Signature]);
    ttSelection:
      Result:=Format('if   (%d){%d}{%d} t=%d',
        [p.DoIf,p.DoThen,p.DoElse,p.EvaluatesTo]);
    ttIteration:
      Result:=Format('for  &({%d}%d{%d}){%d}',
        [p.DoFirst,p.DoIf,p.DoThen,p.Body]);
    ttIterationPE:
      Result:=Format('loop &{%d}({%d}%d{%d})',
        [p.Body,p.DoFirst,p.DoIf,p.DoThen]);
    ttTry:
      Result:=Format(':::  ->%d',[p.Subject]);
    ttDeferred:
      Result:=Format('>>>  ->%d',[p.Subject]);
    ttCatch:
      Result:=Format('???  t=%d v=%d ->%d',
        [p.ItemType,p.FirstItem,p.Subject]);
    ttThrow:
      Result:=Format('!!!  ->%d',[p.Subject]);
    ttSysCall:
      Result:=Format('sys  %d',[p.Op]);
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
      Result:=Format('cls  %s  #%d ctor=%d ->%d <-%d',
        [s.FQN(i),p.ByteSize
        ,p.FirstConstructor,p.FirstItem,p.InheritsFrom]);
    ttConstructor:
      Result:=Format('ctor %d: %d(%d){%d}',
        [p.Parent,p.Signature,p.FirstArgument,p.Body]);
    ttDestructor:
      Result:=Format('dtor %d: (){%d}',
        [p.Parent,p.Body]);
    ttInterface:
      Result:=Format('intf %s  ->%d <-%d',
        [s.FQN(i),p.FirstItem,p.InheritsFrom]);
    ttArgByRef:
      Result:=Format('^arg %s  t=%d',
        [s.FQN(i),p.EvaluatesTo]);
    ttVarByRef:
      Result:=Format('^var %s  @%d t=%d',
        [s.FQN(i),p.Offset,p.EvaluatesTo]);
    ttProperty:
      Result:=Format('prop %s  t=%d get=%d set=%d',
        [s.FQN(i),p.EvaluatesTo,p.ValueFrom,p.AssignTo]);
    else
      Result:=Format('?    "%s" (%.4x) %d,%d,%d,%d',
        [s.FQN(i),p.ThingType,p.ByteSize
        ,p.ItemType,p.FirstItem,p.FirstStatement]);
  end;
end;

procedure StratoDumpSphereData(s:TStratoSphere; const fn:string);
var
  f:TFileStream;
  i:cardinal;
  p:PStratoThing;
  x:string;
  xx:AnsiString;
begin
  f:=TFileStream.Create(fn,fmCreate);
  try
    p:=PStratoThing(s.Header);
    xx:=Format(
      'Strato v=%.8x ini=%d fin=%d ns=%d global=%d #%d'#13#10,
      [PStratoHeader(p).Version
      ,PStratoHeader(p).FirstInitialization
      ,PStratoHeader(p).FirstFinalization
      ,PStratoHeader(p).FirstNameSpace
      ,PStratoHeader(p).FirstGlobalVar
      ,PStratoHeader(p).GlobalByteSize
      ])+
      'index   parent  next    source  line :col what info'#13#10;
    f.Write(xx[1],Length(xx));

    i:=1;
    while i<s.NodeCount do
     begin
      p:=s[i];
      if p.ThingType=ttBinaryData then
       begin
        x:=Format('%7d "%s"',[i,s.GetBinaryData(i)]);
        inc(i,((p.Name+8) div SizeOf(TStratoThing)));
       end
      else
       begin
        try
          //TODO: switches
          if p.Source=0 then
            x:=Format('%7d %7d %7d                   ',
              [i,p.Parent,p.Next,p.Source])
          else
            x:=Format('%7d %7d %7d %7d %5d:%3d ',
              [i,p.Parent,p.Next,p.Source
              ,p.SrcPos div StratoTokenizeLineIndex
              ,p.SrcPos mod StratoTokenizeLineIndex
              ]);//TODO: StratoTokenizeLineIndex from header
          x:=x+StratoDumpThing(s,i,p);
        except
          on e:Exception do
            x:=Format('%7d ! %s',[i,e.Message]);
        end;
       end;
      inc(i);
      xx:=AnsiString(x+#13#10);
      f.Write(xx[1],Length(xx));
     end;
  finally
    f.Free;
  end;
end;

end.
