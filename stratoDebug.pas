unit stratoDebug;

interface

uses stratoTokenizer, stratoSphere;

procedure StratoDumpTokens(const t:TStratoSourceTokenList);
procedure StratoDumpSphereData(s:TStratoSphere; const fn:string);

implementation

uses SysUtils, Classes, stratoDecl;

const
  TokenName:array[TStratoToken] of string=(
    'id','string','numeric','',
    ';',',','.',':','@','^','?','&','(',')','{','}','[',']',
    '<<<','>>>',':::','???','!!!','@@','@@@','??',
    ':=','+=','-=','*=','/=','%=','||=','&&=',
    '=','<>','<','<=','>','>=','&&','||','!','|!',
    '+','-','*','/','%','++','--','<<','>>','?=',
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
    xx:='index   parent  next    source  line:col what info'#13#10;
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
            x:=Format('%7d %7d %7d                 ',
              [i,p.Parent,p.Next,p.Source])
          else
            x:=Format('%7d %7d %7d %7d %4d:%3d',
              [i,p.Parent,p.Next,p.Source,
                p.SrcPos div StratoTokenizeLineIndex,
                p.SrcPos mod StratoTokenizeLineIndex]);
                //TODO: StratoTokenizeLineIndex from header
          case p.ThingType of
            ttSourceFile:
              x:=Format('%s src  fn=%d',
                [x,PStratoSourceFile(p).FileName]);
            ttNameSpace:
              x:=Format('%s ns   %s  ->%d',
                [x,s.FQN(i),p.FirstItem]);
            ttImport:
              x:=Format('%s <<<  %s  %d',
                [x,s.FQN(i),p.Subject]);
            ttTypeDecl:
              x:=Format('%s type %s  #%d ->%d',
                [x,s.FQN(i),p.ByteSize,p.FirstItem]);
            ttRecord:
              x:=Format('%s rec  %s  #%d ->%d',
                [x,s.FQN(i),p.ByteSize,p.FirstItem]);
            ttEnumeration:
              x:=Format('%s enum %s',
                [x,s.FQN(i)]);
            ttArray:
              x:=Format('%s arr  %s  #%d t=%d',
                [x,s.FQN(i),p.ByteSize,p.ItemType]);
            ttAlias,ttGlobal:
              x:=Format('%s ---> %d',
                [x,p.Subject]);
            ttVar:
              x:=Format('%s var  %s  @%d t=%d v=%d',
                [x,s.FQN(i),p.Offset,p.EvaluatesTo,p.InitialValue]);
            ttConstant:
              x:=Format('%s cons %s  t=%d v=%d',
                [x,s.FQN(i),p.EvaluatesTo,p.InitialValue]);
            ttLiteral:
              x:=Format('%s lit  %s  t=%d v=%d',
                [x,s.FQN(i),p.EvaluatesTo,p.InitialValue]);
            ttSignature:
              x:=Format('%s sig  %s  "%d.(%d):%d"',
                [x,s.FQN(i),p.Subject,p.FirstArgument,p.EvaluatesTo]);
            ttFunction:
              x:=Format('%s fn   %s  %d(%d){%d}',
                [x,s.FQN(i),p.Signature,p.FirstArgument,p.Body]);
            ttFnCall:
              x:=Format('%s call %s  fn=%d  %d(%d){%d}',
                [x,s.FQN(i),p.Subject,p.Signature,p.FirstArgument,p.Body]);
            ttArgument:
              x:=Format('%s arg  %s  t=%d d=%d v=%d',
                [x,s.FQN(i),p.EvaluatesTo,p.InitialValue,p.Subject]);
            ttThis:
              x:=Format('%s this @%d t=%d',
                [x,p.Offset,p.EvaluatesTo]);
            ttVarIndex:
              x:=Format('%s .[]  %d.%d[%d] t=%d',
                [x,p.Parent,p.Subject,p.FirstArgument,p.EvaluatesTo]);
            ttCodeBlock:
              x:=Format('%s {}   #%d ->%d,%d t=%d',
                [x,p.ByteSize,p.FirstItem,p.FirstStatement,p.EvaluatesTo]);
            ttAssign:
              x:=Format('%s :=   %d %s %d t=%d',
                [x,p.AssignTo,TokenName[TStratoToken(p.Op)],p.ValueFrom,p.EvaluatesTo]);
            ttUnaryOp:
              x:=Format('%s _x   %s  %d t=%d',
                [x,TokenName[TStratoToken(p.Op)],p.Right,p.EvaluatesTo]);
            ttBinaryOp:
              x:=Format('%s x_x  %d %s %d t=%d',
                [x,p.Left,TokenName[TStratoToken(p.Op)],p.Right,p.EvaluatesTo]);
            ttCast:
              x:=Format('%s cast %d into %d',
                [x,p.Subject,p.Signature]);
            ttSelection:
              x:=Format('%s if   (%d){%d}{%d} t=%d',
                [x,p.DoIf,p.DoThen,p.DoElse,p.EvaluatesTo]);
            ttIteration:
              x:=Format('%s for  &({%d}%d{%d}){%d}',
                [x,p.DoFirst,p.DoIf,p.DoThen,p.Body]);
            ttIterationPE:
              x:=Format('%s loop &{%d}({%d}%d{%d})',
                [x,p.Body,p.DoFirst,p.DoIf,p.DoThen]);
            ttTry:
              x:=Format('%s :::  ->%d',[x,p.Subject]);
            ttDeferred:
              x:=Format('%s >>>  ->%d',[x,p.Subject]);
            ttCatch:
              x:=Format('%s ???  t=%d v=%d ->%d',
                [x,p.ItemType,p.FirstItem,p.Subject]);
            ttThrow:
              x:=Format('%s !!!  ->%d',[x,p.Subject]);
            ttSysCall:
              x:=Format('%s sys  %d',[x,p.Op]);
            ttPointer:
              x:=Format('%s ptr  %s  t=%d',
                [x,s.FQN(i),p.EvaluatesTo]);
            ttAddressOf:
              x:=Format('%s addr %d t=%d',
                [x,p.ValueFrom,p.EvaluatesTo]);
            ttDereference:
              x:=Format('%s dref %d t=%d',
                [x,p.ValueFrom,p.EvaluatesTo]);
            ttClass:
              x:=Format('%s cls  %s  #%d ->%d <-%d',
                [x,s.FQN(i),p.ByteSize,p.FirstItem,p.InheritsFrom]);
            ttConstructor:
              x:=Format('%s ctor %d: %d(%d){%d}',
                [x,p.Parent,p.Signature,p.FirstArgument,p.Body]);
            ttDestructor:
              x:=Format('%s dtor %d: (){%d}',
                [x,p.Parent,p.Body]);
            ttInterface:
              x:=Format('%s intf %s  ->%d <-%d',
                [x,s.FQN(i),p.FirstItem,p.InheritsFrom]);
            ttArgByRef:
              x:=Format('%s ^arg %s  t=%d',
                [x,s.FQN(i),p.EvaluatesTo]);
            ttVarByRef:
              x:=Format('%s ^var %s  @%d t=%d',
                [x,s.FQN(i),p.Offset,p.EvaluatesTo]);
            else
              x:=Format('%s ?    "%s" (%.4x) %d,%d,%d,%d',
                [x,s.FQN(i),p.ThingType,p.ByteSize
                ,p.ItemType,p.FirstItem,p.FirstStatement]);
          end;
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
