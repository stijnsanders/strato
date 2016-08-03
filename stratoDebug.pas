unit stratoDebug;

interface

uses stratoTokenizer, stratoSphere, stratoDecl;

procedure StratoDumpTokens(const t:TStratoSourceTokenList);
function StratoDumpThing(s:TStratoSphere;p:TStratoIndex):string;
procedure StratoDumpSphereData(t:TStratoStore; const fn:string);

implementation

uses SysUtils, Classes;

const
  TokenName:array[TStratoToken] of string=(
    'id','string','numeric','',
    ';',',','.',':','=','@','^','?','&',
    '(',')','{','}','[',']',
    '---',':::','???','!!!','@@','@@@','??',
    ':=','+=','-=','*=','/=','%=','||=','&&=',
    '==','<>','<','<=','>','>=','&&','||','!','|!',
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

function StratoDumpThing(s:TStratoSphere;p:TStratoIndex):string;
begin
  if p=0 then Result:='' else
  case s.t(p) of
    ttSourceFile:
      Result:=Format('src  fn=%d fs=%d mod=%d dep=%d',
        [s.r(p,tf_SourceFile_FileName)
        ,s.v(p,tf_SourceFile_FileSize)
        ,s.r(p,tf_SourceFile_Module)
        ,s.r(p,tf_SourceFile_FirstDependency)
        ]);
    ttModule:
      Result:=Format('mod  ns=%d var=%d ini=%d fin=%d',
        [s.r(p,tf_Module_FirstNameSpace)
        ,s.r(p,tf_Module_FirstGlobalVar)
        ,s.r(p,tf_Module_Initialization)
        ,s.r(p,tf_Module_Finalization)
        ]);
    ttDependency:
      Result:=Format('dep  src=%d "%s"',
        [s.r(p,tfTarget)
        ,s.GetBinaryData(s.rr(p,[tfTarget,tf_SourceFile_FileName]))
        ]);
    ttBinaryData:
      Result:=Format('"%s"',[s.GetBinaryData(p)]);
    ttNameSpace:
      Result:=Format('ns   %s  ->%d',
        [s.FQN(p)
        ,s.r(p,tfFirstItem)
        ]);
    ttTypeDecl:
      Result:=Format('type %s  #%d ->%d',
        [s.FQN(p)
        ,s.v(p,tfByteSize)
        ,s.r(p,tfFirstItem)
        ]);
    ttRecord:
      Result:=Format('rec  %s  #%d ->%d',
        [s.FQN(p)
        ,s.v(p,tfByteSize)
        ,s.r(p,tfFirstItem)
        ]);
    ttEnumeration:
      Result:=Format('enum %s  ->%d',
        [s.FQN(p)
        ,s.r(p,tfFirstItem)
        ]);
    ttLiteral:
      Result:=Format('lit  t=%d v=%d',
        [s.r(p,tfEvaluatesTo)
        ,s.r(p,tfInitialValue)
        ]);
    ttVar:
      Result:=Format('var  %s  @%d t=%d v=%d',
        [s.FQN(p)
        ,integer(s.v(p,tfOffset))
        ,s.r(p,tfEvaluatesTo)
        ,s.r(p,tfInitialValue)
        ]);
    ttConstant:
      Result:=Format('cons %s  t=%d v=%d',
        [s.FQN(p)
        ,s.r(p,tfEvaluatesTo)
        ,s.r(p,tfInitialValue)
        ]);
    ttCodeBlock:
      if s.r(p,tfEvaluatesTo)=0 then
        Result:=Format('{}   #%d var->%d cmd->%d',
          [s.v(p,tfByteSize)
          ,s.r(p,tfFirstItem)
          ,s.r(p,tfFirstStatement)
          ])
      else
        Result:=Format('{}:  #%d var->%d cmd->%d t=%d',
          [s.v(p,tfByteSize)
          ,s.r(p,tfFirstItem)
          ,s.r(p,tfFirstStatement)
          ,s.r(p,tfEvaluatesTo)
          ]);
    ttImport:
      Result:=Format('<<<  %s  %d',
        [s.FQN(p)
        ,s.r(p,tfTarget)
        ]);
    ttAlias:
      Result:=Format('---> %d',
        [s.r(p,tfTarget)
        ]);
    ttGlobal:
      Result:=Format('glob %d #%d  %s',
        [s.r(p,tfSubject)
        ,s.v(p,tfByteSize)
        ,s.FQN(s.r(p,tfSubject))
        ]);
    ttSignature:
      Result:=Format('sig  %s  %d.(%d):%d',
        [s.FQN(p)
        ,s.r(p,tfTarget)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttMember:
      Result:=Format('mem  %s  ->%d',
        [s.FQN(p)
        ,s.r(p,tfFirstItem)
        ]);
    ttOverload:
      Result:=Format('fn   %s  %d(%d){%d}',
        [s.FQN(p)
        ,s.r(p,tfsignature)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfBody)
        ]);
    ttFnCall:
      if s.r(p,tfEvaluatesTo)<>0 then
        Result:=Format('call %s  %d(%d):%d',
          [s.Store.Dict.Str[s.v(p,tfName)]
          ,s.r(p,tfTarget)
          ,s.r(p,tfFirstArgument)
          ,s.r(p,tfEvaluatesTo)
          ])
      else
        Result:=Format('call %s  %d(%d)',
          [s.Store.Dict.Str[s.v(p,tfName)]//s.FQN(p)
          ,s.r(p,tfTarget)
          ,s.r(p,tfFirstArgument)
          ]);
    ttArgument:
      Result:=Format('arg  %s  t=%d d=%d v=%d',
        [s.FQN(p)
        ,s.r(p,tfEvaluatesTo)
        ,s.r(p,tfInitialValue)
        ,s.r(p,tfTarget)//not ValueFrom!
        ]);
    ttAssign:
      Result:=Format(':=   %d %s %d',
        [s.r(p,tfAssignTo)
        ,TokenName[TStratoToken(s.v(p,tfOperator))]
        ,s.r(p,tfValueFrom)
        ]);
    ttUnaryOp:
      Result:=Format('_x   %s %d  t=%d',
        [TokenName[TStratoToken(s.v(p,tfOperator))]
        ,s.r(p,tfRight)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttBinaryOp:
      Result:=Format('x_y  %d %s %d  t=%d',
        [s.r(p,tfLeft)
        ,TokenName[TStratoToken(s.v(p,tfOperator))]
        ,s.r(p,tfRight)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttCast:
      Result:=Format('cast %d into %d',
        [s.r(p,tfTarget)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttSelection:
      Result:=Format('if   (%d){%d}{%d} t=%d',
        [s.r(p,tfDoIf)
        ,s.r(p,tfDoThen)
        ,s.r(p,tfDoElse)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttIteration:
      Result:=Format('for  &({%d}%d{%d}){%d}',
        [s.r(p,tfDoFirst)
        ,s.r(p,tfDoIf)
        ,s.r(p,tfDoThen)
        ,s.r(p,tfBody)
        ]);
    ttIterationPE:
      Result:=Format('loop &{%d}({%d}%d{%d})',
        [s.r(p,tfBody)
        ,s.r(p,tfDoFirst)
        ,s.r(p,tfDoIf)
        ,s.r(p,tfDoThen)
        ]);
    ttTry:
      Result:=Format(':::  ->%d',[s.r(p,tfTarget)]);
    ttThrow:
      Result:=Format('!!!  ->%d',[s.r(p,tfTarget)]);
    ttDeferred:
      Result:=Format('>>>  ->%d',[s.r(p,tfTarget)]);
    ttCatch:
      Result:=Format('???  t=%d v=%d ->%d',
        [s.r(p,tfTarget)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfBody)
        ]);
    ttSysCall:
      Result:=Format('sys  %.4x',[s.v(p,tfOperator)]);
    ttArray:
      Result:=Format('arr  %s  #%d t=%d',
        [s.FQN(p)
        ,s.v(p,tfByteSize)
        ,s.v(p,tfSubject)
        ]);
    ttArrayIndex:
      Result:=Format('x[]  %d[%d]  t=%d',
        [s.r(p,tfTarget)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttField:
      Result:=Format('x.y  %d.%d  t=%d',
        [s.r(p,tfSubject)
        ,s.r(p,tfTarget)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttThis:
      Result:=Format('this @%d t=%d',
        [s.v(p,tfOffset)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttPointer:
      Result:=Format('ptr  %s  t=%d',
        [s.FQN(p)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttAddressOf:
      Result:=Format('addr %d t=%d',
        [s.r(p,tfValueFrom)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttDereference:
      Result:=Format('dref %d t=%d',
        [s.r(p,tfValueFrom)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttClass:
      Result:=Format('cls  %s  #%d ->%d <-%d',
        [s.FQN(p)
        ,s.v(p,tfByteSize)
        ,s.r(p,tfFirstItem)
        ,s.r(p,tfInheritsFrom)
        ]);
    ttConstructors:
      Result:=Format('ctor ->%d',
        [s.r(p,tfFirstItem)
        ]);
    ttConstructor:
      Result:=Format('ctor %d(%d){%d}',
        [s.r(p,tfSignature)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfBody)
        ]);
    ttDestructor:
      Result:=Format('dtor %d: %d(){%d}',
        [s.r(p,tfParent)
        ,s.r(p,tfSignature)
        ,s.r(p,tfBody)
        ]);
    ttInterface:
      Result:=Format('intf %s  ->%d <-%d',
        [s.FQN(p)
        ,s.r(p,tfFirstItem)
        ,s.r(p,tfInheritsFrom)
        ]);
    ttArgByRef:
      Result:=Format('^arg %s  t=%d',
        [s.FQN(p)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttVarByRef:
      Result:=Format('^var %s  @%d t=%d',
        [s.FQN(p)
        ,integer(s.v(p,tfOffset))
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttClassRef:
      Result:=Format('cref %s  t=%d',
        [s.FQN(p)
        ,s.r(p,tfEvaluatesTo)
        ]);
    ttPropertyGet:
      Result:=Format('pget %s  %d[%d]{%d}',
        [s.FQN(p)
        ,s.r(p,tfSignature)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfBody)
        ]);
    ttPropertySet:
      Result:=Format('pset %s  %d[%d]{%d}',
        [s.FQN(p)
        ,s.r(p,tfSignature)
        ,s.r(p,tfFirstArgument)
        ,s.r(p,tfBody)
        ]);
    ttPropCall:
      if s.v(p,tfOperator)=0 then
        Result:=Format('prop %s  %d[%d]',
          [s.FQN(s.r(p,tfTarget))
          ,s.r(p,tfTarget)
          ,s.r(p,tfFirstArgument)
          ])
      else
        Result:=Format('prop %s  %d[%d] %s %d',
          [s.FQN(s.r(p,tfTarget))
          ,s.r(p,tfTarget)
          ,s.r(p,tfFirstArgument)
          ,TokenName[TStratoToken(s.v(p,tfOperator))]
          ,s.r(p,tfEvaluatesTo)
          ]);
    else
      Result:='?    '+s.DebugInfo(p);
  end;
end;

procedure StratoDumpSphereData(t:TStratoStore; const fn:string);
var
  s:TStratoSphere;
  f:TFileStream;
  p,q,p1,p2:TStratoIndex;
  py,px:cardinal;
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
  s:=TStratoSphere.Create(t,nil);//TODO: readonly?
  f:=TFileStream.Create(fn,fmCreate);
  try
    xx:=
      //'Strato v=?
      'index   parent  next    source  line :col what info'#13#10;
    f.Write(xx[1],Length(xx));

    p:=0;
    while t.NextIndex(p) do
     begin
      case s.t(p) of
        ttBinaryData:
         begin
          xx:=s.GetBinaryData(p);
          x:=Format('%7d "%s"',[p,xx]);
          inc(p,((Length(xx)+8-1) div SizeOf(TStratoThing)));
         end;
        ttSourceFile:
          x:=Format('%7d %33s ',[p,''])+
            StratoDumpThing(s,p);
        else
          try
            x:=Format('%7d %33s ',[p,'']);
            if (s.v(p,tfSrcPos)=0) or
              not(StratoGetSourceFile(s,p,q,py,px,p1,p2)) then
             begin
              xn(15,p1);
              xn(23,p2);
             end
            else
             begin
              xn(15,p1);
              xn(23,p2);
              xn(31,q);
              xn(37,py);
              x[38]:=':';
              xn(41,px);
             end;
            x:=x+StratoDumpThing(s,p);
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
    s.Free;
  end;
end;

end.
