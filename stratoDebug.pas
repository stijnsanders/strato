unit stratoDebug;

interface

uses stratoTokenizer, stratoSphere, stratoDecl;

procedure StratoDumpTokens(const t:TStratoSourceTokenList);
function StratoDumpThing(s:TStratoSphere;p:TStratoIndex):string;
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
      Result:=Format('src  ini=%d fin=%d fn=%d fs=%d',
        [s.SourceFile(p).InitializationCode
        ,s.SourceFile(p).FinalizationCode
        ,s.SourceFile(p).FileName
        ,s.SourceFile(p).FileSize
        ]);
    ttBinaryData:
      Result:=Format('"%s"',[s.GetBinaryData(p)]);
    ttNameSpace:
      Result:=Format('ns   %s  ->%d  src=%d ini=%d fin=%d',
        [s.FQN(p)
        ,s.NameSpace(p).FirstItem
        ,s.NameSpace(p).SourceFile
        ,s.NameSpace(p).FirstInitialization
        ,s.NameSpace(p).FirstFinalization
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
      Result:=Format('===> %d  %s',
        [s.r(p,tfTarget)
        ,s.FQN(s.r(p,tfTarget))
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
          [s.Dict[s.v(p,tfName)]
          ,s.r(p,tfTarget)
          ,s.r(p,tfFirstArgument)
          ,s.r(p,tfEvaluatesTo)
          ])
      else
        Result:=Format('call %s  %d(%d)',
          [s.Dict[s.v(p,tfName)]//s.FQN(p)
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
      Result:=Format('x_x  %d %s %d  t=%d',
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
      Result:=Format('?    "%s" %s',
        [s.FQN(p)
        ,s.DebugInfo(p)
        ]);
  end;
end;

function StratoGetSourceFile(s:TStratoSphere;p:TStratoIndex;
  var q:TStratoIndex;var LineIndex:cardinal):boolean;
begin
  //assert p<>0
  //assert Sphere[p].SrcPos<>0
  //assert not Sphere[p].ThingType in [ttHeader,ttSourceFile,ttBinaryData]
  q:=p;
  while (q<>0) and not(s.t(q) in [ttNameSpace,ttOverload,ttConstructor]) do
    q:=s.r(q,tfParent);
  if q<>0 then
    case s.t(q) of
      ttNameSpace:q:=s.NameSpace(q).SourceFile;
      ttOverload,ttConstructor:q:=s.r(q,tfSourceFile);
      else q:=0;//raise?
    end;
  if q=0 then LineIndex:=1 else
   begin
    LineIndex:=s.SourceFile(q).SrcPosLineIndex;
    if LineIndex=0 then LineIndex:=1;
   end;
  Result:=q<>0;
end;

procedure StratoDumpSphereData(s:TStratoSphere; const fn:string);
var
  f:TFileStream;
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
    xx:=Format(
      'Strato v=%.8x ini=%d fin=%d ns=%d global=%d #%d'#13#10,
      [s.Header.Version
      ,s.Header.FirstInitialization
      ,s.Header.FirstFinalization
      ,s.Header.FirstNameSpace
      ,s.Header.FirstGlobalVar
      ,s.Header.GlobalByteSize
      ])+
      'index   parent  next    source  line :col what info'#13#10;
    f.Write(xx[1],Length(xx));

    p:=0;
    while s.NextIndex(p) do
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
            if s.t(p)=ttNameSpace then
             begin
              xn(15,s.r(p,tfParent));
              xn(23,s.r(p,tfNext));
              xn(31,s.NameSpace(p).SourceFile);
             end
            else
            if (s.v(p,tfSrcPos)=0) or not(StratoGetSourceFile(s,p,q,l)) then
             begin
              xn(15,s.r(p,tfParent));
              xn(23,s.r(p,tfNext));
             end
            else
             begin
              xn(15,s.r(p,tfParent));
              xn(23,s.r(p,tfNext));
              xn(31,q);
              xn(37,s.v(p,tfSrcPos) div l);
              x[38]:=':';
              xn(41,s.v(p,tfSrcPos) mod l);
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
  end;
end;

end.
