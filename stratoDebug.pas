unit stratoDebug;

interface

{$D-}
{$L-}

uses stratoTokenizer, stratoSphere, stratoDecl;

procedure StratoDumpToken(const t:TStratoSourceToken);
function StratoDumpThing(s:TStratoSphere;p:xItem):string;
procedure StratoDumpSphereData(t:TStratoStore;const fn:string);

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
    'EOF','');

procedure StratoDumpToken(const t:TStratoSourceToken);
begin
  Writeln(Format('%.8d %.6d %.4d %s',[t.SrcPos,t.Index,t.Length,TokenName[t.Token]]));
end;

function StratoDumpThing(s:TStratoSphere;p:xItem):string;
var
  d:string;
  i,j,l:integer;
  t:xTypeNr;
  src:PxSourceFile;

  function ii:xItem;
  var
    jj:integer;
  begin
    case d[i] of
      '0'..'9':jj:=byte(d[i]) and $F;
      'A'..'F':jj:=(byte(d[i]) and $F)+9;
      else jj:=0;//raise?
    end;
    case jj of
      0:Result:=xItem(t);
      1:Result:=s.n(p,vSrcPos)^;
      2:Result:=s.n(p,fParent)^;
      else
        if xTypeDef[j+jj-3]<f_FieldsMax then
          Result:=s.n(p,xTypeDef[j+jj-3])^
        else
          raise Exception.CreateFmt('Unkown index %d "%s"',[jj,d]);
    end;
  end;

begin
  d:='';//default
  Result:='';//default
  t:=s.n(p,vTypeNr)^;
  case t of

    n_BinaryData   :Result:=Format('#%d "%s"',[s.n(p,vSrcPos)^,s.GetBinaryData(p)]);

    nSourceFile    ://d:='src  fn=$2 fs=$3 ns=$7 g=$8 ini=$9 fin=$A';
     begin
      src:=s.Store.SourceFile(p);
      Result:=Format('src  fn=$%.8x fs=%d ns=$%.8x g=$%.8x ini=$%.8x fin=$%.8x',
        [src.FileName
        ,src.FileSize
        ,src.NameSpaces
        ,src.Globals
        ,src.InitializationBlock
        ,src.FinalizationBlock
        ]);
     end;

    nNameSpace     :d:='ns   ??  ->$5';
{
    nDependency    :d:='dep  src=%d "%s"',
        [s.r(p,tfTarget)
        ,s.GetBinaryData(s.rr(p,[tfTarget,tf_SourceFile_FileName]))
        ]);
}
    nImport        :d:='<<<  ??  ->$5';
    nGlobal        :d:='glob $4 #5  ?4';
    nPrivate       :d:='priv';

    nTypeDecl      :d:='type ??  #5';
    nSignature     :d:='sig  ??  $5.($6):$7';
    nArgument      :d:='arg  ??  t=$5 v=$6';
    nArgByRef      :d:='^arg ??  t=$5 v=$6';//?
    nVarDecl       :d:='var  ??  @7 t=$5 v=$6';
    nVarByRef      :d:='^var ??  @7 t=$5 v=$6';//?
    nLiteral       :d:='lit  t=$3 v=$4';
    nConstant      :d:='cons ??  t=$5 v=$6';
    nTypeAlias     :d:='---> $4';

    nMember        :d:='mem  ??  ->$5';
    nOverload      :d:='fn   ?2  $4($5){$6}';
    nArray         :d:='arr  ??  #5 t=$6';
    nRecord        :d:='rec  ??  #5 ->$6';
    nEnumeration   :d:='enum ??  ->$5';

    nClass         :d:='cls  ??  #7 ->$6 <-$5';
    nClassRef      :d:='cref ??  t=$5';
    nInterface     :d:='intf ??  ->$6 <-$5';
    nConstructors  :d:='ctor ->$5';
    nConstructor   :d:='ctor $4($5){$6}';
    nDestructor    :d:='dtor $4(){$6}';
    nPropertyGet   :d:='pget ?2  $4[$5]{$6}';
    nPropertySet   :d:='pset ?2  $4[$5]{$6}';

    nCodeBlock     :
      if s.n(p,fReturnType)^=0 then
                    d:='{}   #5 var->$4 cmd->$6'
      else          d:='{}:  #5 var->$4 cmd->$6 t=$7';
    nSysCall       :d:='sys  #4';
    nFCall         :d:='fc() $4($5)';
    nSCall         :d:='sc() $4.$5($6)';
    nVCall         :d:='vc() $4.$5($6)';
//    nPropGetCall   :d:='prop ?4 $4[$5]';//?
//    nPropSetCall   :d:='prop ?5 %5 *4 [$6]';//?
    nThis          :d:='this @7 t=$5 v=$6';
    nPointer       :d:='ptr  $4';
    nAddressOf     :d:='addr $4 t=$5';
    nDereference   :d:='dref $4';
    nField         :d:='x.y  $4.$5';
    nArrayIndex    :d:='x[]  $4[$5]';
    nCast          :d:='cast $4 into $5';

    nAssign        :d:=':=   $5 *4 $6';
    nUnaryOp       :d:='_x   *4 $5  t=$6';
    nBinaryOp      :d:='x_y  $5 *4 $6  t=$7';
    nSelection     :d:='if   ($4){$5}{$6} t=$7';
    nIteration     :d:='for  &({$4}$5{$6}){$7}';
    nIterPostEval  :d:='loop &{$7}({$4}$5{$6})';

    nTry           :d:=':::  ';
    nThrow         :d:='!!!  $4';
    nDeferred      :d:='>>>  $4';
    nCatchAll      :d:='???  {$4}';
    nCatchTypes    :d:='???  (->$4){$5}';
    nCatchNamed    :d:='???  (!4:$5){$6}';

    else
      if p<>0 then Result:=Format('?    x%s',[s.DebugInfo(p)]);
  end;
  l:=Length(d);
  if l<>0 then
   begin
    Result:=Copy(d,1,5);
    i:=6;
    j:=xTypeDefX[t];
    if j=0 then raise Exception.CreateFmt('x%.7x field not defined',[cardinal(t)]);
    while i<=l do
     begin
      inc(i);
      case d[i-1] of
        '?':if d[i]='?' then Result:=Result+s.FQN(p) else Result:=Result+s.FQN(ii);
        '$':Result:=Format('%s$%.8x',[Result,ii]);
        '#':Result:=Format('%s#%d',[Result,ii]);
        '@':Result:=Format('%s@%d',[Result,ii]);
        '*':Result:=Result+TokenName[TStratoToken(ii)];
        '!':Result:=Result+s.Store.Dict.Str[ii];
        else
         begin
          dec(i);
          Result:=Result+d[i];
         end;
      end;
      inc(i);
     end;
   end;
end;

procedure StratoDumpSphereData(t:TStratoStore; const fn:string);
var
  s:TStratoSphere;
  f:TFileStream;
  p,p1,p2:xItem;
  py,px:cardinal;
  x:string;
  xx:AnsiString;
  procedure xa(zz:cardinal;z:xItem);
  const
    hex:array[0..15] of char='0123456789ABCDEF';
  var
    zi:cardinal;
  begin
    zi:=zz+8;
    while (zi<>zz) do
     begin
      dec(zi);
      x[zi]:=hex[cardinal(z) and $F];
      cardinal(z):=cardinal(z) shr 4;
     end;
  end;
  procedure xn(zz,z:cardinal);
  begin
    while (z<>0) and (zz<>0) do
     begin
      x[zz]:=char($30+cardinal(z) mod 10);
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
      'index    parent   next     line :col what info'#13#10;
    f.Write(xx[1],Length(xx));

    p:=0;
    while t.NextItem(p) do
     begin
      case s.n(p,vTypeNr)^ of
        n_BinaryData:
          x:=Format('%.8x #%d "%s"',[cardinal(p),s.n(p,vSrcPos)^,string(s.GetBinaryData(p))]);
        nSourceFile:
          x:=Format('%.8x %27s ',[cardinal(p),''])+StratoDumpThing(s,p);
        else
          try
            x:=Format('%.8x %27s ',[cardinal(p),'']);
            if not(StratoGetSourceFile(s,p,py,px,p1,p2)) then
             begin
              xa(10,p1);
              xa(19,p2);
             end
            else
             begin
              xa(10,p1);
              xa(19,p2);
              if py<>0 then
               begin
                xn(32,py);
                x[33]:=':';
                xn(36,px);
               end;
             end;
            x:=x+StratoDumpThing(s,p);
          except
            on e:Exception do
              x:=Format('$%.8x ! %s',[cardinal(p),e.Message]);
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

initialization
  xDisplay:=StratoDumpThing;
end.
