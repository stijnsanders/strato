unit stratoDebug;

interface

{$D-}
{$L-}

uses stratoTokenizer, stratoSphere, stratoDecl;

function StratoDumpThing(p:rItem):string;
function StratoGetSourceFile(p:rItem;
  var srcLine,srcColumn:cardinal;var p1,p2:rItem):boolean;
procedure StratoDumpSphereData(const fn:string;
  IncludeDictionaryNodes,ReverseFilesOrder:boolean);

implementation

uses SysUtils, Classes;

const
  TokenName:array[TStratoToken] of string=(
    'id','string','numeric','',
    ';',',','.',':','=','@','^','?','&','$','#','~',
    '(',')','{','}','[',']',
    '---',':::','???','!!!','...','@@','@@@','??','##',
    ':=','+=','-=','*=','/=','%=','||=','&&=',
    '==','<>','<','<=','>','>=','&&','||','!','|!',
    '+','-','*','/','%','++','--','<<','>>','..',
    '<<<','>>>','@?','?=',
    '','EOF','');

function DebugInfo(p:rItem):string;
var
  i,j,k,l:cardinal;
  t:xTypeNr;
begin
  i:=p.x div StratoSphereBlockBase;
  j:=p.x mod StratoSphereBlockBase;
  t:=Blocks[i][j];
  Result:=NodeTypeToStr(t);
  if t=n_BinaryData then
    Result:=Format('%s: #%d',[Result,Blocks[i][j+1]])
  else
  if t<n_TypeNr_Low then
    Result:=Format('%s: ???',[Result])
  else
   begin
    l:=(t div n_TypeNr_Base);
    k:=xTypeDefX[t];
    inc(j);
    while l<>0 do
     begin
      if xTypeDef[k]<iName then
        Result:=Result+', '+IntToHex(Blocks[i][j],3)
      else
        Result:=Result+', '+ItemToStr(xxr(Blocks[i][j]));
      inc(j);
      inc(k);
      dec(l);
     end;
   end;
  Result[8]:=':';//was ','
end;

function DictKeyToStr(ii:integer):string;
var
  b,i:integer;
begin
  Result:=Format('$%.8x "',[ii]);
  for i:=0 to 3 do
   begin
    b:=(ii shr (8*(3-i))) and $FF;
    if b<>0 then
      if (b<32) or (b>126) then
        Result:=Result+'?'
      else
        Result:=Result+char(AnsiChar(b));
   end;
  Result:=Result+'"';
end;

function StratoDumpThing(p:rItem):string;
var
  d:string;
  a,b,i,l:integer;
  sx:PxSourceFile;

  function ii:xValue;
  begin
    Result:=Blocks[a][b+(byte(d[i]) and $F)+1];
  end;

begin
  a:=p.x div StratoSphereBlockBase;
  b:=p.x mod StratoSphereBlockBase;
  if b=0 then
   begin
    sx:=SourceFile(a);
    Result:='src '
      +' fn='+ItemToStr(xxr(sx.FileName))
      +' fs='+IntToStr(sx.FileSize)
      +' ns->'+ItemToStr(xxr(sx.NameSpaces))
      +' l='+ItemToStr(xxr(sx.Local))
      +' ini='+ItemToStr(xxr(sx.InitializationBlock))
      +' fin='+ItemToStr(xxr(sx.FinalizationBlock))
      +' g->'+ItemToStr(xxr(sx.Globals))
      +' d->'+ItemToStr(xxr(sx.Dictionary))
   end
  else
   begin
    d:='';//default
    Result:='';//default
    case p.NodeType of

      n_BinaryData   :
       begin
        Result:=Format('#%d "%s"',[Blocks[a][b+1],BinaryData(p)]);
       end;

      n_NameData     :d:='-    X2  ->$3';
      nNameSpace     :d:='ns   ??  ->$4';
      nType          :d:='type ??  #5 ->$4';
      nRecord        :d:='rec  ??  #5 ->$4';
      nEnum          :d:='enum ??  ->$4';
      nArray         :d:='arr  ??  #6 t=$5 ->$4';

      nLiteral       :d:='lit  v=$2 t=$1';
      nConstant      :d:='cons ??  v=$5 t=$4';

      nSignature     :d:='sig  ??  $4.($5):$6';
      nSigArg        :d:='arg  ??  t=$4 v=$5';
      nSigArgByRef   :d:='^arg ??  t=$4';
      nMember        :d:='mem  !2  ->$3';
      nOverload      :d:='fn   ?0  $3($4){$5}';
      nPointer       :d:='ptr  [$5] ->$4';
      nTypeAlias     :d:='=typ ??  t=$4';

      nGlobal        :d:='glob ?3 [$3]';

      nClass         :d:='cls  ??  #5 <-$6 ->$4';
      nClassRef      :d:='cref ??  t=$4';
      nCtors         :d:='ctor ->$2';
      nCtor          :d:='ctor $3($4){$5}';
      nPropGet       :d:='pget ?0  $3[$4]{$5}';
      nPropSet       :d:='pset ?0  $3[$4]{$5}';

      nDtor          :d:='dtor -(){$3}';
      nInterface     :d:='intf ??  <-$5 ->$4';

      nCodeBlock     :
        if p.r(iReturnType).x=0 then
                      d:='{}   #4 var->$3 cmd->$5'
        else          d:='{}:  #4 var->$3 cmd->$5 t=$6';
      nVar           :d:='var  ??  @4 t=$5 v=$6';
      nVarByRef      :d:='var^ ??  @4 t=$5';
      nVarReadOnly   :d:='var# ??  @4 t=$5 v=$6';
      nThis          :d:='this @2 t=$3';

      nSCall         :d:='sc() $4($3)';
      nFCall         :d:='fc() $4($3)';
      nVCall         :d:='vc() $5.$4($3)';
      nICall         :d:='ic() $5.$4($3)';
      nCallArg       :d:='arg  v=$3 t=$4';

      nCast          :d:='cast $3 into $4';
      //nAddressOf     :d:='addr $3 t=$4';
      //nDereference   :d:='dref $3 t=$4';
      nArrayIndex    :d:='x[]  $3[$4] t=$5';
      nField         :d:='x.y  $3.$4';

      nAssign        :d:=':=   $4 *3 $5';
      nUnaryOp       :d:='_x   *3 $4  t=$5';
      nBinaryOp      :d:='x_y  $4 *3 $5  t=$6';
      nSelection     :
        if p.r(iReturnType).x=0 then
                      d:='if   ($3){$4}{$5}'
        else          d:='if:  ($3){$4}{$5} t=$6';
      nIteration     :d:='for  ($3) {$4} t=$5';
      nIterPostEval  :d:='loop {$4} ($3) t=$5';
      nRange         :d:='rng  ??  $4 .. $5 t=$6';
      nRangeIndex    :d:='r<-i $3 <- $4';

      nTry           :d:=':::  ';
      nThrow         :d:='!!!  $3';
      nDefer         :d:='>>>  ->$3';

      nCatch         :d:='???  (->$3,$4){$5}';

      else
        if p.x<>0 then Result:=Format('?    x%s',[DebugInfo(p)]);
    end;
    l:=Length(d);
    if l<>0 then
     begin
      Result:=Copy(d,1,5);
      i:=6;
      while i<=l do
       begin
        inc(i);
        case d[i-1] of
          '?':
            if d[i]='?' then
              Result:=Result+UTF8ToString(FQN(p))
            else
              Result:=Result+UTF8ToString(FQN(xxr(ii)));
          '$':Result:=Result+ItemToStr(xxr(ii));
          '#':Result:=Format('%s#%d',[Result,ii]);
          '@':Result:=Format('%s@%d',[Result,ii]);
          '*':Result:=Result+TokenName[TStratoToken(ii)];
          'X':Result:=Result+DictKeyToStr(ii);
          '!':Result:=Result+UTF8ToString(GetName(ii));
          //'\':Result:=Result+d[i];
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
end;

function StratoGetSourceFile(p:rItem;
  var srcLine,srcColumn:cardinal;var p1,p2:rItem):boolean;
var
  i,j,k,l:cardinal;
  t:xTypeNr;
begin
  Result:=false;//default
  i:=p.x div StratoSphereBlockBase;
  j:=p.x mod StratoSphereBlockBase;
  l:=0;
  if i<BlocksCount then l:=SourceFiles[Blocks[i][0]].SrcPosLineIndex;
  if l=0 then l:=1;
  t:=p.NodeType;
  if (t>=n_TypeNr_Low) and (t<=n_TypeNr_High) then
   begin
    k:=xTypeDefX[t];
    if xTypeDef[k  ]=iParent then p1.x:=Blocks[i][j+1] else p1.x:=0;
    if xTypeDef[k+1]=iNext   then p2.x:=Blocks[i][j+2] else p2.x:=0;
    if xTypeDef[k+2]=vSrcPos then
     begin
      i:=Blocks[i][j+3];
      srcLine  :=i div l;
      srcColumn:=i mod l;
      Result:=true;
     end
    else
      if xTypeDef[k]=vSrcPos then //nLiteral only?
       begin
        i:=Blocks[i][j+1];
        srcLine  :=i div l;
        srcColumn:=i mod l;
        Result:=true;
       end;
   end;
end;

procedure StratoDumpSphereData(const fn:string;
  IncludeDictionaryNodes,ReverseFilesOrder:boolean);
var
  f:TFileStream;
  p,p1,p2:rItem;
  i,j,k,l,py,px:cardinal;
  t:xTypeNr;
  x:string;
  xx:AnsiString;
{
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
      x[zi]:=hex[z and $F];
      z:=z shr 4;
     end;
  end;
}
  procedure xn(zz,z:cardinal);
  begin
    if z<>0 then
     begin
      if (z div StratoSphereBlockBase)<>k then x[zz-6]:='^';
      z:=z mod StratoSphereBlockBase;
      while (z<>0) and (zz<>0) do
       begin
        x[zz]:=char($30+z mod 10);
        z:=z div 10;
        dec(zz);
       end;
     end;
  end;
  procedure xm(zz,z:cardinal);
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
    xx:=
      //'Strato v=?version?
      'index   parent  next    line :col what info'#13#10;
    f.Write(xx[1],Length(xx));

    for i:=0 to SourceFilesCount-1 do
     begin
      if ReverseFilesOrder then j:=SourceFilesCount-1-i else j:=i;
      //write file info here?
      for k:=0 to BlocksCount-1 do
        if Blocks[k][0]=j then
         begin
          p.x:=k * StratoSphereBlockBase;

          xx:=AnsiString(Format('[%d]%s'#13#10,[k,
            Copy(StratoDumpThing(p),6,99)]));
          f.Write(xx[1],Length(xx));

          inc(p.x,2);
          while (p.x mod StratoSphereBlockBase)<Blocks[k][1] do
           begin
            t:=p.NodeType;
            if IncludeDictionaryNodes or (t<>n_NameData) then
             begin
              if t=n_BinaryData then
                x:=Format('%8s%s',['',StratoDumpThing(p)])
              else
               begin
                x:=Format('%34s',['']);
                try
                  if not(StratoGetSourceFile(p,py,px,p1,p2)) then
                   begin
                    xn(15,p1.x);
                    xn(23,p2.x);
                   end
                  else
                   begin
                    xn(15,p1.x);
                    xn(23,p2.x);
                    if py<>0 then
                     begin
                      xm(29,py);
                      x[30]:=':';
                      xm(33,px);
                     end;
                   end;
                  x:=x+StratoDumpThing(p);
                except
                  on e:Exception do
                    x:=x+' ! '+e.Message;
                end;
               end;
              xn(7,p.x);
              xx:=AnsiString(x+#13#10);
              f.Write(xx[1],Length(xx));
             end;

            //next
            l:=(t div n_TypeNr_Base)+1;
            if t=n_BinaryData then
              inc(l,(Blocks[k][(p.x mod StratoSphereBlockBase)+1]+
                SizeOf(xValue)-1) div SizeOf(xValue));
            inc(p.x,l);
           end;
         end;
     end;
  finally
    f.Free;
  end;
end;

initialization
  //xDisplay:=StratoDumpThing;
end.
