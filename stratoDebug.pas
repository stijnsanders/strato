unit stratoDebug;

interface

{$D-}
{$L-}

uses stratoTokenizer, stratoSphere, stratoDecl;

function StratoDumpThing(p:xNode):UTF8String;
function StratoGetSourceFile(p:xNode;
  var srcLine,srcColumn:cardinal;var p1,p2:xNode):boolean;
procedure StratoDumpSphereData(const fn:string;ReverseFilesOrder:boolean);

var
  IncludeDictionaryNodes:boolean;

const
  TokenName:array[TStratoToken] of UTF8String=(
    'id','string','numeric','',
    ';',',','.',':','=','@','^','?','&','$','#','~',
    '(',')','{','}','[',']',
    '---',':::','???','!!!','...','@@','@@@','??','##',
    ':=','+=','-=','*=','/=','%=','||=','&&=',
    '==','<>','<','<=','>','>=','&&','||','!','|!',
    '+','-','*','/','%','++','--','<<','>>','..',
    '<<<','>>>','@?','?=',
    '','EOF','');

implementation

uses SysUtils, Classes, stratoTools;

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

function StratoDumpThing(p:xNode):UTF8String;
var
  q,v:PxKeyValue;
  r:xRef;
  i,l:cardinal;
const
  hex:array[0..15] of UTF8Char='0123456789ABCDEF';

  function rr(pp:PxKeyValue):UTF8String;
  begin
    if pp.i=0 then Result:=IntToStr8(pp.v) else
      Result:='$'+IntToStr8(pp.i)+'#'+IntToStr8(pp.v);
  end;

begin
  if p.sphere=nil then
    Result:='?nullref'
  else
   begin
    if p.index>=p.sphere.kCount then
      Result:='###Item index out of range '+UTF8String(p.AsString)+
        ' ('+IntToStr8(p.sphere.kCount)+')'
    else
     begin
      q:=p.sphere[p.index];
      case q.k of

      xUnassigned:
        Result:='?unassigned';//assert i=0

      xBinaryData:
       begin
        Result:='('+IntToStr8(q.i)+')"'+p.sphere[p.index].BinaryData+'"';
        q:=nil;//skip fields
       end;

      xListEntry:
       begin
        if IncludeDictionaryNodes then Result:='list ->'+rr(q);
        q:=nil;//skip fields
       end;

      xDictionary_Entry:
       begin
        Result:='';
        i:=q.i;
        l:=4;
        while (l<>0) and (i<>0) do
         begin
          case i and $FF of
            $00:;//end loop?
            $20..$7E:
              Result:=UTF8String(AnsiChar(i and $FF))+Result;
            else
              Result:=SphereIndexPrefix+UTF8String(hex[(i shr 4) and $F]+hex[i and $F])+SphereIndexSuffix+Result;
          end;
          i:=i shr 8;
          dec(l);
         end;
        Result:='dict "'+Result
          +'" ->'+IntToStr8(q.v)
          //+' next='+IntToStr8(q.n)
          //+' ^'+IntToStr8()
          ;
        q:=nil;//skip fields
       end;
      xDictionary_Tail:
       begin
        Result:='dict ^'+IntToStr8(q.v);
        q:=nil;//skip fields
       end;
      xStageList:
        Result:='?stage';

      nSphere:
        //'$' done by StratoDumpSphereData
        Result:=' #'+IntToStr8(p.sphere.kCount);

      nNameSpace:    Result:='ns '+p.sphere.FQN(p.index);

      nType:         Result:='type '+p.sphere.FQN(p.index);
      nLiteral:      Result:='literal';
      nConstant:     Result:='const '+p.sphere.FQN(p.index);
      nArray:        Result:='array '+p.sphere.FQN(p.index);
      nEnum:         Result:='enum '+p.sphere.FQN(p.index);
      nRecord:       Result:='record '+p.sphere.FQN(p.index);
      nPointer:      Result:='pointer '+p.sphere.FQN(p.index);
      nTypeAlias:    Result:='alias '+p.sphere.FQN(p.index);
      nSignature:    Result:='sig';// '+p.sphere.FQN(p.index);
      nSigArg:       Result:='arg '+p.sphere.GetName(p.sphere.n(p.index));
      nSigArgByRef:  Result:='^arg '+p.sphere.GetName(p.sphere.n(p.index));
      nClass:        Result:='class '+p.sphere.FQN(p.index);
      nClassRef:     Result:='^class '+p.sphere.FQN(p.index);
      nCtor:         Result:='ctor';//$3($4){$5}
      nDtor:         Result:='dtor';//-(){$}
      nPropGet:      Result:='pget';//?0  $3[$4]{$5}
      nPropSet:      Result:='pset';//?0  $3[$4]{$5}
      nInterface:    Result:='interface';
      nOverload:     Result:='ovl';
      //nOverload      :d:='fn   ?0  $3($4){$5}';


      nCodeBlock:    Result:='{}';
      nVar:          Result:='var '+p.sphere.FQN(p.index);
      nVarByRef:     Result:='^var '+p.sphere.FQN(p.index);
      nVarReadOnly:  Result:='var# '+p.sphere.FQN(p.index);
      nThis:         Result:='this';

      nSCall:        Result:='sc()';
      nFCall:        Result:='fc()';
      nVCall:        Result:='vc()';
      nICall:        Result:='ic()';
      nCallArg:      Result:='arg';

      nCast:         Result:='cast';
      //nAddressOf:    Result:='addr';
      //nDereference:  Result:='dref';
      nArrayIndex:   Result:='x[]';
      nField:        Result:='x.y';

      nAssign:       Result:=':=';
      nUnaryOp:      Result:='_x';
      nBinaryOp:     Result:='x_y';
      nSelection:    Result:='if';
      nIteration:    Result:='for';
      nIterPostEval: Result:='loop';
      nRange:        Result:='range';
      nRangeIndex:   Result:='r<-i';

      nTry:          Result:='try';
      nThrow:        Result:='throw';
      nDefer:        Result:='defer';

      nCatch:        Result:='???';

      //add new above here
      else
       begin
        Result:='?'+IntToStr8(cardinal(q.k));
        if q.k>n_Max then q:=nil;
       end;
      end;
      while (q<>nil) and (q.n<>0) do
       begin
        r:=q.n;
        q:=p.sphere[r];
        case q.k of

        xUnassigned,xStageList:q:=nil;//end loop?

        iParent:
          Result:=Result+' ^'+IntToStr8(q.v);
        vSrcPos:
         begin
          v:=p.sphere.v(0,vSphere_SrcPosLineIndex);
          if v=nil then
            l:=1
          else
           begin
            l:=v.v;
            if l=0 then l:=1;
           end;
          Result:=Result+' ['+IntToStr8(integer(q.v div l))+':'+IntToStr8(integer(q.v mod l))+']';
         end;
        dName:
          if IncludeDictionaryNodes then
            Result:=Result+' n='+IntToStr8(q.v);
          //else Result:=Result+' n="'+s.GetName(q.v)+'"';?
        iType:
          Result:=Result+' t='+rr(q);
        iValue:
          Result:=Result+' v='+rr(q);
        vByteSize:
          Result:=Result+' #'+IntToStr8(q.v);
        lChildren:
          Result:=Result+' ->'+IntToStr8(r);
        vOffset:
          Result:=Result+' @'+IntToStr8(integer(q.v));
        vOperator:
          Result:=Result+' _="'+TokenName[TStratoToken(q.v)]+'"';

        iSubject:
          Result:=Result+' subject='+rr(q);
        iTarget:
          Result:=Result+' target='+rr(q);
        iSignature:
          Result:=Result+' sig='+rr(q);
        iReturnType:
          Result:=Result+' r='+rr(q);
        iInheritsFrom:
          Result:=Result+' i='+rr(q);

        iBody:
          Result:=Result+' {}='+rr(q);
        iLeft:
          Result:=Result+' left='+rr(q);
        iRight:
          Result:=Result+' right='+rr(q);
        iPredicate:
          Result:=Result+' predicate='+rr(q);
        iDoTrue:
          Result:=Result+' true='+rr(q);
        iDoFalse:
          Result:=Result+' false='+rr(q);

        iArgVar:
          Result:=Result+' i='+rr(q);
        lArguments:
          Result:=Result+' args->'+IntToStr8(r);

        lCodeBlock_Locals:
          Result:=Result+' l->'+IntToStr8(r);
        lCodeBlock_Statements:
          Result:=Result+' {x}->'+IntToStr8(r);
        vCodeBlock_LocalsSize:
          Result:=Result+' {#'+IntToStr8(q.v)+'}';

        iSphere_FileName:
          Result:=Result+' fn='+IntToStr8(q.v);
        vSphere_FileSize:
          Result:=Result+' ('+IntToStr8((q.v+1023)div 1024)+' KiB)';
        vSphere_FileHash:;
        vSphere_SrcPosLineIndex:;
        lSphere_Errors:
          Result:=Result+' errors->'+IntToStr8(r);
//        lSphere_Imports:
//          Result:=Result+' import->'+IntToStr8(r);
//        lSphere_NameSpaces:
//          Result:=Result+' ns->'+IntToStr8(r);
        iSphere_Local:
          Result:=Result+' local='+IntToStr8(q.v);
        lSphere_Globals:
          Result:=Result+' global->'+IntToStr8(r);
        lSphere_Dictionary:
          if IncludeDictionaryNodes then
            Result:=Result+' dict->'+IntToStr8(r);
        iSphere_InitializationBlock:
          Result:=Result+' init='+IntToStr8(q.v);
        iSphere_FinalizationBlock:
          Result:=Result+' fini='+IntToStr8(q.v);

        //TODO: more!

        //add new above here
        else
          Result:=Result+' ?'+IntToStr8(cardinal(q.k))+'=$'+IntToStr8(q.i)+'#'+IntToStr8(q.v);
        end;
       end;
     end;
   end;
end;

function StratoGetSourceFile(p:xNode;
  var srcLine,srcColumn:cardinal;var p1,p2:xNode):boolean;
var
  l:cardinal;
  v:PxKeyValue;
begin
  Result:=false;//default
  if (p.sphere<>nil) and (p.Key<>xDictionary_Entry) and (p.Key<>xBinaryData) then
   begin
    v:=p.sphere.v(0,vSphere_SrcPosLineIndex);
    if v=nil then
      l:=1
    else
     begin
      l:=v.v;
      if l=0 then l:=1;
     end;
    v:=p.sphere.v(p.index,vSrcPos);
    if v<>nil then
     begin
      Result:=true;
      srcLine  :=v.v div l;
      srcColumn:=v.v mod l;
     end;
    //TODO: while not() then :=(iParent)
   end;
end;

procedure StratoDumpSphereData(const fn:string;ReverseFilesOrder:boolean);
var
  f:TFileStream;
  s:TStratoSphere;
  i,sx:cardinal;
  p,q:xNode;
  procedure fw(const xx:UTF8String);
  begin
    //if xx<>''?
    f.Write(xx[1],Length(xx));
  end;
  procedure fl(Key:xKey);
  var
    xx:UTF8String;
    l:xNode;
    b:boolean;
    r:PxKeyValue;
  begin
    l.Start(p,Key);
    if not l.Done then
     begin
      xx:=IntToStr8(l.index)+': list';
      b:=false;
      while l.Next(q) do
       begin
        r:=s[l.index];
        if r.i=0 then
          xx:=xx+' '+IntToStr8(r.v)
        else
          xx:=xx+' $'+IntToStr8(r.i)+'#'+IntToStr8(r.v);
        b:=true;
        if Length(xx)>=72 then
         begin
          xx:=xx+#13#10;
          f.Write(xx[1],Length(xx));
          b:=false;
          xx:='   ';
         end;
       end;
      if b then
       begin
        xx:=xx+#13#10;
        f.Write(xx[1],Length(xx));
       end;
     end;
  end;
const
  UTF8ByteOrderMark:array[0..2] of byte=($EF,$BB,$BF);
begin
  f:=TFileStream.Create(fn,fmCreate);
  try
    f.Write(UTF8ByteOrderMark[0],3);
    //TODO: strato version?
    if SpheresCount<>0 then
      for i:=0 to SpheresCount-1 do
       begin
        if ReverseFilesOrder then sx:=SpheresCount-1-i else sx:=i;
        s:=Spheres[sx];
        p.s(s,0);
        while p.index<s.kCount do
         begin
          if p.index=0 then //assert s.k[0].k=nSphere
           begin
            fw('$'+IntToStr8(sx+1)+StratoDumpThing(p)+#13#10);
            fl(lSphere_Errors);
            //fl(lSphere_Imports);
            fl(lChildren);//fl(lSphere_NameSpaces);
            fl(lSphere_Globals);
            //p.index:=16?
           end
          else
            case s[p.index].k of
            xUnassigned,xStageList:;
            xDictionary_Entry,xDictionary_Tail,xListEntry:
              if IncludeDictionaryNodes then
                fw(IntToStr8(p.index)+': '+StratoDumpThing(p)+#13#10);
            xBinaryData:
             begin
              fw(IntToStr8(p.index)+': '+StratoDumpThing(p)+#13#10);
              inc(p.index,(s[p.index].i+SizeOf(xKeyValue)-1) div SizeOf(xKeyValue));
             end;
            else
             begin
              fw(IntToStr8(p.index)+': '+StratoDumpThing(p)+#13#10);
              case s[p.index].k of
                nNameSpace..nInterface:
                 begin
                  fl(lArguments);
                  fl(lChildren);
                 end;
                nCodeBlock:
                 begin
                  fl(lChildren);//?
                  fl(lArguments);
                  fl(lCodeBlock_Locals);
                  fl(lCodeBlock_Statements);
                 end;
                nSCall,nFCall,nVCall,nICall:
                  fl(lArguments);
              end;
             end;
            end;
          inc(p.index);
          while (p.index<s.kCount) and
            not((s.k[p.index].k<n_Max) or (s.k[p.index].k>=xBinaryData)) do
            inc(p.index);
         end;

        fw(#13#10);

        {
        p.index:=0;
        while p.index<s.kCount do
         begin
          fw(Format('%.8d %.8d %.8d %s'#13#10,[cardinal(p.index)
            ,cardinal(s[p.index].k),cardinal(s[p.index].v)
            ,KeyToStr(s[p.index].k)
            ]));
          inc(p.index);
         end;
        }

       end;
  finally
    f.Free;
  end;
end;

initialization
  IncludeDictionaryNodes:=false;//default
end.
