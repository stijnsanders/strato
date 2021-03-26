unit stratoParseBase;

interface

uses stratoDecl, stratoSphere, stratoSource, stratoPred, stratoTokenizer;

type
  TStratoParserBase=class(TObject)
  private
    FSphere:TStratoSphere;
    FSource:TStratoSource;

    procedure InlineError(Sender:TObject;Line,LPos:cardinal;
      const ErrorMsg:string);

  protected

    stackSize,stackIndex:cardinal;
    stack:array of record
      pr:TPrecedence;
      p1,p2:xNode;
      sp:xSrcPos;
    end;
    stackPushed:boolean;

    procedure Push(pr:TPrecedence;p1,p2:xNode;sp:xSrcPos);
    procedure Pop(var pr:TPrecedence;var p1,p2:xNode;var sp:xSrcPos);
    function Peek:TPrecedence;
    function Peek1:xNode;
    function Peek2:xNode;

    function kn(Key:xKey;Node:xNode):xKeyValue; inline;
    function Local(Node:xNode):xRef; inline;

  public
    constructor Create(ASphere:TStratoSphere;ASource:TStratoSource;
      InlineErrors:boolean);

    function Add(Key:xKey;NewLength:cardinal;
      const Values:array of xKeyValue):xNode; overload;
    function Add(Parent:xRef;List:xKey;NewKey:xKey;NewLength:cardinal;
      const Values:array of xKeyValue):xNode; overload;
    function Add(Parent:xRef;List:xKey;NewKey:xKey;NewLength:cardinal;
      Name:xName;const Values:array of xKeyValue;
      var NewItem:xNode):boolean; overload;

    procedure Parse; virtual;

    property Sphere:TStratoSphere read FSphere;
    property Source:TStratoSource read FSource;
    //property SrcIndex: cardinal read FSrc;
  end;

implementation

uses SysUtils, stratoTools;

const
  stackGrowSize=$100;

{ TStratoParserBase }

constructor TStratoParserBase.Create(ASphere: TStratoSphere;
  ASource: TStratoSource; InlineErrors: boolean);
begin
  inherited Create;
  FSphere:=ASphere;
  FSource:=ASource;

  if InlineErrors then FSource.OnError:=InlineError;

  stackSize:=stackGrowSize;
  SetLength(stack,stackSize);

  Sphere.SetVal(0,iSphere_FileName,0,
    Sphere.AddBinaryData(UTF8Encode(StripKnownPath(FSource.FilePath))));
  Sphere.SetVal(0,vSphere_FileSize,0,FSource.FileSize);
  Sphere.SetVal(0,vSphere_SrcPosLineIndex,0,FSource.LineIndex);
  //TODO: Sphere.SetVal(0,vSphere_FileHash,
end;

procedure TStratoParserBase.InlineError(Sender: TObject; Line, LPos: cardinal;
  const ErrorMsg: string);
begin
  {$IFDEF DEBUG}
  asm int 3 end;
  {$ENDIF}

  Sphere.Append(0,lSphere_Errors,Sphere.AddBinaryData(
    UTF8Encode(Format('### %s(%d:%d): %s',
      [(Sender as TStratoSource).FilePath,Line,LPos,ErrorMsg]))));
end;

procedure TStratoParserBase.Push(pr:TPrecedence;p1,p2:xNode;sp:xSrcPos);
begin
  stackPushed:=true;//see Combine
  if stackIndex=stackSize then
   begin
    inc(stackSize,stackGrowSize);//grow
    SetLength(stack,stackSize);
   end;
  stack[stackIndex].pr:=pr;
  stack[stackIndex].p1:=p1;
  stack[stackIndex].p2:=p2;
  stack[stackIndex].sp:=sp;
  inc(stackIndex);
end;

procedure TStratoParserBase.Pop(var pr:TPrecedence;var p1,p2:xNode;
  var sp:xSrcPos);
begin
  {$IFDEF DEBUG}
  if stackIndex=0 then raise Exception.Create('Can''t pop from empty stack');
  {$ENDIF}
  dec(stackIndex);
  pr:=stack[stackIndex].pr;
  p1:=stack[stackIndex].p1;
  p2:=stack[stackIndex].p2;
  sp:=stack[stackIndex].sp;
  {$IFDEF DEBUG}
  stack[stackIndex].pr:=p___;
  stack[stackIndex].p1.none;
  stack[stackIndex].p2.none;
  stack[stackIndex].sp:=0;
  {$ENDIF}
end;

function TStratoParserBase.Peek:TPrecedence;
begin
 if stackIndex=0 then
   Result:=p___
 else
   Result:=stack[stackIndex-1].pr;
end;

function TStratoParserBase.Peek1:xNode;
begin
 if stackIndex=0 then
   Result.none
 else
   Result:=stack[stackIndex-1].p1;
end;

function TStratoParserBase.Peek2:xNode;
begin
 if stackIndex=0 then
   Result.none
 else
   Result:=stack[stackIndex-1].p2;
end;

function TStratoParserBase.kn(Key:xKey;Node:xNode):xKeyValue;
begin
  Result.k:=Key;
  Result.n:=xRef(0);//handled by sphere
  if (Node.sphere=nil) or (Node.sphere=Sphere) then
    Result.i:=0
  else
    Result.i:=SphereIndex(Node.sphere);
  Result.v:=Node.index;
end;

function TStratoParserBase.Local(Node:xNode):xRef;
begin
  if Node.sphere=Sphere then
    Result:=Node.index
  else
   begin
    Source.Error('Unexpected element out of sphere');
    Result:=0;
   end;
end;

function TStratoParserBase.Add(Key:xKey;NewLength:cardinal;
  const Values:array of xKeyValue):xNode;
var
  i,l:cardinal;
  p:xRef;
begin
  l:=Length(Values);
  if l+1>NewLength then
    raise Exception.Create('More values than space allocated');
  p:=Sphere.Add(Key,NewLength);
  if l<>0 then
    for i:=0 to l-1 do
      Sphere.SetVal(p,Values[i].k,Values[i].i,Values[i].v);
  Result.s(Sphere,p);
end;

function TStratoParserBase.Add(Parent:xRef;List:xKey;NewKey:xKey;NewLength:cardinal;
  const Values:array of xKeyValue):xNode;
var
  i,l:cardinal;
  p:xRef;
begin
  l:=Length(Values);
  if l+1>NewLength then raise Exception.Create('More values than space allocated');
  p:=Sphere.Add(NewKey,NewLength);
  //Sphere.SetVal(Result,iParent,Parent);?
  if l<>0 then
    for i:=0 to l-1 do
      Sphere.SetVal(p,Values[i].k,Values[i].i,Values[i].v);
  Sphere.Append(Parent,List,p);
  Result.s(Sphere,p);
end;

function TStratoParserBase.Add(Parent:xRef;List:xKey;NewKey:xKey;
  NewLength:cardinal;Name:xName;const Values:array of xKeyValue;
  var NewItem:xNode):boolean;
var
  p,p1:xNode;
  q:xRef;
  i,l:cardinal;
begin
  p.s(Sphere,Parent);
  p.Start(p,List);
  while p.Next(p1) and not(Sphere.IsName(p1,Name)) do ;
  if p.Done then
   begin
    q:=Sphere.Add(NewKey,NewLength);
    l:=Length(Values);
    if l+2>NewLength then raise Exception.Create('More values than space allocated');
    Sphere.SetVal(q,dName,0,Name);
    //Sphere.SetVal(q,iParent,Parent);?
    if l<>0 then
      for i:=0 to l-1 do
        Sphere.SetVal(q,Values[i].k,Values[i].i,Values[i].v);
    Sphere.Append(Parent,List,q);
    Result:=true;
   end
  else
   begin
    q:=Sphere.Add(NewKey,NewLength);//create placeholder entry to avoid further errors
    Result:=false;
   end;
  NewItem.s(Sphere,q);
end;

procedure TStratoParserBase.Parse;
begin
  stackSize:=stackGrowSize;
  SetLength(stack,stackSize);

  //see inheritants
end;

end.
