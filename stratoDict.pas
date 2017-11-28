unit stratoDict;

interface

{$D-}
{$L-}

type
  TStringDictionaryNodeData=record
    Next,Index,Length:cardinal;
    Value:UTF8String;//ATTENTION: acces 'Value' via nodes indexed with 'Index'
  end;
  PStringDictionaryNodeData=^TStringDictionaryNodeData;

  TStringDictionary=class(TObject)
  private
    FNodes:array of TStringDictionaryNodeData;
    FNodesSize,FNodesCount,FLastIndex:cardinal;
    procedure CheckSize;
    function GetStr(Idx:cardinal):UTF8String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetIdx(const x:UTF8String):cardinal;
    function StrIdx(const x:UTF8String):cardinal;//int64?
    property StrCount:cardinal read FLastIndex;
    property Str[Idx:cardinal]:UTF8String read GetStr; default;
  end;

implementation

const
  StringDictionaryNodesGrowSize=$20;

{ TStringDictionary }

constructor TStringDictionary.Create;
begin
  inherited Create;
  FNodesSize:=StringDictionaryNodesGrowSize;
  FNodesCount:=1;
  FLastIndex:=0;
  SetLength(FNodes,FNodesSize shl 8);
  //ZeroMemory(@FNodes[0]...
end;

destructor TStringDictionary.Destroy;
begin
  SetLength(FNodes,0);
  inherited;
end;

procedure TStringDictionary.CheckSize;
begin
  if FNodesCount=FNodesSize then //grow
   begin
    inc(FNodesSize,StringDictionaryNodesGrowSize);
    SetLength(FNodes,FNodesSize shl 8);
    //ZeroMemory(?
   end;
end;

function TStringDictionary.GetIdx(const x: UTF8String): cardinal;
var
  i,l,p:cardinal;
  n:PStringDictionaryNodeData;
  y:UTF8String;
begin
  l:=Length(x)+1;//assert xx:UTF8String :: xx[Length(xx)+1]=#0
  if l=1 then Result:=0 else
   begin
    //p:=0;
    i:=1;
    n:=@FNodes[{p or }byte(x[i])];
    Result:=cardinal(-1);
    while Result=cardinal(-1) do
      if n.Index=0 then
        Result:=0
      else
       begin
        y:=FNodes[n.Index].Value;//assert reference-counted strings
        while (i<>l) and (i<>n.Length) and (x[i]=y[i]) do inc(i);
        if (i=l) and (i=n.Length) then
          if x[i]=y[i] then
            Result:=n.Index
          else
            Result:=0
        else
        if (i=n.Length) and (n.Next<>0) then
         begin
          p:=n.Next shl 8;
          n:=@FNodes[p or byte(x[i])];
         end
        else
          Result:=0;
       end;
   end;
end;

function TStringDictionary.StrIdx(const x: UTF8String): cardinal;
var
  i,l,p:cardinal;
  n,m:PStringDictionaryNodeData;
  y:UTF8String;
begin
  l:=Length(x)+1;//assert xx:UTF8String :: xx[Length(xx)+1]=#0
  if l=1 then Result:=0 else
   begin
    //p:=0;
    i:=1;
    n:=@FNodes[{p or }byte(x[i])];
    Result:=0;
    while Result=0 do
      if n.Index=0 then
       begin
        //add
        inc(FLastIndex);
        //assert FLastIndex<FNodesCount
        FNodes[FLastIndex].Value:=x;
        n.Index:=FLastIndex;
        n.Length:=l;
        Result:=FLastIndex;
       end
      else
       begin
        y:=FNodes[n.Index].Value;//assert reference-counted strings
        while (i<>l) and (i<>n.Length) and (x[i]=y[i]) do inc(i);
        if (i=l) and (i=n.Length) then
          if x[i]=y[i] then
            Result:=n.Index
          else
           begin
            //store on node
            inc(FLastIndex);
            //assert FLastIndex<FNodesCount
            FNodes[FLastIndex].Value:=x;
            n.Index:=FLastIndex;
            Result:=FLastIndex;
           end
        else
        if (i=n.Length) and (n.Next<>0) then
         begin
          p:=n.Next shl 8;
          n:=@FNodes[p or byte(x[i])];
         end
        else
         begin
          //split
          CheckSize;
          inc(FLastIndex);
          //assert FLastIndex<FNodesCount
          FNodes[FLastIndex].Value:=x;
          p:=FNodesCount shl 8;
          m:=@FNodes[p or byte(y[i])];
          m.Next:=n.Next;
          m.Index:=n.Index;
          m.Length:=n.Length;
          m:=@FNodes[p or byte(x[i])];
          m.Index:=FLastIndex;
          m.Length:=l;
          if l<n.Length then n.Index:=FLastIndex;
          n.Next:=FNodesCount;
          n.Length:=i;
          inc(FNodesCount);
          Result:=FLastIndex;
         end;
       end;
   end;
end;

function TStringDictionary.GetStr(Idx: cardinal): UTF8String;
begin
  if Idx>FLastIndex then
    Result:='' //raise?
  else
    Result:=FNodes[Idx].Value;
end;

end.

