unit stratoSource;

interface

{$D-}
{$L-}

uses SysUtils, Classes, stratoTokenizer;

const
  StratoTokenMaxLookForward=12;//more? config?

type
  TStratoSourceErrorHandler=procedure(Sender:TObject;Line,LPos:cardinal;
    const ErrorMsg:string) of object;

  TStratoSource=class(TObject)
  private
    FSource:UTF8String;
    FTA,FTB,FLineIndex:cardinal;
    FTokens:array[0..StratoTokenMaxLookForward-1] of TStratoSourceToken;
    FFilePath:string;
    FFileSize,FErrors:cardinal;
    FOnError:TStratoSourceErrorHandler;
    procedure PeekToken(var i:cardinal);
  public
    procedure LoadFromFile(const FilePath:string);
    function NextToken(var st:TStratoToken):boolean;
    function Token:TStratoToken; //advances index
    function IsNext(const st:array of TStratoToken):boolean; //advances index on true!
    function IsNextID(const st:array of TStratoToken):boolean; //(doesn't advance index)
    function IsNextBetween(st1,st2:TStratoToken):boolean;//(doesn't advance index)
    procedure Skip(st:TStratoToken);
    function GetID(var SrcPos: cardinal): UTF8String;
    function GetStr: UTF8String;
    function GetStrs: UTF8String;
    procedure Error(const msg: string);
    procedure ErrorN(const msg: string;const nn: UTF8String);
    function SrcPos: cardinal;
    property FilePath:string read FFilePath;
    property FileSize:cardinal read FFileSize;
    property LineIndex:cardinal read FLineIndex;
    property ErrorCount:cardinal read FErrors write FErrors;
    property OnError:TStratoSourceErrorHandler read FOnError write FOnError;
  end;

implementation

const
  DefaultLineIndex=1000;

{ TStratoSource }

procedure TStratoSource.LoadFromFile(const FilePath: string);
var
  f:TFileStream;
  i:integer;
  x:AnsiString;
  y:WideString;
  z:word;
begin
  //TODO: writeln(FilePath);
  FFilePath:=FilePath;
  //load file
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    i:=f.Size;
    FFileSize:=i;
    f.Read(z,2);
    if z=$FEFF then //UTF16 byte order mark
     begin
      dec(i,2);
      SetLength(y,i div 2);
      f.Read(y[1],i);
      FSource:=UTF8Encode(y);
     end
    else
    if z=$BBEF then //UTF-8 byte order mark
     begin
      z:=0;
      f.Read(z,1);
      //assert z=$00BF
      f.Read(FSource[1],i-3);
     end
    else
     begin
      f.Position:=0;
      SetLength(x,i);
      f.Read(x[1],i);
      FSource:=UTF8String(x);
     end;
  finally
    f.Free;
  end;
  //TODO: EOL's here and determine best SrcPosLineIndex?
  FLineIndex:=DefaultLineIndex;
  FErrors:=0;
  //initialize tokenizer
  StratoTokenizeInit(FSource,FLineIndex,FTokens[0]);
  FTA:=0;
  FTB:=0;
end;

function TStratoSource.NextToken(var st:TStratoToken):boolean;
begin
  st:=FTokens[FTA].Token;
  Result:=st<>st_EOF;
  PeekToken(FTA);
end;

procedure TStratoSource.PeekToken(var i:cardinal);
begin
  if i=FTB then
   begin
    inc(FTB);
    if FTB=StratoTokenMaxLookForward then FTB:=0;
    StratoTokenizeNext(FSource,FLineIndex,FTokens[i],FTokens[FTB]);
    i:=FTB;
   end
  else
   begin
    inc(i);
    if i=StratoTokenMaxLookForward then i:=0;
   end;
end;

function TStratoSource.Token: TStratoToken;
begin
  Result:=FTokens[FTA].Token;
  if Result=st_EOF then Error('unexpected end of file');
  PeekToken(FTA);
end;

function TStratoSource.IsNext(const st: array of TStratoToken): boolean;
var
  i,j,l:cardinal;
begin
  i:=0;
  j:=FTA;
  l:=Length(st);
  Result:=true;//default
  {$IFDEF DEBUG}
  if l>=StratoTokenMaxLookForward then
   begin
    Error('Maximum token look forward exceeded');
    Result:=false;
   end
  else
  {$ENDIF}
   begin
    while Result and (i<l) do
     begin
      if FTokens[j].Token=st[i] then
       begin
        inc(i);
        PeekToken(j);
       end
      else
        Result:=false;
     end;
    if Result then PeekToken(FTA);//advance index (only once!)
   end;
end;

function TStratoSource.IsNextID(const st: array of TStratoToken): boolean;
var
  i,j,l:cardinal;
begin
  i:=0;
  j:=FTA;
  Result:=true;//default
  while (Result) and (i<StratoTokenMaxLookForward) do
   begin
    if (((i and 1)=0) and (FTokens[j].Token=stIdentifier)) or
       (((i and 1)=1) and (FTokens[j].Token=stPeriod)) then
     begin
      inc(i);
      PeekToken(j);
     end
    else
      Result:=false;
   end;
  if i=StratoTokenMaxLookForward then
   begin
    Error('Maximum token look forward exceeded');
    Result:=false;
   end
  else
    if (i and 1)=1 then
     begin
      l:=Length(st);
      if i+l>=StratoTokenMaxLookForward then
       begin
        Error('Maximum token look forward exceeded');
        Result:=false;
       end
      else
       begin
        i:=0;
        Result:=true;//default
        while Result and (i<l) do
          if FTokens[j].Token=st[i] then
           begin
            inc(i);
            PeekToken(j);
           end
          else
            Result:=false;
        if Result then PeekToken(FTA);//advance index (only once!)
       end;
     end
    else
      Result:=false;
end;

function TStratoSource.IsNextBetween(st1, st2: TStratoToken): boolean;
begin
  Result:=(st1<=FTokens[FTA].Token) and (st2>=FTokens[FTA].Token);
end;

procedure TStratoSource.Skip(st: TStratoToken);
begin
  if st=FTokens[FTA].Token then PeekToken(FTA);
end;

function TStratoSource.GetID(var SrcPos: cardinal): UTF8String;
var
  i:cardinal;
begin
  i:=FTA;
  if i=0 then i:=StratoTokenMaxLookForward;
  dec(i);
  Result:=Copy(FSource,FTokens[i].Index,FTokens[i].Length);
  SrcPos:=FTokens[i].SrcPos;
end;

function TStratoSource.GetStrs: UTF8String;
begin
  Result:=GetStr;
  while IsNext([stStringLiteral]) do
    Result:=Result+GetStr;
end;

function TStratoSource.GetStr: UTF8String;
var
  f,i,j,k,l,r:cardinal;
  a:byte;
  b:boolean;
begin
  f:=FTA;
  if f=0 then f:=StratoTokenMaxLookForward;
  dec(f);
  //assert FTokens[f].Token=stStringLiteral
  r:=0;
  SetLength(Result,FTokens[f].Length);
  case FSource[FTokens[f].Index] of
    ''''://Pascal-style
     begin
      i:=FTokens[f].Index+1;
      j:=i+FTokens[f].Length-2;
      while i<>j do
       begin
        inc(r);
        Result[r]:=FSource[i];
        if FSource[i]='''' then inc(i);//TODO: if not in multi-byte UTF8!
        inc(i);
        //TODO: clip leading whitespace on EOLs?
       end;
     end;
    '"'://C-style (or Python-style triple double quotes)
     begin
      i:=FTokens[f].Index;
      j:=i+FTokens[f].Length-1;
      if (FTokens[f].Length>=6) and (FSource[i]='"')
        and (FSource[i+1]='"') and (FSource[i+2]='"')
        //and (FSource[j-1]='"') and (FSource[j-2]='"') and (FSource[j-3]='"')
        then
       begin
        //Python-style: triple double quotes
        inc(i,3);
        dec(j,2);
        //find common indentation
        //TODO: just first line for now, check all lines?
        l:=i;
        while (l<>j) and (FSource[l]<>#13) and (FSource[l]<>#10) do inc(l);
        k:=0;
        while (l<>j) and (FSource[l]<=' ') do
         begin
          inc(l);
          inc(k);
         end;
        b:=false;
       end
      else
       begin
        //C-style
        inc(i);
        b:=true;
        k:=0;//counter warning
       end;
      while i<>j do
       begin
         case FSource[i] of
           '\'://backslash: escape
           begin
            inc(i);
            if (FSource[i]=#13) or (FSource[i]=#10) then
             begin
              //skip
              if (FSource[i]=#13) and (i+1<>j) and (FSource[i+1]=#10) then inc(i);
              if not b then
               begin
                l:=k;
                while (l<>0) and (i<j-1) and (FSource[i+1]<=' ') do
                 begin
                  inc(i);
                  dec(l);
                 end;
               end;
             end
            else
             begin
              case FSource[i] of
                'a':a:=7;
                'b':a:=8;
                'f':a:=12;
                'n':a:=10;
                'r':a:=13;
                't':a:=9;
                'v':a:=11;
                '0'..'9':
                 begin
                  //TODO: check three digits!
                  if i+2<j then a:=
                    ((byte(FSource[i  ]) and $7) shl 6) or
                    ((byte(FSource[i+1]) and $7) shl 3) or
                    ( byte(FSource[i+2]) and $7       )
                  else
                    a:=byte(FSource[i]);//?
                  inc(i,2);
                 end;
                //'u'://TODO: unicode!
                'x':
                 begin
                  {
                  //TODO: unicode!!!
                  if (i+3<j)
                    and(s[i  ] in ['0'..'9','A'..'F','a'..'f'])
                    and(s[i  ] in ['0'..'9','A'..'F','a'..'f'])
                    and(s[i  ] in ['0'..'9','A'..'F','a'..'f'])
                    and(s[i  ] in ['0'..'9','A'..'F','a'..'f'])
                    then a:=
                       (((byte(FSource[i  ]) and $1F)+9*((byte(FSource[i  ]) shr 6)and 1)) shl 12)
                    or (((byte(FSource[i+1]) and $1F)+9*((byte(FSource[i+1]) shr 6)and 1)) shl 8)
                    or (((byte(FSource[i+2]) and $1F)+9*((byte(FSource[i+2]) shr 6)and 1)) shl 4)
                    or  ((byte(FSource[i+3]) and $1F)+9*((byte(FSource[i+3]) shr 6)and 1))
                  else
                  }
                  if (i+1<j)
                    and(FSource[i  ] in ['0'..'9','A'..'F','a'..'f'])
                    and(FSource[i+1] in ['0'..'9','A'..'F','a'..'f'])
                    then a:=
                       (((byte(FSource[i  ]) and $1F)+9*((byte(FSource[i  ]) shr 6)and 1)) shl 4)
                    or  ((byte(FSource[i+1]) and $1F)+9*((byte(FSource[i+1]) shr 6)and 1))
                  else
                    a:=byte('x');
                 end;
                else a:=byte(FSource[i]);
              end;
              inc(r);
              Result[r]:=AnsiChar(a);
             end;
           end;
          #13,#10:
           begin
            if (FSource[i]=#13) and (i+1<>j) and (FSource[i+1]=#10) then
             begin
              inc(r);
              Result[r]:=#13;
              inc(i);
             end;
            if b then
             begin
              Error('unterminated string literal');
              i:=j-1;
             end
            else
             begin
              inc(r);
              Result[r]:=FSource[i];
              l:=k;
              while (l<>0) and (i<j-1) and (FSource[i+1]<=' ') do
               begin
                inc(i);
                dec(l);
               end;
             end;
           end;
          else
           begin
            inc(r);
            Result[r]:=FSource[i];//TODO: what if multi-byte UTF8?
           end;
        end;
        inc(i);
        //TODO: clip leading whitespace on EOLs?
       end;
     end;
    else
      Error('unsupported string literal type');
  end;
  SetLength(Result,r);
end;

procedure TStratoSource.Error(const msg:string);
var
  i,x,y:cardinal;
begin
  inc(FErrors);
  i:=FTA;//FTLast?
  if i=0 then i:=StratoTokenMaxLookForward;
  dec(i);
  //TODO: display FFilePath here relative to 'project root'?
  x:=FTokens[i].SrcPos div FLineIndex;
  y:=FTokens[i].SrcPos mod FLineIndex;
  //TODO: config switch append code snippet
//  if FTokens[i].Length>40 then s:=' "'+Copy(FSource,FTokens[i].Index,40)+'...'
//    else s:=' "'+Copy(FSource,FTokens[i].Index,FTokens[i].Length)+'"';
  Writeln(ErrOutput,Format('%s(%d:%d): %s',[FFilePath,x,y,msg]));//Index?
  //raise?
  if @FOnError<>nil then FOnError(Self,x,y,msg);
end;

procedure TStratoSource.ErrorN(const msg:string;const nn:UTF8String);
begin
  Error(msg+' "'+UTF8ToString(nn)+'"');
end;

function TStratoSource.SrcPos: cardinal;
var
  i:cardinal;
begin
  i:=FTA;//FTLast?
  if i=0 then i:=StratoTokenMaxLookForward;
  dec(i);
  Result:=FTokens[i].SrcPos;
end;

end.
