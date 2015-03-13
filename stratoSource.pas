unit stratoSource;

interface

uses SysUtils, Classes, stratoTokenizer;

type
  TStratoSourceErrorHandler=procedure(Sender:TObject;Line,LPos:cardinal;
    const ErrorMsg:string) of object;

  TStratoSource=class(TObject)
  private
    FTIndex,FTLast,FTLength,FErrors:integer;
    FLineIndex,FFileSize:cardinal;
    FTContent:boolean;
    FTokens:TStratoSourceTokenList;
    FSource:UTF8String;
    FFilePath:string;
    FOnError:TStratoSourceErrorHandler;
  public
    procedure LoadFromFile(const FilePath:string);
    function IsEmpty:boolean;
    function NextToken(var st:TStratoToken):boolean;
    function Token:TStratoToken;
    function IsNext(const st:array of TStratoToken):boolean; //advances index on true!
    function IsNextID(const st:array of TStratoToken):boolean; //(doesn't advance index)
    function IsNextBetween(st1,st2:TStratoToken):boolean;//(doesn't advance index)
    procedure Skip(st:TStratoToken);
    function GetID: UTF8String;
    function GetStr: UTF8String;
    function GetStrs: UTF8String;
    procedure Error(const msg: string);
    function SrcPos:integer;
    property FilePath:string read FFilePath;
    property FileSize:cardinal read FFileSize;
    property LineIndex:cardinal read FLineIndex;
    property ErrorCount:integer read FErrors;
    property Tokens:TStratoSourceTokenList read FTokens;
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
  //parse data
  FTokens:=StratoTokenize(FSource,FLineIndex);
  FTLength:=Length(FTokens);
  FTIndex:=0;
  FTLast:=0;
  FErrors:=0;
end;

function TStratoSource.IsEmpty: boolean;
begin
  Result:=Length(FTokens)=0;
end;

function TStratoSource.NextToken(var st:TStratoToken):boolean;
begin
  if FTIndex<FTLength then
   begin
    st:=Token;
    Result:=true;
   end
  else
    Result:=false;
end;

function TStratoSource.Token: TStratoToken;
begin
  if FTContent then inc(FTIndex);
  if FTIndex<FTLength then
   begin
    Result:=FTokens[FTIndex].Token;
    FTLast:=FTIndex;
    FTContent:=Result<st_Fixed;
    if not FTContent then inc(FTIndex);
   end
  else
   begin
    Error('unexpected end of file');
    Result:=st_Unknown;
   end;
end;

function TStratoSource.IsNext(const st: array of TStratoToken): boolean;
var
  i,l:integer;
begin
  l:=Length(st);
  if FTIndex+l>FTLength then Result:=false else
   begin
    i:=0;
    while (i<>l) and (FTokens[FTIndex+i].Token=st[i]) do inc(i);
    Result:=i=l;
    if Result then
     begin
      FTLast:=FTIndex;
      FTContent:=FTokens[FTIndex].Token<st_Fixed;
      if not FTContent then inc(FTIndex);
     end;
   end;
end;

function TStratoSource.IsNextID(const st: array of TStratoToken): boolean;
var
  i,j,l:integer;
begin
  i:=FTIndex;
  while (i<>FTLength) and (FTokens[i].Token=stIdentifier) do
   begin
    inc(i);
    if (i<>FTLength) and (FTokens[i].Token=stPeriod) then inc(i);
   end;
  l:=Length(st);
  j:=0;
  while (i<>FTLength) and (j<>l) and (FTokens[i].Token=st[j]) do
   begin
    inc(i);
    inc(j);
   end;
  Result:=j=l;
end;

function TStratoSource.IsNextBetween(st1, st2: TStratoToken): boolean;
begin
  if FTIndex<FTLength then
    Result:=(st1<=FTokens[FTIndex].Token) and (st2>=FTokens[FTIndex].Token)
  else
    Result:=false;
end;

procedure TStratoSource.Skip(st: TStratoToken);
begin
  //assert not FContent
  //assert st>=st_Fixed
  if (FTIndex<FTLength) and (FTokens[FTIndex].Token=st) then
   begin
    FTLast:=FTIndex;
    FTContent:=false;
    inc(FTIndex);
   end;
end;

function TStratoSource.GetID: UTF8String;
begin
  //assert (FTIndex>0) and (FTIndex<=FTLength) and (FTokens[FTIndex].Token=stIdentifier)
  Result:=Copy(FSource,FTokens[FTIndex].Index,FTokens[FTIndex].Length);
  FTLast:=FTIndex;
  FTContent:=false;
  inc(FTIndex);
end;

function TStratoSource.GetStrs: UTF8String;
begin
  Result:='';
  while (FTIndex<FTLength) and (FTokens[FTIndex].Token=stStringLiteral) do
    Result:=Result+GetStr;
end;

function TStratoSource.GetStr: UTF8String;
var
  i,j,k,l,r:cardinal;
  a:byte;
  b:boolean;
begin
  //assert FTIndex<FTLength
  //assert FTokens[FTIndex].Token=stStringLiteral
  r:=0;
  SetLength(Result,FTokens[FTIndex].Length);
  case FSource[FTokens[FTIndex].Index] of
    ''''://Pascal-style
     begin
      i:=FTokens[FTIndex].Index+1;
      j:=i+FTokens[FTIndex].Length-2;
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
      i:=FTokens[FTIndex].Index;
      j:=i+FTokens[FTIndex].Length-1;
      if (FTokens[FTIndex].Length>=6) and (FSource[i]='"')
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
  FTContent:=false;
  inc(FTIndex);
end;

procedure TStratoSource.Error(const msg:string);
var
  x,y:cardinal;
begin
  inc(FErrors);
  if FTLast<FTLength then
   begin
    x:=FTokens[FTLast].SrcPos div FLineIndex;
    y:=FTokens[FTLast].SrcPos mod FLineIndex;
    //TODO: config switch append code snippet
//    if FTokens[FTLast].Length>40 then s:=' "'+Copy(FSource,FTokens[FTLast].Index,40)+'...'
//      else s:=' "'+Copy(FSource,FTokens[FTLast].Index,FTokens[FTLast].Length)+'"';
    Writeln(ErrOutput,Format('%s(%d:%d): %s',[FFilePath,x,y,msg]));//Index?
   end
  else
   begin
    x:=0;
    y:=0;
    Writeln(ErrOutput,Format('%s(EOF): %s',[FFilePath,msg]));
   end;
  //raise?
  if @FOnError<>nil then FOnError(Self,x,y,msg);
end;

function TStratoSource.SrcPos: integer;
begin
  if FTLast<FTLength then
    Result:=FTokens[FTLast].SrcPos
  else
    Result:=0;
end;

end.
