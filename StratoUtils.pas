unit StratoUtils;

interface

function stratoFileToStr(const FilePath:string):string;
procedure stratoStrToFile(const FilePath,Data:string);
function stratoFileToMem(const FilePath:string):pointer;
procedure stratoMemToFile(const FilePath:string;Data:pointer;Length:NativeInt);
function stratoCommandLine:string;

implementation

uses SysUtils, Classes;

const
  UTF8ByteOrderMark:UTF8STring=#$EF#$BB#$BF;

function stratoFileToStr(const FilePath:string):string;
var
  f:TFileStream;
  i:integer;
  s:string;
begin
  //TODO: try except throw
  //TODO: added security: limit access to specific folder
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    i:=f.Size-3;
    s:=#0#0#0;
    f.Read(s[1],3);
    if s<>UTF8ByteOrderMark then
      raise Exception.Create('stratoFileToStr: Only UTF8-files supported');
    SetLength(Result,i);
    f.Read(Result[1],i);
  finally
    f.Free;
  end;
end;

procedure stratoStrToFile(const FilePath,Data:string);
var
  f:TFileStream;
  s:UTF8String;
begin
  //TODO: added security: limit access to specific folder
  s:=UTF8Encode(Data);
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    f.Write(UTF8ByteOrderMark[1],3);
    f.Write(s[1],Length(s));
  finally
    f.Free;
  end;
end;

function stratoFileToMem(const FilePath:string):pointer;
var
  m:TMemoryStream;
begin
  //TODO: file mapping?
  //TODO: try except throw
  //TODO: added security: limit access to specific folder
  m:=TMemoryStream.Create;//TODO:destructor call where??
  m.LoadFromFile(FilePath);
  Result:=m.Memory;
end;

procedure stratoMemToFile(const FilePath:string;Data:pointer;Length:NativeInt);
var
  f:tFileStream;
begin
  //TODO: added security: limit access to specific folder
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    f.Write(Data^,Length);
  finally
    f.Free;
  end;
end;

function stratoCommandLine:string;
begin
  SetLength(Result,MAX_PATH);
  SetLength(Result,GetCommandLine(Result,MAX_PATH));
end;

end.