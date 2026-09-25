unit stratoTokenizer;

interface

{$D-}
{$L-}

{$I stratoTokens_T.inc}

const
  st_EOF=TStratoToken($FFFE);

type
  TStratoSourceToken=record
    Token:TStratoToken;
    Index,Length,SrcPos:cardinal;
  end;

procedure StratoTokenizeInit(const Code: UTF8String; LineIndex: cardinal;
  var FirstToken: TStratoSourcetoken);
procedure StratoTokenizeNext(const Code: UTF8String; LineIndex: cardinal;
  const CurrentToken: TStratoSourceToken; var NextToken: TStratoSourceToken);

function ParseInteger(const lit: UTF8String): Int64;

implementation

procedure StratoTokenizeInit(const Code: UTF8String; LineIndex:cardinal;
  var FirstToken:TStratoSourcetoken);
var
  t:TStratoSourceToken;
begin
  t.Token:=st_Unknown;
  t.Index:=1;
  t.Length:=0;
  t.SrcPos:=LineIndex+1;//start at 1:1
  StratoTokenizeNext(Code,LineIndex,t,FirstToken);
end;

procedure StratoTokenizeNext(const Code: UTF8String; LineIndex: cardinal;
  const CurrentToken: TStratoSourceToken; var NextToken: TStratoSourceToken);
var
  CodeIndex,CodeLength,ln,ls:cardinal;
  n,n1,n2:UTF8Char;

  function nn(Fwd:cardinal):UTF8Char;
  var
    x:cardinal;
  begin
    x:=CodeIndex+Fwd;
    if x<=CodeLength then Result:=Code[x] else Result:=#0;
  end;

  procedure doWhiteSpace; //inc(CodeIndex) detecting EOL's
  begin
    inc(CodeIndex);
    case n of
      #13://CR
       begin
        if nn(0)=#10 then inc(CodeIndex);//CRLF
        inc(ln);
        ls:=CodeIndex;
       end;
      #10://LF
       begin
        inc(ln);
        ls:=CodeIndex;
       end;
      //TODO: #9: count tabs as 4 of 8
      //TODO: #12: page count?
    end;
  end;

  procedure doToken(t:TStratoToken;n:cardinal);
  begin
    inc(CodeIndex,n);
    NextToken.Token:=t;
    NextToken.Length:=CodeIndex-NextToken.Index;
  end;

  procedure doSkipToEOL;
  begin
    while (CodeIndex<=CodeLength)
      and not(Code[CodeIndex] in [#13,#10,#12])
      do inc(CodeIndex);
    if (CodeIndex<CodeLength)
      and (Code[CodeIndex]=#13)
      and (Code[CodeIndex+1]=#10)
      then inc(CodeIndex);
    inc(CodeIndex);
    inc(ln);
    ls:=CodeIndex;
  end;

  procedure doSkipToEndOfBlock(const Delimiter:UTF8String);
  var
    i,l:cardinal;
  begin
    l:=Length(Delimiter);
    inc(CodeIndex,l);//assert start-stop delimiters same length
    i:=0;
    repeat
      if (CodeIndex+l>CodeLength) then
       begin
        //too close to EOF for a delimiter
        CodeIndex:=CodeLength+1;//?
        //Fail('Undelimited block "'+Delimiter+'"');//?
        i:=l;
       end
      else
      if (Code[CodeIndex+i]=Delimiter[i+1]) then
        inc(i)
      else
       begin
        i:=0;
        n:=Code[CodeIndex];
        doWhiteSpace;//keep counting lines with EOL's
       end;
    until i=l;
    inc(CodeIndex,l);
  end;

  procedure doIdentifier;
  begin
    inc(CodeIndex);
    while (CodeIndex<=CodeLength)
      and (Code[CodeIndex] in ['0'..'9','A'..'Z','_','a'..'z'])
      do inc(CodeIndex);
    doToken(stIdentifier,0);
  end;

  procedure doCString;
  var
    inStr:boolean;
  begin
    //assert n='"'
    inc(CodeIndex);
    inStr:=true;
    while inStr do
     begin
      if CodeIndex>CodeLength then
        n:=#0
      else
       begin
        n:=Code[CodeIndex];
        inc(CodeIndex);
       end;
      case n of
        #0,#13,#10:
          //Fail('Unterminated string')
          inStr:=false;
        '\':
          if nn(0) in ['\','"','0'..'9','A'..'Z','a'..'z'] then
            inc(CodeIndex)
          else
            //Fail('Unsupported escape character')
            inc(CodeIndex);//inStr:=false;
        '"':
          inStr:=false;
      end;
     end;
    doToken(stStringLiteral,0);
  end;

  procedure doPascalString;
  var
    inStr:boolean;
  begin
    //assert n=''''
    inc(CodeIndex);
    inStr:=true;
    while inStr do
     begin
      if CodeIndex>CodeLength then
        n:=#0
      else
       begin
        n:=Code[CodeIndex];
        inc(CodeIndex);
       end;
      if n in [#0,#13,#10] then
        //Fail('Unterminated string')
        inStr:=false
      else
      if n='''' then
        if nn(0)='''' then
          inc(CodeIndex)
        else
          inStr:=false;
     end;
    doToken(stStringLiteral,0);
  end;

  procedure doMultiLineString;
  begin
    doSkipToEndOfBlock('"""');
    doToken(stStringLiteral,0);
  end;

  procedure doNumeric;
  var
    isFloat,isSciNot:boolean;
  begin
    //n='-': is a separate token (and unary operator)
    n1:=nn(1);
    if (n='0') and (n1 in ['X','x']) then //hexadecimal
     begin
      inc(CodeIndex);
      while (CodeIndex<=CodeLength)
        and (Code[CodeIndex] in ['0'..'9','A'..'F','a'..'f'])
        do inc(CodeIndex);
     end
    else
    if (n='0') and (n1 in ['B','b']) then //binary
     begin
      inc(CodeIndex);
      while (CodeIndex<=CodeLength)
        and (Code[CodeIndex] in ['0','1','_'])
        do inc(CodeIndex);
     end
    else
    if (n='0') and (n1 in ['O','o']) then //octal
     begin
      inc(CodeIndex);
      while (CodeIndex<=CodeLength)
        and (Code[CodeIndex] in ['0'..'7','_'])
        do inc(CodeIndex);
     end
    else
     begin
      isFloat:=false;//default
      isSciNot:=false;//default
      while Code[CodeIndex] in ['0'..'9'] do
       begin
        inc(CodeIndex);
        if not(IsFloat) and (nn(0)='.') then //floating point
         begin
          isFloat:=true;
          inc(CodeIndex);//'.'
         end;
        if not(isSciNot) and (nn(0) in ['E','e']) then
         begin
          isSciNot:=true;
          inc(CodeIndex);
          if nn(0) in ['-','+'] then inc(CodeIndex);
         end;
       end;
     end;
    doToken(stNumericLiteral,0);
  end;

  procedure doCoalesce;
  begin
    while (CodeIndex<=CodeLength) and (Code[CodeIndex]=n) do inc(CodeIndex);
    doToken(stHRule,0);
  end;

const
  st_Invalid=TStratoToken($FFFF);
begin
  CodeIndex:=CurrentToken.Index+CurrentToken.Length;
  CodeLength:=Length(Code);
  ls:=CurrentToken.Index-(CurrentToken.SrcPos mod LineIndex)+1;//line start
  ln:=CurrentToken.SrcPos div LineIndex;//line number
  NextToken.Token:=st_Invalid;
  repeat
    NextToken.Index:=CodeIndex;
    NextToken.SrcPos:=ln*LineIndex+(CodeIndex+1-ls);
    if CodeIndex>CodeLength then
      NextToken.Token:=st_EOF
    else
     begin
      n:=Code[CodeIndex];
      if n<=' ' then
        doWhiteSpace
      else

        {$I stratoTokens.inc} //see "-Z"

     end;
  until NextToken.Token<>st_Invalid;
  //Writeln(Format('%s %d:%d',[StratoTokenNames[NextToken.Token],NextToken.SrcPos,NextToken.Length]));
end;

function ParseInteger(const lit: UTF8string): Int64;
var
  neg:boolean;
  i,l:integer;
  c:AnsiChar;
begin
  i:=1;
  l:=Length(lit);
  //negative values support for ParseLiteral
  if (l<>0) and (lit[1]='-') then
   begin
    neg:=true;
    inc(i);
   end
  else
    neg:=false;
  if (l>i+1) and (lit[i]='0') then c:=lit[i+1] else c:=' ';
  case c of
    'x','X'://hex
     begin
      Result:=0;
      inc(i,2);
      while i<=l do
       begin
        case lit[i] of
          '0'..'9':
            Result:=(Result shl 4) or (byte(lit[i]) and $F);
          'A'..'F','a'..'f':
            Result:=(Result shl 4) or (9+(byte(lit[i]) and $7));
        end;
        inc(i);
       end;
     end;
    'b','B'://binary
     begin
      Result:=0;
      inc(i,2);
      while i<=l do
       begin
        case lit[i] of
          '0':Result:=Result shl 1;
          '1':Result:=(Result shl 1) or 1;
        end;
        inc(i);
       end;
     end;
    'o','O'://octal
     begin
      Result:=0;
      inc(i,2);
      while i<=l do
       begin
        Result:=(Result shl 3) or (byte(lit[i]) and $7);
        inc(i);
       end;
     end;
    //TODO more?
    else
     begin
      Result:=0;
      while i<=l do
       begin
        Result:=(Result*10)+(byte(lit[i]) and $F);
        inc(i);
       end;
	    //TODO: scientific, floating point
     end;
  end;
  if neg then Result:=-Result;
end;

end.
