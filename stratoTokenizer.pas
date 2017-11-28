unit stratoTokenizer;

interface

{$D-}
{$L-}

type
  TStratoToken=(
    stIdentifier,
    stStringLiteral,
    stNumericLiteral,

    st_Fixed, //all below have fixed content

    stSemiColon,    //";"
    stComma,        //','
    stPeriod,       //"."
    stColon,        //":"
    stDefine,       //"=", specifically not ":=" or "=="!
    stAt,           //"@"
    stCaret,        //"^"
    stQuestionMark, //"?"
    stAmpersand,    //"&"
    stDollar,       //'$'
    stHash,         //'#'
    stTilde,        //'~'

    stPOpen,stPClose, //"()" parentheses
    stCOpen,stCClose, //"{}" curly braces
    stBOpen,stBClose, //"[]" brackets

    stHRule,       //"---" (or more)
    stThreeColons, //":::"
    stThreeWhats,  //"???"
    stThreeBangs,  //"!!!"
    stEllipsis,    //"..."

    stAtAt,     //"@@": this/self
    stAtAtAt,   //"@@@": inherited/base
    stWhatWhat, //"??": result
    stHashHash, //"##": interation

    stOpAssign,    //":="
    stOpAssignAdd, //"+="
    stOpAssignSub, //"-="
    stOpAssignMul, //"*="
    stOpAssignDiv, //"/="
    stOpAssignMod, //"%="
    stOpAssignOr,  //"||="
    stOpAssignAnd, //"&&="
    stOpEQ,  //"==" //see alo stDefine,stOpAssign
    stOpNEQ, //"!="
    stOpLT,  //"<"
    stOpLTE, //"<="
    stOpGT,  //">"
    stOpGTE, //">="

    stOpAnd, //"&&"
    stOpOr,  //"||"
    stOpNot, //"!"
    stOpXor, //"|!"

    stOpAdd, //"+"
    stOpSub, //"-"
    stOpMul, //"*"
    stOpDiv, //"/"
    stOpMod, //"%"
    stOpInc, //"++"
    stOpDec, //"--"
    stOpShl, //"<<"
    stOpShr, //">>"
    stOpRange, //".."

    stThreeLT,//"<<<" stOpRol
    stThreeGT,//">>>" stOpRor

    stOpSizeOf,//"@?"
    stOpTypeIs,//"?="

    st_EOF, //(past) end of file
    st_Unknown
  );

  TStratoSourceToken=record
    Token:TStratoToken;
    Index,Length,SrcPos:cardinal;
  end;

procedure StratoTokenizeInit(const Code: UTF8string; LineIndex:cardinal;
  var FirstToken:TStratoSourcetoken);
procedure StratoTokenizeNext(const Code: UTF8string; LineIndex: cardinal;
  const CurrentToken:TStratoSourceToken; var NextToken:TStratoSourceToken);

function ParseInteger(const lit: string): Int64;

implementation

procedure StratoTokenizeInit(const Code: UTF8string; LineIndex:cardinal; var FirstToken:TStratoSourcetoken);
var
  t:TStratoSourceToken;
begin
  t.Token:=st_Unknown;
  t.Index:=1;
  t.Length:=0;
  t.SrcPos:=LineIndex+1;//start at 1:1
  StratoTokenizeNext(Code,LineIndex,t,FirstToken);
end;

procedure StratoTokenizeNext(const Code: UTF8string; LineIndex: cardinal;
  const CurrentToken:TStratoSourceToken; var NextToken:TStratoSourceToken);
var
  CodeIndex,CodeLength:cardinal;
  ln,ls:cardinal;

  procedure Add(Len:cardinal;t:TStratoToken);
  begin
    NextToken.Token:=t;
    NextToken.Index:=CodeIndex;
    NextToken.Length:=Len;
    NextToken.SrcPos:=ln*LineIndex+(CodeIndex-ls);
    //TODO: count tab as 4 (or 8 or 2)?
  end;

  function CodeNext(Fwd:cardinal):AnsiChar;
  begin
    if CodeIndex+Fwd<=CodeLength then
      Result:=Code[CodeIndex+Fwd]
    else
      Result:=#0;
  end;

  procedure incCodeIndexX; //inc(CodeIndex) detecting EOL's
  begin
    case Code[CodeIndex] of
      #13:
       begin
        inc(ln);
        if CodeNext(1)=#10 then inc(CodeIndex);
        ls:=CodeIndex;
       end;
      #10:
       begin
        inc(ln);
        ls:=CodeIndex;
       end;
    end;
    inc(CodeIndex);
  end;

  procedure SkipWhiteSpace;
  begin
    while (CodeIndex<=CodeLength) and (Code[CodeIndex]<=' ') do incCodeIndexX;
  end;

  procedure GetIdentifier;
  var
    i:cardinal;
  begin
    i:=CodeIndex;
    while (i<=CodeLength) and (Code[i] in ['0'..'9','A'..'Z','_','a'..'z']) do inc(i);
    Add(i-CodeIndex,stIdentifier);
  end;

  procedure AddX(Len:cardinal;t:TStratoToken); //add, but detect EOL's
  var
    i,l:cardinal;
  begin
    i:=CodeIndex;
    l:=CodeIndex+Len;
    if l>CodeLength then l:=CodeLength;
    while CodeIndex<>l do incCodeIndexX;
    CodeIndex:=i;
    Add(Len,t);
  end;

var
  i:cardinal;
const
  st_Invalid=TStratoToken(-1);
begin
  CodeIndex:=CurrentToken.Index+CurrentToken.Length;
  CodeLength:=Length(Code);
  ls:=CurrentToken.Index-(CurrentToken.SrcPos mod LineIndex);//line start
  ln:=CurrentToken.SrcPos div LineIndex;//line number
  NextToken.Token:=st_Invalid;
  while NextToken.Token=st_Invalid do
   begin
    SkipWhiteSpace;
    if CodeIndex>CodeLength then
     begin
      NextToken.Token:=st_EOF;
      NextToken.Index:=CodeLength+1;
      NextToken.SrcPos:=(ln+1)*LineIndex+1;
      Exit;
     end;
    case Code[CodeIndex] of
      '/':
        case CodeNext(1) of
          '/'://comment to EOL
           begin
            inc(CodeIndex,2);
            while (CodeIndex<=CodeLength) and not(Code[CodeIndex] in [#10,#12,#13]) do inc(CodeIndex);
            //EOL itself: see ShipWhiteSpace below
           end;
          '*'://comment block
           begin
            inc(CodeIndex,2);
            //TODO: nested comment blocks
            while (CodeIndex<CodeLength) and not((Code[CodeIndex]='*') and (Code[CodeIndex+1]='/')) do incCodeIndexX;
            inc(CodeIndex,2);
           end;
          else
            Add(1,stOpDiv);
        end;
      '(':Add(1,stPOpen);
      ')':Add(1,stPClose);
      '{':Add(1,stCOpen);
      '}':Add(1,stCClose);
      '[':Add(1,stBOpen);
      ']':Add(1,stBClose);
      '&':
        case CodeNext(1) of
          '&':
            case CodeNext(2) of
              '=':Add(3,stOpAssignAnd);
              else Add(2,stOpAnd);
            end;
          else Add(1,stAmpersand);
        end;
      '$':Add(1,stDollar);
      '#':
        case CodeNext(1) of
          '#':Add(2,stHashHash);
          else Add(1,stHash);
        end;
      '~':Add(1,stTilde);
      '|':
        case CodeNext(1) of
          '!':Add(2,stOpXor);
          '|':
            case CodeNext(2) of
              '=':Add(3,stOpAssignOr);
              else Add(2,stOpOr);
            end;
          else Add(1,st_Unknown);//stPipe
        end;
      ''''://string
       begin
        i:=CodeIndex+1;
        //TODO: support # syntax?
        while (i<=CodeLength) and (Code[i]<>'''') do
         begin
          inc(i);
          if (i<CodeLength) and (Code[i]='''') and (Code[i+1]='''') then
            inc(i,2);
         end;
        AddX(i-CodeIndex+1,stStringLiteral);
       end;
      '"'://string
        if (CodeNext(1)='"') and (CodeNext(2)='"') then
         begin
          i:=CodeIndex+3;
          while (i+2<=CodeLength) and not((Code[i]='"') and
            (Code[i+1]='"') and (Code[i+2]='"')) do
           begin
            if Code[i]='\' then inc(i);//?
            inc(i);
           end;
          AddX(i-CodeIndex+3,stStringLiteral);
         end
        else
         begin
          i:=CodeIndex+1;
          while (i<=CodeLength) and (Code[i]<>'"') do
           begin
            if Code[i]='\' then inc(i);
            inc(i);
           end;
          AddX(i-CodeIndex+1,stStringLiteral);
         end;
      '0'..'9'://digits
       begin
        i:=CodeIndex;
        if Code[CodeIndex]='0' then
          case CodeNext(1) of //see also ParseInteger
            'x','X'://hex
             begin
              inc(i,2);
              while (i<=CodeLength) and (Code[i] in ['0'..'9','A'..'F','a'..'f']) do inc(i);
             end;
            'b','B'://binary
             begin
              inc(i,2);
              while (i<=CodeLength) and (Code[i] in ['0','1','_']) do inc(i);
             end;
            'o','O'://octal
             begin
              inc(i,2);
              while (i<=CodeLength) and (Code[i] in ['0'..'7']) do inc(i);
             end;
            //TODO more?
          end;
        if i=CodeIndex then
         begin
          inc(i);
          while (i<=CodeLength) and (Code[i] in ['0'..'9']) do inc(i);
         end;
        Add(i-CodeIndex,stNumericLiteral);
        //TODO: scientific, floating point
       end;
      '+':
        case CodeNext(1) of
          '+':Add(2,stOpInc);
          '=':Add(2,stOpAssignAdd);
          else Add(1,stOpAdd);
        end;
      '-':
        case CodeNext(1) of
          '-':
            case CodeNext(2) of
              '-':
               begin
                i:=CodeIndex+3;
                while (i<=CodeLength) and (Code[i]='-') do inc(i);
                Add(i-CodeIndex,stHRule);
               end;
               //'>':Add(3,stLongArrow);
              else Add(2,stOpDec);
            end;
          '=':Add(2,stOpAssignSub);
          //'>':Add(2,stArrow);
          else Add(1,stOpSub);
        end;
      '%':
        case CodeNext(1) of
          '=':Add(2,stOpAssignMod);
          else Add(1,stOpMod);
        end;
      '*':
        case CodeNext(1) of
          //'*':Add(2,stOpPower);
          '=':Add(2,stOpAssignMul);
          else Add(1,stOpMul);
        end;
      '@':
        case CodeNext(1) of
          '@':
            case CodeNext(2) of
              '@':Add(3,stAtAtAt);
              else Add(2,stAtAt);
            end;
          '?':Add(2,stOpSizeOf);
          else Add(1,stAt);
        end;
      '^':
        Add(1,stCaret);
      '<':
        case CodeNext(1) of
          '>':Add(2,stOpNEQ);
          '<':
            case CodeNext(2) of
              '<':Add(3,stThreeLT);//decl:import, logic:roll left
              else Add(2,stOpShl);
            end;
          '=':Add(2,stOpLTE);
          else Add(1,stOpLT);
        end;
      '>':
        case CodeNext(1) of
          '>':
            case CodeNext(2) of
              '>':Add(3,stThreeGT);
              else Add(2,stOpShr);
            end;
          '=':Add(2,stOpGTE);
          else Add(1,stOpGT);
        end;
      '=':
        case CodeNext(1) of
          //'?':Add(2,stOpTypeIs);
          //'>':Add(2,stFatArrow);//TODO
          '=':Add(2,stOpEq);
          else Add(1,stDefine);
        end;
      '?':
        case CodeNext(1) of
          '?':
            case CodeNext(2) of
              '?':Add(3,stThreeWhats);
              else Add(2,stWhatWhat);
            end;
          '=':Add(2,stOpTypeIs);
          //':':Add(2,stOpElvis);//TODO: https://en.wikipedia.org/wiki/Elvis_operator
          //'.':Add(2,stWhatPeriod);
          else Add(1,stQuestionMark);//stOpIf
        end;
      '!':
        case CodeNext(1) of
          '=':Add(2,stOpNEQ);
          '!':
            case CodeNext(2) of
              '!':Add(3,stThreeBangs);
              else
               begin
                Add(1,stOpNot);
                Add(1,stOpNot);
               end;
            end;
          else Add(1,stOpNot);
        end;
      '.':
        case CodeNext(1) of
          '.':
            case CodeNext(2) of
              '.':Add(3,stEllipsis);
              else Add(2,stOpRange);
            end;
          else Add(1,stPeriod);
        end;
      ',':Add(1,stComma);
      ';':Add(1,stSemiColon);
      ':':
        case CodeNext(1) of
          ':':
            case CodeNext(2) of
              ':':Add(3,stThreeColons);
              else Add(2,st_Unknown);//stNameSpace?
            end;
          '=':Add(2,stOpAssign);
          else Add(1,stColon);
        end;
      'A'..'Z','_','a'..'z':GetIdentifier;
      else Add(1,st_Unknown);
    end;
   end;
  //SetLength(Result,ri);
end;

function ParseInteger(const lit: string): Int64;
var
  neg:boolean;
  i,l:integer;
  c:char;
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
