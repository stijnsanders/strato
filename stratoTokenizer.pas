unit stratoTokenizer;

interface

type
  TStratoToken=(
    stIdentifier,
    stStringLiteral,
    stNumericLiteral,

    st_Fixed,//all below have fixed content

    stSemiColon,//";"
    stComma,//','
    stPeriod, //"."
    stColon,//":"
    stAt,//"@"
    stCaret,//"^"
    stQuestionMark,//"?"
    stAmpersand,//"&"

    stPOpen,stPClose,//"()"
    stAOpen,stAClose,//"{}"
    stBOpen,stBClose,//"[]"

    stImport,//"<<<"
    stDefer,//">>>"
    stTry,//":::"
    stCatch,//"???"
    stThrow,//"!!!"

    stThis,//"@@"
    stInherited,//"@@@"
    stResult,//"??"

    stOpAssign, //":="
    stOpAssignAdd, //"+="
    stOpAssignSub, //"-="
    stOpAssignMul, //"*="
    stOpAssignDiv, //"/="
    stOpAssignMod, //"%="
    stOpAssignOr, //"||="
    stOpAssignAnd, //"&&="
    stOpEQ,
    stOpNEQ,
    stOpLT,
    stOpLTE,
    stOpGT,
    stOpGTE,

    stOpAnd,
    stOpOr,
    stOpNot,
    stOpXor,

    stOpAdd,
    stOpSub,
    stOpMul,
    stOpDiv,
    stOpMod,
    stOpInc,
    stOpDec,
    stOpShl,
    stOpShr,

    stOpSizeOf,//"@?"
    stOpTypeIs,//"?="

    st_Unknown
  );

  TStratoSourceToken=record
    Token:TStratoToken;
    Index,Length,SrcPos:integer;
  end;

  TStratoSourceTokenList=array of TStratoSourceToken;

const
  StratoTokenizeLineIndex=1000;

function StratoTokenize(const Code: UTF8String): TStratoSourceTokenList;

implementation

function StratoTokenize(const Code: UTF8String): TStratoSourceTokenList;
var
  CodeIndex,CodeLength:integer;
  rl,ri,ln,lx:integer;

  procedure Add(Len:integer;t:TStratoToken);
  begin
    if ri=rl then
     begin
      inc(rl,$1000);//grow
      SetLength(Result,rl);
     end;
    Result[ri].Index:=CodeIndex;
    Result[ri].Length:=Len;
    Result[ri].Token:=t;
    Result[ri].SrcPos:=ln*StratoTokenizeLineIndex+(CodeIndex-lx)+1; //TODO: count tab as 4 (or 8)?
    inc(ri);
    inc(CodeIndex,Len);
  end;

  function CodeNext(Fwd:integer):AnsiChar;
  begin
    if CodeIndex+Fwd<=CodeLength then Result:=Code[CodeIndex+Fwd] else Result:=#0;
  end;

  procedure incCodeIndexX; //inc(CodeIndex) detecting EOL's
  begin
    case Code[CodeIndex] of
      #13:
       begin
        inc(ln);
        if CodeNext(1)=#10 then inc(CodeIndex);
        lx:=CodeIndex+1;
       end;
      #10:
       begin
        inc(ln);
        lx:=CodeIndex+1;
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
    i:integer;
  begin
    i:=CodeIndex;
    while (i<=CodeLength) and (Code[i] in ['0'..'9','A'..'Z','_','a'..'z']) do inc(i);
    Add(i-CodeIndex,stIdentifier);
  end;

var
  i:integer;
begin
  CodeIndex:=1;
  CodeLength:=Length(Code);
  ri:=0;
  rl:=0;
  lx:=1;
  ln:=1;
  SkipWhiteSpace;
  while CodeIndex<=CodeLength do
   begin
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
      '{':Add(1,stAOpen);
      '}':Add(1,stAClose);
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
      '|':
        case CodeNext(1) of
          '!':Add(2,stOpXor);
          '|':
            case CodeNext(2) of
              '=':Add(3,stOpAssignOr);
              else Add(2,stOpOr);
            end;
          else Add(1,st_Unknown);//raise?
        end;
      ''''://string
       begin
        i:=CodeIndex+1;
        while (i<=CodeLength) and (Code[i]<>'''') do
         begin
          //TODO: detect EOL?
          while (i<=CodeLength) and (Code[i]<>'''') do inc(i);
          if (i<CodeLength) and (Code[i+1]='''') then inc(i,2);
         end;
        Add(i-CodeIndex+1,stStringLiteral);
       end;
      '"'://string
       begin
        i:=CodeIndex+1;
        while (i<=CodeLength) and (Code[i]<>'"') do
         begin
          //TODO: detect EOL?
          if Code[i]='\' then inc(i);
          inc(i);
         end;
        Add(i-CodeIndex+1,stStringLiteral);
       end;
      '0'..'9'://digits
       begin
        i:=CodeIndex+1;
        //TODO: sci,hex,oct,bin...
        while (i<=CodeLength) and (Code[i] in ['0'..'9']) do inc(i);
        Add(i-CodeIndex,stNumericLiteral);
       end;
      '+':
        case CodeNext(1) of
          '+':Add(2,stOpInc);
          '=':Add(2,stOpAssignAdd);
          else Add(1,stOpAdd);
        end;
      '-':
        case CodeNext(1) of
          '-':Add(2,stOpDec);
          '=':Add(2,stOpAssignSub);
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
              '@':Add(3,stInherited);
              else Add(2,stThis);
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
              '<':Add(3,stImport);
              else Add(2,stOpShl);
            end;
          '=':Add(2,stOpLTE);
          else Add(1,stOpLT);
        end;
      '>':
        case CodeNext(1) of
          '>':
            case CodeNext(2) of
              '>':Add(3,stDefer);
              else Add(2,stOpShr);
            end;
          '=':Add(2,stOpGTE);
          else Add(1,stOpGT);
        end;
      '=':
        case CodeNext(1) of
          '?':Add(2,stOpTypeIs);
          '=':Add(2,stOpEq);//raise?
          else Add(1,stOpEq);
        end;
      '?':
        case CodeNext(1) of
          '?':
            case CodeNext(2) of
              '?':Add(3,stCatch);
              else Add(2,stResult);
            end;
          else Add(1,stQuestionMark);//stOpIf
        end;
      '!':
        case CodeNext(1) of
          '=':Add(2,stOpNEQ);
          '!':
            case CodeNext(2) of
              '!':Add(3,stThrow);
              else Add(2,st_Unknown);//raise? Add(1,stOpNot)?
            end;
          else Add(1,stOpNot);
        end;
      '.':Add(1,stPeriod);
      ',':Add(1,stComma);
      ';':Add(1,stSemiColon);
      ':':
        case CodeNext(1) of
          ':':
            case CodeNext(2) of
              ':':Add(3,stTry);
              else Add(2,st_Unknown);//raise?
            end;
          '=':Add(2,stOpAssign);
          else Add(1,stColon);
        end;
      'A'..'Z','_','a'..'z':GetIdentifier;
      else Add(1,st_Unknown);//raise?
    end;
    SkipWhiteSpace;
   end;
  SetLength(Result,ri);
end;

end.
