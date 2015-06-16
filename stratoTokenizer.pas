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
    stDefine,//"=", specifically not ":=" or "=="!
    stAt,//"@"
    stCaret,//"^"
    stQuestionMark,//"?"
    stAmpersand,//"&"

    stPOpen,stPClose,//"()"
    stAOpen,stAClose,//"{}"
    stBOpen,stBClose,//"[]"

    stHRule,//"---"
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
    stOpEQ,  //"=="
    stOpNEQ, //"!="
    stOpLT,  //"<"
    stOpLTE, //"<="
    stOpGT,  //">"
    stOpGTE, //">="

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
    Index,Length,SrcPos:cardinal;
  end;

  TStratoSourceTokenList=array of TStratoSourceToken;

function StratoTokenize(const Code: UTF8String;
  LineIndex: cardinal): TStratoSourceTokenList;

implementation

function StratoTokenize(const Code: UTF8String;
  LineIndex: cardinal): TStratoSourceTokenList;
var
  CodeIndex,CodeLength:cardinal;
  rl,ri,ln,lx:cardinal;

  procedure Add(Len:cardinal;t:TStratoToken);
  begin
    if ri=rl then
     begin
      inc(rl,$1000);//grow
      SetLength(Result,rl);
     end;
    Result[ri].Index:=CodeIndex;
    Result[ri].Length:=Len;
    Result[ri].Token:=t;
    //TODO: count tab as 4 (or 8 or 2)?
    Result[ri].SrcPos:=ln*LineIndex+(CodeIndex-lx)+1;
    inc(ri);
    inc(CodeIndex,Len);
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
          '-':
            case CodeNext(1) of
              '-':
               begin
                i:=CodeIndex+3;
                while (i<=CodeLength) and (Code[i]='-') do inc(i);
                Add(i-CodeIndex,stHRule);
               end;
              else Add(2,stOpDec);
            end;
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
          '=':Add(2,stOpEq);
          else Add(1,stDefine);
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
