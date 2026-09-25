unit stratoTokGen;

interface

procedure StratoGenerateTokenizer(const FilePath:string);

type
  TGlyph=record
    Start,Stop:UTF8Char;
    Token,Task:string;
    Next:array of TGlyph;
    function InRange(c:UTF8Char):boolean; inline;
  end;
  PGlyph=^TGlyph;

implementation

uses SysUtils, Classes;

procedure StratoGenerateTokenizer(const FilePath:string);
var
  sl,slV,slT,slN:TStringList;
  sli:integer;

  t1,t2:string;
  tx,gx,gl:integer;
  gRoot:TGlyph;
  g:PGlyph;
  c1,c2:UTF8Char;

  procedure Fail(const msg:string);
  begin
    //raise?
    Writeln('#'+IntToStr(sli+1)+': '+msg);
  end;

  function uc(c:char):UTF8Char;
  begin
    Result:=UTF8Char(c);
    if (c=#0) or (c>=#$7F) then
      raise Exception.Create('Invalid character (currently only Unicode basic plane supported)');
  end;

  function ud(c:UTF8Char):string;
  begin
    if c<' ' then
      Result:='#'+IntToStr(byte(c))
    else
    if c='''' then
      Result:=''''''''''
    else
      Result:=''''+char(c)+'''';
  end;

  procedure gSub(gg:PGlyph;depth:integer;const prefix:string);
  var
    gx:integer;
    g:PGlyph;
    d:string;
  begin
    if Length(gg.Next)<>0 then
     begin
      d:=IntToStr(depth);
      sl.Add(prefix+' begin');
      sl.Add(prefix+'  n'+d+':=nn('+d+');');
      for gx:=0 to Length(gg.Next)-1 do
       begin
        g:=@gg.Next[gx];

        if g.Stop=#0 then
          sl.Add(prefix+'  if n'+d+'='+ud(g.Start)+' then')
        else
          sl.Add(prefix+'  if (n'+d+')>='+ud(g.Start)+') and (n'+d+'<='+ud(g.Stop)+') then');

        gSub(g,depth+1,prefix+'  ');
       end;
     end;

    g:=gg;
    if g.Task='' then
      if g.Token='' then
        sl.Add(prefix+'  doToken(st_Unknown,'+IntToStr(depth)+')')
      else
        sl.Add(prefix+'  doToken(st'+g.Token+','+IntToStr(depth)+')')
    else
      if g.Token='' then
        sl.Add(prefix+'  do'+g.Task)
      else
        sl.Add(prefix+'  do'+g.Task+' //st'+g.Token);//+'(st'+g.Token+')'?

    if Length(gg.Next)<>0 then
      sl.Add(prefix+' end');
    sl.Add(prefix+'else');

  end;

begin
  sl:=TStringList.Create;
  slV:=TStringList.Create;
  slT:=TStringList.Create;
  slN:=tstringList.Create;
  try
    Writeln('Generating tokenizer using '+FilePath);
    sl.LoadFromFile(FilePath);

    sli:=0;
    slV.Delimiter:=#9;
    slV.QuoteChar:=#0;
    slV.StrictDelimiter:=true;
    slT.Delimiter:=':';
    slT.StrictDelimiter:=true;

    slN.Sorted:=true;
    slN.Duplicates:=dupIgnore;

    //skip header
    while (sli<sl.Count) and (sl[sli]<>'') do inc(sli);
    inc(sli);//empty line

    while sli<sl.Count do
     begin
      slV.DelimitedText:=sl[sli];
      inc(sli);
      if slV.Count<3 then
        Fail('Insufficient values on line')
      else
       begin

        t1:=slV[0];
        t2:=slV[1];
        if (t1='') and (t2='') then
          //ignore
        else
         begin
          slT.DelimitedText:=slV[2];
          if slT.Count<1 then
            Fail('Undefined token name')
          else
           begin

            if t2='' then
             begin

              g:=@gRoot;
              tx:=1;

              while tx<=Length(t1) do
               begin
                c1:=uc(t1[tx]);
                inc(tx);
                gx:=0;
                gl:=Length(g.Next);
                while (gx<gl) and not(
                  g.Next[gx].InRange(c1)
                ) do inc(gx);
                if gx<gl then
                 begin
                  g:=@g.Next[gx];
                  //assert g.Start=c1 and g.Stop=#0
                  if g.Stop<>#0 then
                    Fail('Combining singe values and ranges not supported');
                 end
                else
                 begin
                  SetLength(g.Next,gl+1);
                  g:=@g.Next[gl];
                  g.Start:=c1;
                  g.Stop:=#0;
                 end;
               end;

             end
            else
             begin
              if not((Length(t1)=1) and (Length(t2)=1)) then
                Fail('Range only allowed on single characters');

              c1:=uc(t1[1]);
              c2:=uc(t2[1]);

              gx:=0;
              gl:=Length(gRoot.Next);
              while (gx<gl) and not(
                gRoot.Next[gx].InRange(c1) or
                gRoot.Next[gx].InRange(c2)
              ) do inc(gx);
              if (gx<gl) then
                Fail('Overlapping ranges not allowed');
              SetLength(gRoot.Next,gl+1);
              g:=@gRoot.Next[gl];
              g.Start:=c1;
              g.Stop:=c2;

             end;


            g.Token:=slT[0];
            if slT.Count>1 then g.Task:=slT[1] else g.Task:='';

            if g.Token<>'' then slN.Add(g.Token);

            //TODO: g.Weight?

           end;

          //if slV[3]<>'' then //remark
         end;

       end;
     end;

  finally
    slT.Free;
    slV.Free;
    sl.Free;
  end;

  //TODO: sort
  //TODO: (sort by weights?)

  sl:=tstringList.Create;
  try
    {
    sl.Add('unit stratoTokenizer;');
    sl.Add('');
    sl.Add('interface');
    sl.Add('');
    sl.Add('implementation');
    sl.Add('');
    sl.Add('...');
    sl.Add('');
    }

    sl.Add('// ATTENTION: generated by "strato -tokgen"');
    //DateTimeToStr?
    sl.Add('');

    for gx:=0 to Length(gRoot.Next)-1 do
     begin
      g:=@gRoot.Next[gx];

      if g.Stop=#0 then
        sl.Add('  if n='+ud(g.Start)+' then')
      else
        sl.Add('  if (n>='+ud(g.Start)+') and (n<='+ud(g.Stop)+') then');

      gSub(g,1,'  ');

     end;

    sl.Add('  doToken(st_Unknown,1);');

    sl.Add('');
    {
    sl.Add('end.');
    sl.SaveToFile(ChangeFileExt(FilePath,'.pas'));
    }
    sl.SaveToFile(ChangeFileExt(FilePath,'.inc'));

    //token type
    sl.Clear;
    sl.Add('type');
    sl.Add('  TStratoToken=(');
    sl.Add('    st_Unknown,');
    for sli:=0 to slN.Count-1 do
     begin
      t1:=slN[sli];
      if sli<slN.Count-1 then t2:=',' else t2:='';
      sl.Add('    st'+t1+t2);
     end;
    sl.Add('  );');
    sl.SaveToFile(ChangeFileExt(FilePath,'_T.inc'));

    //token names const array
    sl.Clear;
    sl.Add('const');
    sl.Add('  StratoTokenNames:array[TStratoToken] of UTF8String=(');
      sl.Add('    '''',//Unknown');
    for sli:=0 to slN.Count-1 do
     begin
      t1:=slN[sli];
      if sli<slN.Count-1 then t2:=',' else t2:='';
      sl.Add('    '''+t1+''''+t2);
     end;
    sl.Add('  );');
    sl.SaveToFile(ChangeFileExt(FilePath,'_N.inc'));

  finally
    sl.Free;
    slN.Free;
  end;

end;

{ TGlyph }

function TGlyph.InRange(c: UTF8Char): boolean;
begin
  if Stop=#0 then
    Result:=Start=c
  else
    Result:=(c>=Start) and (c<=Stop);
end;

end.
