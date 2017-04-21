program strato;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  stratoTokenizer in 'stratoTokenizer.pas',
  stratoSphere in 'stratoSphere.pas',
  stratoDict in 'stratoDict.pas',
  stratoDecl in 'stratoDecl.pas',
  stratoSource in 'stratoSource.pas',
  stratoParse in 'stratoParse.pas',
  stratoDebug in 'stratoDebug.pas',
  stratoRunTime in 'stratoRunTime.pas',
  stratoLogic in 'stratoLogic.pas',
  stratoFn in 'stratoFn.pas',
  stratoExec in 'stratoExec.pas',
  stratoDebugView in 'stratoDebugView.pas' {frmDebugView};

var
  n:TStratoStore;
  s:TStratoSource;
  m:TStratoMachine;
  p:TStratoSphere;
  i,j,k,l,ec:integer;
  x:string;

  function xNext:string;
  begin
    if j=k then
      if i>l then
        Result:=''
      else
       begin
        Result:=ParamStr(i);
        inc(i);
       end
    else
     begin
      Result:=Copy(x,j+1,k-j);
      j:=k;
     end;
  end;

var
  DoRun,DoDebug,DoTokenize,DoRunTime,DoInlineErrors:boolean;
  DoDump,DoDumpHR,LastFN:string;

begin
  if ParamCount=0 then
   begin
    Writeln('strato interpreter');
    //TODO version, URL
    Writeln('Usage:');
    Writeln('  strato [switches] [file(s)]');
    Writeln('Switches:');
    Writeln('  -T             tokenize file(s)');
    Writeln('  -C             compile only, don''t run');
    Writeln('  -U <filename>  binary dump sphere');
    Writeln('  -H <filename>  human-readable dump sphere');
    Writeln('  -I <filename>  import sphere');
    Writeln('  -E             inline errors into sphere as binary entries');
    Writeln('  -D             enable debug viewer');
    Writeln('  -X             shorthand for -EUHD');

    //TODO: export LLVMIR
    //TODO: params from file
    //TODO: define path(s)
    //TODO: arguments to program

   end
  else
    try
      //defaults
      DoRun:=true;
      DoDebug:=false;
      DoTokenize:=false;
      DoRunTime:=true;
      DoInlineErrors:=false;
      DoDump:='';
      DoDumpHR:='';
      LastFN:='';

      ec:=0;
      n:=TStratoStore.Create;
      try
        //settings
        n.ReadSettings(ChangeFileExt(ParamStr(0),'.ini'));

        //arguments
        l:=ParamCount;
        i:=1;
        while i<=l do
         begin
          x:=ParamStr(i);//assert x<>''
          inc(i);
          k:=Length(x);
          if (k>1) and (x[1]='-') then
           begin
            j:=2;
            while j<=k do
             begin
              case x[j] of
                'T':
                 begin
                  DoTokenize:=true;
                  DoRunTime:=false;
                  DoRun:=false;
                 end;
                'C':DoRun:=false;
                'D':DoDebug:=true;
                'U':DoDump:=xNext;
                'H':DoDumpHR:=xNext;
                'I':
                 begin
                  n.LoadFromFile(xNext);//TODO: merge?
                  DoRunTime:=false;
                 end;
                'E':DoInlineErrors:=true;
                'X':
                 begin
                  if LastFN='' then
                    Writeln('write a filename first, then -X');
                  DoDump:=LastFN+'u';
                  DoDumpHR:=LastFN+'v';
                  DoInlineErrors:=true;
                  DoDebug:=true;
                 end;
                else
                  Writeln('unknown switch "'+x[j]+'"');
              end;
              inc(j);
             end;
           end
          else
           begin

            //run-time
            if DoRunTime then
             begin
              DefaultTypes(n,DoInlineErrors,ec);
              DoRunTime:=false;
             end;

            //do file
            s:=TStratoSource.Create;
            s.LoadFromFile(x);
            LastFN:=x;//used by -X above
            if DoTokenize then
              StratoDumpTokens(s.Tokens)
            else
             begin
              StratoParseSource(n,s,DoInlineErrors);
              inc(ec,s.ErrorCount);
             end;
            //TODO: free s?
            
           end;
         end;

        //dump
        if DoDump<>'' then n.SaveToFile(DoDump);
        if DoDumpHR<>'' then StratoDumpSphereData(n,DoDumpHR);

        //run
        if DoRun then
          if ec=0 then
           begin

            p:=TStratoSphere.Create(n,nil);
            m:=TStratoMachine.Create(DoDebug);
            try
              m.Run(p);
            finally
              m.Free;
              p.Free;
            end;

           end
          else
           begin
            Writeln(ErrOutput,IntToStr(ec)+' error(s)');
            ExitCode:=1;
           end;

      finally
        n.Free;
      end;
    except
      on E: Exception do
       begin
        Writeln(ErrOutput,E.ClassName, ': ', E.Message);
        ExitCode:=1;
       end;
    end;
end.
