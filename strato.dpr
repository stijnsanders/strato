program strato;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  stratoDecl in 'stratoDecl.pas',
  stratoTokenizer in 'stratoTokenizer.pas',
  stratoSphere in 'stratoSphere.pas',
  stratoTools in 'stratoTools.pas',
  stratoSource in 'stratoSource.pas',
  stratoPred in 'stratoPred.pas',
  stratoLit in 'stratoLit.pas',
  stratoParse in 'stratoParse.pas',
  stratoFn in 'stratoFn.pas',
  stratoLogic in 'stratoLogic.pas',
  stratoDebug in 'stratoDebug.pas',
  stratoExec in 'stratoExec.pas',
  stratoDebugView in 'stratoDebugView.pas' {frmDebugView};

var
  s:TStratoSource;
  p:TStratoParser;
  m:TStratoMachine;
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
  DoRun,DoDebug,DoRunTime,DoInlineErrors,DoDumpHRF,DoDumpHRR:boolean;
  DoDump,DoDumpHR,LastFN:string;

begin
  if ParamCount=0 then
   begin
    Writeln('strato interpreter');
    //TODO version, URL
    Writeln('Usage:');
    Writeln('  strato [switches] [file(s)]');
    Writeln('Switches:');
    Writeln('  -C             compile only, don''t run');
    Writeln('  -U <filename>  binary dump sphere');
    Writeln('  -H <filename>  human-readable dump sphere');
    Writeln('  -F             human-readable dump sphere: include dictionary nodes');
    Writeln('  -R             human-readable dump sphere: reverse files order');
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
      DoRunTime:=true;
      DoInlineErrors:=false;
      DoDump:='';
      DoDumpHR:='';
      DoDumpHRF:=false;
      DoDumpHRR:=true;
      LastFN:='';

      ec:=0;
      //settings
      ReadSettings(ChangeFileExt(ParamStr(0),'.ini'));

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
                DoRunTime:=false;
                DoRun:=false;
               end;
              'C':DoRun:=false;
              'D':DoDebug:=true;
              'U':DoDump:=xNext;
              'H':DoDumpHR:=xNext;
              'F':DoDumpHRF:=true;
              'R':DoDumpHRR:=false;
              'I':
               begin
                LoadFromFile(xNext);//TODO: merge?
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
try


          //run-time
          if DoRunTime then
           begin
            LastFN:=ResolveKnownPath('$compiler\Strato.xs');
            s:=TStratoSource.Create;
            try
              s.LoadFromFile(LastFN);
              p:=TStratoParser.Create(s,DoInlineErrors);
              try

try
                p.Parse;//calls DeclareIntrinsicTypes
except
  on e:Exception do
   begin
    if DoInlineErrors then s.Error('### ['+E.ClassName+']: '+E.Message);
    //raise;
    Writeln(ErrOutput,'"'+LastFN+'"'+E.ClassName+': '+E.Message);
    ExitCode:=1;
    inc(ec);
   end;
end;
              finally
                p.Free;
              end;
              inc(ec,s.ErrorCount);
            finally
              s.Free;
            end;
            //TODO: objects with reference counting
            //TODO: strings with reference counting, copy-on-write, etc
            DoRunTime:=false;
           end;

if ec=0 then begin
          //do file
          s:=TStratoSource.Create;
          try
            s.LoadFromFile(x);
            LastFN:=x;//used by -X above
            if not s.IsNext([st_EOF]) then
             begin
              p:=TStratoParser.Create(s,DoInlineErrors);
              try

try
                p.Parse;
except
  on e:Exception do
   begin
    if DoInlineErrors then s.Error('### ['+E.ClassName+']: '+E.Message);
    //raise;
    Writeln(ErrOutput,'"'+LastFN+'"'+E.ClassName+': '+E.Message);
    ExitCode:=1;
    inc(ec);
   end;
end;

              finally
                p.Free;
              end;
             end;
            inc(ec,s.ErrorCount);
          finally
            s.Free;
          end;
end;          

except
  on e:Exception do
   begin
    Writeln(ErrOutput,'"'+LastFN+'"'+E.ClassName+': '+E.Message);
    ExitCode:=1;
    inc(ec);
   end;
end;

         end;
       end;

      //dump
      if DoDump<>'' then SaveToFile(DoDump);
      if DoDumpHR<>'' then
        StratoDumpSphereData(DoDumpHR,DoDumpHRF,DoDumpHRR);

      //run
      if DoRun then
        if ec=0 then
         begin

          m:=TStratoMachine.Create(DoDebug);
          try
            m.Run;
          finally
            m.Free;
          end;

         end
        else
         begin
          Writeln(ErrOutput,IntToStr(ec)+' error(s)');
          ExitCode:=1;
         end;

    except
      on E: Exception do
       begin
        Writeln(ErrOutput,E.ClassName, ': ', E.Message);
        ExitCode:=1;
       end;
    end;
end.
