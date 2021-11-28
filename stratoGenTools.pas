unit stratoGenTools;

interface

uses stratoDecl, stratoSphere;

function rs(p:xNode):AnsiString;

implementation

uses SysUtils;

function rs(p:xNode):AnsiString;
begin
  if p.IsNone then Result:='nil' else
    Result:=AnsiString(Format('x%di%d',
      [SphereIndex(p.sphere)
      ,p.index
      ]));
end;

end.
