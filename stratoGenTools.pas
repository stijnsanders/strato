unit stratoGenTools;

interface

uses stratoDecl, stratoSphere;

function rs(p:rItem):AnsiString;

implementation

uses SysUtils;

function rs(p:rItem):AnsiString;
begin
  Result:=AnsiString(Format('x%di%d',
    [p.x div StratoSphereBlockBase
    ,p.x mod StratoSphereBlockBase
    ]));
end;

end.
