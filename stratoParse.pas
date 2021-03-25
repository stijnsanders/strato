unit stratoParse;

interface

uses stratoParseLogic, stratoDecl, stratoSphere;

type
  TStratoParser=class(TStratoParserLogic)
  public
    procedure Parse; override;
  end;

implementation

uses SysUtils, stratoTokenizer, stratoPred;

const
  stackGrowSize=$100;

{ TStratoParser }

procedure TStratoParser.Parse;
begin
  inherited;
  SyntaxClass:=scDeclarative;
  while not Source.IsNext([st_EOF]) do
    case SyntaxClass of
      scDeclarative:
        ParseDeclaration;
      scDeclarative_Record:
        ParseRecord;
      scImperative:
        ParseLogic;
      else
        raise Exception.Create('Syntax Class not supported');
    end;

  //TODO: flag sourcefile done
end;

initialization

  //function(Source:TStratoSource;DoInlineErrors:boolean):TStratoSphere;
end.
