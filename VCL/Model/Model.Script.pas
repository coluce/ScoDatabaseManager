unit Model.Script;

interface

uses
  Model.Interfaces, System.Classes;

type
  TModelScript = class(TInterfacedObject, IModelScript)
  private
    FID: string;
    FSQL: TStringList;

    function GetID: string;
    procedure SetID(const Value: string);

    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
  public
    constructor Create(const ASQL: string);
    destructor Destroy; override;

    property ID: string read GetID write SetID;
    property SQL: TStrings read GetSQL write SetSQL;
  end;


implementation

uses
  System.SysUtils;

{ TModelScript }

constructor TModelScript.Create(const ASQL: string);
begin
  FID := TGuid.NewGuid.ToString;
  FSQL := TStringList.Create;
  FSQL.Add(ASQL);
end;

destructor TModelScript.Destroy;
begin
  FSQL.Free;
  inherited;
end;

function TModelScript.GetID: string;
begin
  Result := FID;
end;

function TModelScript.GetSQL: TStrings;
begin
  Result := FSQL;
end;

procedure TModelScript.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TModelScript.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

end.
