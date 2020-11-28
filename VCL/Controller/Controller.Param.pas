unit Controller.Param;

interface

uses
  Controller.Interfaces, Model.Interfaces;

type
  TControllerParam = class(TInterfacedObject, IControllerParam)
  private
    FModelParam: IModelTable;
  public
    constructor Create;
    function GetParam(const ASession, AKey, ADefault: string): string;
    procedure SetParam(const ASession, AKey, AValue: string);
  end;

implementation

uses
  Model.Factory, Model.Types, System.SysUtils;

{ TControllerParam }

constructor TControllerParam.Create;
begin
  FModelParam := TModelTableFactory.New('TPARAM');
end;

function TControllerParam.GetParam(const ASession, AKey, ADefault: string): string;
var
  vParams: TArray<TTableParam>;
begin
  Result := ADefault;

  SetLength(vParams, 2);
  vParams[0] := TTableParam.Create('SESSION', ASession);
  vParams[1] := TTableParam.Create('KEY', AKey);
  FModelParam.Find(vParams);

  if not FModelParam.DataSet.IsEmpty then
    Result := FModelParam.DataSet.FieldByName('VALUE').AsString;
end;

procedure TControllerParam.SetParam(const ASession, AKey, AValue: string);
var
  vParams: TArray<TTableParam>;
begin

  SetLength(vParams, 2);
  vParams[0] := TTableParam.Create('SESSION', ASession);
  vParams[1] := TTableParam.Create('KEY', AKey);
  FModelParam.Find(vParams);

  if FModelParam.DataSet.IsEmpty then
  begin
    FModelParam.DataSet.Append;
    FModelParam.DataSet.FieldByName('SESSION').AsString := AKey;
    FModelParam.DataSet.FieldByName('KEY').AsString := AKey;
  end
  else
  begin
    FModelParam.DataSet.Edit;
  end;
  FModelParam.DataSet.FieldByName('VALUE').AsString := AValue;
  FModelParam.DataSet.Post;
  FModelParam.ApplyUpdates;
end;

end.
