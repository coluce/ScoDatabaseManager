unit Model.Imp.Updater;

interface

uses
  Model.Interfaces;

type
  TModelStrcutureUpdater = class(TInterfacedObject, IModelStructureUpdater)
  private
    FConnection: IModelConnection;
    FScripts: TArray<IModelScript>;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: boolean;
    procedure AddField(const ATableName: string; const AFieldName: string; const AFieldType: string;
      AFieldSize: integer = 0);
    procedure AddScript(const AScript: IModelScript);
  end;

implementation

{ TModelStrcutureUpdater }

uses Model.Factory, System.SysUtils;

procedure TModelStrcutureUpdater.AddField(const ATableName: string; const AFieldName: string; const AFieldType: string;
      AFieldSize: integer = 0);
var
  vScript: IModelScript;
begin
//  vScript := TModelFactory.Script(ATableName, AFieldName, AFieldType);
//  Self.AddScript(vScript);
// o parametro é const, portanto, o objeto será destruido antes de ser usado
end;

procedure TModelStrcutureUpdater.AddScript(const AScript: IModelScript);
begin

  if not Assigned(AScript) then
    Exit;

  SetLength(FScripts, Length(FScripts) + 1);
  FScripts[Length(FScripts) - 1] := AScript;

end;

constructor TModelStrcutureUpdater.Create;
begin
  FConnection := TModelFactory.MainConnection;
  FConnection.Open;
  SetLength(FScripts, 0);
end;

destructor TModelStrcutureUpdater.Destroy;
begin
  SetLength(FScripts, 0);
  inherited;
end;

function TModelStrcutureUpdater.Execute: boolean;
var
  vScript: IModelScript;
begin
  try
    for vScript in FScripts do
    begin
      FConnection.ExecScript(vScript);
    end;
    Result := True;
  except
    on E: exception do
    begin
      raise;
    end;
  end;
end;

end.
