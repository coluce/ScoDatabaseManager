unit Model.Updater;

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
    procedure AddField(const AFieldName: string; const AFieldType: string; AFieldSize: integer = 0);
    procedure AddScript(const AScript: IModelScript);
  end;

implementation

{ TModelStrcutureUpdater }

uses Model.Factory;

procedure TModelStrcutureUpdater.AddField(const AFieldName, AFieldType: string; AFieldSize: integer);
begin

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
  FConnection := TModelConnectionFactory.New;
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
  for vScript in FScripts do
  begin
    FConnection.ExecScript(vScript);
  end;
end;

end.
