unit Controller.DataBase;

interface

uses
  View.DataBase, Model.Types, Controller.Interfaces, Model.Interfaces;

type

  TControllerDataBase = class(TInterfacedObject, IControllerDataBase)
  private
    FView: TViewDataBase;
    FDataBase: TDataBase;
    FConnection: IModelConnection;

    function GetConnected: boolean;
    procedure SetConnected(const Value: boolean);

    procedure UpdateStatusBar;

  public
    constructor Create(const ADataBase: TDataBase);
    destructor Destroy; override;

    procedure Show;
    procedure ShowModal;

    procedure FillTableNames;

  published
    property Connected: boolean read GetConnected write SetConnected;
  end;

implementation

uses
  System.Classes, Model.Factory;

{ TControllerDataBase }

constructor TControllerDataBase.Create(const ADataBase: TDataBase);
begin
  FDataBase := ADataBase;
  FView := TViewDatabase.Create(Self);
  FConnection := TModelConnectionFactory.New;

end;

destructor TControllerDataBase.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerDataBase.FillTableNames;
var
  vList: TStringList;
  vTable: string;
begin
  vList := TStringList.Create;
  try
    FConnection.GetConnection.GetTableNames('','','', vList);
    for vTable in vList do
    begin
      FView.TreeViewTabelas.Items.Add(nil, vTable);
    end;

  finally
    vList.Free;
  end;
end;

function TControllerDataBase.GetConnected: boolean;
begin
  Result := FConnection.Active;
  UpdateStatusbar;
end;

procedure TControllerDataBase.SetConnected(const Value: boolean);
begin
  FConnection.Active := Value;
  UpdateStatusbar;
end;

procedure TControllerDataBase.Show;
begin
  FView.Show;
  UpdateStatusBar;
end;

procedure TControllerDataBase.ShowModal;
begin
  FView.ShowModal;
  UpdateStatusBar;
end;

procedure TControllerDataBase.UpdateStatusBar;
begin
  FView.StatusBar1.Panels[1].Text := FDataBase.Server.IP + ':' + FDataBase.Path;
  if FConnection.Active then
  begin
    FView.StatusBar1.Panels[0].Text := 'Conectado';
  end
  else
  begin
    FView.StatusBar1.Panels[0].Text := 'Desconectado';
  end;
end;

end.
