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
  System.Classes, Model.Factory, Vcl.ComCtrls;

{ TControllerDataBase }

constructor TControllerDataBase.Create(const ADataBase: TDataBase);
begin
  FDataBase := ADataBase;
  FView := TViewDatabase.Create(Self);
  FConnection := TModelConnectionFactory.Firebird(FDataBase);

end;

destructor TControllerDataBase.Destroy;
begin
  FView.Free;
  inherited;
end;

procedure TControllerDataBase.FillTableNames;

  procedure FillTriggers(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Triggers');
    FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, 'Não implementado ainda');
  end;

  procedure FillContraints(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Constraints');
    FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, 'Não implementado ainda');
  end;

  procedure FillFields(const ANode: TTreeNode; const ATableName: string);
  var
    vNodeTitulo: TTreeNode;
    vFields: TStrings;
    vField: string;
  begin
    vNodeTitulo := FView.TreeViewTabelas.Items.AddChild(ANode, 'Fields');
    vFields := TStringList.Create;
    try
      FConnection.GetConnection.GetFieldNames('','', ATableName, '', vFields);
      for vField in vFields do
      begin
        FView.TreeViewTabelas.Items.AddChild(vNodeTitulo, vField);
      end;
    finally
      vFields.Free;
    end;
  end;

var
  vList: TStringList;
  vTable: string;
  vNode: TTreeNode;
begin
  vList := TStringList.Create;
  try
    FConnection.GetConnection.GetTableNames('','','', vList);

    FView.SynSQLSyn1.TableNames := vList;

    FView.TreeViewTabelas.Items.BeginUpdate;
    try
      for vTable in vList do
      begin
        vNode := FView.TreeViewTabelas.Items.Add(nil, vTable);
        FillFields(vNode, vTable);
        FillTriggers(vNode, vTable);
        FillContraints(vNode, vTable);
      end;
    finally
      FView.TreeViewTabelas.Items.EndUpdate;
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

  if FConnection.Active then
  begin
    FView.FDQuery1.Connection := FConnection.GetConnection;
  end
  else
  begin
    FView.FDQuery1.Close;
    FView.FDQuery1.Connection := nil;
  end;

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
