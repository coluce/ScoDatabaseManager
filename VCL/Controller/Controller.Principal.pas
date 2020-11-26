unit Controller.Principal;

interface

uses
  View.Principal, Model.Interfaces;

type
  TControllerPrincipal = class
  private
    FView: TViewPrincipal;
    FModelServer: IModelTable;
  public
    constructor Create(const AView: TViewPrincipal);
    procedure FillList;
    procedure NewServer;
  end;

var
  ControllerPrincipal: TControllerPrincipal;

implementation

uses
  Model.Factory, Vcl.Dialogs, System.SysUtils, Vcl.ComCtrls;

{ TControllerPrincipal }

constructor TControllerPrincipal.Create(const AView: TViewPrincipal);
begin
  FView := AView;
  FModelServer := TModelTablefactory.New('TSERVER');
end;

procedure TControllerPrincipal.NewServer;
var
  vName: string;
  vIP: string;
begin
  vName := InputBox('Novo Server', 'Nome', 'Localhost');
  vIP := InputBox('Novo Server', 'IP', '127.0.0.1');
  FModelServer.DataSet.Append;
  FModelServer.DataSet.FieldByName('ID').AsString := TGUID.NewGuid.ToString;
  FModelServer.DataSet.FieldByName('NAME').AsString := vName;
  FModelServer.DataSet.FieldByName('IP').AsString := vIP;
  FModelServer.DataSet.Post;
  FModelServer.ApplyUpdates;
end;

procedure TControllerPrincipal.FillList;
var
  vItem: TTreeNode;
begin
  FView.TreeView1.Items.Clear;
  FModelServer.DataSet.Close;
  FModelServer.DataSet.Open;

  while not FModelServer.DataSet.Eof do
  begin
    vItem := FView.TreeView1.Items.Add(nil, FModelServer.DataSet.FieldByName('IP').AsString + ' | ' + FModelServer.DataSet.FieldByName('NAME').AsString);
//    vItem.ID := FModelServer.DataSet.FieldByName('ID').AsString;
//    vItem.Name := FModelServer.DataSet.FieldByName('Name').AsString;
//    vItem.IP := FModelServer.DataSet.FieldByName('IP').AsString;
    FModelServer.DataSet.Next;
  end;

end;

end.
