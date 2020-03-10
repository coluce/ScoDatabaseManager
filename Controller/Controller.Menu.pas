unit Controller.Menu;

interface

uses
  Model.Interfaces,
  View.Default,
  FMX.Forms,
  FMX.ListBox,
  FMX.StdCtrls,
  FMX.Objects;

type

  TListBoxItem = class(FMX.ListBox.TListBoxItem)
  private
    FBtnOpt: TSpeedButton;
    FBtnPlay: TSpeedButton;
    FBtnSaveToDisk: TSpeedButton;
    FRecAtual: TRectangle;
    FConfig: IDataBaseConfig;
    FIsActual: boolean;
    procedure SetConfig(const Value: IDataBaseConfig);
    procedure SetIsActual(const Value: boolean);
    procedure DoOnQueryClick(Sender: TObject);
    procedure DoOnOptionsClick(Sender: TObject);
    procedure DoOnSaveToDiskClick(Sender: TObject);
  public
    procedure CreateButtons;
  published
    property Config: IDataBaseConfig read FConfig write SetConfig;
    property IsActual: boolean read FIsActual write SetIsActual;
  end;

  TControllerMenu = class
  private
    FOwner: TFormPadrao;
    FListBox: TListBox;
    class var FSelf: TControllerMenu;
  public
    constructor Create(const AOwner: TFormPadrao);
    procedure Refresh(const AActualID: string);
    class procedure Start(const AOwner: TFormPadrao);
    class procedure Stop;
  published
    class function Instance: TControllerMenu;
  end;

implementation

uses
  Controller.Principal,
  Model.Factory,
  FMX.Types,
  FMX.Graphics,
  System.SysUtils,
  System.UITypes;

{ TListBoxItem }

procedure TListBoxItem.CreateButtons;
begin
  FBtnPlay := TSpeedButton.Create(Self);
  FBtnPlay.Parent := Self;
  FBtnPlay.Align := TAlignLayout.MostRight;
  FBtnPlay.StyleLookup := 'playtoolbutton';
  FBtnPlay.OnClick := DoOnQueryClick;
  FBtnPlay.Hint := 'Executar querys...';
  FBtnPlay.ShowHint := True;
  FBtnPlay.Margins.Top := 3;
  FBtnPlay.Margins.Bottom := 3;
  FBtnPlay.Margins.Right := 3;
  FBtnPlay.Margins.Left := 3;
  FBtnPlay.Width := 40;

  FBtnOpt := TSpeedButton.Create(Self);
  FBtnOpt.Parent := Self;
  FBtnOpt.Align := TAlignLayout.Right;
  FBtnOpt.StyleLookup := 'optionstoolbutton';
  FBtnOpt.OnClick := DoOnOptionsClick;
  FBtnOpt.Hint := 'Configurações';
  FBtnOpt.ShowHint := True;
  FBtnOpt.Margins.Top := 3;
  FBtnOpt.Margins.Bottom := 3;
  FBtnOpt.Margins.Right := 3;
  FBtnOpt.Margins.Left := 3;
  FBtnOpt.Width := 40;

  FBtnSaveToDisk := TSpeedButton.Create(Self);
  FBtnSaveToDisk.Parent := Self;
  FBtnSaveToDisk.Align := TAlignLayout.Right;
  FBtnSaveToDisk.StyleLookup := 'escapetoolbutton';
  FBtnSaveToDisk.OnClick := DoOnSaveToDiskClick;
  FBtnSaveToDisk.Hint := 'Definir como alias em uso...';
  FBtnSaveToDisk.ShowHint := True;
  FBtnSaveToDisk.Margins.Top := 3;
  FBtnSaveToDisk.Margins.Bottom := 3;
  FBtnSaveToDisk.Margins.Right := 3;
  FBtnSaveToDisk.Margins.Left := 3;
  FBtnSaveToDisk.Width := 40;

end;

procedure TListBoxItem.DoOnOptionsClick(Sender: TObject);
begin
  TControllerPrincipal.Instance.ShowConfig(FConfig);
end;

procedure TListBoxItem.DoOnQueryClick(Sender: TObject);
begin
  TControllerPrincipal.Instance.ShowDataBase(FConfig);
end;

procedure TListBoxItem.DoOnSaveToDiskClick(Sender: TObject);
begin
  TControllerPrincipal.Instance.SaveToDisk(FConfig);
end;

procedure TListBoxItem.SetConfig(const Value: IDataBaseConfig);
begin
  FConfig := Value;
end;

procedure TListBoxItem.SetIsActual(const Value: boolean);
begin
  FIsActual := Value;

  if Assigned(FRecAtual) then
  begin
    Self.RemoveObject(FRecAtual);
    FreeAndNil(FRecAtual);
  end;

  if FIsActual then
  begin
    FRecAtual := TRectangle.Create(Self);
    FRecAtual.Parent := Self;
    FRecAtual.Stored := False;
    FRecAtual.HitTest := False;
    FRecAtual.Stroke.Kind := TBrushKind.None;
    FRecAtual.Fill.Color := TAlphaColorRec.Mediumaquamarine;
    FRecAtual.Margins.Top := 3;
    FRecAtual.Margins.Bottom := 3;
    FRecAtual.Width := 3;
    FRecAtual.Align := TAlignLayout.MostLeft;
  end;
end;

{ TControllerMenu }

constructor TControllerMenu.Create(const AOwner: TFormPadrao);
begin
  inherited Create;
  FOwner := AOwner;

  FListBox := TListBox.Create(FOwner.layPrincipal);
  FOwner.layPrincipal.AddObject(FListBox);
  FListBox.Align := TAlignLayout.Client;

  FListBox.Margins.Left := 8;
  FListBox.Margins.Right := 8;
  FListBox.Margins.Top := 8;
  FListBox.Margins.Bottom := 8;
  FListBox.DefaultItemStyles.ItemStyle := 'listboxitembottomdetail';
  FListBox.DisableFocusEffect := True;

end;

class function TControllerMenu.Instance: TControllerMenu;
begin
  Result := TControllerMenu.FSelf;
end;

procedure TControllerMenu.Refresh(const AActualID: string);
var
  vConf: IDataBaseConfig;
  vDao: IDao<IDataBaseConfig>;
  vItem: TListBoxItem;
  vLista: TArray<IDataBaseConfig>;
begin
  vDao := TDataBaseConfigFactory.Dao;
  vLista := vDao.Get;

  FListBox.Clear;
  FListBox.BeginUpdate;
  try
    for vConf in vLista do
    begin
      vItem := TListBoxItem.Create(FListBox);
      vItem.Parent := FListBox;
      vItem.Config := vConf;

      vItem.Height := 55;
      vItem.ItemData.Text := vConf.Description;
      vItem.ItemData.Detail := vConf.ServerName + ' : ' + vConf.DataBase;

      vItem.IsActual := not(AActualID.Trim.IsEmpty) and vConf.ID.Equals(AActualID);
      vItem.CreateButtons;

      FListBox.AddObject(vItem);
    end;
    FListBox.ItemIndex := 0;
  finally
    FListBox.EndUpdate;
  end;
end;

class procedure TControllerMenu.Start(const AOwner: TFormPadrao);
begin
  FSelf := TControllerMenu.Create(AOwner);
end;

class procedure TControllerMenu.Stop;
begin
  FSelf.Free;
end;

end.
