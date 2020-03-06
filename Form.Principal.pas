unit Form.Principal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FireDAC.Stan.StorageXML,
  FMX.Objects, FMX.TabControl, Form.Query, Model.Config, Form.FileLayout,
  Model.FileLayout;

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

  TFormPrincipal = class(TForm)
    lstConfigs: TListBox;
    tlb1: TToolBar;
    btnAdd: TSpeedButton;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    sty1: TStyleBook;
    ListBoxItem1: TListBoxItem;
    btnOptions: TSpeedButton;
    dlg1: TSaveDialog;
    Circle1: TCircle;
    btnRefresh: TSpeedButton;
    btn1: TSpeedButton;
    tbcPrincipal: TTabControl;
    tabMenu: TTabItem;
    tabQuery: TTabItem;
    tabFileLayout: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnQueryClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
    { Private declarations }
    FID: string;
    FLista: TArray<IDataBaseConfig>;
    FFormQuery: TFormQuery;
    FFormFileLayout: TFormFileLayout;
    FFileLayout: IFileLayout;
    procedure ListBoxRefresh;
    procedure SaveToDisk(AConfig: IDataBaseConfig);
    procedure ShowConfig(AConfig: IDataBaseConfig; const AIsActual: boolean = False);
    procedure ShowQuery(AConfig: IDataBaseConfig);
    procedure ShowFileLayout;
  public
    { Public declarations }
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  Form.Config;

{$R *.fmx}

procedure TFormPrincipal.btnOptionsClick(Sender: TObject);
begin
//  ShowConfig;
end;

procedure TFormPrincipal.btnQueryClick(Sender: TObject);
begin
  ShowQuery(FLista[lstConfigs.ItemIndex]);
end;

procedure TFormPrincipal.btnRefreshClick(Sender: TObject);
begin
  ListBoxRefresh;
end;

procedure TFormPrincipal.btnAddClick(Sender: TObject);
begin
  ShowConfig(nil);
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
  function GetIDInUse: string;
  var
    vFile: TStrings;
  const
    FILE_NAME: string = 'E:\dese.git\Executaveis\FormulaCerta\alterdb.ini';
  begin
    Result := EmptyStr;
    if FileExists(FILE_NAME) then
    begin
      vFile := TStringList.Create;
      try
        vFile.LoadFromFile(FILE_NAME);
        Result := vFile[0];
      finally
        vFile.DisposeOf;
      end;
    end;
  end;
begin
  FID := GetIDInUse;
  ListBoxRefresh;
  Self.WindowState := TWindowState.wsMaximized;
end;

procedure TFormPrincipal.ListBoxRefresh;
var
  vConf: IDataBaseConfig;
  vDao: IDao<IDataBaseConfig>;
  vItem: TListBoxItem;
begin
  vDao := TDataBaseConfigFactory.Dao;
  FLista := vDao.Get;

  lstConfigs.Clear;
  lstConfigs.BeginUpdate;
  try
    for vConf in FLista do
    begin
      vItem := TListBoxItem.Create(lstConfigs);
      vItem.Parent := lstConfigs;
      vItem.Config := vConf;

      vItem.Height := 55;
      vItem.ItemData.Text := vConf.Description;
      vItem.ItemData.Detail := vConf.ServerName + ' : ' + vConf.DataBase;

      vItem.IsActual := not(FID.Trim.IsEmpty) and vConf.ID.Equals(FID);
      vItem.CreateButtons;

      lstConfigs.AddObject(vItem);
    end;
    lstConfigs.ItemIndex := 0;
  finally
    lstConfigs.EndUpdate;
  end;
end;

procedure TFormPrincipal.SaveToDisk(AConfig: IDataBaseConfig);
var
  vFile: TStrings;
begin
  if MessageDlg('Definir como atual?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbYes) = mrYes then
  //if dlg1.Execute then
  begin
    FID := AConfig.ID;
    vFile := TStringList.Create;
    try
      vFile.Add(FID);
      vFile.Add('[SERVIDOR]');
      vFile.Add('NOMESERVIDOR=' + AConfig.ServerName);
      vFile.Add('SERVER=192.168.1.11');
      vFile.Add('CONECCAO=TCP');
      vFile.Add('');
      vFile.Add('[SERVIDORPAFECF]');
      vFile.Add('NOMESERVIDOR=Osamar');
      vFile.Add('CONECCAO=TCP');
      vFile.Add('');
      vFile.Add('[PATH]');
      vFile.Add('PATHPAR=' + AConfig.DataBase);
      vFile.Add('');
      vFile.Add('[HARDLOCK]');
      vFile.Add('HARDLOCK=SAFENET');
      vFile.Add('SERVER=192.168.1.11');
      vFile.SaveToFile('E:\dese.git\Executaveis\FormulaCerta\alterdb.ini');
      //vFile.SaveToFile(dlg1.FileName);
    finally
      vFile.DisposeOf;
    end;
  end;
  ListBoxRefresh;
end;

procedure TFormPrincipal.ShowConfig(AConfig: IDataBaseConfig; const AIsActual: boolean = False);
var
  vForm: TFormConfig;
  vDao: IDao<IDataBaseConfig>;
  vNew: boolean;
begin
  vNew := not Assigned(AConfig);

  vForm := TFormConfig.Create(Self);
  try
    if vNew then
    begin
      AConfig := TDataBaseConfigFactory.Config;
      AConfig.ID := TGUID.NewGuid.ToString;
      vForm.btnDelete.Enabled := False;
      vForm.btnOK.Enabled := False;
    end;
    vForm.ID := AConfig.ID;
    vForm.Description := AConfig.Description;
    vForm.ServerName := AConfig.ServerName;
    vForm.DataBase := AConfig.DataBase;
    vForm.ShowModal;
    if vForm.Action <> TCrudAction.caNone then
    begin
      vDao := TDataBaseConfigFactory.Dao;
      if vForm.Action = TCrudAction.caSave then
      begin
        AConfig.ID := vForm.ID;
        AConfig.Description := vForm.Description;
        AConfig.ServerName := vForm.ServerName;
        AConfig.DataBase := vForm.DataBase;
        vDao.Save(AConfig);

        if AIsActual then
        begin
          FormPrincipal.SaveToDisk(AConfig);
        end;

      end;
      if
        (not vNew) and
        (vForm.Action = TCrudAction.caDelete)
      then
      begin
        vDao.Delete(AConfig.ID);
      end;
      ListBoxRefresh;
    end;
  finally
    FreeAndNil(vForm);
  end;
end;

procedure TFormPrincipal.ShowFileLayout;
begin
  if not Assigned(FFormFileLayout) then
  begin
    FFormFileLayout := TFormFileLayout.Create(Self);
    tabFileLayout.AddObject(FFormFileLayout.layConteudo);
  end;
  FFormFileLayout.Start;
  tbcPrincipal.ActiveTab := tabFileLayout;
end;

procedure TFormPrincipal.ShowQuery(AConfig: IDataBaseConfig);
begin
  if not Assigned(FFormQuery) then
  begin
    FFormQuery := TFormQuery.Create(Self);
    tabQuery.AddObject(FFormQuery.layConteudo);
  end;
  FFormQuery.Start(AConfig);
  tbcPrincipal.ActiveTab := tabQuery;
end;

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
//  if IsActual then
//    DoOnSaveToDiskClick(Sender);
  FormPrincipal.ShowConfig(FConfig);
end;

procedure TListBoxItem.DoOnQueryClick(Sender: TObject);
begin
  FormPrincipal.ShowQuery(FConfig);
end;

procedure TListBoxItem.DoOnSaveToDiskClick(Sender: TObject);
begin
  FormPrincipal.SaveToDisk(FConfig);
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

end.
