unit View.Query;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  FMX.TreeView, Model.Config, System.ImageList, FMX.ImgList, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.FMXUI.Wait, Data.DB,
  FireDAC.Comp.Client, FMX.Menus, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.Objects, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, FireDAC.Comp.DataSet, FireDAC.DApt,
  Model.Interfaces;

type

  TConnectionWidget = class(TThread)
  private
    FStatusRectangle: TRectangle;
    FConnection: TFDConnection;
    FListTable: TListView;
    FLabelStatus: TLabel;
    FConfig: IDataBaseConfig;
    FIndicator: TAniIndicator;
    procedure GetTableName;
  protected
    procedure Execute; override;
  public
    constructor Create(AStatusRec: TRectangle; AIndicator: TAniIndicator; ATreeTable: TListView; AConfig: IDataBaseConfig; ALabelStatus: TLabel);
    destructor Destroy; override;
  published
    property Connection: TFDCOnnection read FConnection;
  end;

  TFormQuery = class(TForm)
    tlbQuery: TToolBar;
    spl1: TSplitter;
    mmoQuery: TMemo;
    layConteudo: TLayout;
    btnAdd: TSpeedButton;
    ilQuery: TImageList;
    pmTable: TPopupMenu;
    mniSelect: TMenuItem;
    mniInsert: TMenuItem;
    mniDelete: TMenuItem;
    mniQuerys: TMenuItem;
    mniEstrutura: TMenuItem;
    mniEstruturaCreate: TMenuItem;
    mniEstruturaCampos: TMenuItem;
    pnlComandos: TPanel;
    layQuery: TLayout;
    btnRun: TSpeedButton;
    grdQuery: TGrid;
    spl2: TSplitter;
    rtgStatus: TRectangle;
    stb1: TStatusBar;
    lblStatusBar: TLabel;
    lvTables: TListView;
    aniQuery: TAniIndicator;
    mtbResultado: TFDMemTable;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvTablesItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure btnRunClick(Sender: TObject);
  private
    { Private declarations }
    FConfig: IDataBaseConfig;
    FTables: TStrings;
    FConnectionWidget: TConnectionWidget;
    procedure SetConfig(const Value: IDataBaseConfig);
  public
    { Public declarations }
    procedure Start(const AConfig: IDataBaseConfig);
    procedure Stop;
  published
    { Published declarations }
    property Config: IDataBaseConfig read FConfig write SetConfig;
  end;

implementation

{$R *.fmx}

uses
  Form.Principal,
  System.IOUtils;

{ TFormQuery }

procedure TFormQuery.btnAddClick(Sender: TObject);
begin
  Stop;
  //FormPrincipal.tbcPrincipal.ActiveTab := FormPrincipal.tabMenu;
end;

procedure TFormQuery.btnRunClick(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      vQuery: TFDQuery;
    begin
      vQuery := TFDQuery.Create(nil);
      try
        vQuery.Connection := FConnectionWidget.Connection;
        vQuery.SQL.Text := mmoQuery.Lines.Text;

        TThread.Synchronize(
          nil,
          procedure
          begin
            aniQuery.Visible := True;
            aniQuery.Enabled := True;
          end
        );

        vQuery.Open;

        TThread.Synchronize(
          nil,
          procedure
          begin
            grdQuery.BeginUpdate;
            try
              mtbResultado.CloneCursor(vQuery);
            finally
              grdQuery.EndUpdate;
            end;

            aniQuery.Visible := False;
            aniQuery.Enabled := False;

          end
        );
      finally
        vQuery.Free;
      end;
    end
  ).Start;
end;

procedure TFormQuery.FormCreate(Sender: TObject);
begin
  FTables := TStringList.Create;
end;

procedure TFormQuery.FormDestroy(Sender: TObject);
begin
  FTables.DisposeOf;
end;

procedure TFormQuery.lvTablesItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  mmoQuery.Lines.Clear;
  mmoQuery.Lines.Add('select');
  mmoQuery.Lines.Add('  *');
  mmoQuery.Lines.Add('from');
  mmoQuery.Lines.Add('  ' + AItem.Text);
  mmoQuery.Lines.Add('where');
  mmoQuery.Lines.Add('  1 = 2');
end;

procedure TFormQuery.SetConfig(const Value: IDataBaseConfig);
begin
  FConfig := Value;
end;

procedure TFormQuery.Start(const AConfig: IDataBaseConfig);
begin
  mmoQuery.Lines.Clear;
  lvTables.Items.Clear;
  if mtbResultado.Active then
  begin
    mtbResultado.Close;
  end;
  SetConfig(AConfig);
  FConnectionWidget := TConnectionWidget.Create(rtgStatus, aniQuery, lvTables, AConfig, lblStatusBar);
  FConnectionWidget.Start;
end;

procedure TFormQuery.Stop;
begin
  FConnectionWidget.Terminate;
  if not FConnectionWidget.Terminated then
  begin
    FConnectionWidget.WaitFor;
  end;
  FreeAndNil(FConnectionWidget);
  lvTables.Items.Clear;
  rtgStatus.Fill.Color := $FFE0E0E0;
end;

{ TConnectionWidget }

constructor TConnectionWidget.Create(AStatusRec: TRectangle; AIndicator: TAniIndicator; ATreeTable: TListView; AConfig: IDataBaseConfig; ALabelStatus: TLabel);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FStatusRectangle := AStatusRec;
  FListTable := ATreeTable;
  FLabelStatus := ALabelStatus;
  FIndicator := AIndicator;
  FConfig := AConfig;
end;

destructor TConnectionWidget.Destroy;
begin
  if FConnection.Connected then
  begin
    try
      FConnection.Close;
    except

    end;
  end;
  FreeAndNil(FConnection);
  inherited;
end;

procedure TConnectionWidget.Execute;
begin
  inherited;

  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := 'FB';
  FConnection.Params.Database := TPath.Combine(FConfig.DataBase,'ALTERDB.IB');
  FConnection.Params.Values['Server'] := FConfig.ServerName;
  FConnection.Params.UserName := 'SYSDBA';
  FConnection.Params.Password := 'masterkey';

  FLabelStatus.Text := '';
  if not FConnection.Connected then
  begin
    try
      TThread.Synchronize(
        nil,
        procedure
        begin
          FLabelStatus.Text := 'Conectando ...';
          FStatusRectangle.Fill.Color := TAlphaColorRec.Yellow;
          FIndicator.Visible := True;
          FIndicator.Enabled := True;
        end
      );
      try
        FConnection.Open;
      finally
        TThread.Synchronize(
          nil,
          procedure
          begin
            FIndicator.Visible := False;
            FIndicator.Enabled := False;
          end
        );
      end;
    except

    end;
  end;

  if FConnection.Connected then
  begin
    TThread.Synchronize(
      nil,
      procedure
      begin
        GetTableName;
      end
    );
  end;

  while not Terminated do
  begin
    sleep(500);
    if Terminated then
      Exit;
    if FConnection.Connected then
    begin
      TThread.Synchronize(
        nil,
        procedure
        begin
          FLabelStatus.Text := 'Conectado';
          FStatusRectangle.Fill.Color := TAlphaColorRec.Mediumaquamarine;
        end
      );
    end
    else
    begin
      TThread.Synchronize(
        nil,
        procedure
        begin
          FLabelStatus.Text := 'Não conectado..';
          FStatusRectangle.Fill.Color := TAlphaColorRec.Red; //$FFE0E0E0;
        end
      );
    end;
  end;

  if FConnection.Connected then
  begin
    try
      FConnection.Close;
    except

    end;
  end;

end;

procedure TConnectionWidget.GetTableName;
var
  vTable: TStrings;
  vNome: string;
  vItem: TListViewItem;
  i: integer;
begin
  FListTable.Items.Clear;
  if FConnection.Connected then
  begin
    FLabelStatus.Text := 'Carregando tabelas ...';
    FListTable.BeginUpdate;
    try
      vTable := TStringList.Create;
      try
        FConnection.GetTableNames('','','',vTable);
        for i:= 1 to vTable.Count - 1 do
        begin
          vItem := FListTable.Items.Add;
          vItem.Text := vTable[i];
        end;
      finally
        FreeAndNil(vTable);
      end;
    finally
      FListTable.EndUpdate;
    end;
  end;
end;

end.

