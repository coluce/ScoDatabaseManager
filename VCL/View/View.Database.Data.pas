unit View.Database.Data;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Data.DB, SynEdit, SynMemo, Vcl.Grids, Vcl.DBGrids, Vcl.ToolWin, SynEditHighlighter,
  SynHighlighterSQL, Model.Types, Controller.Interfaces, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.WinXCtrls,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, View.Default,
  Vcl.Buttons, FireDAC.Stan.StorageXML;

type
  TViewDatabaseData = class(TViewDefault)
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    pnlQuery: TPanel;
    Panel4: TPanel;
    ToolBar2: TToolBar;
    MemoQuery: TSynMemo;
    SynSQLSyn1: TSynSQLSyn;
    DataSource1: TDataSource;
    ImageListTabelas: TImageList;
    ImageListQuery: TImageList;
    ActionListQuery: TActionList;
    acnQueryExecutar: TAction;
    ToolButton1: TToolButton;
    pnlTables: TPanel;
    Panel1: TPanel;
    ToggleSwitch1: TToggleSwitch;
    TreeViewTabelas: TTreeView;
    acnQueryImportar: TAction;
    acnQueryExportar: TAction;
    PageControlMain: TPageControl;
    tabQuery: TTabSheet;
    TabSheetResult: TTabSheet;
    TabSheetLog: TTabSheet;
    TabSheetHistory: TTabSheet;
    GridResultado: TDBGrid;
    Panel5: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    DBNavigator1: TDBNavigator;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    Splitter2: TSplitter;
    TabSheetResultExecutionPlan: TTabSheet;
    TabSheetResultFields: TTabSheet;
    Memo1: TMemo;
    SaveDataDialog: TSaveDialog;
    OpenDataDialog: TOpenDialog;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToggleSwitch1Click(Sender: TObject);
    procedure TreeViewTabelasDblClick(Sender: TObject);
    procedure acnQueryExecutarExecute(Sender: TObject);
    procedure acnQueryExportarExecute(Sender: TObject);
    procedure acnQueryImportarExecute(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerDataBaseData;
  public
    { Public declarations }
    constructor Create(const AController: IControllerDatabaseData); reintroduce;
  published
    { Published declarations }
  end;

implementation

{$R *.dfm}

{ TViewDatabase }

procedure TViewDatabaseData.acnQueryExecutarExecute(Sender: TObject);
begin
  FController.ExecuteQuery;
end;

procedure TViewDatabaseData.acnQueryExportarExecute(Sender: TObject);
begin
  inherited;
  FController.ExportData;
end;

procedure TViewDatabaseData.acnQueryImportarExecute(Sender: TObject);
begin
  inherited;
  FController.ImportData;
end;

constructor TViewDatabaseData.Create(const AController: IControllerDatabaseData);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewDatabaseData.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  {FController é o dono desta tela, e é uma interface, este é o unico ponto onde a interface está sendo usada
   se não decrementar o uso da interface ao fechar a tela, não destroi o objeto e não destroi esta tela
  }
  FController._Release;
end;

procedure TViewDatabaseData.ToggleSwitch1Click(Sender: TObject);
begin
  FController.ToogleSwitchClick;
end;

procedure TViewDatabaseData.TreeViewTabelasDblClick(Sender: TObject);
begin
  FController.FillSQLFromTreeView;
end;

end.
