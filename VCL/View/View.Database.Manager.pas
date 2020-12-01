unit View.Database.Manager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Data.DB, SynEdit, SynMemo, Vcl.Grids, Vcl.DBGrids, Vcl.ToolWin, SynEditHighlighter,
  SynHighlighterSQL, Model.Types, Controller.Interfaces, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.WinXCtrls,
  System.ImageList, Vcl.ImgList, System.Actions, Vcl.ActnList, View.Default,
  Vcl.Buttons;

type
  TViewDatabaseManager = class(TViewDefault)
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    pnlQuery: TPanel;
    Panel3: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    ToolBar2: TToolBar;
    MemoQuery: TSynMemo;
    SynSQLSyn1: TSynSQLSyn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GridResultado: TDBGrid;
    MemoLog: TMemo;
    DBNavigator1: TDBNavigator;
    Panel5: TPanel;
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
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    acnQueryImportar: TAction;
    acnQueryExportar: TAction;
    PageControl2: TPageControl;
    tabQuery: TTabSheet;
    tabManager: TTabSheet;
    Panel2: TPanel;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    acnManagerBackup: TAction;
    acnManagerRestore: TAction;
    TreeViewFiles: TTreeView;
    Splitter3: TSplitter;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToggleSwitch1Click(Sender: TObject);
    procedure TreeViewTabelasDblClick(Sender: TObject);
    procedure acnQueryExecutarExecute(Sender: TObject);
    procedure acnManagerBackupExecute(Sender: TObject);
    procedure acnManagerRestoreExecute(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerDataBase;
  public
    { Public declarations }
    constructor Create(const AController: IControllerDatabase); reintroduce;
  published
    { Published declarations }
  end;

implementation

{$R *.dfm}

{ TViewDatabase }

procedure TViewDatabaseManager.acnManagerBackupExecute(Sender: TObject);
begin
  inherited;
  FController.Backup;
end;

procedure TViewDatabaseManager.acnManagerRestoreExecute(Sender: TObject);
begin
  inherited;
  FController.Restore;
end;

procedure TViewDatabaseManager.acnQueryExecutarExecute(Sender: TObject);
begin
  FController.ExecuteQuery;
end;

constructor TViewDatabaseManager.Create(const AController: IControllerDatabase);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewDatabaseManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  {FController é o dono desta tela, e é uma interface, este é o unico ponto onde a interface está sendo usada
   se não decrementar o uso da interface ao fechar a tela, não destroi o objeto e não destroi esta tela
  }
  FController._Release;
end;

procedure TViewDatabaseManager.ToggleSwitch1Click(Sender: TObject);
begin
  FController.ToogleSwitchClick;
end;

procedure TViewDatabaseManager.TreeViewTabelasDblClick(Sender: TObject);
begin
  FController.FillSQLFromTreeView;
end;

end.
