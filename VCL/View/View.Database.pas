unit View.Database;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Data.DB, SynEdit, SynMemo, Vcl.Grids, Vcl.DBGrids, Vcl.ToolWin, SynEditHighlighter,
  SynHighlighterSQL, Model.Types, Controller.Interfaces, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.WinXCtrls, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TViewDatabase = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    TreeViewTabelas: TTreeView;
    Splitter1: TSplitter;
    Panel2: TPanel;
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
    ToggleSwitch1: TToggleSwitch;
    FDQuery1: TFDQuery;
    DataSource1: TDataSource;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToggleSwitch1Click(Sender: TObject);
    procedure TreeViewTabelasDblClick(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerDataBase;
    FDatabase: TDataBase;
    procedure SetDataBase(const Value: TDataBase);
  public
    { Public declarations }
    constructor Create(const AController: IControllerDatabase); reintroduce;
  published
    { Published declarations }
    property DataBase: TDataBase read FDataBase write SetDataBase;
  end;

implementation

{$R *.dfm}

{ TViewDatabase }

constructor TViewDatabase.Create(const AController: IControllerDatabase);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewDatabase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  {FController é o dono desta tela, e é uma interface, este é o unico ponto onde a interface está sendo usada
   se não decrementar o uso da interface ao fechar a tela, não destroi o objeto e não destroi esta tela
  }
  FController._Release;
end;

procedure TViewDatabase.SetDataBase(const Value: TDataBase);
begin
  FDataBase := Value;
  Self.Caption := FDatabase.Name;
end;

procedure TViewDatabase.ToggleSwitch1Click(Sender: TObject);
begin
  FController.Connected := ToggleSwitch1.IsOn;
  if FController.Connected then
  begin
    FController.FillTableNames;
  end;
end;

procedure TViewDatabase.TreeViewTabelasDblClick(Sender: TObject);
begin
  if TreeViewTabelas.Selected.Level = 0 then
  begin
    MemoQuery.Clear;
    MemoQuery.Lines.Add('select');
    MemoQuery.Lines.Add('  *');
    MemoQuery.Lines.Add('from');
    MemoQuery.Lines.Add('  ' + TreeViewTabelas.Selected.Text);
  end;
end;

end.
