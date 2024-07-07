unit View.Layout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, SynEdit, SynDBEdit, SynEditHighlighter,
  SynHighlighterIni, Vcl.StdCtrls, Vcl.Mask, Controller.Interfaces,
  View.Default, Vcl.Buttons;

type
  TViewLayout = class(TViewDefault)
    DBGrid1: TDBGrid;
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    StatusBar1: TStatusBar;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Splitter1: TSplitter;
    DBEdit1: TDBEdit;
    lblNome: TLabel;
    lblLayout: TLabel;
    SynIniSyn1: TSynIniSyn;
    DBSynEdit1: TDBSynEdit;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FController: IControllerLayout;
  public
    { Public declarations }
    constructor Create(const AController: IControllerLayout); reintroduce;
  end;

implementation

{$R *.dfm}
{ TViewLayout }

constructor TViewLayout.Create(const AController: IControllerLayout);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewLayout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  { liberar da memoria esta instancia de interface, se não fica presa nesta tela, e quem destroi esta tela é a interface ao ser destruida }
  FController._Release;
end;

end.
