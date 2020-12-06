unit View.Param.Manager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Controller.Interfaces,
  Data.DB, Vcl.ComCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls, Vcl.ExtCtrls;

type
  TViewParamManager = class(TViewDefault)
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    StatusBar1: TStatusBar;
    DataSource1: TDataSource;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FController: IControllerParamManager;
  public
    { Public declarations }
    constructor Create(const AController: IControllerParamManager); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}
{ TViewParamManager }

constructor TViewParamManager.Create(const AController
  : IControllerParamManager);
begin
  inherited Create(nil);
  FController := AController;
end;

destructor TViewParamManager.Destroy;
begin

  inherited;
end;

procedure TViewParamManager.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  FController._Release;
end;

end.
