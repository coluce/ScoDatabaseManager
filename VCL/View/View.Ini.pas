unit View.Ini;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Buttons, SynEditHighlighter, SynHighlighterIni, SynEdit, SynMemo, Vcl.DBCtrls, Vcl.StdCtrls,
  System.Actions, Vcl.ActnList, Data.DB, Controller.Interfaces,
  System.ImageList, Vcl.ImgList, View.Default;

type
  TViewIni = class(TViewDefault)
    Panel1: TPanel;
    Bevel1: TBevel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    SynMemo1: TSynMemo;
    SynIniSyn1: TSynIniSyn;
    ActionList1: TActionList;
    acnExportar: TAction;
    acnCancelar: TAction;
    ComboBoxLayout: TComboBox;
    FileSaveDialog1: TFileSaveDialog;
    Label3: TLabel;
    ImageList1: TImageList;
    edtLocalDestino: TEdit;
    SpeedButton3: TSpeedButton;
    procedure acnExportarExecute(Sender: TObject);
    procedure acnCancelarExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBoxLayoutCloseUp(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { Private declarations }
    FController: IControllerIni;
  public
    { Public declarations }
    constructor Create(const AController: IControllerIni); reintroduce;
  end;

var
  ViewIni: TViewIni;

implementation

{$R *.dfm}

procedure TViewIni.acnCancelarExecute(Sender: TObject);
begin
  Close;
end;

procedure TViewIni.acnExportarExecute(Sender: TObject);
begin
  FController.ExportToDrive;
  Close;
end;

procedure TViewIni.ComboBoxLayoutCloseUp(Sender: TObject);
begin
  FController.FillPreview;
end;

constructor TViewIni.Create(const AController: IControllerIni);
begin
  inherited Create(nil);
  FController := AController;
end;

procedure TViewIni.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FController._Release;
end;

procedure TViewIni.SpeedButton3Click(Sender: TObject);
begin
  if FileSaveDialog1.Execute then
    edtLocalDestino.Text := FileSaveDialog1.FileName;
end;

end.
