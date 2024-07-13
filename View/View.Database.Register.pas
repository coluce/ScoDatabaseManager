unit View.Database.Register;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Vcl.StdCtrls;

type
  TViewRegisterDatabase = class(TViewDefault)
    Image1: TImage;
    Panel1: TPanel;
    Bevel1: TBevel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label1: TLabel;
    EditNome: TEdit;
    Label2: TLabel;
    EditLocal: TEdit;
    edtBackupFolder: TEdit;
    lblBackupFolder: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FResultado: TModalResult;
  public
    { Public declarations }
    property Resultado: TModalResult read FResultado write FResultado;
  end;

implementation

{$R *.dfm}

procedure TViewRegisterDatabase.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // inherited;

end;

procedure TViewRegisterDatabase.FormCreate(Sender: TObject);
begin
  inherited;
  FResultado := mrCancel;
end;

procedure TViewRegisterDatabase.FormShow(Sender: TObject);
begin
  // inherited;
end;

procedure TViewRegisterDatabase.SpeedButton1Click(Sender: TObject);
begin
  inherited;
  Self.Resultado := mrOk;
  Close;
end;

procedure TViewRegisterDatabase.SpeedButton2Click(Sender: TObject);
begin
  inherited;
  Self.Resultado := mrCancel;
  Close;
end;

end.
