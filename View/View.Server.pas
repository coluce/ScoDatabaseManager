unit View.Server;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TViewServer = class(TViewDefault)
    Image1: TImage;
    Panel1: TPanel;
    Bevel1: TBevel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label1: TLabel;
    EditNome: TEdit;
    Label2: TLabel;
    EditLocal: TEdit;
    lblPort: TLabel;
    spnPort: TSpinEdit;
    edtPassword: TEdit;
    edtUserName: TEdit;
    lblUsername: TLabel;
    lblPassword: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    FResultado: TModalResult;
  public
    { Public declarations }
    property Resultado: TModalResult read FResultado write FResultado;
  end;

implementation

{$R *.dfm}

procedure TViewServer.FormCreate(Sender: TObject);
begin
  inherited;
  FResultado := mrCancel;
end;

procedure TViewServer.SpeedButton1Click(Sender: TObject);
begin
  inherited;
  FResultado := mrOK;
  Close;
end;

procedure TViewServer.SpeedButton2Click(Sender: TObject);
begin
  inherited;
  Close;
end;

end.
