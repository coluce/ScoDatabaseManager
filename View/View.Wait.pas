unit View.Wait;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TViewWait = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TViewWait.Timer1Timer(Sender: TObject);
var
  vDots: string;
  I: Integer;
begin
  Timer1.Tag := Timer1.Tag + 1;
  if Timer1.Tag > 3 then
  begin
    Timer1.Tag := 1;
  end;

  vDots := EmptyStr;
  for I := 1 to Timer1.Tag do
  begin
    vDots := vDots + '.';
  end;
  Label1.Caption := 'Copiando ' + vDots;
end;

end.
