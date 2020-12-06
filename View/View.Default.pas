unit View.Default;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Controller.Interfaces;

type
  TViewDefault = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  protected
    FControllerWindow: IControllerWindow;
  public
    { Public declarations }
  end;

implementation

uses
  Controller.Factory;

{$R *.dfm}

procedure TViewDefault.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FControllerWindow.SavePosition;
end;

procedure TViewDefault.FormCreate(Sender: TObject);
begin
  FControllerWindow := TControllerFactory.Window(Self);
end;

procedure TViewDefault.FormShow(Sender: TObject);
begin
  FControllerWindow.RestorePosition;
end;

end.
