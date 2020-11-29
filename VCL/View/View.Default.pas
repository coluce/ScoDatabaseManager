unit View.Default;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Controller.Interfaces;

type
  TViewDefault = class(TForm)
  private
    { Private declarations }
  Protected
    FControllerWindow: IControllerWindow;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
