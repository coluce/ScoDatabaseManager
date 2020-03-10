unit Form.Principal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FireDAC.Stan.StorageXML,
  FMX.Objects, FMX.TabControl, Controller.Principal;

type

  TFormPrincipal = class(TForm, IFormThemed)
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    styLight: TStyleBook;
    styDark: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetDark;
    procedure SetLight;
    function IsDark: boolean;
    function IsLight: boolean;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  System.IOUtils,
  View.Config;

{$R *.fmx}

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  TControllerPrincipal.Start(Self);
  TControllerPrincipal.Instance.ShowMenu;
  Self.WindowState := TWindowState.wsMaximized;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  TControllerPrincipal.Stop;
end;

function TFormPrincipal.IsDark: boolean;
begin
  Result := Self.StyleBook = styDark;
end;

function TFormPrincipal.IsLight: boolean;
begin
  Result := not IsDark;
end;

procedure TFormPrincipal.SetDark;
begin
  Self.StyleBook := styDark;
end;

procedure TFormPrincipal.SetLight;
begin
  Self.StyleBook := styLight;
end;

end.
