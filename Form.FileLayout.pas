unit Form.FileLayout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, Form.Padrao, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.ScrollBox,
  FMX.Memo, FMX.Effects, FMX.Objects, Model.FileLayout;

type
  TFormFileLayout = class(TFormPadrao)
    edtFileDirectory: TEdit;
    edtFileName: TEdit;
    mmoFileLayout: TMemo;
    tlbQuery: TToolBar;
    btnVoltar: TSpeedButton;
    rtgCabecalho: TRectangle;
    lblFileDirectory: TLabel;
    lblFileName: TLabel;
    btnSave: TSpeedButton;
    procedure btnVoltarClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    { Private declarations }
    FLayout: IFileLayout;
  public
    { Public declarations }
    procedure Start(const AFileLayout: IFileLayout);
  end;

var
  FormFileLayout: TFormFileLayout;

implementation

uses
  Form.Principal;

{$R *.fmx}

procedure TFormFileLayout.btnSaveClick(Sender: TObject);
begin
  inherited;
  FLayout.DefaultDirectory := edtFileDirectory.Text;
  FLayout.DefaultName := edtFileName.Text;
  FLayout.Layout := mmoFileLayout.Text;

  TFileLayoutDao.Create.Save(FLayout);

  FormPrincipal.tbcPrincipal.ActiveTab := FormPrincipal.tabMenu;

end;

procedure TFormFileLayout.btnVoltarClick(Sender: TObject);
begin
  inherited;
  FormPrincipal.tbcPrincipal.ActiveTab := FormPrincipal.tabMenu;
end;

procedure TFormFileLayout.Start(const AFileLayout: IFileLayout);
begin
  FLayout := AFileLayout;
  edtFileDirectory.Text := FLayout.DefaultDirectory;
  edtFileName.Text := FLayout.DefaultName;
  mmoFileLayout.Text := FLayout.Layout;
end;

end.

