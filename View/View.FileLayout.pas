unit View.FileLayout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, View.Default,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.ScrollBox,
  FMX.Memo, FMX.Effects, FMX.Objects, Model.Interfaces;

type
  TViewFileLayout = class(TFormPadrao)
    mmoFileLayout: TMemo;
    tlbQuery: TToolBar;
    btnVoltar: TSpeedButton;
    Layout1: TLayout;
    edtFileDirectory: TEdit;
    lblFileDirectory: TLabel;
    edtFileName: TEdit;
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

implementation

uses
  Form.Principal,
  Controller.Principal, Model.FileLayout;

{$R *.fmx}

procedure TViewFileLayout.btnSaveClick(Sender: TObject);
begin
  inherited;
  FLayout.DefaultDirectory := edtFileDirectory.Text;
  FLayout.DefaultName := edtFileName.Text;
  FLayout.Layout := mmoFileLayout.Text;

  TFileLayoutDao.Create.Save(FLayout);

  TControllerPrincipal.Instance.ShowMenu;

end;

procedure TViewFileLayout.btnVoltarClick(Sender: TObject);
begin
  inherited;
  TControllerPrincipal.Instance.ShowMenu;
end;

procedure TViewFileLayout.Start(const AFileLayout: IFileLayout);
begin
  FLayout := AFileLayout;
  edtFileDirectory.Text := FLayout.DefaultDirectory;
  edtFileName.Text := FLayout.DefaultName;
  mmoFileLayout.Text := FLayout.Layout;
end;

end.

