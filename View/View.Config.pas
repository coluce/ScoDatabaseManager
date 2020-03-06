unit View.Config;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

type
  TCrudAction = (caNone, caSave, caDelete);

  TFormConfig = class(TForm)
    edtID: TEdit;
    lblID: TLabel;
    edtServerName: TEdit;
    lblServerName: TLabel;
    edtDataBase: TEdit;
    lblDataBase: TLabel;
    btnOK: TButton;
    layRodape: TLayout;
    rtg1: TRectangle;
    btnSaveToDatabase: TButton;
    btnDelete: TButton;
    edtDescription: TEdit;
    lblDescription: TLabel;
    procedure EditButton1Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveToDatabaseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FAction: TCrudAction;
    procedure SetDataBase(const Value: string);
    procedure SetID(const Value: string);
    procedure SetServerName(const Value: string);
    function GetDataBase: string;
    function GetServerName: string;
    function GetID: string;
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    { Private declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property ID: string read GetID write SetID;
    property ServerName: string read GetServerName write SetServerName;
    property DataBase: string read GetDataBase write SetDataBase;
    property Description: string read GetDescription write SetDescription;
    property Action: TCrudAction read FAction;
  end;

implementation

{$R *.fmx}

uses
  Form.Principal;

{ TFormConfig }

procedure TFormConfig.btnDeleteClick(Sender: TObject);
begin
  FAction := caDelete;
  Close;
end;

procedure TFormConfig.btnOKClick(Sender: TObject);
begin
  FAction := caNone;
  Close;
end;

procedure TFormConfig.btnSaveToDatabaseClick(Sender: TObject);
begin
  FAction := caSave;
  Close;
end;

procedure TFormConfig.EditButton1Click(Sender: TObject);
begin
  edtID.Text := TGUID.NewGuid.ToString;
end;

procedure TFormConfig.FormCreate(Sender: TObject);
begin
  FAction := caNone;
end;

function TFormConfig.GetDataBase: string;
begin
  Result := edtDataBase.Text
end;

function TFormConfig.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TFormConfig.GetID: string;
begin
  Result := edtID.Text;
end;

function TFormConfig.GetServerName: string;
begin
  Result := edtServerName.Text;
end;

procedure TFormConfig.SetDataBase(const Value: string);
begin
  edtDataBase.Text := Value;
end;

procedure TFormConfig.SetDescription(const Value: string);
begin
  edtDescription.Text := Value;
end;

procedure TFormConfig.SetID(const Value: string);
begin
  edtID.Text := Value;
end;

procedure TFormConfig.SetServerName(const Value: string);
begin
  edtServerName.Text := Value;
end;

end.
