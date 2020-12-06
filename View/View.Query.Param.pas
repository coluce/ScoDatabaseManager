unit View.Query.Param;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Data.DB, Vcl.Grids,
  Vcl.DBGrids, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TViewQueryParam = class(TForm)
    GridParam: TDBGrid;
    Panel1: TPanel;
    Bevel1: TBevel;
    btnOK: TButton;
    btnCancelar: TButton;
    DataSourceParam: TDataSource;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
