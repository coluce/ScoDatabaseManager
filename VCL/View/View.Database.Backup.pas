unit View.Database.Backup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, View.Default, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TViewDataBaseBackup = class(TViewDefault)
    Panel2: TPanel;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    TreeViewBackupFiles: TTreeView;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
