unit Controller.Imp.Window;

interface

uses
  Controller.Interfaces, Vcl.Forms;

type
  TControllerWindow = class(TInterfacedObject, IControllerWindow)
  private
    FView: TForm;
    FControllerParam: IControllerParam;
  public
    constructor Create(const AView: TForm);
    procedure SavePosition;
    procedure RestorePosition;
  end;

implementation

uses
  Controller.Factory, System.SysUtils;

{ TControllerWindow }

constructor TControllerWindow.Create(const AView: TForm);
begin
  FView := AView;
  FControllerParam := TControllerFactory.Param;
end;

procedure TControllerWindow.RestorePosition;
begin
  FView.Height := StrToInt(FControllerParam.GetParam(FView.ClassName, 'ALTURA',   FView.Height.ToString));
  FView.Width  := StrToInt(FControllerParam.GetParam(FView.ClassName, 'LARGURA',  FView.Width.ToString));
  FView.Left   := StrToInt(FControllerParam.GetParam(FView.ClassName, 'ESQUERDA', FView.Left.ToString));
  FView.Top    := StrToInt(FControllerParam.GetParam(FView.ClassName, 'CIMA',     FView.Top.ToString));
end;

procedure TControllerWindow.SavePosition;
begin
  FControllerParam.SetParam(FView.ClassName, 'ALTURA', FView.Height.ToString);
  FControllerParam.SetParam(FView.ClassName, 'LARGURA', FView.Width.ToString);
  FControllerParam.SetParam(FView.ClassName, 'ESQUERDA', FView.Left.ToString);
  FControllerParam.SetParam(FView.ClassName, 'CIMA', FView.Top.ToString);
end;

end.
