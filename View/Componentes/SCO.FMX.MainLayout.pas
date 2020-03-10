unit SCO.FMX.MainLayout;

interface

uses
  FMX.TabControl,
  System.Classes, FMX.Forms, System.Generics.Collections;

type

  TControleTela = record
  private
    Objeto    : TForm;
    TabItem   : TTabItem;
    ClassType : TClass;
  end;

  TSCOFMXMainLayout = class(TTabControl)
  private
    FLista: TDictionary<TClass,TControleTela>;
    FAccessList: TList<TComponentClass>;
    FLastAccess: TComponentClass;
    FActualForm: TForm;
    FLayoutName: string;
    acnChaneTab : TChangeTabAction;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenForm(AClassType : TComponentClass; SmoothTransition: Boolean = True);
    procedure GoBack;
    procedure ClearAccessList;
  published
    property ActualForm : TForm  read FActualForm;
    property LayoutName : string read FLayoutName write FLayoutName;
    property ChangeTab: TChangeTabAction read acnChaneTab;
  end;

implementation

uses
  FMX.Layouts, System.SysUtils;

{ TSCOFMXMainLayout }

procedure TSCOFMXMainLayout.ClearAccessList;
begin
  FAccessList.Clear;
  FAccessList.TrimExcess;
end;

constructor TSCOFMXMainLayout.Create(AOwner: TComponent);
begin
  inherited;
  FLista := TDictionary<TClass,TControleTela>.Create;
  FAccessList := TList<TComponentClass>.Create;
  FLastAccess := nil;
  FLayoutName := 'layPrincipal';
  Self.TabPosition := TTabPosition.None;
  acnChaneTab := TChangeTabAction.Create(Self);
end;

destructor TSCOFMXMainLayout.Destroy;
begin
  FAccessList.Clear;
  FAccessList.DisposeOf;
  FAccessList := nil;

  FLista.Clear;
  FLista.DisposeOf;
  FLista := nil;
  inherited;
end;

procedure TSCOFMXMainLayout.GoBack;
var
  vClass: TComponentClass;
begin
  if FAccessList.Count > 0 then
  begin
    vClass := FAccessList.Last;
    FAccessList.Remove(FLastAccess);
    FAccessList.TrimExcess;
    FLastAccess := nil;
    OpenForm(vClass);
  end;
end;

procedure TSCOFMXMainLayout.OpenForm(AClassType : TComponentClass; SmoothTransition: Boolean = True);
  procedure RegisterAccess;
  begin
    if FLastAccess <>  nil then
    begin
      if FLastAccess <> AClassType then
      begin
        FAccessList.Add(FLastAccess);
      end;
    end;
    FLastAccess := AClassType;
  end;
var
   xControle: TControleTela;
   xLayoutPrincipal: TLayout;
begin
  if not FLista.ContainsKey(AClassType) then
  begin
    Application.CreateForm(AClassType,xControle.Objeto);
    xLayoutPrincipal := TLayout(xControle.Objeto.FindComponent(FLayoutName));
    if not Assigned(xLayoutPrincipal) then
    begin
      xControle.Objeto.DisposeOf;
      xControle.Objeto := nil;
      raise Exception.Create('Formulário não contem um "' + FLayoutName + '"');
    end else begin
      xControle.ClassType := AClassType;
      xControle.TabItem   := TTabItem.Create(Self);
      AddObject(xControle.TabItem);

      xControle.TabItem.AddObject(xLayoutPrincipal);
      FLista.Add(AClassType,xControle);
    end;
  end else begin
    FLista.TryGetValue(AClassType,xControle);
  end;
  RegisterAccess;
  FActualForm  := xControle.Objeto;
  if SmoothTransition then
  begin
    acnChaneTab.Target := Self;
    acnChaneTab.Tab    := xControle.TabItem;
    acnChaneTab.Execute;
  end else begin
    ActiveTab          := xControle.TabItem;
  end;
end;

end.
