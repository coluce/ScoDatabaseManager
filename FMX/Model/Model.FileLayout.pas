unit Model.FileLayout;

interface

uses
  Model.Interfaces;

type

  TFileLayout = class(TInterfacedObject, IFileLayout)
  private
    FDefaultDirectory: string;
    FDefaultName: string;
    FLayout: string;
    FThemeLight: boolean;
    function GetDefaultDirectory: string;
    function GetDefaultName: string;
    function GetLayout: string;
    function GetThemeLight: boolean;
    procedure SetDefaultDirectory(const Value: string);
    procedure SetDefaultName(const Value: string);
    procedure SetLayout(const Value: string);
    procedure SetThemeLight(const Value: boolean);
  public
    constructor Create;
  published
    property DefaultDirectory: string read GetDefaultDirectory write SetDefaultDirectory;
    property DefaultName: string read GetDefaultName write SetDefaultName;
    property Layout: string read GetLayout write SetLayout;
    property ThemeLight: boolean read GetThemeLight write SetThemeLight;
  end;

  TFileLayoutDao = class(TInterfacedObject, IDao<IFileLayout>)
  published
    function Save(const Entity: IFileLayout): boolean;
    function Delete(const ID: string): boolean;
    function Get(ID: string = ''): TArray<IFileLayout>;
  end;

implementation

uses
  System.IOUtils,
  System.IniFiles,
  System.SysUtils,
  System.Classes;

{ TFileLayout }

constructor TFileLayout.Create;
begin
  FDefaultDirectory := EmptyStr;
  FDefaultName := EmptyStr;
  FLayout := EmptyStr;
end;

function TFileLayout.GetDefaultDirectory: string;
begin
  Result := FDefaultDirectory;
end;

function TFileLayout.GetDefaultName: string;
begin
  Result := FDefaultName;
end;

function TFileLayout.GetLayout: string;
begin
  Result := FLayout;
end;

function TFileLayout.GetThemeLight: boolean;
begin
  Result := FThemeLight;
end;

procedure TFileLayout.SetDefaultDirectory(const Value: string);
begin
  FDefaultDirectory := Value;
end;

procedure TFileLayout.SetDefaultName(const Value: string);
begin
  FDefaultName := Value;
end;

procedure TFileLayout.SetLayout(const Value: string);
begin
  FLayout := Value;
end;

procedure TFileLayout.SetThemeLight(const Value: boolean);
begin
  FThemeLight := Value;
end;

{ TFileLayoutDao }

function TFileLayoutDao.Delete(const ID: string): boolean;
begin
  DeleteFile(ID);
end;

function TFileLayoutDao.Get(ID: string): TArray<IFileLayout>;
var
  vIniFile: TIniFile;
  vFile: TStrings;
begin

  SetLength(Result,1);
  Result[0] := TFileLayout.Create;

  if not FileExists(ChangeFileExt(ParamStr(0),'.db')) then
  begin
    Exit;
  end;

  vIniFile := TIniFile.Create(ChangeFileExt(ParamStr(0),'.db'));
  try
    Result[0].DefaultDirectory := vIniFile.ReadString('LAYOUT', 'DIRECTORY', EmptyStr);
    Result[0].DefaultName := vIniFile.ReadString('LAYOUT', 'NAME', EmptyStr);
    Result[0].ThemeLight := vIniFile.ReadBool('LAYOUT', 'THEME', True);
  finally
    vIniFile.Free;
  end;

  if not FileExists(ChangeFileExt(ParamStr(0),'.layout')) then
  begin
    Exit;
  end;

  vFile := TStringList.Create;
  try
    vFile.LoadFromFile(ChangeFileExt(ParamStr(0),'.layout'));
    Result[0].Layout := vFile.Text;
  finally
    vFile.Free;
  end;

end;

function TFileLayoutDao.Save(const Entity: IFileLayout): boolean;
var
  vIniFile: TIniFile;
  vFile: TStrings;
begin
  vIniFile := TIniFile.Create(ChangeFileExt(ParamStr(0),'.db'));
  try
    vIniFile.WriteString('LAYOUT', 'DIRECTORY', Entity.DefaultDirectory);
    vIniFile.WriteString('LAYOUT', 'NAME', Entity.DefaultName);
    vIniFile.WriteBool('LAYOUT', 'THEME', Entity.ThemeLight);
  finally
    vIniFile.Free;
  end;

  vFile := TStringList.Create;
  try
    vFile.Add(Entity.Layout);
    vFile.SaveToFile(ChangeFileExt(ParamStr(0),'.layout'));
  finally
    vFile.Free;
  end;

end;

end.
