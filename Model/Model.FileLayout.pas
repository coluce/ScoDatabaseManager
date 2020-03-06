unit Model.FileLayout;

interface

uses
  Model.DAO;

type
  IFileLayout = interface
    ['{0BA0C01A-91CE-4382-9DFE-7E715873E39E}']

    function GetDefaultDirectory: string;
    function GetDefaultName: string;
    function GetLayout: string;

    procedure SetDefaultDirectory(const Value: string);
    procedure SetDefaultName(const Value: string);
    procedure SetLayout(const Value: string);

    property DefaultDirectory: string read GetDefaultDirectory write SetDefaultDirectory;
    property DefaultName: string read GetDefaultName write SetDefaultName;
    property Layout: string read GetLayout write SetLayout;
  end;

  TFileLayout = class(TInterfacedObject, IFileLayout)
  private
    FDefaultDirectory: string;
    FDefaultName: string;
    FLayout: string;
    function GetDefaultDirectory: string;
    function GetDefaultName: string;
    function GetLayout: string;
    procedure SetDefaultDirectory(const Value: string);
    procedure SetDefaultName(const Value: string);
    procedure SetLayout(const Value: string);
  public
    constructor Create;
  published
    property DefaultDirectory: string read GetDefaultDirectory write SetDefaultDirectory;
    property DefaultName: string read GetDefaultName write SetDefaultName;
    property Layout: string read GetLayout write SetLayout;
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

{ TFileLayoutDao }

function TFileLayoutDao.Delete(const ID: string): boolean;
begin
  DeleteFile(ID);
end;

function TFileLayoutDao.Get(ID: string): TArray<IFileLayout>;
var
  vFile: TIniFile;
begin

  SetLength(Result,1);
  Result[0] := TFileLayout.Create;

  if not FileExists(ChangeFileExt(ParamStr(0),'.db')) then
  begin
    Exit;
  end;

  vFile := TIniFile.Create(ChangeFileExt(ParamStr(0),'.db'));
  try
    Result[0].DefaultDirectory := vFile.ReadString('LAYOUT', 'DIRECTORY', EmptyStr);
    Result[0].DefaultName := vFile.ReadString('LAYOUT', 'NAME', EmptyStr);
    Result[0].Layout := vFile.ReadString('LAYOUT', 'FILE', EmptyStr);
  finally
    vFile.Free;
  end;
end;

function TFileLayoutDao.Save(const Entity: IFileLayout): boolean;
var
  vFile: TIniFile;
begin
  vFile := TIniFile.Create(ChangeFileExt(ParamStr(0),'.db'));
  try
    vFile.WriteString('LAYOUT', 'DIRECTORY', Entity.DefaultDirectory);
    vFile.WriteString('LAYOUT', 'NAME', Entity.DefaultName);
    vFile.WriteString('LAYOUT', 'FILE', Entity.Layout);
  finally
    vFile.Free;
  end;
end;

end.
