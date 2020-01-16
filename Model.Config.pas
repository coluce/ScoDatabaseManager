unit Model.Config;

interface

uses
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.StorageXML,
  Data.DB,
  System.Classes;

type

  IConfig = interface
    ['{53B98FFB-22DC-454C-B54C-467A179E872C}']

    function GetID: string;
    procedure SetID(const Value: string);
    property ID: string read GetID write SetID;

    function GetDescription: string;
    procedure SetDescription(const Value: string);
    property Description: string read GetDescription write SetDescription;

    function GetServerName: string;
    procedure SetServerName(const Value: string);
    property ServerName: string read GetServerName write SetServerName;

    function GetDataBase: string;
    procedure SetDataBase(const Value: string);
    property DataBase: string read GetDataBase write SetDataBase;

  end;

  TConfig = class(TInterfacedObject, IConfig)
  private
    FID: string;
    FDescription: string;
    FServerName: string;
    FDataBase: string;
    function GetID: string;
    procedure SetID(const Value: string);
    function GetServerName: string;
    procedure SetServerName(const Value: string);
    function GetDataBase: string;
    procedure SetDataBase(const Value: string);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
  published
    property ID: string read GetID write SetID;
    property Description: string read GetDescription write SetDescription;
    property ServerName: string read GetServerName write SetServerName;
    property DataBase: string read GetDataBase write SetDataBase;
  end;

  IDao<T> = interface
    ['{2DC064A3-ABBE-4CD0-BF2D-CED9D5917AAF}']
    function Save(const Entity: T): boolean;
    function Delete(const ID: string): boolean;
    function Get(ID: string = ''): TArray<T>;
  end;

  TConfigDao = class(TInterfacedObject, IDao<IConfig>)
  private
    FDados: TFDMemTable;
    FFileName: string;
    procedure SaveToFile;
    procedure LoadFromFile;
    procedure EntityToDataSet(const Entity: IConfig);
    function DataSetToEntity: IConfig;
  public
    constructor Create;
    destructor Destroy; override;
  published
    function Save(const Entity: IConfig): boolean;
    function Delete(const ID: string): boolean;
    function Get(ID: string = ''): TArray<IConfig>;
  end;

  TConfigFactory = class
  public
    class function Config: IConfig;
    class function Dao: IDao<IConfig>;
  end;

implementation

uses
  System.TypInfo,
  System.Rtti, System.Variants, System.SysUtils, FireDAC.Stan.Intf;

{ TConfigDao }

constructor TConfigDao.Create;
begin
  FDados := TFDMemTable.Create(nil);
  FDados.FieldDefs.Add('ID',ftString,50);
  FDados.FieldDefs.Add('Description',ftString,100);
  FDados.FieldDefs.Add('ServerName',ftString,100);
  FDados.FieldDefs.Add('DataBase',ftString,100);
  FDados.CreateDataSet;
  FFileName := 'Dados.xml';
  LoadFromFile;
end;

function TConfigDao.DataSetToEntity: IConfig;
var
  Contexto: TRttiContext;
  Tipo: TRttiType;
  Propriedade: TRttiProperty;
  Valor: variant;
  Componente: TComponent;
begin
  Result := TConfigFactory.Config;
  (*
  // Cria o contexto do RTTI
  Contexto := TRttiContext.Create;
  // Obtém as informações de RTTI da classe TFuncionario
  Tipo := Contexto.GetType(TypeInfo(TConfig));

  try
    // Faz uma iteração nas propriedades do objeto
    for Propriedade in Tipo.GetProperties do
    begin
      if Assigned(FDados.FindField(Propriedade.Name)) then
      begin
        //Propriedade.SetValue(Result, TValue.FromVariant(FDados.FindField(Propriedade.Name).AsVariant));
        Propriedade.SetValue(Result as TObject, TValue.From<string>(FDados.FindField(Propriedade.Name).AsString));
      end;

//      // Obtém o valor da propriedade
//      Valor := Propriedade.GetValue(Funcionario).AsVariant;
//
//      // Encontra o componente relacionado, como, por exemplo, "CampoNome"
//      Componente := FindComponent('Campo' + Propriedade.Name);
//
//      // Testa se o componente é da classe "TEdit" para acessar a propriedade "Text"
//      if Componente is TEdit then
//        (Componente as TEdit).Text := Valor;
//
//      // Testa se o componente é da classe "TComboBox" para acessar a propriedade "ItemIndex"
//      if Componente is TComboBox then
//        (Componente as TComboBox).ItemIndex := (Componente as TComboBox).Items.IndexOf(Valor);
//
//      // Testa se o componente é da classe "TRadioGroup" para acessar a propriedade "ItemIndex"
//      if Componente is TRadioGroup then
//        (Componente as TRadioGroup).ItemIndex := (Componente as TRadioGroup).Items.IndexOf(Valor);
//
//      // Testa se o componente é da classe "TCheckBox" para acessar a propriedade "Checked"
//      if Componente is TCheckBox then
//        (Componente as TCheckBox).Checked := Valor;
//
//      // Testa se o componente é da classe "TTrackBar" para acessar a propriedade "Position"
//      if Componente is TTrackBar then
//        (Componente as TTrackBar).Position := Valor;
//
//      // Testa se o componente é da classe "TDateTimePicker" para acessar a propriedade "Date"
//      if Componente is TDateTimePicker then
//        (Componente as TDateTimePicker).Date := Valor;
//
//      // Testa se o componente é da classe "TShape" para acessar a propriedade "Brush.Color"
//      if Componente is TShape then
//        (Componente as TShape).Brush.Color := Valor;
    end;
  finally
    Contexto.Free;
  end;
  *)
  Result.ID := FDados.FieldByName('ID').AsString;
  Result.Description := FDados.FieldByName('Description').AsString;
  Result.ServerName := FDados.FieldByName('ServerName').AsString;
  Result.DataBase := FDados.FieldByName('DataBase').AsString;
end;

function TConfigDao.Delete(const ID: string): boolean;
begin
  Result := False;
  if FDados.Locate('ID',VarArrayOf([ID]),[loCaseInsensitive]) then
  begin
    FDados.Delete;
    SaveToFile;
    Result := True;
  end;
end;

destructor TConfigDao.Destroy;
begin
  FreeAndNil(FDados);
  inherited;
end;

procedure TConfigDao.EntityToDataSet(const Entity: IConfig);
var
  Contexto: TRttiContext;
  Tipo: TRttiType;
  Propriedade: TRttiProperty;
  Valor: variant;
  Componente: TComponent;
  vOnj: TObject;
begin
  if FDados.Locate('ID',VarArrayOf([Entity.ID]),[loCaseInsensitive]) then
  begin
    FDados.Edit;
  end
  else
  begin
    FDados.Append;
  end;

  (*

  // Cria o contexto do RTTI
  Contexto := TRttiContext.Create;
  // Obtém as informações de RTTI da classe TFuncionario
  Tipo := Contexto.GetType(TypeInfo(TConfig));

  try
    // Faz uma iteração nas propriedades do objeto
    for Propriedade in Tipo.GetProperties do
    begin
//      if not Assigned(FDados.FindField(Propriedade.Name)) then
//      begin
//        case Propriedade.PropertyType.TypeKind of
//          tkInteger,
//          tkEnumeration: FDados.FieldDefs.Add(Propriedade.Name, ftInteger);
//
//          tkChar,
//          tkString,
//          tkWChar,
//          tkLString,
//          tkWString,
//          tkUString: FDados.FieldDefs.Add(Propriedade.Name, ftString, 100);
//
//          tkFloat,
//          tkInt64: FDados.FieldDefs.Add(Propriedade.Name, ftFloat);
//
//        end;
//
//      end;

      // Obtém o valor da propriedade
//      Valor := Propriedade.GetValue(Entity).AsVariant;

      vOnj := TObject(Entity);

      case Propriedade.PropertyType.TypeKind of
        tkInteger,
        tkEnumeration: FDados.FindField(Propriedade.Name).AsInteger := Propriedade.GetValue(vOnj).AsInteger;

        tkChar,
        tkString,
        tkWChar,
        tkLString,
        tkWString,
        tkUString: FDados.FindField(Propriedade.Name).AsString := Propriedade.GetValue(vOnj).AsString;

        tkFloat,
        tkInt64: FDados.FindField(Propriedade.Name).AsFloat := Propriedade.GetValue(vOnj).AsExtended;

      end;

//
//      // Encontra o componente relacionado, como, por exemplo, "CampoNome"
//      Componente := FindComponent('Campo' + Propriedade.Name);
//
//      // Testa se o componente é da classe "TEdit" para acessar a propriedade "Text"
//      if Componente is TEdit then
//        (Componente as TEdit).Text := Valor;
//
//      // Testa se o componente é da classe "TComboBox" para acessar a propriedade "ItemIndex"
//      if Componente is TComboBox then
//        (Componente as TComboBox).ItemIndex := (Componente as TComboBox).Items.IndexOf(Valor);
//
//      // Testa se o componente é da classe "TRadioGroup" para acessar a propriedade "ItemIndex"
//      if Componente is TRadioGroup then
//        (Componente as TRadioGroup).ItemIndex := (Componente as TRadioGroup).Items.IndexOf(Valor);
//
//      // Testa se o componente é da classe "TCheckBox" para acessar a propriedade "Checked"
//      if Componente is TCheckBox then
//        (Componente as TCheckBox).Checked := Valor;
//
//      // Testa se o componente é da classe "TTrackBar" para acessar a propriedade "Position"
//      if Componente is TTrackBar then
//        (Componente as TTrackBar).Position := Valor;
//
//      // Testa se o componente é da classe "TDateTimePicker" para acessar a propriedade "Date"
//      if Componente is TDateTimePicker then
//        (Componente as TDateTimePicker).Date := Valor;
//
//      // Testa se o componente é da classe "TShape" para acessar a propriedade "Brush.Color"
//      if Componente is TShape then
//        (Componente as TShape).Brush.Color := Valor;
    end;
  finally
    Contexto.Free;
  end;

  *)

  FDados.FieldByName('ID').AsString := Entity.ID;
  FDados.FieldByName('Description').AsString := Entity.Description;
  FDados.FieldByName('ServerName').AsString := Entity.ServerName;
  FDados.FieldByName('DataBase').AsString := Entity.DataBase;

  FDados.Post;
  SaveToFile;
end;

function TConfigDao.Get(ID: string = ''): TArray<IConfig>;
begin
  SetLength(Result, 0);
  if not ID.Trim.IsEmpty then
  begin
    if FDados.Locate('ID',VarArrayOf([ID]),[loCaseInsensitive]) then
    begin
      SetLength(Result, 1);
      Result[0] := DataSetToEntity;
    end;
  end
  else
  begin
    FDados.First;
    while not FDados.Eof do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := DataSetToEntity;
      FDados.Next;
    end;
  end;
end;

procedure TConfigDao.LoadFromFile;
var
  Contexto: TRttiContext;
  Tipo: TRttiType;
  Propriedade: TRttiProperty;
  Valor: variant;
  Componente: TComponent;
begin
  if FileExists(FFileName) then
  begin
    FDados.LoadFromFile(FFileName, TFDStorageFormat.sfXML);

    (*
    // Cria o contexto do RTTI
    Contexto := TRttiContext.Create;
    // Obtém as informações de RTTI da classe TFuncionario
    Tipo := Contexto.GetType(TypeInfo(TConfig));

    try
      // Faz uma iteração nas propriedades do objeto
      for Propriedade in Tipo.GetProperties do
      begin
        if not Assigned(FDados.FindField(Propriedade.Name)) then
        begin
          case Propriedade.PropertyType.TypeKind of
            tkInteger,
            tkEnumeration: FDados.FieldDefs.Add(Propriedade.Name, ftInteger);

            tkChar,
            tkString,
            tkWChar,
            tkLString,
            tkWString,
            tkUString: FDados.FieldDefs.Add(Propriedade.Name, ftString, 100);

            tkFloat,
            tkInt64: FDados.FieldDefs.Add(Propriedade.Name, ftFloat);

          end;

        end;
      end;

      FDados.FieldDefs.Update;

    finally
      Contexto.Free;
    end;
    *)

  end;
end;

function TConfigDao.Save(const Entity: IConfig): boolean;
begin
  EntityToDataSet(Entity);
end;

procedure TConfigDao.SaveToFile;
begin
  FDados.SaveToFile(FFileName, TFDStorageFormat.sfXML);
end;

{ TConfig }

function TConfig.GetDataBase: string;
begin
  Result := FDataBase;
end;

function TConfig.GetDescription: string;
begin
  Result := FDescription;
end;

function TConfig.GetID: string;
begin
  Result := FID;
end;

function TConfig.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TConfig.SetDataBase(const Value: string);
begin
  FDataBase := Value;
end;

procedure TConfig.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TConfig.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TConfig.SetServerName(const Value: string);
begin
  FServerName := Value;
end;

{ TConficFactory }

class function TConfigFactory.Dao: IDao<IConfig>;
begin
  Result := TConfigDao.Create;
end;

class function TConfigFactory.Config: IConfig;
begin
  Result := TConfig.Create;
end;

end.
