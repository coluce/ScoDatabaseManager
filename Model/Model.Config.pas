unit Model.Config;

interface

uses
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.StorageXML,
  Data.DB,
  System.Classes,
  Model.Interfaces;

type

  TDataBaseConfig = class(TInterfacedObject, IDataBaseConfig)
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

  TDataBaseConfigDao = class(TInterfacedObject, IDao<IDataBaseConfig>)
  private
    FDados: TFDMemTable;
    FFileName: string;
    procedure SaveToFile;
    procedure LoadFromFile;
    procedure EntityToDataSet(const Entity: IDataBaseConfig);
    function DataSetToEntity: IDataBaseConfig;
  public
    constructor Create;
    destructor Destroy; override;
  published
    function Save(const Entity: IDataBaseConfig): boolean;
    function Delete(const ID: string): boolean;
    function Get(ID: string = ''): TArray<IDataBaseConfig>;
  end;

implementation

uses
  System.TypInfo,
  System.Rtti,
  System.Variants,
  System.SysUtils,
  FireDAC.Stan.Intf,
  Model.Factory;

{ TDataBaseConfigDao }

constructor TDataBaseConfigDao.Create;
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

function TDataBaseConfigDao.DataSetToEntity: IDataBaseConfig;
var
  Contexto: TRttiContext;
  Tipo: TRttiType;
  Propriedade: TRttiProperty;
  Valor: variant;
  Componente: TComponent;
begin
  Result := TDataBaseConfigFactory.Config;
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

function TDataBaseConfigDao.Delete(const ID: string): boolean;
begin
  Result := False;
  if FDados.Locate('ID',VarArrayOf([ID]),[loCaseInsensitive]) then
  begin
    FDados.Delete;
    SaveToFile;
    Result := True;
  end;
end;

destructor TDataBaseConfigDao.Destroy;
begin
  FreeAndNil(FDados);
  inherited;
end;

procedure TDataBaseConfigDao.EntityToDataSet(const Entity: IDataBaseConfig);
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

function TDataBaseConfigDao.Get(ID: string = ''): TArray<IDataBaseConfig>;
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

procedure TDataBaseConfigDao.LoadFromFile;
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

function TDataBaseConfigDao.Save(const Entity: IDataBaseConfig): boolean;
begin
  EntityToDataSet(Entity);
end;

procedure TDataBaseConfigDao.SaveToFile;
begin
  FDados.SaveToFile(FFileName, TFDStorageFormat.sfXML);
end;

{ TConfig }

function TDataBaseConfig.GetDataBase: string;
begin
  Result := FDataBase;
end;

function TDataBaseConfig.GetDescription: string;
begin
  Result := FDescription;
end;

function TDataBaseConfig.GetID: string;
begin
  Result := FID;
end;

function TDataBaseConfig.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TDataBaseConfig.SetDataBase(const Value: string);
begin
  FDataBase := Value;
end;

procedure TDataBaseConfig.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TDataBaseConfig.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TDataBaseConfig.SetServerName(const Value: string);
begin
  FServerName := Value;
end;

end.
