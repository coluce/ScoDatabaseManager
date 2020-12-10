unit Model.Imp.Firebird.Functions;

interface

type
  TModelFirebirdFunctions = class
  private
    FDLLPath: string;
  public
    constructor Create(const ADLLPath: string);
    function GetExecutionPlan(const ASQL: string): string;
  end;

implementation

uses
  Windows, System.SysUtils;

{ TModelFirebirdFunctions }

constructor TModelFirebirdFunctions.Create(const ADLLPath: string);
begin
  FDLLPath := ADLLPath;
end;

function TModelFirebirdFunctions.GetExecutionPlan(const ASQL: string): string;
var
  vHandle: THandle;
  //isc_dsql_sql_info: TDLLFunc;
begin
  Result := EmptyStr;
  try
    // load dll in dinamic type(mode)
    vHandle := LoadLibrary(PWideChar(FDLLPath));

    if vHandle <> 0 then
    begin
      // get function address
//      @isc_dsql_sql_info := getProcAddress(vHandle, 'isc_dsql_sql_info');
//
//      // if function address exists
//      if Addr( isc_dsql_sql_info ) <> nil then
//      begin
//        Result := isc_dsql_sql_info(ASQL);
//      end;

    end;

  finally
    freeLibrary (vHandle);
  end;
end;

end.
