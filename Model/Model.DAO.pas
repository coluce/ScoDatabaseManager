unit Model.DAO;

interface

type
  IDao<T> = interface
    ['{2DC064A3-ABBE-4CD0-BF2D-CED9D5917AAF}']
    function Save(const Entity: T): boolean;
    function Delete(const ID: string): boolean;
    function Get(ID: string = ''): TArray<T>;
  end;

implementation

end.
