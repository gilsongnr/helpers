{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ghelpers_db;

{$warn 5023 off : no warning about unused units}
interface

uses
  helpers.DB, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ghelpers_db', @Register);
end.
