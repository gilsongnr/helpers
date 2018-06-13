{*******************************************************************
 Project: Classes helpers

  Copyright (C) 2017 Gilson Nunes Rodrigues

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
  Gilson Nunes Rodrigues - gilson.gnr@gmail.com  
*******************************************************************}

unit Helpers.DB;

interface

{$i gdefines.inc}

uses
  DB, Classes, SysUtils{, uHelperStrings};

type

  { THelperField }

  THelperField = class helper for TField
  public
    constructor CreateAs(aOwner: TDataSet; const aFieldName: string;
                  const aDisplayLabel: string = ''; aRequired: Boolean = False);
  public
    function AsChar(aDef: AnsiChar = #0; aNotNull: Boolean = False): AnsiChar;
    function AsTrim(): string;
    {$IFnDef DelphiNew}
    function AsAnsiString(): AnsiString; inline;
    {$ifdef delphiOld}
    function AsLargeint: Largeint;
    {$endif}

    {$endIF}
    function OldValueAsInteger(): Integer;
    function OldValueAsCurrency(): Currency;
    function OldValueAsBoolean(): Boolean;
    function OldValueAsString(): string;
    function CreateExcptInvalidValue(aDisplayText: Boolean): Exception;
    function GetAsIntegerDef(aDef: Integer): Integer;
    function GetAsFloatDef(const aDef: Double): Double;
  end;

  { HelperStringField }

  HelperStringField = class helper for TStringField
  public
    constructor CreateAs(AOwner: TDataSet; const aFieldName: string; aSize: Integer;
                                              const aDisplayLabel: string = '';
                                              aRequired: Boolean = False);
  end;

  { HelperNumericField }

  HelperNumericField = class helper for TNumericField
  public
    constructor CreateAs(AOwner: TDataSet; const aFieldName: string;
                              const aDisplayLabel : string      = '';
                              aRequired           : Boolean     = False;
                              const aDisplayFormat: string      = '';
                              const aEditFormat   : string      = '';
                              const aAlignment    : TAlignment  = taRightJustify);
  end;

  { HelperDateField }

  HelperDateField = class helper for TDateField
  public
    constructor CreateAs(aOwner: TDataSet; const aFieldName: string; const aDisplayLabel: string = ''; aRequired: Boolean = False);
    class procedure FieldSetText(aSender: TField; const aText: string);
  public
    class var FMaskEditData: string;
  end;

  { HelperBCDField }

  HelperBCDField = class helper for TBCDField
  public
    constructor CreateAs(AOwner: TDataSet; const aFieldName: string; aSize: Integer; const aDisplayLabel: string = ''); overload;
    constructor CreateAs(AOwner: TDataSet; const aFieldName: string; const aDisplayLabel: string = '';
                                                      aRequired: Boolean = False; aCurency: Boolean = False); overload;
  end;

  TCompareRecDataSetProc = function (ds: TDataSet): Boolean is nested;
  //TCompareRecDataSetProc = function (ds: TDataSet): Boolean of object;

  { TDataSetEnumerator }

  TDataSetEnumerator = class
  private
    FRecnoSaved : Integer;
    FDataSet    : TDataSet;
  public
    constructor Create(ADataSet: TDataSet);
    destructor Destroy; override;
    function MoveNext: Boolean;
    property Current: TDataSet read FDataSet;
    function GetEnumerator(): TDataSetEnumerator;
  end;

  { TDataSetHelper }

  TDataSetHelper = class helper for TDataSet
  public
    function  NoEmpty(): Boolean;
    procedure CheckNoEmpty();
    function SetFilter(const aFilter: string): TDataSet;
    procedure CopyRecord(aSrc: TDataset; aAllFieldsRequireds: Boolean = True);
    function LocateCustom(aComparer: TCompareRecDataSetProc; aForward: Boolean = False): Boolean;
    function LocateContaing(const aFieldName, aValue: String; aCaseSensitive: Boolean = True; aForward: Boolean = False): Boolean;
    function GetFieldsValues(): string;
    function SetFieldValue(const aValue: string): TDataSet;
    function SetFieldsValues(aValues: TStrings): TDataSet; overload;
    function SetFieldsValues(const aValues: array of string): TDataSet; overload; 
    function SetFieldsValues(const aValues: string): TDataSet; overload;
    function GetEnumerator(): TDataSetEnumerator;
  end;

  TDataSourceHelper = class helper for TDataSource
  public
    constructor CreateAs(AOwner: TComponent; aDataSet: TDataSet); overload;
    constructor CreateAs(AOwner: TDataSet); overload;
  end;

resourcestring
  rsFieldInvalidValue = '"%s" não é um valor válido para o campo: %s';

implementation

uses
  Variants,
  {$IFDEF FPC} dbconst, {$ELSE} DBConsts, Windows, {$ENDIF}
  {%H-}Math, StrUtils;

{ HelperTField }

constructor THelperField.CreateAs(AOwner: TDataSet; const aFieldName: string; const aDisplayLabel: string;
                                                                                                 aRequired: Boolean);
begin
  Create(AOwner);
  FieldName    := aFieldName;
  DisplayLabel := aDisplayLabel;
  DataSet      := AOwner;
  Required     := aRequired;
end;

function THelperField.AsChar(aDef: AnsiChar; aNotNull: Boolean): AnsiChar;
var
  s: Ansistring;
begin
  s := AsAnsiString;
  if s = '' then
    Result := aDef
  else
  begin
    Result := s[1];
    if not aNotNull then
      if Result = #0 then
        Result := aDef
  end;
end;

function THelperField.AsTrim: string;
begin
  Result := Trim(AsString);
end;

{$IFnDef DelphiNew}
function THelperField.AsAnsiString(): AnsiString;
begin
  Result := AsString;
end;

{$ifdef delphiOld}
function THelperField.AsLargeint: Largeint;
begin
  Result := (Self as TLargeintField).AsLargeInt;
end;
{$endif}

{$endIF}

function THelperField.OldValueAsInteger: Integer;
var
  v: Variant;
begin
  v := OldValue;
  if v = Null then
    Result := 0
  else
    Result := v;
end;

function THelperField.OldValueAsCurrency: Currency;
var
    v: Variant;
begin
  v := OldValue;
  if v = Null then
    Result := 0
  else
    Result := v;
end;

function THelperField.OldValueAsBoolean: Boolean;
var
  v: Variant;
begin
  v := OldValue;
  if v = Null then
    Result := False
  else
    Result := v;
end;

function THelperField.OldValueAsString: string;
begin
  Result := VarToStr(OldValue);
end;

function THelperField.CreateExcptInvalidValue(aDisplayText: Boolean): Exception;
var
  s: string;
begin
  if ADisplayText then
    s := Self.DisplayText
  else
    s := Self.AsString;

  Result := Exception.CreateFmt(rsFieldInvalidValue, [s, DisplayLabel]);
end;

function THelperField.GetAsIntegerDef(aDef: Integer): Integer;
begin
  if IsNull then
    Result := ADef
  else if Self is TStringField then
    Result := StrToIntDef(AsString, aDef)
  else
    Result := AsInteger;
end;

function THelperField.GetAsFloatDef(const aDef: Double): Double;
begin
  if IsNull then
    Result := aDef
  else if Self is TStringField then
    Result := StrToFloatDef(AsString, aDef)
  else
    Result := AsFloat;
end;

{ HelperStringField }

constructor HelperStringField.CreateAs(AOwner: TDataSet; const aFieldName: string;
                aSize: Integer; const aDisplayLabel: string; aRequired: Boolean);
begin
  inherited CreateAs(AOwner, aFieldName, aDisplayLabel, aRequired);
  Size    := aSize;
end;

{ HelperNumericField }

constructor HelperNumericField.CreateAs;
begin
  Create(AOwner);
  FieldName     := aFieldName;
  DisplayLabel  := aDisplayLabel;
  DataSet       := AOwner;
  Required      := aRequired;
  EditFormat    := aEditFormat;
  DisplayFormat := aDisplayFormat;
  EditFormat    := aEditFormat;
  Alignment     := aAlignment;
end;

{ HelperDateField }

constructor HelperDateField.CreateAs(aOwner: TDataSet; const aFieldName, aDisplayLabel: string; aRequired: Boolean);
begin
  Create(AOwner);
  FieldName    := aFieldName;
  DisplayLabel := aDisplayLabel;
  DataSet      := AOwner;
  Required     := aRequired;
  Alignment    := taCenter;
  EditMask     := FMaskEditData;
  OnSetText    := FieldSetText;
end;

class procedure HelperDateField.FieldSetText(aSender: TField; const aText: string);

  function _NumberExist(const s:string): Boolean;
  var
    c: Char;
  begin
    for c in s do
      if c in ['0'..'9'] then
      begin
        Result := True;
        Exit;
      end;

    Result := False;
  end;

begin
  //if Text.NumberExists then
  if _NumberExist(aText) then
    aSender.AsString := aText
  else
    aSender.Clear;
end;

{ HelperBCDField }

constructor HelperBCDField.CreateAs(AOwner: TDataSet; const aFieldName: string; aSize: Integer; const aDisplayLabel: string);
begin
  inherited CreateAs(AOwner, aFieldName, aDisplayLabel);
  Size    := aSize;
end;

constructor HelperBCDField.CreateAs(AOwner: TDataSet; const aFieldName: string; const aDisplayLabel: string;
                                                                            aRequired, aCurency: Boolean);
begin
  inherited CreateAs(AOwner, aFieldName, aDisplayLabel, aRequired);
  Self.currency  := aCurency;
end;

{ TDataSetEnumerator }

constructor TDataSetEnumerator.Create(ADataSet: TDataSet);
begin
  inherited Create();
  FDataSet    := ADataSet;
  FRecnoSaved := 0;
  FDataSet.DisableControls;
end;

destructor TDataSetEnumerator.Destroy;
begin
  if FRecnoSaved > 0 then
    FDataSet.RecNo := FRecnoSaved;

  FDataSet.EnableControls;
  inherited Destroy;
end;

function TDataSetEnumerator.MoveNext: Boolean;
begin
  if FRecnoSaved <> 0 then
    FDataSet.Next
  else
  begin
    FDataSet.First;
    if FDataSet.IsEmpty then
      FRecnoSaved := -1
    else
    begin
      FRecnoSaved := FDataSet.RecNo;
      if FRecnoSaved = 0 then
        FRecnoSaved := -1;
    end;
  end;

  Result := not FDataSet.Eof;
end;

function TDataSetEnumerator.GetEnumerator(): TDataSetEnumerator;
begin
  Result := Self;
end;

{ TDataSetHelper }

function TDataSetHelper.NoEmpty() : Boolean;
begin
  Result := (Self <> nil) and Active and not (Bof and Eof);
end;

procedure TDataSetHelper.CheckNoEmpty();
begin
  if State in dsEditModes then
    Post;

  if not NoEmpty() then
    raise Exception.Create('Não existe registro ativo');
end;

function TDataSetHelper.SetFilter(const aFilter: string): TDataSet;
begin
  Result := Self;
  if Trim(aFilter) = '' then
  begin
    Filtered := False;
    Filter   := '';
  end else
  begin
    Filter   := aFilter;
    Filtered := True;
  end;
end;

procedure TDataSetHelper.CopyRecord(aSrc: TDataset; aAllFieldsRequireds: Boolean);
var
  fs, fd: TField;
begin
  Append;
  try
    for fs in aSrc.Fields do
      if not fs.IsNull then
      begin
        fd := FindField(fs.FieldName);
        if fd <> nil then
          fd.Value := fs.Value
        else if aAllFieldsRequireds then
          DatabaseErrorFmt(SFieldNotFound, [fs.FieldName], Self);
      end;
    Post;
  except
    Cancel;
    raise;
  end;
end;

function TDataSetHelper.LocateCustom(aComparer: TCompareRecDataSetProc; aForward: Boolean) : Boolean;
begin
  CheckBrowseMode;
  if Self.IsEmpty then
  begin
    Result := False;
    Exit;
  end;

  DisableControls;
  try
    if not aForward then
      First
    else if not BOF then
      Next
    else
    begin
      Next;
      Prior;
    end;

    while not EOF do
    begin
      if aComparer(Self) then
      begin
        Result := True;
        Exit;
      end;

      Next;
    end;
  finally
    EnableControls;
  end;
  Result := False;
end;

function TDataSetHelper.LocateContaing(const aFieldName, aValue: String; aCaseSensitive, aForward: Boolean) : Boolean;
var
  vField: TField;
  vValue: string;

  function _CompareCase(ds: TDataSet): Boolean;
  begin
    Result := AnsiPos(aValue, vField.AsString) > 0;
  end;      

  function _CompareNoCase(ds: TDataSet): Boolean;
  begin
    Result := AnsiPos(vValue, UpperCase(vField.AsString)) > 0;
  end;

begin
  vField := FieldByName(aFieldName);
  if aCaseSensitive then
    Result  := Self.LocateCustom(_CompareCase, aForward)
  else
  begin
    vValue  := UpperCase(aValue);
    Result := Self.LocateCustom(_CompareNoCase, aForward);
  end;
end;

function TDataSetHelper.GetFieldsValues() : string;
var
  f: TField;
begin
  Result := '';
  for f in Fields do
    Result := Result + f.FieldName + '=' + f.AsString + sLineBreak;
end;

function TDataSetHelper.SetFieldValue(const aValue : string) : TDataSet;
var
  i: Integer;
begin
  i := Pos('=', aValue);
  if i < 2 then
    raise Exception.CreateFmt('Valor "%s" inválido para SetFieldValue', [aValue]);

  FieldByName(LeftStr(aValue, i-1)).AsString := Copy(aValue, i+1); 
  Result := Self;
end;    

function TDataSetHelper.SetFieldsValues(aValues: TStrings): TDataSet;
var
  s: string;
begin
  Result := Self;
  for s in aValues do
    SetFieldValue(s);
end; 

function TDataSetHelper.SetFieldsValues(const aValues: array of string): TDataSet;
var
  s: string;
begin
  Result := Self;
  for s in aValues do
    SetFieldValue(s);
end;

function TDataSetHelper.SetFieldsValues(const aValues: string): TDataSet;
var
  l: TStrings;
begin
  //for s in TStringEnumerator.Create(aValues) do
  //  SetFieldValue(s);

  l := TStringList.Create;
  try
    l.DelimitedText   :=  ';';
    l.StrictDelimiter :=  True;
    l.DelimitedText   :=  aValues;
    Result := SetFieldsValues(l);
  finally
    l.Free;
  end;
end;

function TDataSetHelper.GetEnumerator() : TDataSetEnumerator;
begin
  Result := TDataSetEnumerator.Create(Self);
end;

{ TDataSourceHelper }

constructor TDataSourceHelper.CreateAs(AOwner: TComponent; aDataSet: TDataSet);
begin
  Create(AOwner);
  DataSet := aDataSet;
end;

constructor TDataSourceHelper.CreateAs(AOwner: TDataSet);
begin
  CreateAs(AOwner, AOwner);
end;


initialization
  TDateField.FMaskEditData := '!99/99/9999;1; ';

end.
