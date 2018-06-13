program project1;

{$i gdefines.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, db, BufDataset, helpers.DB
  { you can add units after this };

type

  { TTestModel }

  TTestModel = class
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  strict private            
    FTab    : TBufDataset;
    FID     : TIntegerField;
    FName   : TStringField;
    FNameEx : TStringField;
    FPrice  : TFloatField;
  public
    property Tab    : TBufDataset     read FTab;
    property ID     : TIntegerField   read FID;
    property Name   : TStringField    read FName;
    property NameEx : TStringField    read FNameEx;
    property Price  : TFloatField     read FPrice;
    function LocateContaning(const s: string): Boolean;
    function LocateContaningAll(const s: string): Boolean;
  end;

procedure TTestModel.AfterConstruction;
begin
  inherited AfterConstruction;
  FTab    := TBufDataset.Create(nil){%H-};
  FID     := TIntegerField.CreateAs(Tab, 'ID');
  FName   := TStringField.CreateAs(Tab, 'NAME', 50, 'Nome');
  FNameEx := TStringField.CreateAs(Tab, 'NAMEEX', 50, 'Nome extra');
  FPrice  := TFloatField.CreateAs(Tab, 'PRICE', '$ venda');
  Tab.CreateDataset;
end;

destructor TTestModel.Destroy;
begin
  FTab.Free;
  inherited Destroy;
end;

function TTestModel.LocateContaning(const s: string): Boolean;
begin
  Result := Tab.LocateContaing(NameEx.FieldName, s, False, True);
end;

function TTestModel.LocateContaningAll(const s: string): Boolean;
var
  vValue: string;

  function _CompareNoCase(ds: TDataSet): Boolean;
  begin
    Result := AnsiPos(vValue, UpperCase(Name.Value)) > 0;
    if not Result then
      Result := AnsiPos(vValue, UpperCase(NameEx.AsString)) > 0;
  end;

begin
  vValue  := UpperCase(s);
  Result  := Tab.LocateCustom(_CompareNoCase, True);
end;

procedure Show(aDta: TTestModel);
begin
  WriteLn(Format('%s: %.3d ', [aDta.ID.DisplayName, aDta.ID.Value]),
            Format('%s: %5.2n ', [aDta.Price.DisplayName, aDta.Price.Value]),
            aDta.Name.DisplayName, aDta.Name.Value, ' / ', aDta.NameEx.Value
            {Format('%s: %-30s ', [aDta.Name.DisplayName, aDta.Name.Value]),
            Format('%s: %s ', [aDta.NameEx.DisplayName, aDta.NameEx.Value])});
end;    

procedure ShowAll(aDta: TTestModel);
var
  ds: TDataSet;
begin
  for ds in aDta.Tab.GetEnumerator() do
    Show(aDta);
end;

procedure Test();
var
  o: TTestModel;
begin
  o := TTestModel.Create();
  try
    o.Tab.Open;
    o.Tab.Append;
    o.ID.Value      := 1;
    o.Name.Value    := 'Primeiro produto teste';
    o.NameEx.Value  := 'vende somente no PDV';
    o.Price.Value   := 10.75;
    o.Tab.Post;


    o.Tab.Append;
    o.ID.Value      := 2;
    o.Name.Value    := 'Segundo produto';
    o.NameEx.Value  := 'Esse eh um produto teste';
    o.Price.Value   := 7.57;
    o.Tab.Post;     


    o.Tab.Append;
    o.Tab.SetFieldsValues(['id=3', 'price=40,5', 'name=farinha de trigo', 'NameEx=brinde']);
    o.Tab.Post;


    o.Tab.Append;
    o.Tab.SetFieldsValues(['id=5', 'price=1,53', 'name=arroz agulhinha', 'NameEx=teste']);
    o.Tab.Post;

    WriteLn('Show All');
    ShowAll(o);

    WriteLn('');
    WriteLn('Show testes');
    o.Tab.First;
    while o.LocateContaning('teste') do
      Show(o);


    WriteLn('');
    WriteLn('Show testes 2');
    o.Tab.First;
    while o.LocateContaningAll('teste') do
      Show(o);
  finally
    o.Free;
  end;
end;

begin

  test();
  ReadLn;
end.

