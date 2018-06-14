unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, helpers.Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
  public
    constructor Create(TheOwner: TComponent); override;  
  private
    procedure MenuTest(aSender: TObject);
    procedure miExit(aSender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent);
var
  s: TMemoryStream;
begin
  inherited Create(TheOwner);
  Self.Menu        := TMainMenu.Create(Self);
  Self.Menu.Images := TImageList.Create(Self);

  s := TMemoryStream.Create;
  try
    s.LoadFromFile('./imagelistdta');
    s.Position := 0;
    Menu.Images.ReadData(s);
  finally
    s.Free;
  end;

  with Self.Menu.Items do
  begin
    with AddNewItem('file') do
    begin
      AddNewItem('Open', @MenuTest);
      AddNewItem('Close', @MenuTest);
      AddNewItem('Exit', 1, @miExit);
    end;

    with AddNewItem('Edit') do
    begin                           
      AddNewItem('Find', 2, @MenuTest);  
      AddNewItem('-');
      AddNewItem('Select', @MenuTest);
      AddNewItem('Copy', @MenuTest);
      AddNewItem('Past', @MenuTest);
    end;

    with AddNewItem('Tools') do
    begin           
      AddNewItem('Calc', @MenuTest);
      AddNewItem('www', @MenuTest);
      AddNewItem('Free Pascal', 0, @MenuTest);
    end;
  end;

  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.Images := Menu.Images;
  PopupMenu.Items.CopySubItens(Menu.Items[1]);
end;

procedure TForm1.MenuTest(aSender: TObject);
begin
  ShowMessage((aSender as TMenuItem).Caption);
end;

procedure TForm1.miExit(aSender: TObject);
begin
  Close;
end;

end.

