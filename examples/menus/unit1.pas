unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, helpers.Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
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
begin
  inherited Create(TheOwner);
  Self.Menu := TMainMenu.Create(Self);
  Self.Menu.Images := ImageList1;
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

