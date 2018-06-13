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

unit Helpers.Menus;

{$i gdefines.inc}

interface

uses
  Menus, Classes;

type
  TMenuItemHelper = class helper for TMenuItem
  public
    function AddItem(aItem: TMenuItem): TMenuItem;
    function InsertItem(aIndex: Integer; aItem: TMenuItem): TMenuItem;
    procedure AddTo(aParent: TMenuItem);
    procedure InsertTo(aParent: TMenuItem; aIndex: Integer);
    function AddNewItem(const aCaption: string; aImagIndex: Integer = -1; aOnClick: TNotifyEvent = nil): TMenuItem; overload;
    function AddNewItem(const aCaption: string; aOnClick: TNotifyEvent): TMenuItem; overload;
    function AddNewItem(aPosition: Integer; const aCaption: string; aOnClick: TNotifyEvent = nil): TMenuItem; overload;
    function CreateClone(aOwner: TComponent): TMenuItem; overload;
    function CreateClone(): TMenuItem; overload;
  end;

implementation

{ TMenuItemHelper }

function TMenuItemHelper.AddItem(aItem: TMenuItem): TMenuItem;
begin
  Self.Add(aItem);
  Result := aItem;
end;

function TMenuItemHelper.InsertItem(aIndex: Integer; aItem: TMenuItem): TMenuItem;
begin
  Self.Insert(aIndex, aItem);
  Result := aItem;
end;

procedure TMenuItemHelper.InsertTo(aParent: TMenuItem; aIndex: Integer);
begin
  aParent.Insert(aIndex, Self);
end;

procedure TMenuItemHelper.AddTo(aParent: TMenuItem);
begin
  aParent.Add(Self);
end;

function TMenuItemHelper.AddNewItem(const aCaption: string; aImagIndex: Integer; aOnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  with Result do
  begin
    Caption     := aCaption;
    OnClick     := aOnClick;
    ImageIndex  := aImagIndex;
  end;
  Add(Result);
end;

function TMenuItemHelper.AddNewItem(const aCaption: string; aOnClick: TNotifyEvent): TMenuItem;
begin
  Result := AddNewItem(aCaption, -1, aOnClick);
end;

function TMenuItemHelper.AddNewItem(aPosition: Integer; const aCaption: string; aOnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  with Result do
  begin
    Caption := aCaption;
    OnClick := aOnClick;
  end;
  Insert(aPosition, Result);;
end;

function TMenuItemHelper.CreateClone(aOwner: TComponent): TMenuItem;
begin
  Result := TMenuItem.Create(aOwner);
  Result.Caption    := Caption;
  Result.ImageIndex := ImageIndex;
  Result.OnClick    := OnClick;
end;

function TMenuItemHelper.CreateClone: TMenuItem;
begin
  Result := CreateClone(Self);
end;

end.
