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

unit Helpers.List;  

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface

uses
  {$IFDEF FPC}
  //Generics.Collections,
  {$ELSE}
  //Generics.Collections,
  {$ENDIF}
  fgl, Classes;

type

  TEnumeratorReverse<T> = class(TEnumerator<T>)
  private
     type TEnumerator = TEnumeratorReverse<T>;
  private
    FList: TList<T>;
    FFIndex: Integer;
    function GetCurrent: T;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(const AList: TList<T>);
    property Current: T read GetCurrent;
    function MoveNext: Boolean;
    function GetEnumerator: TEnumerator;
  end;

   THelperList = class
   public
    class function GetEnumeratorReverse<T>(aList: TList<T>): TEnumeratorReverse<T>;
  end;

implementation

{ THelperList }

class function THelperList.GetEnumeratorReverse<T>(aList: TList<T>): TEnumeratorReverse<T>;
begin
  Result := TEnumeratorReverse<T>.Create(aList);
end;

{ TEnumeratorReverse<T> }

constructor TEnumeratorReverse<T>.Create(const AList: TList<T>);
begin
  inherited Create;
  FList   := AList;
  FFIndex := FList.Count;
end;

function TEnumeratorReverse<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TEnumeratorReverse<T>.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TEnumeratorReverse<T>.GetCurrent: T;
begin
  Result := FList[FFIndex];
end;

function TEnumeratorReverse<T>.MoveNext: Boolean;
begin
  if FFIndex < 0 then
    Exit(False);
  Dec(FFIndex);
  Result := FFIndex >= 0;
end;

function TEnumeratorReverse<T>.GetEnumerator: TEnumerator;
begin
  Result := Self;
end;

end.
