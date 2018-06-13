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

unit helpers.Exceptions;

{$i gdefines.inc}

interface

uses
  {$ifdef MsWindows}
  Windows,
  {$else}
  Unix, BaseUnix,
  {$Endif}
  Classes, SysUtils;

type

  { THelperException }

  THelperException = class helper for Exception
  public
    class function GetMessageFrom(aExceptObject: TObject): string; //static;
    class function GetMessageExcept(): string; //static;
  end;

  { EOSErrorEx }

  EOSErrorEx = class(EOSError)
  public
    constructor CreateAs(const aMsg: string; aError: Integer; const aDescError: string); overload;
    constructor CreateAs(const aMsg: string; aError: Integer); overload;
    constructor Create(const aMsg: string); 
    constructor CreateFmtAs(const aMsg: string; const Args: array of const; aError: Integer);
    constructor CreateFmt(const aMsg: string; const Args: array of const);

    function GetIsAccessDenied(): Boolean; overload;
    function GetIsFileNotFound(): Boolean; overload;
    class function GetLast(): Integer;
    class function GetIsAccessDenied(aErrorCode: Integer): Boolean; overload;
    class function GetIsFileNotFound(aErrorCode: Integer): Boolean; overload;
    class function GetDescriptionError(aErrorCode: Integer): string; overload;
    class function GetDescriptionError(): string; overload;
    class function GetDescError(aErrorCode: Integer): String; overload; deprecated;
    class function GetDescError(): String; overload; deprecated;
  end;

  { EExceptExcept }

  EExceptExcept = class(Exception)
  public
    constructor Create(const msg: string; aException: TObject); overload;
    constructor Create(const msg: string); overload;
    constructor CreateFmt(const Msg: string; const Args: array of const);
  end;

  { ExceptionCoded }

  ExceptionCoded = class(Exception)
  public
    constructor Create(aErroCode: Integer; const aMsg: string); reintroduce; overload;
    constructor Create(aErroCode: Integer); overload;
  private
    FErroCode: Integer;
  public
    property ErroCode: Integer   read FErroCode;
  end;

implementation

{ THelperException }

class function THelperException.GetMessageFrom(aExceptObject: TObject): string;
begin
  if aExceptObject = nil then
    Result := '[nula]'
  else if aExceptObject is Exception then
    Result := Exception(aExceptObject).Message
  else
    Result := '['+aExceptObject.ClassName+']';
end;

class function THelperException.GetMessageExcept(): string;
begin
  Result := GetMessageFrom(ExceptObject);
end;

{ EOSErrorEx }

constructor EOSErrorEx.CreateAs(const aMsg: string; aError: Integer; const aDescError: string);
begin
  if aDescError <> '' then
    inherited Create(aMsg + sLineBreak + aDescError)
  else
    inherited Create(aMsg);

  Self.ErrorCode := aError;
end;

constructor EOSErrorEx.CreateAs(const aMsg: string; aError: Integer);
{$IFNDEF MSWINDOWS}
const
  ERROR_SUCCESS = 0;
{$ENDIF}
begin
  if aError <> ERROR_SUCCESS then
    inherited Create(aMsg + sLineBreak + GetDescriptionError(aError))
  else
    inherited Create(aMsg);

  Self.ErrorCode := aError;
end;

constructor EOSErrorEx.Create(const aMsg: string);
begin
  CreateAs(aMsg, GetLast());
end;

constructor EOSErrorEx.CreateFmtAs(const aMsg: string; const Args: array of const; aError: Integer);
begin
  CreateAs(Format(aMsg, Args), aError);
end;

constructor EOSErrorEx.CreateFmt(const aMsg: string; const Args: array of const);
begin
  Create(Format(aMsg, Args));
end;

function EOSErrorEx.GetIsAccessDenied: Boolean;
begin
  Result := GetIsAccessDenied(ErrorCode);
end;

function EOSErrorEx.GetIsFileNotFound: Boolean;
begin
  Result := GetIsFileNotFound(ErrorCode);
end;

class function EOSErrorEx.GetLast(): Integer;
begin
  {$IFDEF FPC}
  Result := SysUtils.GetLastOSError();
  {unix errno^}
  {$ELSE}
  Result := Windows.GetLastError();
  {$ENDIF}
end;

class function EOSErrorEx.GetIsAccessDenied(aErrorCode: Integer): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := aErrorCode = 5;
  {$ELSE}
  {$IFDEF LINUX}
  Result := aErrorCode = ESysEACCES;
  {$ELSE}
  raise ENoimplement.Create(Self, 'GetIsAccessDenied');
  {$ENDIF}
  {$ENDIF}
end;

class function EOSErrorEx.GetIsFileNotFound(aErrorCode: Integer): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := aErrorCode = 2;
  {$ELSE}
  {$IFDEF LINUX}
  Result := aErrorCode = ESysENOENT;
  {$ELSE}
  raise ENoimplement.Create(Self, 'GetIsFileNotFound');
  {$ENDIF}
  {$ENDIF}
end;

function _GetSysErrorMessage(aErrorCode: Integer): String; inline;
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
  {$define NoGetSysErrorMessage}
  {$ENDIF}
{$endif}

{$IFDEF NoGetSysErrorMessage}
const
  MaxMsgSize = Format_Message_Max_Width_Mask;
var
  MsgBuffer: PAnsiChar;
  i: DWORD;
  r: unicodestring;
begin
  GetMem(MsgBuffer, MaxMsgSize);
  try
    //FillChar(MsgBuffer^, MaxMsgSize, #0);
    i := FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM,
                   nil,
                   aErrorCode,
                   MakeLangId(LANG_NEUTRAL, SUBLANG_DEFAULT),
                   MsgBuffer,                 
                   MaxMsgSize,                
                   nil);

    widestringmanager.Ansi2UnicodeMoveProc(MsgBuffer, CP_ACP, r{%H-}, i);
  finally
    FreeMem(MsgBuffer, MaxMsgSize);
  end;
  Result := String(r);
  {unix strerror(aErrorCode)}
end;
{$ELSE}
begin
  Result := SysUtils.SysErrorMessage(aErrorCode);
end;
{$ENDIF}

class function EOSErrorEx.GetDescriptionError(aErrorCode: Integer): String;
begin
  Result := IntToStr(aErrorCode) + ' - ' + _GetSysErrorMessage(aErrorCode);
end;

class function EOSErrorEx.GetDescriptionError(): String;
begin
  Result := GetDescriptionError(GetLast());
end;

class function EOSErrorEx.GetDescError(aErrorCode: Integer): String;
begin
  Result := GetDescriptionError(aErrorCode);
end;

class function EOSErrorEx.GetDescError: String;
begin
  Result := GetDescriptionError();
end;

{ EExceptExcept }

constructor EExceptExcept.Create(const msg: string; aException: TObject);
begin
  inherited Create(msg + sLineBreak + sLineBreak + Exception.GetMessageFrom(aException));
end;

constructor EExceptExcept.Create(const msg: string);
begin
  Create(msg, ExceptObject);
end;

constructor EExceptExcept.CreateFmt(const Msg: string; const Args: array of const);
begin
  Create(Format(Msg, Args));
end;

{ ExceptionCoded }

constructor ExceptionCoded.Create(aErroCode: Integer; const aMsg: string);
begin
  CreateFmt('%d - %s', [aErroCode, aMsg]);
  FErroCode := aErroCode;
end;

constructor ExceptionCoded.Create(aErroCode: Integer);
begin
  CreateFmt('Erro: %d', [aErroCode]);
  FErroCode := aErroCode;
end;

end.

