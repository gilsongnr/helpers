program project1;

{$i gdefines.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, sysutils, helpers.Exceptions
  { you can add units after this };

const
  cSendFail = 1;
  cReadFail = 2;

function Send(): Boolean;
begin
  Result := True;          
  writeln('Send ', Result);
end;

function Read(): Boolean;
begin
  Result := False;         
  writeln('Read ', Result);
end;

procedure Test1();
begin
  if not Send() then
    raise ExceptionCoded.Create(cSendFail, 'Send failed');

  if not Read() then
    raise ExceptionCoded.Create(cReadFail, 'Read failed');
end;

procedure Test();
var
  c: Char;
begin
  try
    Test1();
  except
    on e: ExceptionCoded do
    begin
      if e.ErroCode = cReadFail then
      begin
        WriteLn('no readed, try read again? Y/N');
        ReadLn(c);
        if upcase(c) = 'Y' then
        begin
          WriteLn('read special OK');
          Exit;
        end;
      end;
      raise;
    end else raise;
  end;
end;

begin
  try
    test();
  except
    WriteLn(Exception.GetMessageExcept());
  end;
  WriteLn('');
  WriteLn('press return key to exit');
  ReadLn;
end.

