library dxfwrite;

{$mode objfpc}{$H+}

uses
  Classes,sysutils
  ;

function ScriptDefinition : PChar;stdcall;
begin
  Result := ''
       +#10+''
            ;
end;

procedure ScriptCleanup;stdcall;
begin
end;

exports

  ScriptDefinition,
  ScriptCleanup;
end.
