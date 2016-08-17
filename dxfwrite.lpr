library dxfwrite;

{$mode objfpc}{$H+}

uses
  Classes,sysutils,DXF_read,DXF_write,DXF_structs,DXF_Utils
  ;

var
  aFile : DXF_Object;
  ActiveLayer: DXF_Layer;
  APoints : array[0..max_vertices_per_polyline-1] of Point3D;
  NumPoints : Integer;

function LoadImage(aPath : PChar) : Boolean;stdcall;
var
  Skipped: Tstrings;
begin
  try
    Skipped := TStringList.Create;
    if Assigned(aFile) then FreeAndNil(aFile);
    aFile := DXF_Object.Create_from_file(aPath,Skipped);
  finally
    Skipped.Free;
  end;
end;
function SaveImage(aPath : PChar) : Boolean;stdcall;
begin
  if not Assigned(aFile) then aFile := DXF_Object.create('promet-erp');
  aFile.save_to_file(aPath);
end;
function AddLayer(aName : PChar) : Boolean;
begin
  Result:=False;
  if not Assigned(aFile) then exit;
  ActiveLayer := aFile.new_layer(aName,False);
  Result := Assigned(ActiveLayer);
end;
procedure InitFile;
begin
  if not Assigned(aFile) then
    begin
      aFile := DXF_Object.create('promet-erp');
      AddLayer('default');
    end;
end;
procedure StartPolyLine(x,y,z : Double);stdcall;
begin
  InitFile;
  NumPoints := 1;
  APoints[0].x:=x;
  APoints[0].y:=y;
  APoints[0].z:=z;
end;
procedure PolyLinePoint(x,y,z : Double);stdcall;
begin
  inc(NumPoints,1);
  APoints[NumPoints-1].x:=x;
  APoints[NumPoints-1].y:=y;
  APoints[NumPoints-1].z:=z;
end;
procedure EndPolyLine;stdcall;
var
  aLine: Polyline_;
begin
  aLine := Polyline_.create(origin3D,NumPoints,@APoints[0],0,False);
  ActiveLayer.add_entity_to_layer(aLine);
end;
procedure ClosePolyLine;stdcall;
var
  aLine: Polyline_;
begin
  aLine := Polyline_.create(origin3D,NumPoints,@APoints[0],0,False);
  ActiveLayer.add_entity_to_layer(aLine);
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function LoadImage(aPath : PChar) : Boolean;stdcall;'
       +#10+'function SaveImage(aPath : PChar) : Boolean;stdcall;'
       +#10+'function AddLayer(aName : PChar) : Boolean;'
       +#10+'procedure StartPolyLine(x,y,z : Double);stdcall;'
       +#10+'procedure PolyLinePoint(x,y,z : Double);stdcall;'
       +#10+'procedure EndPolyLine;stdcall;'
       +#10+'procedure ClosePolyLine;stdcall;'
            ;
end;

procedure ScriptCleanup;stdcall;
begin
  ActiveLayer:=nil;
  FreeAndNil(aFile);
end;

exports
  LoadImage,
  SaveImage,
  AddLayer,
  StartPolyLine,
  PolyLinePoint,
  EndPolyLine,
  ClosePolyLine,

  ScriptDefinition,
  ScriptCleanup;

initialization
  aFile := nil;
  ActiveLayer:=nil;
end.
