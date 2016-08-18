library dxfwrite;

{$mode objfpc}{$H+}

uses
  Classes,sysutils,DXF_read,DXF_write,DXF_structs,DXF_Utils
  ;

var
  aFile : DXF_Object;
  aWorkFile : DXF_Object;
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
    if FileExists(aPath) then
      aFile := DXF_Object.Create_from_file(aPath,Skipped);
  finally
    Skipped.Free;
  end;
end;
function ReloadWorkImage(aPath : PChar) : Boolean;stdcall;
var
  Skipped: Tstrings;
begin
  try
    Skipped := TStringList.Create;
    if Assigned(aWorkFile) then FreeAndNil(aWorkFile);
    if FileExists(aPath) then
      aWorkFile := DXF_Object.Create_from_file(aPath,Skipped);
  finally
    Skipped.Free;
  end;
end;
function TranslateWorkImage(x,y,z : Double) : Boolean;stdcall;
var
  lp1: Integer;
  layer1: DXF_Layer;
  lp2: Integer;
  elist1: Entity_List;
  lp3: Integer;
  ent: DXF_Entity;
  rPoint : Point3D;
begin
  rPoint.x:=x;
  rPoint.y:=y;
  rPoint.z:=z;
  for lp1:=0 to aWorkFile.layer_lists.Count-1 do begin
    layer1 := DXF_Layer(aWorkFile.layer_lists[lp1]);
    for lp2:=0 to layer1.entity_lists.count-1 do begin
      elist1 := Entity_List(layer1.entity_lists[lp2]);
      for lp3:=elist1.entities.count-1 downto 0 do begin
        ent := DXF_Entity(elist1.entities[lp3]);
        if not (Ent is Block_) then
          ent.translate(rPoint);
      end;
    end;
  end;
end;
procedure InitFile;
begin
  if not Assigned(aFile) then
    begin
      aFile := DXF_Object.create('promet-erp');
      ActiveLayer := aFile.create_or_find_layer('0');
    end;
end;
function MergeWorkImageToImage : Boolean;stdcall;
begin
  InitFile;
  aFile.merge_files(aWorkFile);
end;
function SaveImage(aPath : PChar) : Boolean;stdcall;
begin
  if not Assigned(aFile) then aFile := DXF_Object.create('promet-erp');
  aFile.save_to_file(aPath);
end;
function SaveWorkImage(aPath : PChar) : Boolean;stdcall;
begin
  if not Assigned(aWorkFile) then aWorkFile := DXF_Object.create('promet-erp');
  aWorkFile.save_to_file(aPath);
end;
function AddLayer(aName : PChar) : Boolean;
begin
  Result:=False;
  if not Assigned(aFile) then exit;
  ActiveLayer := aFile.new_layer(aName,False);
  Result := Assigned(ActiveLayer);
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
       +#10+'function ReloadWorkImage(aPath : PChar) : Boolean;stdcall;'
       +#10+'function SaveWorkImage(aPath : PChar) : Boolean;stdcall;'
       +#10+'function AddLayer(aName : PChar) : Boolean;'
       +#10+'procedure StartPolyLine(x,y,z : Double);stdcall;'
       +#10+'procedure PolyLinePoint(x,y,z : Double);stdcall;'
       +#10+'procedure EndPolyLine;stdcall;'
       +#10+'procedure ClosePolyLine;stdcall;'
       +#10+'function TranslateWorkImage(x,y,z : Double) : Boolean;stdcall;'
       +#10+'function MergeWorkImageToImage : Boolean;stdcall;'
            ;
end;

procedure ScriptCleanup;stdcall;
begin
  ActiveLayer:=nil;
  FreeAndNil(aFile);
  FreeAndNil(aWorkFile);
end;

exports
  LoadImage,
  SaveImage,
  ReloadWorkImage,
  SaveWorkImage,
  AddLayer,
  StartPolyLine,
  PolyLinePoint,
  EndPolyLine,
  ClosePolyLine,
  TranslateWorkImage,
  MergeWorkImageToImage,

  ScriptDefinition,
  ScriptCleanup;

initialization
  aFile := nil;
  ActiveLayer:=nil;
end.
