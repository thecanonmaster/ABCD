unit ms3d;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, abcmodeltypes;

const
  STR_PSEUDOHEADER = '// MilkShape 3D ASCII';
  STR_FRAMES = 'Frames: ';
  STR_FRAME = 'Frame: ';
  STR_MESHES = 'Meshes: ';
  STR_MATERIALS = 'Materials: ';
  STR_BONES = 'Bones: ';
  STR_GROUPCOMMS = 'GroupComments: ';
  STR_MATERIALCOMMS = 'MaterialComments: ';
  STR_BONECOMMS = 'BoneComments: ';
  STR_MODELCOMMS = 'ModelComment: ';

type
  LTBOOL1 = Byte;

procedure ABCToMS3DAscii(model: PABCModel);

implementation

function FindUVPair(Mesh: PABCPiecesItem; Id: Word): PUVPair;
var i: Cardinal;
begin
  for i := 0 to Int64(Mesh^.nNumTris) * 3 - 1 do
  begin
    if Mesh^.aUVMap[i].nTriFS = Id then
      Exit(@Mesh^.aUVMap[i].UV)
  end;
end;

function LTVectorToStr(V: PLTVector): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y) + ' ' +
           FormatFloat('0.000000', V^.z);
end;

function UVPairToStr(P: PUVPair): string;
begin
  Result := FormatFloat('0.000000', P^.a) + ' ' +
           FormatFloat('0.000000', P^.b);
end;

procedure WriteABCPiece(Mesh: PABCPiecesItem; sl: TStringList);
var i: Cardinal;
begin
  // vertices
  sl.Add(IntToStr(Mesh^.nNumVerticles));
  for i := 0 to Mesh^.nNumVerticles - 1 do
  begin
    sl.Add('0 ' +
              LTVectorToStr(@Mesh^.aVertexList[i].vVertex) + ' ' +
              UVPairToStr(FindUVPair(Mesh, i)) + ' ' +
              '-1');
  end;

  // normals
  sl.Add(IntToStr(Mesh^.nNumVerticles));
  for i := 0 to Mesh^.nNumVerticles - 1 do
  begin
    sl.Add(LTVectorToStr(@Mesh^.aVertexList[i].vNormal));
  end;

  // tris
  sl.Add(IntToStr(Mesh^.nNumTris));
  for i := 0 to Mesh^.nNumTris - 1 do
  begin
    sl.Add('0 ' +
    IntToStr(Mesh^.aUVMap[i * 3].nTriFS) + ' ' +
    IntToStr(Mesh^.aUVMap[i * 3 + 1].nTriFS) + ' ' +
    IntToStr(Mesh^.aUVMap[i * 3 + 2].nTriFS) + ' ' +
    IntToStr(Mesh^.aUVMap[i * 3].nTriFS) + ' ' +
    IntToStr(Mesh^.aUVMap[i * 3 + 1].nTriFS) + ' ' +
    IntToStr(Mesh^.aUVMap[i * 3 + 2].nTriFS) + ' ' +  '1');
  end;
end;

procedure WriteMeshes(model: PABCModel; sl: TStringList);
var i: Cardinal;
begin
  sl.Add(STR_MESHES + IntToStr(model^.Pieces.nPiecesCount));

  for i := 0 to model^.Pieces.nPiecesCount - 1 do
  begin
    sl.Add('"' + model^.Pieces.aItems[i].szName + '"' + ' 0 0');
    WriteABCPiece(@model^.Pieces.aItems[i], sl);
  end;
end;

procedure WriteFooter(model: PABCModel; sl: TStringList);
begin
  sl.Add('');
  sl.Add('Bones: 0');
  sl.Add('GroupComments: 0');
  sl.Add('MaterialComments: 0');
  sl.Add('BoneComments: 0');
  sl.Add('ModelComment: 0');
end;

procedure WriteMaterials(model: PABCModel; sl: TStringList);
begin
  sl.Add('');
  sl.Add(STR_MATERIALS + '1');
  sl.Add('"Texture0"');
  sl.Add('0.200000 0.200000 0.200000 1.000000');
  sl.Add('0.800000 0.800000 0.800000 1.000000');
  sl.Add('0.000000 0.000000 0.000000 1.000000');
  sl.Add('0.000000 0.000000 0.000000 1.000000');
  sl.Add('0.000000');
  sl.Add('1.000000');
  sl.Add('"D:\MyProgs\skins\1x1_square.dtx"');
  sl.Add('""');
end;

procedure WriteHeader(model: PABCModel; sl: TStringList);
begin
  sl.Add(STR_PSEUDOHEADER);
  sl.Add('');
  sl.Add(STR_FRAMES + '30');
  sl.Add(STR_FRAME + '1');
end;

procedure ABCToMS3DAscii(model: PABCModel);
var slData: TStringList;
begin
  slData := TStringList.Create;
  WriteHeader(model, slData);
  WriteMeshes(model, slData);
  WriteMaterials(model, slData);
  WriteFooter(model, slData);
  slData.SaveToFile('test.txt');
  slData.Free;
end;

end.

