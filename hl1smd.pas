unit hl1smd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, abcmodeltypes;

const
  STR_HEADER = 'version 1';
  STR_END = 'end';
  STR_NODES = 'nodes';
  STR_SKELETON = 'skeleton';
  STR_TRIS = 'triangles';

type
  LTBOOL2 = Boolean;
  ParentArray = array of Word;

procedure ABCToHL1SMD(model: PABCModel);
procedure ABCToHL1SMDFix(model: PABCModel);

implementation

function LTRotationToStr(V: PLTRotation): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y) + ' ' +
           FormatFloat('0.000000', V^.z);{ + ' ' +
            FormatFloat('0.000000', V^.w);  }
end;

function LTVectorToStr(V: PLTVector): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y) + ' ' +
           FormatFloat('0.000000', V^.z);
end;

function LTVectorToStrSMD(V: PLTVector): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y) + ' ' +
           FormatFloat('0.000000', V^.z);
end;

function LTVectorToStrSMDFix(V: PLTVector): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y * -1.0) + ' ' +
           FormatFloat('0.000000', V^.z * -1.0);
end;

function UVPairToStr(P: PUVPair): string;
begin
  Result := FormatFloat('0.000000', P^.a) + ' ' +
           FormatFloat('0.000000', P^.b);
end;

function UVPairToStrSMD(P: PUVPair): string;
begin
  Result := FormatFloat('0.000000', P^.a) + ' ' +
           FormatFloat('0.000000', 1 - P^.b);
end;

procedure WriteHeader(model: PABCModel; sl: TStringList);
begin
  sl.Add(STR_HEADER);
end;

procedure WriteNode(model: PABCModel; sl: TStringList; var i: Cardinal);
var nParent: Integer;
begin
  //sl.add(IntToStr(model^.Nodes.aItems[i].nTransformIndex + ' ' +
  //model^.Nodes.aItems[i].szName + ' -1');
end;

procedure WriteNodes(model: PABCModel; sl: TStringList);
var i, nLen: Cardinal;
begin
  sl.Add(STR_NODES);
  // temporary
  //sl.Add('0 "root" -1');
  nLen := Length(model^.Nodes.aItems);
  i := 0;
  WriteNode(model, sl, i);
  sl.Add(STR_END);
end;

procedure WriteTriangles(model: PABCModel; sl: TStringList);
var i: Cardinal;
    A: array[0..2] of Word;
    B: array[0..2] of PUVPair;
begin
  sl.Add(STR_TRIS);
  for i := 0 to model^.Pieces.aItems[0].nNumTris - 1 do
  begin
    sl.Add(model^.Pieces.aItems[0].szName);
    A[0] := model^.Pieces.aItems[0].aUVMap[i * 3].nTriFS;
    A[1] := model^.Pieces.aItems[0].aUVMap[i * 3 + 1].nTriFS;
    A[2] := model^.Pieces.aItems[0].aUVMap[i * 3 + 2].nTriFS;
    B[0] := @model^.Pieces.aItems[0].aUVMap[i * 3].UV;
    B[1] := @model^.Pieces.aItems[0].aUVMap[i * 3 + 1].UV;
    B[2] := @model^.Pieces.aItems[0].aUVMap[i * 3 + 2].UV;
    sl.Add('0 ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[0]].vVertex) + ' ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[0]].vNormal) + ' ' +
    UVPairToStrSMD(B[0]));

    sl.Add('0 ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[1]].vVertex) + ' ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[1]].vNormal) + ' ' +
    UVPairToStrSMD(B[1]));

    sl.Add('0 ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[2]].vVertex) + ' ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[2]].vNormal) + ' ' +
    UVPairToStrSMD(B[2]));
  end;
  sl.Add(STR_END);
end;

procedure WriteTrianglesFix(model: PABCModel; sl: TStringList);
var i: Cardinal;
    A: array[0..2] of Word;
    B: array[0..2] of PUVPair;
begin
  sl.Add(STR_TRIS);
  for i := 0 to model^.Pieces.aItems[0].nNumTris - 1 do
  begin
    sl.Add(model^.Pieces.aItems[0].szName);
    A[0] := model^.Pieces.aItems[0].aUVMap[i * 3].nTriFS;
    A[1] := model^.Pieces.aItems[0].aUVMap[i * 3 + 1].nTriFS;
    A[2] := model^.Pieces.aItems[0].aUVMap[i * 3 + 2].nTriFS;
    B[0] := @model^.Pieces.aItems[0].aUVMap[i * 3].UV;
    B[1] := @model^.Pieces.aItems[0].aUVMap[i * 3 + 1].UV;
    B[2] := @model^.Pieces.aItems[0].aUVMap[i * 3 + 2].UV;
    sl.Add('0 ' +
    //IntToStr(model^.Pieces.aItems[0].aVertexList[A[0]].nUnknownCardinal1) + ' ' +
    //IntToStr(model^.Pieces.aItems[0].aVertexList[A[0]].nBoneIndex) + ' ' +
   // LTRotationToStr(@model^.Pieces.aItems[0].aVertexList[A[0]].rUnknownRotation) + ' ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[0]].vNormal) + ' ' +
    UVPairToStrSMD(B[0]));

    sl.Add('0 ' +
    //IntToStr(model^.Pieces.aItems[0].aVertexList[A[1]].nUnknownCardinal1) + ' ' +
    //IntToStr(model^.Pieces.aItems[0].aVertexList[A[1]].nBoneIndex) + ' ' +
   // LTRotationToStr(@model^.Pieces.aItems[0].aVertexList[A[1]].rUnknownRotation) + ' ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[1]].vNormal) + ' ' +
    UVPairToStrSMD(B[1]));

    sl.Add('0 ' +
    //IntToStr(model^.Pieces.aItems[0].aVertexList[A[2]].nUnknownCardinal1) + ' ' +
    //IntToStr(model^.Pieces.aItems[0].aVertexList[A[2]].nBoneIndex) + ' ' +
   // LTRotationToStr(@model^.Pieces.aItems[0].aVertexList[A[2]].rUnknownRotation) + ' ' +
    LTVectorToStrSMD(@model^.Pieces.aItems[0].aVertexList[A[2]].vNormal) + ' ' +
    UVPairToStrSMD(B[2]));
  end;
  sl.Add(STR_END);
end;

procedure WriteSkeleton(model: PABCModel; sl: TStringList);
begin
  sl.Add(STR_SKELETON);
  // temporary
  sl.Add('time 0');
  sl.Add('0 0.000000 0.000000 0.000000 0.000000 0.000000 0.000000');
  sl.Add(STR_END);
end;

procedure ABCToHL1SMD(model: PABCModel);
var slData: TStringList;
begin
  slData := TStringList.Create;
  WriteHeader(model, slData);
  WriteNodes(model, slData);
  WriteSkeleton(model, slData);
  //WriteTriangles(model, slData);
  slData.SaveToFile('export.smd');
  slData.Free;
end;

procedure ABCToHL1SMDFix(model: PABCModel);
var slData: TStringList;
begin
  slData := TStringList.Create;
  WriteHeader(model, slData);
  WriteNodes(model, slData);
  WriteSkeleton(model, slData);
  WriteTrianglesFix(model, slData);
  slData.SaveToFile('export.smd');
  slData.Free;
end;

end.

