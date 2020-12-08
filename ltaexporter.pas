unit ltaexporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, abcmodeltypes;

const
  // root data
  ARRAY_HEADER = 'lt-model-0 ';

  // main data
  NODE_ONLOADCMDS = 'on-load-cmds ';
  {begin}
    NODE_ANIMBINDS = 'anim-bindings ';
    {begin}
      ARRAY_ANIMBIND = 'anim-binding ';
      {begin}
        PROP_NAME = 'name ';
        ARRAY_DIMS = 'dims ';
        ARRAY_TRANS = 'translation ';
        PROP_INTERPTIME = 'interp-time ';
      {end}
    NODE_NODEFLAGS = 'set-node-flags ';
    PROP_CMDSTRING = 'set-command-string ';
    NODE_LODGROUPS = 'lod-groups ';
    {begin}
      ARRAY_LODGROUP = 'create-lod-group ';
      ARRAY_LODDIST = 'lod-dists ';
      ARRAY_SHAPES = 'shapes ';
    {end}
    PROP_GLOBALRADIUS = 'set-global-radius ';
    //NODE_OBBLIST = 'add-node-obb-list';
    NODE_ADDSOCKETS = 'add-sockets ';
    {begin}
      ARRAY_SOCKET = 'socket ';
      PROP_PARENT = 'parent ';
      ARRAY_POS = 'pos ';
      ARRAY_QUAT = 'quat ';
      ARRAY_SCALE = 'scale ';
    {end}
    NODE_CHILDMODELS = 'add-childmodels ';
    {begin}
      ARRAY_CHILDMODEL = 'child-model ';
      {begin}
        PROP_FILENAME = 'filename ';
        PROP_SAVEINDEX = 'save-index ';
        NODE_NODERELATIONS = 'node-relations ';
        //ARRAY_POS = 'pos ';
        //ARRAY_QUAT = 'quat ';
      {end}
    {end}
    NODE_ANIMWEIGHTSETS = 'anim-weightsets ';
    {begin}
      ARRAY_ANIMWEIGHTSET = 'anim-weightset ';
      {begin}
        //PROP_NAME = 'name ';
        ARRAY_WEIGHTS = 'weights ';
      {end}
    {end}
    ARRAY_ADDDEFORMER = 'add-deformer ';
    {begin}
      ARRAY_SKELDEFORMER = 'skel-deformer ';
      {begin}
        PROP_TARGET = 'target ';
        ARRAY_INFLUENCES = 'influences ';
        NODE_WEIGHTSETS = 'weightsets ';
      {end}
    {end}
  {end}
  ARRAY_HIERARCHY = 'hierarchy ';
  {begin}
    NODE_CHILDREN = 'children ';
    {begin}
      ARRAY_TRANSFORM = 'transform ';
      {begin}
      NODE_MATRIX = 'matrix ';
      //NODE_CHILDRED = 'children ';
      {end}
    {end}
  {end}
  ARRAY_SHAPE = 'shape ';
  {begin}
    ARRAY_GEOMETRY = 'geometry ';
    {begin}
      ARRAY_MESH = 'mesh ';
      {begin}
        NODE_VERTEX = 'vertex ';
        NODE_NORMALS = 'normals ';
        NODE_UVS = 'uvs ';
        ARRAY_TEXFS = 'tex-fs ';
        ARRAY_TRIFS = 'tri-fs ';
      {end}
    {end}
    ARRAY_TEXINDICES = 'texture-indices ';
  {end}
  ARRAY_ANIMSET = 'animset ';
  {begin}
    ARRAY_KEYFRAME = 'keyframe  ';
    {begin}
      //ARRAY_KEYFRAME = 'keyframe  ';
      {begin}
        ARRAY_TIMES = 'times ';
        ARRAY_VALUES = 'values ';
      {end}
    {end}
    NODE_ANIMS = 'anims ';
    {begin}
      ARRAY_ANIM = 'anim ';
      //PROP_PARENT = 'parent';
      ARRAY_FRAMES = 'frames ';
      {begin}
        NODE_POSQUAT = 'posquat ';
      {end}
    {end}
  {end}

type

  { TLTAExporter }

  TLTAExporter = class(TObject)
  protected
    m_slExport: TStringList;
    m_pABCModel: PABCModel;

    // generic data writers
    procedure WriteNodeStart(Level: Integer; ID: string; Name: string);
    procedure WriteNodeEnd(Level: Integer);
    procedure WriteArrayStart(Level: Integer; ID: string; Name: string);
    procedure WriteArrayEnd(Level: Integer);

    procedure WriteGenericProp(Level: Integer; ID: string; Value: string);
    procedure WriteGenericSet(Level: Integer; Args: array of string);
    procedure WriteGenericPropStr(Level: Integer; ID: string; Value: string);
    procedure WriteArrayVector(Level: Integer; ID: string; Vector: PLTVector);
    procedure WriteVector(Level: Integer; Vector: PLTVector);
    procedure WriteArrayRotation(Level: Integer; ID: string; Rotation: PLTRotation);
    procedure WriteRotation(Level: Integer; Rotation: PLTRotation);
    procedure WriteUVPair(Level: Integer; UVPair: PUVPair);

    // data writers
    procedure WriteLTModel;
    procedure WriteOnLoadCmds(Level: Integer);
    procedure WriteAnimBinding(Level: Integer; AnimBind: PABCAnimBindingsItem);
    procedure WriteAnimBindings(Level: Integer);
    procedure WriteNodeFlags(Level: Integer);
    procedure WriteLODGroup(Level: Integer; Piece: PABCPiecesItem);
    procedure WriteLODGroups(Level: Integer);
    procedure WriteSocket(Level: Integer; Node: PABCNodesItem; Socket: PABCSocketsItem);
    procedure WriteSockets(Level: Integer);
    procedure WriteChildModel(Level: Integer);
    procedure WriteChildModels(Level: Integer);
    procedure WriteAnimWeightSet(Level: Integer; AnimWeightSet: PABCWeightSetsItem);
    procedure WriteAnimWeightSets(Level: Integer);
    procedure WriteInfluences(Level: Integer);
    procedure WriteWeightSets(Level: Integer; Piece: PABCPiecesItem);
    procedure WriteDeformers(Level: Integer);
    procedure WriteUVs(Level: Integer; Piece: PABCPiecesItem);
    procedure WriteVerticles(Level: Integer; Piece: PABCPiecesItem);
    procedure WriteShapes(Level: Integer);
    procedure WriteAnims(Level: Integer; Anim: PABCAnimationItem);
    procedure WriteAnimSets(Level: Integer);
    procedure WriteTransforms(var nIndex: Cardinal; Level: Integer);
    procedure WriteHierarchy(Level: Integer);



    // helpers
    function GetLevel(Level: Integer): string;
    function GetInterpTime(S: string; Animation: PABCAnimation): Cardinal;
    function GetNodeFromIndex(nIndex: Cardinal): PABCNodesItem;
  public
    procedure ExportText(S: string);
    constructor Create(M: PABCModel);
    destructor Destroy; override;
  end;


implementation

function TLTAExporter.GetLevel(Level: Integer): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to Level do
  begin
    Result := Result + '	';
  end;
end;

procedure TLTAExporter.ExportText(S: string);
begin
  WriteLTModel;
  m_slExport.SaveToFile(S);
end;

constructor TLTAExporter.Create(M: PABCModel);
begin
  m_pABCModel := M;
  m_slExport := TStringList.Create;
end;

destructor TLTAExporter.Destroy;
begin
  m_slExport.Free;
end;


procedure TLTAExporter.WriteNodeStart(Level: Integer; ID: string; Name: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + Name);
  m_slExport.Add(GetLevel(Level + 1) +  '(');
end;

procedure TLTAExporter.WriteNodeEnd(Level: Integer);
begin
  m_slExport.Add(GetLevel(Level + 1) +  ')');
  m_slExport.Add(GetLevel(Level) +  ')');
end;

procedure TLTAExporter.WriteArrayStart(Level: Integer; ID: string; Name: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + Name);
end;

procedure TLTAExporter.WriteArrayEnd(Level: Integer);
begin
  m_slExport.Add(GetLevel(Level) +  ')');
end;

procedure TLTAExporter.WriteGenericSet(Level: Integer; Args: array of string);
var i: Integer;
    szBuffer: string;
begin
  szBuffer := '';
  for i := 0 to Length(Args) - 1 do
  begin
    szBuffer := szBuffer + Args[i] + ' ';
  end;
  m_slExport.Add(GetLevel(Level) + '(' + szBuffer + ')');
end;

procedure TLTAExporter.WriteGenericProp(Level: Integer; ID: string; Value: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + Value + ' )');
end;

procedure TLTAExporter.WriteGenericPropStr(Level: Integer; ID: string; Value: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + '"' + Value + '" )');
end;

procedure TLTAExporter.WriteArrayVector(Level: Integer; ID: string; Vector: PLTVector);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID);
  m_slExport.Add(GetLevel(Level + 1) + '(' + LTVectorToStrC(Vector) + ' )');
  m_slExport.Add(GetLevel(Level) + ')');
end;

procedure TLTAExporter.WriteVector(Level: Integer; Vector: PLTVector);
begin
  m_slExport.Add(GetLevel(Level) + '(' + LTVectorToStrC(Vector) + ' )');
end;

procedure TLTAExporter.WriteArrayRotation(Level: Integer; ID: string; Rotation: PLTRotation);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID);
  m_slExport.Add(GetLevel(Level + 1) + '(' + LTRotationToStrC(Rotation) + ' )');
  m_slExport.Add(GetLevel(Level) + ')');
end;

procedure TLTAExporter.WriteRotation(Level: Integer; Rotation: PLTRotation);
begin
  m_slExport.Add(GetLevel(Level) + '(' + LTRotationToStrC(Rotation) + ' )');
end;

procedure TLTAExporter.WriteUVPair(Level: Integer; UVPair: PUVPair);
begin
  m_slExport.Add(GetLevel(Level) + '(' + UVToStrC(UVPair) + ' )');
end;

function TLTAExporter.GetNodeFromIndex(nIndex: Cardinal): PABCNodesItem;
var i: Cardinal;
begin
  for i := 0 to Length(m_pABCModel^.Nodes.aItems) - 1 do
  begin
    if m_pABCModel^.Nodes.aItems[i].nTransformIndex = nIndex then
      Exit(@m_pABCModel^.Nodes.aItems[i]);
  end;
end;

function TLTAExporter.GetInterpTime(S: string; Animation: PABCAnimation): Cardinal;
var i: Cardinal;
begin
  for i:=0 to Animation^.nAnimCount - 1 do
  begin
    if Animation^.aItems[i].szName = S then Exit(Animation^.aItems[i].nInterpTime)
  end;
end;

procedure TLTAExporter.WriteLTModel;
begin
  m_slExport.Add('');
  WriteArrayStart(0, ARRAY_HEADER, '');
  WriteOnLoadCmds(1);
  WriteHierarchy(1);
  WriteShapes(1);
  WriteAnimSets(1);
  WriteArrayEnd(0);
end;

procedure TLTAExporter.WriteOnLoadCmds(Level: Integer);
begin
  WriteNodeStart(Level, NODE_ONLOADCMDS, '');
  WriteAnimBindings(Level + 2);
  WriteNodeFlags(Level + 2);
  WriteGenericPropStr(Level + 2, PROP_CMDSTRING, m_pABCModel^.Header.szCmdString);
  WriteLODGroups(Level + 2);
  WriteGenericProp(Level + 2, PROP_GLOBALRADIUS, FormatFloat('0.000000', m_pABCModel^.Header.fGlobalRadius));
  WriteSockets(Level + 2);
  //WriteChildModels(Level + 2);
  WriteAnimWeightSets(Level + 2);
  WriteDeformers(Level + 2);
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteAnimBindings(Level: Integer);
var i: Cardinal;
begin
  WriteNodeStart(Level, NODE_ANIMBINDS, '');
  for i := 0 to m_pABCModel^.AnimBindings.nRealAnimsCount - 1 do
  begin
    WriteAnimBinding(Level + 2, @m_pABCModel^.AnimBindings.aItems[i]);
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteAnimBinding(Level: Integer; AnimBind: PABCAnimBindingsItem);
var nInterpTime: Cardinal;
begin
  nInterpTime := GetInterpTime(AnimBind^.szName, @m_pABCModel^.Animation);
  WriteArrayStart(Level, ARRAY_ANIMBIND, '');

  WriteGenericPropStr(Level + 1, PROP_NAME, AnimBind^.szName);
  WriteArrayVector(Level + 1, ARRAY_DIMS, @AnimBind^.vDims);
  WriteArrayVector(Level + 1, ARRAY_TRANS, @AnimBind^.vTranslation);
  WriteGenericProp(Level + 1, PROP_INTERPTIME, IntToStr(nInterpTime));

  WriteArrayEnd(Level);
end;

procedure TLTAExporter.WriteNodeFlags(Level: Integer);
var i: Cardinal;
begin
  WriteNodeStart(Level, NODE_NODEFLAGS, '');
  for i := 0 to Length(m_pABCModel^.Nodes.aItems) - 1 do
  begin
    WriteGenericSet(Level + 2, [ '"' + m_pABCModel^.Nodes.aItems[i].szName + '"',
                          IntToStr(m_pABCModel^.Nodes.aItems[i].nFlags)]);
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteLODGroups(Level: Integer);
var i: Cardinal;
begin
  WriteNodeStart(Level, NODE_LODGROUPS, '');
  // for each pieces..for now
  if m_pABCModel^.Pieces.nPiecesCount > 0 then
  for i := 0 to m_pABCModel^.Pieces.nPiecesCount - 1 do
  begin
    WriteLODGroup(Level + 2, @m_pABCModel^.Pieces.aItems[i]);
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteLODGroup(Level: Integer; Piece: PABCPiecesItem);
begin
  WriteArrayStart(Level, ARRAY_LODGROUP, '"' + Piece^.szName + '" ');

  WriteArrayStart(Level + 1, ARRAY_LODDIST, '');
  WriteGenericSet(Level + 2, ['0.000000']);
  WriteArrayEnd(Level + 1);

  WriteArrayStart(Level + 1, ARRAY_SHAPES, '');
  WriteGenericSet(Level + 2, ['"' + Piece^.szName + '"']);
  WriteArrayEnd(Level + 1);

  WriteArrayEnd(Level);
end;

procedure TLTAExporter.WriteSockets(Level: Integer);
var i: Cardinal;
    pNode: PABCNodesItem;
begin
  if m_pABCModel^.Sockets.nSocketsCount = 0 then Exit;

  WriteNodeStart(Level, NODE_ADDSOCKETS, '');

  for i := 0 to m_pABCModel^.Sockets.nSocketsCount - 1 do
  begin
    pNode := GetNodeFromIndex(m_pABCModel^.Sockets.aItems[i].nNodeIndex);
    WriteSocket(Level + 2, pNode, @m_pABCModel^.Sockets.aItems[i]);
  end;

  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteChildModel(Level: Integer);
//var i: Cardinal;
begin
  {WriteArrayStart(Level, ARRAY_CHILDMODEL, '');
  WriteGenericPropStr(Level + 1, PROP_FILENAME, 'SELF');
  WriteNodeStart(Level + 1, NODE_NODERELATIONS, '');
  for i := 0 to Length(m_pABCModel^.ChildModels.aItems)-1 do
  begin
    WriteArrayStart(Level + 3, '', '');
    WriteArrayVector(Level + 4, ARRAY_POS, @m_pABCModel^.ChildModels.aItems[i].vPos);
    WriteArrayRotation(Level + 4, ARRAY_QUAT, @m_pABCModel^.ChildModels.aItems[i].rRot);
    WriteArrayEnd(Level + 3);
  end;
  WriteNodeEnd(Level + 1);
  WriteArrayEnd(Level); }
end;

procedure TLTAExporter.WriteChildModels(Level: Integer);
//var i: Cardinal;
//    PosSum: Single = 0;
//    QuatSum: Integer = 0;
begin
{  for i := 0 to Length(m_pABCModel^.ChildModels.aItems)-1 do
  begin
    with m_pABCModel^.ChildModels.aItems[i] do
    begin
      PosSum := PosSum + (vPos.x + vPos.y + vPos.z);
      QuatSum := QuatSum + Integer(not (rRot.x = 0.0) and (rRot.y = 0.0) and (rRot.z = 0.0) and (rRot.w = 1.0));
    end;
  end;

  if (PosSum <> 0) or (QuatSum <> 0) then
  begin
    WriteNodeStart(Level, NODE_CHILDMODELS, '');
    WriteChildModel(Level + 2);
    WriteNodeEnd(Level);
  end;   }
end;

procedure TLTAExporter.WriteAnimWeightSets(Level: Integer);
var i: Cardinal;
begin
  if m_pABCModel^.Nodes.WeightSets.nNumSets = 0 then Exit;

  WriteNodeStart(Level, NODE_ANIMWEIGHTSETS, '');

  for i := 0 to m_pABCModel^.Nodes.WeightSets.nNumSets - 1 do
  begin
    WriteAnimWeightSet(Level + 2, @m_pABCModel^.Nodes.WeightSets.aItems[i]);
  end;

  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteSocket(Level: Integer; Node: PABCNodesItem; Socket: PABCSocketsItem);
begin
  WriteArrayStart(Level, ARRAY_SOCKET, '"' + Socket^.szName + '" ');

  WriteGenericPropStr(Level + 1, PROP_PARENT, Node^.szName);
  WriteArrayVector(Level + 1, ARRAY_POS, @Socket^.vPos);
  WriteArrayRotation(Level + 1, ARRAY_QUAT, @Socket^.rQuat);

  WriteArrayEnd(Level);
end;

procedure TLTAExporter.WriteAnimWeightSet(Level: Integer; AnimWeightSet: PABCWeightSetsItem);
var i: Integer;
    szBuffer: string;
begin
  WriteArrayStart(Level, ARRAY_ANIMWEIGHTSET, '');
  WriteGenericPropStr(Level + 1, PROP_NAME, AnimWeightSet^.szName);
  WriteArrayStart(Level + 1, ARRAY_WEIGHTS, '');
  szBuffer := '';
  for i := 0 to AnimWeightSet^.nNumItems - 1 do
  begin
    szBuffer := szBuffer + FormatFloat('0.000000', AnimWeightSet^.afWeights[i])  + ' ';
  end;
  m_slExport.Add(GetLevel(Level + 2) + '(' + szBuffer + ')');
  WriteArrayEnd(Level + 1);
  WriteArrayEnd(Level);
end;

procedure TLTAExporter.WriteInfluences(Level: Integer);
var i: Integer;
    szBuffer: string;
begin
  WriteArrayStart(Level, ARRAY_INFLUENCES, '');
  szBuffer := '';
  for i := 0 to Length(m_pABCModel^.Nodes.aItems) - 1 do
  begin
    szBuffer := szBuffer + '"' + m_pABCModel^.Nodes.aItems[i].szName + '" ';
  end;
  m_slExport.Add(GetLevel(Level + 1) + '(' + szBuffer + ')');
  WriteArrayEnd(Level);
end;

procedure TLTAExporter.WriteWeightSets(Level: Integer; Piece: PABCPiecesItem);
var i, j: Cardinal;
    szBuffer: string;
begin
  WriteNodeStart(Level, NODE_WEIGHTSETS, '');
  for i := 0 to Piece^.nNumVerticles - 1 do
  begin


    //WriteGenericSet(Level + 2, [IntToStr(Piece^.aVertexList[i].nBoneIndex), '1.000000']);
    szBuffer := '';
    for j := 0 to Piece^.aVertexList[i].nNumWeights - 1 do
    begin
      //szBuffer := szBuffer + IntToStr(Piece^.aVertexList[i].anBoneIndex[j]) + ' ' +
      //         FormatFloat('0.000000', Piece^.aVertexList[i].arUnknownRotation[j].w) + ' ';
      szBuffer := szBuffer + IntToStr(Piece^.aVertexList[i].aWeightList[j].nNodeIndex) + ' ' +
               FormatFloat('0.000000', Piece^.aVertexList[i].aWeightList[j].fBias) + ' ';
    end;
    m_slExport.Add(GetLevel(Level + 2) + '(' + szBuffer + ')');

  end;
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteDeformers(Level: Integer);
var i: Cardinal;
begin
  if m_pABCModel^.Pieces.nPiecesCount > 0 then
  for i := 0 to m_pABCModel^.Pieces.nPiecesCount - 1 do
  begin
    WriteArrayStart(Level, ARRAY_ADDDEFORMER, '');
    WriteArrayStart(Level + 1, ARRAY_SKELDEFORMER, '');
    WriteGenericPropStr(Level + 2, PROP_TARGET, m_pABCModel^.Pieces.aItems[i].szName);
    WriteInfluences(Level + 2);
    WriteWeightSets(Level + 2, @m_pABCModel^.Pieces.aItems[i]);
    WriteArrayEnd(Level + 1);
    WriteArrayEnd(Level);
  end;
end;

procedure TLTAExporter.WriteVerticles(Level: Integer; Piece: PABCPiecesItem);
var i: Cardinal;
begin
  WriteNodeStart(Level, NODE_VERTEX, '');
  for i := 0 to Piece^.nNumVerticles - 1 do
  begin
    WriteVector(Level + 2, @Piece^.aVertexList[i].vVertex);
  end;
  WriteNodeEnd(Level);
  WriteNodeStart(Level, NODE_NORMALS, '');
  for i := 0 to Piece^.nNumVerticles - 1 do
  begin
    WriteVector(Level + 2, @Piece^.aVertexList[i].vNormal);
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteUVs(Level: Integer; Piece: PABCPiecesItem);
var i: Cardinal;
    szBuffer1, szBuffer2: string;
begin
  szBuffer1 := '';
  szBuffer2 := '';
  WriteNodeStart(Level, NODE_UVS, '');
  for i := 0 to Int64(Piece^.nNumTris) * 3 - 1 do
  begin
    WriteUVPair(Level + 2, @Piece^.aUVMap[i].UV);
    szBuffer1 := szBuffer1 + IntToStr(Piece^.aUVMap[i].nTriFS) + ' ';
    szBuffer2 := szBuffer2 + IntToStr(i) + ' ';
  end;
  WriteNodeEnd(Level);

  WriteArrayStart(Level, ARRAY_TEXFS, '');
  m_slExport.Add(GetLevel(Level + 1) + '(' + szBuffer2 + ')');
  WriteArrayEnd(Level);

  WriteArrayStart(Level, ARRAY_TRIFS, '');
  m_slExport.Add(GetLevel(Level + 1) + '(' + szBuffer1 + ')');
  WriteArrayEnd(Level);
end;

procedure TLTAExporter.WriteShapes(Level: Integer);
var pPiece: PABCPiecesItem;
    i: Cardinal;
begin
  if m_pABCModel^.Pieces.nPiecesCount > 0 then
  for i := 0 to m_pABCModel^.Pieces.nPiecesCount - 1 do
  begin
    pPiece := @m_pABCModel^.Pieces.aItems[i];

    WriteArrayStart(Level, ARRAY_SHAPE, '"' + pPiece^.szName + '" ');
    WriteArrayStart(Level + 1, ARRAY_GEOMETRY, '');
    WriteArrayStart(Level + 2, ARRAY_MESH, '"' + pPiece^.szName + '" ');

    WriteVerticles(Level + 3, pPiece);
    WriteUVs(Level + 3, pPiece);

    WriteArrayEnd(Level + 2);
    WriteArrayEnd(Level + 1);

    WriteArrayStart(Level + 1, ARRAY_TEXINDICES, '');
    WriteGenericSet(Level + 2, [ IntToStr(pPiece^.nTextureIndex)]);

    WriteArrayEnd(Level + 1);

    WriteArrayEnd(Level);
  end;
end;

procedure TLTAExporter.WriteAnims(Level: Integer; Anim: PABCAnimationItem);
var i, j: Cardinal;
    pNode: PABCNodesItem;
begin
  WriteNodeStart(Level, NODE_ANIMS, '');
  for i := 0 to Length(Anim^.aNodeAnims) - 1 do
  begin
    WriteArrayStart(Level + 2, ARRAY_ANIM, '');
    pNode := GetNodeFromIndex(i);
    WriteGenericPropStr(Level + 3, PROP_PARENT, pNode^.szName);
    WriteArrayStart(Level + 3, ARRAY_FRAMES, '');
    WriteNodeStart(Level + 4, NODE_POSQUAT, '');


    for j := 0 to Length(Anim^.aNodeAnims[i].avPos) - 1 do
    begin
      WriteArrayStart(Level + 6, '', '');
      WriteVector(Level + 7, @Anim^.aNodeAnims[i].avPos[j]);
      WriteRotation(Level + 7, @Anim^.aNodeAnims[i].arRot[j]);
      WriteArrayEnd(Level + 6);
    end;


    WriteNodeEnd(Level + 4);
    WriteArrayEnd(Level + 3);
    WriteArrayEnd(Level + 2);
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAExporter.WriteAnimSets(Level: Integer);
var i, j: Cardinal;
    pAnim: PABCAnimationItem;
    szBuffer1, szBuffer2: string;
begin
  for i := 0 to m_pABCModel^.Animation.nAnimCount - 1 do
  begin
    pAnim := @m_pABCModel^.Animation.aItems[i];

    WriteArrayStart(Level, ARRAY_ANIMSET, '"' + pAnim^.szName + '" ');
    WriteArrayStart(Level + 1, ARRAY_KEYFRAME, '');
    WriteArrayStart(Level + 2, ARRAY_KEYFRAME, '');

    szBuffer1 := '';
    szBuffer2 := '';
    for j := 0 to pAnim^.nKeyFrames - 1 do
    begin
      szBuffer1 := szBuffer1 + IntToStr(pAnim^.aKeyFrames[j].nTime) + ' ';
      szBuffer2 := szBuffer2 + '"' + pAnim^.aKeyFrames[j].szCmd + '" ';
    end;

    WriteArrayStart(Level + 3, ARRAY_TIMES, '');
    m_slExport.Add(GetLevel(Level + 4) + '(' + szBuffer1 + ')');
    WriteArrayEnd(Level + 3);

    WriteArrayStart(Level + 3, ARRAY_VALUES, '');
    m_slExport.Add(GetLevel(Level + 4) + '(' + szBuffer2 + ')');
    WriteArrayEnd(Level + 3);

    WriteArrayEnd(Level + 2);
    WriteArrayEnd(Level + 1);

    WriteAnims(Level + 1, pAnim);

    WriteArrayEnd(Level);
  end;
end;

procedure TLTAExporter.WriteTransforms(var nIndex: Cardinal; Level: Integer);
var i: Cardinal;
begin
  with m_pABCModel^.Nodes.aItems[nIndex] do
  begin
    WriteArrayStart(Level, ARRAY_TRANSFORM, '"' + szName + '" ');
    WriteNodeStart(Level + 1, NODE_MATRIX, '');

    for i := 0 to 3 do
    begin
      WriteRotation(Level + 3, @aMatrix[i]);
    end;
    Inc(nIndex, 1);

    WriteNodeEnd(Level + 1);


    if nNumChildred > 0 then
    begin
      WriteNodeStart(Level + 1, NODE_CHILDREN, '');
      for i := 0 to nNumChildred - 1 do
      begin
        WriteTransforms(nIndex, Level + 3);
      end;
      WriteNodeEnd(Level + 1);
    end;


    WriteArrayEnd(Level);
  end;
end;

procedure TLTAExporter.WriteHierarchy(Level: Integer);
var nIndex: Cardinal;
begin
  WriteArrayStart(Level, ARRAY_HIERARCHY, '');
  WriteNodeStart(Level + 1, NODE_CHILDREN, '');
  nIndex := 0;
  WriteTransforms(nIndex, Level + 3);
  WriteNodeEnd(Level + 1);
  WriteArrayEnd(Level);
end;

end.

