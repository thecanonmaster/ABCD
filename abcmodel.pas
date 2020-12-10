unit abcmodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, abcmodeltypes;

const
  ID_HEADER = 0;
  ID_PIECES = 1;
  ID_NODES = 2;
  ID_CHILDMODELS = 3;
  ID_ANIMATION = 4;
  ID_SOCKETS = 5;
  ID_ANIMBINDINGS = 6;
  ID_KEYFRAME = 7;
  ID_NODEANIMLIST = 8;
  ID_ANIMWEIGHTSETS = 9;
  ID_UVMAP = 10;
  ID_VERTEX = 11;
  ID_VERTEX_WEIGHT = 12;
  ID_MAX = 13;
  ID_BLANK = $FF;

type

  { TIdentifier }

  TIdentifier = class(TObject)
     nID: Byte;
     anIndexes: array[0..3] of Cardinal;
  end;

  { TABCParser }

  TABCParser = class(TObject)
  private
    { private declarations }
    m_ABCModel: TABCModel;
    m_pWorkStream: TMemoryStream;
    m_slView: TStringList;
    m_nTotalBytes: Cardinal;
    m_pLogProc: TLogProc;
    m_pViewProc: TViewProc;
    m_anOffsetHolder: array[0..ID_MAX-1] of Int64;
    m_bSmthWentWrong: Boolean;
    procedure WLog(S: string);
    function LTVectorToStr(V: PLTVector): string;
    function LTRotationToStr(R: PLTRotation): string;
    function UVPairToStr(U: PUVPair): string;
    function ReadModel: Boolean;
    function ReadHeader(nNextOffset: Cardinal; nOffset: Cardinal): Boolean;
    procedure ReadPieces(nNextOffset: Cardinal; nOffset: Cardinal);
    procedure ReadNodes(nNextOffset: Cardinal; nOffset: Cardinal);
    function ReadNodeItem(var nIndex: Cardinal; nChildren: Cardinal; nTempParentIndex: Word): Boolean;
    function FindNodeFromTransformIndex(nIndex: Word): Cardinal;
    procedure ReadChildModels(nNextOffset: Cardinal; nOffset: Cardinal);
    procedure ReadAnimation(nNextOffset: Cardinal; nOffset: Cardinal);
    procedure ReadSockets(nNextOffset: Cardinal; nOffset: Cardinal);
    procedure ReadAnimBindings(nNextOffset: Cardinal; nOffset: Cardinal);
    procedure WLogHeader;
    procedure WLogChildModels;
    procedure WLogSockets;
    procedure WLogAnimBindings;
    procedure DestructorHelper;
    function UnknownDataToString(S: string): string;
    procedure WritePieces;
    procedure ViewHeader;
    procedure ViewNode(index: Cardinal);
    procedure ViewPiece(index: Cardinal);
    procedure ViewUVMap(index1: Cardinal);
    procedure ViewVertex(index1: Cardinal; index2: Cardinal);
    procedure ViewVertexWeight(index1: Cardinal; index2: Cardinal);
    procedure ViewAnimation(index: Cardinal);
    procedure ViewAnimBinding(index: Cardinal);
    procedure ViewSocket(index: Cardinal);
    procedure ViewAnimWeightSet(index: Cardinal);
    procedure ViewKeyFrame(index1: Cardinal; index2: Cardinal);
    procedure ViewNodeAnimList(index1: Cardinal; index2: Cardinal);
    procedure ViewChildModel(index1: Cardinal);
    function CreateID(nID: Byte; anData: array of Cardinal): TIdentifier;
  public
    { public declarations }
    property LogProc: TLogProc read m_pLogProc write m_pLogProc;
    property ViewProc: TViewProc read m_pViewProc write m_pViewProc;
    property ABCModel: TABCModel read m_ABCModel;
    function OpenFile(S: string): Boolean;
    procedure DumpData(var Buffer; szName: string; Len: Int64);
    procedure ABCDataToTreeView(tv: TTreeView);
    procedure ViewTVNode(P: Pointer);
    //procedure FixExportedModel(S: string);
    procedure ImportSelfChildModel(szModelFile: string; szSCMFile: string);
    procedure OffsetChildModels(szModelFile: string; bIgnoreSelf: Boolean; nNodeIndex: Integer; fX, fY, fZ: LTFloat);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

function TABCParser.UnknownDataToString(S: string): string;
var i, L: Integer;
    szTemp: String;
begin
  L := Length(S);
  SetLength(Result, L + L);
  for i := 0 to L - 1 do
  begin
    szTemp := IntToHex(Byte(S[i + 1]), 2);
    Result[i * 2 + 1] := szTemp[1];
    Result[i * 2 + 2] := szTemp[2];
  end;
end;


procedure TABCParser.ViewAnimation(index: Cardinal);
begin
  with m_ABCModel.Animation.aItems[index] do
  begin
    m_slView.Add('Anim name|' + szName);
    m_slView.Add('Dimensions|' + LTVectorToStr(@vDimensions));
    m_slView.Add('Compression type|' + IntToStr(nCompressionType));
    m_slView.Add('Interpolation time|' + IntToStr(nInterpTime));
    m_slView.Add('Num keyframes|' + IntToStr(nKeyFrames));
  end;
end;

procedure TABCParser.ViewAnimBinding(index: Cardinal);
begin
  with m_ABCModel.AnimBindings.aItems[index] do
  begin
    m_slView.Add('Anim name|' + szName);
    m_slView.Add('Dimensions|' + LTVectorToStr(@vDims));
    m_slView.Add('Translation|' + LTVectorToStr(@vTranslation));
  end;
end;

procedure TABCParser.ViewSocket(index: Cardinal);
begin
  with m_ABCModel.Sockets.aItems[index] do
  begin
    m_slView.Add('Socket name|' + szName);
    m_slView.Add('Node index|' + IntToStr(nNodeIndex));
    m_slView.Add('Pos|' + LTVectorToStr(@vPos));
    m_slView.Add('Quat|' + LTRotationToStr(@rQuat));
  end;
end;

procedure TABCParser.ViewAnimWeightSet(index: Cardinal);
var i: Cardinal;
begin
  with m_ABCModel.Nodes.WeightSets.aItems[index] do
  begin
    for i := 0 to nNumItems-1 do
    begin
      m_slView.Add(m_ABCModel.Nodes.aItems[i].szName + '|' + FloatToStr(afWeights[i]));
    end;
  end;
end;

procedure TABCParser.ViewKeyFrame(index1: Cardinal; index2: Cardinal);
begin
  with m_ABCModel.Animation.aItems[index1].aKeyFrames[index2] do
  begin
    m_slView.Add('Time|' + IntToStr(nTime));
    m_slView.Add('Command line|' + szCmd);
  end;
end;

procedure TABCParser.ViewNodeAnimList(index1: Cardinal; index2: Cardinal);
var i, nLen: Cardinal;
begin
  with m_ABCModel.Animation.aItems[index1].aNodeAnims[index2] do
  begin
    nLen := Length(avPos);
    for i := 0 to nLen - 1 do
    begin
      m_slView.Add('Pos[' + IntToStr(i) + ']|' + LTVectorToStr(@avPos[i]));
      m_slView.Add('Rot[' + IntToStr(i) + ']|' + LTRotationToStr(@arRot[i]));
    end;
  end;
end;

procedure TABCParser.ViewChildModel(index1: Cardinal);
var i, nLen: Cardinal;
begin
  with m_ABCModel.ChildModels.aItems[index1] do
  begin
    m_slView.Add('UnknownCardinal|' + IntToStr(nUnknownCardinal));
    nLen := Length(aRelations);
    for i := 0 to nLen - 1 do
    begin
      m_slView.Add('Pos[' + IntToStr(i) + ']|' + LTVectorToStr(@aRelations[i].vPos));
      m_slView.Add('Rot[' + IntToStr(i) + ']|' + LTRotationToStr(@aRelations[i].rRot));
    end;
  end;
end;

function TABCParser.CreateID(nID: Byte; anData: array of Cardinal): TIdentifier;
begin
  Result := TIdentifier.Create;
  Result.nID := nID;
  Result.anIndexes[0] := anData[0];
  Result.anIndexes[1] := anData[1];
  Result.anIndexes[2] := anData[2];
  Result.anIndexes[3] := anData[3];
end;

procedure TABCParser.ViewHeader;
begin
  with m_ABCModel.Header do
  begin
    m_slView.Add('File version|' + IntToStr(nFileVersion));

    m_slView.Add('Keyframe count|' + IntToStr(nKeyframeCount));
    m_slView.Add('Anims count|' + IntToStr(nAnimsCount));
    m_slView.Add('Nodes count|' + IntToStr(nNodesCount));
    m_slView.Add('Pieces count|' + IntToStr(nPiecesCount));
    m_slView.Add('ChildModel count|' + IntToStr(nChildModelCount));
    m_slView.Add('Tris count|' + IntToStr(nTrisCount));
    m_slView.Add('Verts count|' + IntToStr(nVertsCount));
    m_slView.Add('VertWeights count|' + IntToStr(nVertWeightsCount));
    m_slView.Add('LOD count|' + IntToStr(nLODCount));
    m_slView.Add('Socket count|' + IntToStr(nSocketCount));
    m_slView.Add('WeightSet count|' + IntToStr(nWeightSetCount));
    m_slView.Add('String count|' + IntToStr(nStringCount));
    m_slView.Add('String length total|' + IntToStr(nStringLengthTotal));

    m_slView.Add('Command string|' + szCmdString);
    m_slView.Add('Global radius|' + FloatToStr(fGlobalRadius));
    m_slView.Add('Num LODs|' + IntToStr(nNumLOD));
  end;
end;

procedure TABCParser.ViewNode(index: Cardinal);
begin
  with m_ABCModel.Nodes.aItems[index] do
  begin
    m_slView.Add('Node name|' + szName);
    m_slView.Add('Transformation index|' + IntToStr(nTransformIndex));
    m_slView.Add('Parent index|' + IntToStr(nParentIndex));
    m_slView.Add('Flags|' + IntToStr(nFlags));
    m_slView.Add('Matrix[x]|' + LTRotationToStr(@aMatrix[0]));
    m_slView.Add('Matrix[y]|' + LTRotationToStr(@aMatrix[1]));
    m_slView.Add('Matrix[z]|' + LTRotationToStr(@aMatrix[2]));
    m_slView.Add('Matrix[w]|' + LTRotationToStr(@aMatrix[3]));
    m_slView.Add('Num children|' + IntToStr(nNumChildred));
  end;
end;

procedure TABCParser.ViewPiece(index: Cardinal);
begin
  with m_ABCModel.Pieces.aItems[index] do
  begin
{    nTextureIndex: Word;
    fSpecularPower: LTFloat;
    fSpecularScale: LTFloat;
    fUnknownFloat1: LTFloat;
    nUnknownWord2: Word;
    nNameLength: Word;
    szName: string;
    nNumTris: Cardinal;
    aUVMap: TABCPieceUVMapList;
    nNumVerticles: Cardinal;
    aVertexList: TABCPieceVertexList; }
    m_slView.Add('Piece name|' + szName);
    m_slView.Add('Texture index|' + IntToStr(nTextureIndex));
    m_slView.Add('Specular power|' + FloatToStr(fSpecularPower));
    m_slView.Add('Specular scale|' + FloatToStr(fSpecularScale));
    m_slView.Add('LOD Weight|' + FloatToStr(fLODWeight));
    m_slView.Add('Unknown2|' + IntToStr(nUnknownWord2));
    m_slView.Add('Number of tris|' + IntToStr(nNumTris));
    m_slView.Add('Number of verts|' + IntToStr(nNumVerticles));
  end;
end;

procedure TABCParser.ViewUVMap(index1: Cardinal);
var i: Cardinal;
begin
  with m_ABCModel.Pieces.aItems[index1] do
  begin
    for i := 0 to Length(aUVMap) - 1 do
    begin
      m_slView.Add('Tri FS[' + IntToStr(i) + ']|' + IntToStr(aUVMap[i].nTriFS));
      m_slView.Add('UV Pair[' + IntToStr(i) + ']|' + UVPairToStr(@aUVMap[i].UV));
    end;
  end;
end;

procedure TABCParser.ViewVertex(index1: Cardinal; index2: Cardinal);
begin
  with m_ABCModel.Pieces.aItems[index1].aVertexList[index2] do
  begin
    m_slView.Add('Num Weights|' + IntToStr(nNumWeights));
    m_slView.Add('Unknown1|' + IntToStr(nUnknownWord));
    m_slView.Add('Normal|' + LTVectorToStr(@vNormal));
    m_slView.Add('Vertex|' + LTVectorToStr(@vVertex));
  end;
end;

procedure TABCParser.ViewVertexWeight(index1: Cardinal; index2: Cardinal);
var i: Cardinal;
begin
  with m_ABCModel.Pieces.aItems[index1].aVertexList[index2] do
  begin
    for i := 0 to Length(aWeightList) - 1 do
    begin
      m_slView.Add('Node Index[' + IntToStr(i) + ']|' + IntToStr(aWeightList[i].nNodeIndex));
      m_slView.Add('Location[' + IntToStr(i) + ']|' + LTVectorToStr(@aWeightList[i].vLocation));
      m_slView.Add('Bias[' + IntToStr(i) + ']|' + FloatToStr(aWeightList[i].fBias));
    end;
  end;
end;

procedure TABCParser.ViewTVNode(P: Pointer);
var pID: TIdentifier;
begin
  m_slView.Clear;
  if P <> nil then
  begin
    pID := TIdentifier(P);
    case pID.nID of
      ID_HEADER: ViewHeader;
      ID_PIECES: ViewPiece(pID.anIndexes[0]);
      ID_NODES:  ViewNode(pID.anIndexes[0]);
      ID_ANIMATION: ViewAnimation(pID.anIndexes[0]);
      ID_SOCKETS: ViewSocket(pID.anIndexes[0]);
      ID_KEYFRAME: ViewKeyFrame(pID.anIndexes[0], pID.anIndexes[1]);
      ID_ANIMBINDINGS: ViewAnimBinding(pID.anIndexes[0]);
      ID_ANIMWEIGHTSETS: ViewAnimWeightSet(pID.anIndexes[0]);
      ID_NODEANIMLIST: ViewNodeAnimList(pID.anIndexes[0], pID.anIndexes[1]);
      ID_CHILDMODELS: ViewChildModel(pID.anIndexes[0]);
      ID_UVMAP: ViewUVMap(pID.anIndexes[0]);
      ID_VERTEX: ViewVertex(pID.anIndexes[0], pID.anIndexes[1]);
      ID_VERTEX_WEIGHT: ViewVertexWeight(pID.anIndexes[0], pID.anIndexes[1]);
    end;
  end;
  m_pViewProc(m_slView);
end;


procedure TABCParser.OffsetChildModels(szModelFile: string;
  bIgnoreSelf: Boolean; nNodeIndex: Integer; fX, fY, fZ: LTFloat);
var
  i, j: Cardinal;
begin
  m_pWorkStream.Position := m_anOffsetHolder[ID_CHILDMODELS];
  with ABCModel.ChildModels do
  begin
    if nChildModelsCount > 0 then
    begin
      m_pWorkStream.WriteDWord(nChildModelsCount);
      for i := 0 to nChildModelsCount - 1 do
      begin
        //m_pWorkStream.WriteDWord(aItems[i].nUnknownCardinal);
        m_pWorkStream.Position := m_pWorkStream.Position + SizeOf(Cardinal);
        if i <> 0 then
        begin
          //m_pWorkStream.WriteWord(aItems[i].nNameLength);
          //m_pWorkStream.Write(aItems[i].szName[1], aItems[i].nNameLength);
          m_pWorkStream.Position := m_pWorkStream.Position + SizeOf(Word) + Length(aItems[i].szName);
        end;
        for j := 0 to Length(aItems[i].aRelations) - 1 do
        begin
          if (nNodeIndex = j) and ((i > 0) or not bIgnoreSelf) then
          begin
            aItems[i].aRelations[j].vPos.x := aItems[i].aRelations[j].vPos.x + fX;
            aItems[i].aRelations[j].vPos.y := aItems[i].aRelations[j].vPos.y + fY;
            aItems[i].aRelations[j].vPos.z := aItems[i].aRelations[j].vPos.z + fZ;

            m_pWorkStream.Write(aItems[i].aRelations[j].vPos, SizeOf(LTVector));
            m_pWorkStream.Position := m_pWorkStream.Position + SizeOf(LTRotation);
            //m_pWorkStream.Write(aItems[i].aRelations[j].rRot, SizeOf(LTRotation));
          end
          else
          begin
            m_pWorkStream.Position := m_pWorkStream.Position + SizeOf(LTVector);
            m_pWorkStream.Position := m_pWorkStream.Position + SizeOf(LTRotation);
          end;
        end;
      end;
    end;
  end;
  WLog('File with offsetted child models: ' + szModelFile);
  m_pWorkStream.SaveToFile(szModelFile);
end;

procedure TABCParser.ImportSelfChildModel(szModelFile: string; szSCMFile: string);
var pMS: TMemoryStream;
    sBuf: TABCChildModels;
    szHeader: array[0..2] of Char;
    nLen, i: Cardinal;
begin
  pMS := TMemoryStream.Create;
  pMS.LoadFromFile(szSCMFile);
  pMS.Read(szHeader{%H-}, 3);
  if szHeader = 'SCM' then
  begin
    pMS.Read({%H-}sBuf.nChildModelsCount, 4);
    pMS.Read(nLen{%H-}, 4);
    SetLength(sBuf.aItems, 1);
    pMS.Read(sBuf.aItems[0].nUnknownCardinal, 4);
    SetLength(sBuf.aItems[0].aRelations, nLen);
    for i := 0 to nLen-1 do
    begin
      pMS.Read(sBuf.aItems[0].aRelations[i].vPos, SizeOf(LTVector));
      pMS.Read(sBuf.aItems[0].aRelations[i].rRot, SizeOf(LTRotation));
    end;

    m_pWorkStream.Position := m_anOffsetHolder[ID_CHILDMODELS];
    with ABCModel.ChildModels do
    begin
      m_pWorkStream.WriteDWord(sBuf.nChildModelsCount);
      m_pWorkStream.WriteDWord(sBuf.aItems[0].nUnknownCardinal);
      for i := 0 to nLen-1 do
      begin
        m_pWorkStream.Write(sBuf.aItems[0].aRelations[i].vPos, SizeOf(LTVector));
        m_pWorkStream.Write(sBuf.aItems[0].aRelations[i].rRot, SizeOf(LTRotation));
      end;
    end;
  end;
  pMS.Free;
  WLog('File with imported child model: ' + szModelFile);
  m_pWorkStream.SaveToFile(szModelFile);
end;

procedure TABCParser.DestructorHelper;
var i,j: Cardinal;
    nNodes: Cardinal;
begin
  // header
  with m_ABCModel.Header do
  begin
    SetLength(szCmdString, 0);
    //SetLength(szUnknownBlock2, 0);
    SetLength(afLODDistances, 0);
  end;

  if m_bSmthWentWrong then Exit;

  // pieces
  with m_ABCModel.Pieces do
  begin
    if nPiecesCount > 0 then
    for i := 0 to nPiecesCount - 1 do
    begin
      SetLength(aItems[i].szName, 0);
      SetLength(aItems[i].aUVMap, 0);
      for j := 0 to aItems[i].nNumVerticles -1 do
      begin
        //SetLength(aItems[i].aVertexList[j].anBoneIndex, 0);
        //SetLength(aItems[i].aVertexList[j].arUnknownRotation, 0);
        SetLength(aItems[i].aVertexList[j].aWeightList, 0);
      end;
      SetLength(aItems[i].aVertexList, 0);
    end;
    SetLength(aItems, 0);
  end;

  // nodes
  with m_ABCModel.Nodes do
  begin
    if WeightSets.nNumSets > 0 then
    begin
      for i := 0 to WeightSets.nNumSets-1 do
      begin
        SetLength(WeightSets.aItems[i].szName, 0);
        SetLength(WeightSets.aItems[i].afWeights, 0);
      end;
      SetLength(WeightSets.aItems, 0);
    end;
    nNodes := Length(aItems);
    for i := 0 to nNodes - 1 do
    begin
      SetLength(aItems[i].szName, 0);
    end;
    SetLength(aItems, 0);
  end;

  // animations
  with m_ABCModel.Animation do
  begin
    for i := 0 to nAnimCount-1 do
    begin
      SetLength(aItems[i].szName, 0);
      for j := 0 to aItems[i].nKeyFrames-1 do
      begin
        SetLength(aItems[i].aKeyFrames[j].szCmd, 0);
      end;
      for j := 0 to nNodes-1 do
      begin
        SetLength(aItems[i].aNodeAnims[j].avPos, 0);
        SetLength(aItems[i].aNodeAnims[j].arRot, 0);
      end;
    end;
    SetLength(aItems, 0);
  end;

  // anim bindings
  with m_ABCModel.AnimBindings do
  begin
    for i := 0 to nRealAnimsCount-1 do
    begin
      SetLength(aItems[i].szName, 0);
    end;
    SetLength(aItems, 0);
  end;

  // child models
  with m_ABCModel.ChildModels do
  begin
    for i := 0 to nChildModelsCount-1 do
    begin
      SetLength(aItems[i].szName, 0);
      SetLength(aItems[i].aRelations, 0);
    end;
    SetLength(aItems, 0);
  end;

  // sockets
  with m_ABCModel.Sockets do
  begin
    if nSocketsCount > 0 then
    for i := 0 to nSocketsCount - 1 do
    begin
      SetLength(aItems[i].szName, 0);
    end;
  end;
end;


procedure TABCParser.DumpData(var Buffer; szName: string; Len: Int64);
var fStream: TFileStream;
begin
  fStream := TFileStream.Create(szName, fmCreate + fmOpenWrite);
  fStream.Write(Buffer, Len);
  fStream.Free;
end;

procedure TABCParser.WLog(S: string);
begin
  m_pLogProc(S);
end;

function TABCParser.LTVectorToStr(V: PLTVector): string;
begin
  Result := '<' + FormatFloat('0.000000', V^.x) + ', ' +
           FormatFloat('0.000000', V^.y) + ', ' +
           FormatFloat('0.000000', V^.z) + '>';
end;

function TABCParser.LTRotationToStr(R: PLTRotation): string;
begin
  Result := '<' + FormatFloat('0.000000', R^.x) + ', ' +
           FormatFloat('0.000000', R^.y) + ', ' +
           FormatFloat('0.000000', R^.z) + ', ' +
           FormatFloat('0.000000', R^.w) +
           '>';
end;

function TABCParser.UVPairToStr(U: PUVPair): string;
begin
  Result := '<' + FormatFloat('0.000000', U^.a) + ', ' +
           FormatFloat('0.000000', U^.b) + '>';
end;

procedure TABCParser.WLogHeader;
//var S: string;
    //i: Integer;
begin
  with m_ABCModel.Header do
  begin
    WLog('|  FileVersion = ' + IntToStr(nFileVersion));

    {S := ' - ';
    for i := 0 to 12 do
    begin
      S := S + IntToStr(anUnknownBlock1[i]) + ' - ';
    end;
    WLog('|  UnknownBlock = ' + S);  }

    WLog('|  KeyframeCount = ' + IntToStr(nKeyframeCount));
    WLog('|  AnimsCount = ' + IntToStr(nAnimsCount));
    WLog('|  NodesCount = ' + IntToStr(nNodesCount));
    WLog('|  PiecesCount = ' + IntToStr(nPiecesCount));
    WLog('|  ChildModelCount = ' + IntToStr(nChildModelCount));
    WLog('|  TrisCount = ' + IntToStr(nTrisCount));
    WLog('|  VertsCount = ' + IntToStr(nVertsCount));
    WLog('|  VertWeightsCount = ' + IntToStr(nVertWeightsCount));
    WLog('|  LODCount = ' + IntToStr(nLODCount));
    WLog('|  SocketCount = ' + IntToStr(nSocketCount));
    WLog('|  WeightSetCount = ' + IntToStr(nWeightSetCount));
    WLog('|  StringCount = ' + IntToStr(nStringCount));
    WLog('|  StringLengthTotal = ' + IntToStr(nStringLengthTotal));

    WLog('|  CommandLine = ' + szCmdString);
    WLog('|  GlobalRadius = ' + FloatToStr(fGlobalRadius));
    WLog('|  NumLODs = ' + IntToStr(nNumLOD));
  end;
end;

procedure TABCParser.WLogChildModels;
begin
  with m_ABCModel.ChildModels do
  begin
    WLog('|  ChildModelsCount = ' + IntToStr(nChildModelsCount));
  end;
end;

procedure TABCParser.WLogSockets;
var i: Cardinal;
begin
  with m_ABCModel.Sockets do
  begin
    WLog('|  SocketCount = ' + IntToStr(nSocketsCount));
    if nSocketsCount >0 then
    for i := 0 to nSocketsCount - 1 do
    begin
      WLog('|  SocketName' + IntToStr(i) + ' = ' + aItems[i].szName);
      WLog('|  |  NodeIndex' + IntToStr(i) + ' = ' + IntToStr(aItems[i].nNodeIndex));
      WLog('|  |  Quat' + IntToStr(i) + ' = ' + LTRotationToStr(@aItems[i].rQuat));
      WLog('|  |  Pos' + IntToStr(i) + ' = ' + LTVectorToStr(@aItems[i].vPos));
    end;
  end;
end;

procedure TABCParser.WLogAnimBindings;
var i: Cardinal;
begin
  with m_ABCModel.AnimBindings do
  begin
    WLog('|  AnimCount = ' + IntToStr(nRealAnimsCount));
    for i := 0 to nRealAnimsCount - 1 do
    begin
      WLog('|  AnimName' + IntToStr(i) + ' = ' + aItems[i].szName);
      WLog('|  |  Dims' + IntToStr(i) + ' = ' + LTVectorToStr(@aItems[i].vDims));
      WLog('|  |  Translations' + IntToStr(i) + ' = ' + LTVectorToStr(@aItems[i].vTranslation));
    end;
  end;
end;

function TABCParser.ReadHeader(nNextOffset: Cardinal; nOffset: Cardinal): Boolean;
begin
  WLog('Header chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  with m_ABCModel.Header do
  begin
    m_pWorkStream.Read(nFileVersion, 4);
    //m_pWorkStream.Read(anUnknownBlock1, 52);

    m_pWorkStream.Read(nKeyframeCount, 4);
    m_pWorkStream.Read(nAnimsCount, 4);
    m_pWorkStream.Read(nNodesCount, 4);
    m_pWorkStream.Read(nPiecesCount, 4);
    m_pWorkStream.Read(nChildModelCount, 4);
    m_pWorkStream.Read(nTrisCount, 4);
    m_pWorkStream.Read(nVertsCount, 4);
    m_pWorkStream.Read(nVertWeightsCount, 4);
    m_pWorkStream.Read(nLODCount, 4);
    m_pWorkStream.Read(nSocketCount, 4);
    m_pWorkStream.Read(nWeightSetCount, 4);
    m_pWorkStream.Read(nStringCount, 4);
    m_pWorkStream.Read(nStringLengthTotal, 4);

    m_pWorkStream.Read(nCmdLenght, 2);
    SetLength(szCmdString, nCmdLenght);
    if nCmdLenght > 0 then m_pWorkStream.Read(szCmdString[1], nCmdLenght);
    m_pWorkStream.Read(fGlobalRadius, 4);
    m_pWorkStream.Read(nNumLOD, 4);
    m_pWorkStream.Read(anPadding, 60);

    if nNumLOD > 0 then
    begin
      SetLength(afLODDistances, nNumLOD);
      m_pWorkStream.Read(afLODDistances[0], nNumLOD * sizeof(LTFloat));
    end;

    //SetLength(szUnknownBlock2, nNextOffset - m_pWorkStream.Position);
    //m_pWorkStream.Read(szUnknownBlock2[1], nNextOffset - m_pWorkStream.Position);
  end;
  // debug
  //DumpData(m_ABCModel.Header.szUnknownBlock2[1], 'zz_Header.dat' , Length(m_ABCModel.Header.szUnknownBlock2));
  //WLogHeader;

  Result := True;
end;

procedure TABCParser.ReadPieces(nNextOffset: Cardinal; nOffset: Cardinal);
var i, j, k: Cardinal;
begin
  WLog('Pieces chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  //WLog('NextOffset: ' + IntToHex(nNextOffset, 8) + 'h');
  with m_ABCModel.Pieces do
  begin
    m_pWorkStream.Read(nWeightCount, 4);
    m_pWorkStream.Read(nPiecesCount, 4);
    SetLength(aItems, nPiecesCount);
    if nPiecesCount > 0 then
    for i := 0 to nPiecesCount - 1 do
    begin
      with aItems[i] do
      begin
        m_pWorkStream.Read(nTextureIndex, 2);
        m_pWorkStream.Read(fSpecularPower, 4);
        m_pWorkStream.Read(fSpecularScale, 4);
        m_pWorkStream.Read(fLODWeight, 4);
        m_pWorkStream.Read(nUnknownWord2, 2);
        m_pWorkStream.Read(nNameLength, 2);
        SetLength(szName, nNameLength);
        m_pWorkStream.Read(szName[1], nNameLength);
        m_pWorkStream.Read(nNumTris, 4);
        SetLength(aUVMap, nNumTris * 3);

        for j := 0 to Int64(nNumTris) * 3 - 1 do
        begin
          m_pWorkStream.Read(aUVMap[j].UV, SizeOf(TUVPair));
          m_pWorkStream.Read(aUVMap[j].nTriFS, 2);
        end;
        WLog('Verticles start at ' + IntToHex(m_pWorkStream.Position, 8) + 'h');
        m_pWorkStream.Read(nNumVerticles, 4);
        SetLength(aVertexList, nNumVerticles);
        for j := 0 to nNumVerticles - 1 do
        begin
          m_pWorkStream.Read(aVertexList[j].nNumWeights, 2);
          m_pWorkStream.Read(aVertexList[j].nUnknownWord, 2);
          //SetLength(aVertexList[j].arUnknownRotation, aVertexList[j].nNumWeights);
          //SetLength(aVertexList[j].anBoneIndex, aVertexList[j].nNumWeights);
          SetLength(aVertexList[j].aWeightList, aVertexList[j].nNumWeights);
          for k := 0 to aVertexList[j].nNumWeights - 1 do
          begin
            //m_pWorkStream.Read(aVertexList[j].anBoneIndex[k], 4);
            //m_pWorkStream.Read(aVertexList[j].arUnknownRotation[k], SizeOf(LTRotation));
            m_pWorkStream.Read(aVertexList[j].aWeightList[k].nNodeIndex, 4);
            m_pWorkStream.Read(aVertexList[j].aWeightList[k].vLocation, SizeOf(LTVector));
            m_pWorkStream.Read(aVertexList[j].aWeightList[k].fBias, 4);
          end;
          //m_pWorkStream.Read(aVertexList[j].nBoneIndex, 4);
          //m_pWorkStream.Read(aVertexList[j].rUnknownRotation, SizeOf(LTRotation));
          m_pWorkStream.Read(aVertexList[j].vVertex, SizeOf(LTVector));
          m_pWorkStream.Read(aVertexList[j].vNormal, SizeOf(LTVector));
        end;

      end;
    end;
  end;

  //with m_ABCModel.Pieces do

end;

function TABCParser.FindNodeFromTransformIndex(nIndex: Word): Cardinal;
var i: Cardinal;
begin
  for i := 0 to Length(m_ABCModel.Nodes.aItems) - 1 do
  begin
    if m_ABCModel.Nodes.aItems[i].nTransformIndex = nIndex then Exit(i);
  end;
  Result := 0;
end;

function TABCParser.ReadNodeItem(var nIndex: Cardinal; nChildren: Cardinal; nTempParentIndex: Word): Boolean;
var i: Cardinal;
begin

  Result := True;
  nIndex := nIndex + 1;
  SetLength(m_ABCModel.Nodes.aItems, nIndex);

  with m_ABCModel.Nodes.aItems[nIndex - 1] do
  begin
    m_pWorkStream.Read(nNameLength, 2);
    SetLength(szName, nNameLength);
    m_pWorkStream.Read(szName[1], nNameLength);
    m_pWorkStream.Read(nTransformIndex, 2);
    nParentIndex := nTempParentIndex;
    m_pWorkStream.Read(nFlags, 1);
    m_pWorkStream.Read(aMatrix, SizeOf(LTRotation) * 4);
    m_pWorkStream.Read(nNumChildred, 4);

    if nNumChildred = 0 then
      Exit(False);

    for i := 0 to nNumChildred - 1 do
      ReadNodeItem(nIndex, nNumChildred, nTransformIndex);

  end;

end;

procedure TABCParser.ReadNodes(nNextOffset: Cardinal; nOffset: Cardinal);
var nIndex, i: Cardinal;
begin
  WLog('Nodes chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  nIndex := 0;
  ReadNodeItem(nIndex, 0, $FFFF);

  with m_ABCModel.Nodes.WeightSets do
  begin
    m_pWorkStream.Read(nNumSets, 4);

    if nNumSets > 0 then
    begin
      SetLength(aItems, nNumSets);
      for i := 0 to nNumSets - 1 do
      begin
        m_pWorkStream.Read(aItems[i].nNameLength, 2);
        SetLength(aItems[i].szName, aItems[i].nNameLength);
        m_pWorkStream.Read(aItems[i].szName[1], aItems[i].nNameLength);
        m_pWorkStream.Read(aItems[i].nNumItems, 4);
        SetLength(aItems[i].afWeights, aItems[i].nNumItems);
        m_pWorkStream.Read(aItems[i].afWeights[0], aItems[i].nNumItems * 4);
      end;
    end;

  end;
end;

procedure TABCParser.ReadChildModels(nNextOffset: Cardinal; nOffset: Cardinal);
var i, j: Cardinal;
begin
  WLog('ChildModels chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  {with m_ABCModel.ChildModels do
  begin
    m_pWorkStream.Read(nChildModelsCount, 4);
    SetLength(szUnknownBlock, nNextOffset - m_pWorkStream.Position);
    m_pWorkStream.Read(szUnknownBlock[1], nNextOffset - m_pWorkStream.Position);
    WLog('ChildModels size = ' + IntToStr(Length(szUnknownBlock)));
  end;
  DumpData(m_ABCModel.ChildModels.szUnknownBlock[1], 'zz_ChildModels.dat' , Length(m_ABCModel.ChildModels.szUnknownBlock)); }
  with m_ABCModel.ChildModels do
  begin
    m_pWorkStream.Read(nChildModelsCount, 4);
    SetLength(aItems, nChildModelsCount);
    m_pWorkStream.Read(aItems[0].nUnknownCardinal, 4);
    aItems[0].nNameLength := 4;
    aItems[0].szName := 'SELF';
    SetLength(aItems[0].aRelations, Length(ABCModel.Nodes.aItems));
    for i := 0 to Length(ABCModel.Nodes.aItems)-1 do
    begin
      m_pWorkStream.Read(aItems[0].aRelations[i].vPos, SizeOf(LTVector));
      m_pWorkStream.Read(aItems[0].aRelations[i].rRot, SizeOf(LTRotation));
    end;

    for j := 1 to nChildModelsCount - 1 do
    begin
      m_pWorkStream.Read(aItems[j].nNameLength, 2);
      SetLength(aItems[j].szName, aItems[j].nNameLength);
      m_pWorkStream.Read(aItems[j].szName[1], aItems[j].nNameLength);
      m_pWorkStream.Read(aItems[j].nUnknownCardinal, 4);
      SetLength(aItems[j].aRelations, Length(ABCModel.Nodes.aItems));
      for i := 0 to Length(ABCModel.Nodes.aItems)-1 do
      begin
        m_pWorkStream.Read(aItems[j].aRelations[i].vPos, SizeOf(LTVector));
        m_pWorkStream.Read(aItems[j].aRelations[i].rRot, SizeOf(LTRotation));
      end;
    end;

  end;
end;

procedure TABCParser.ReadAnimation(nNextOffset: Cardinal; nOffset: Cardinal);
var n, i, j, k: Cardinal;
begin
  WLog('Animation chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  with m_ABCModel.Animation do
  begin
    m_pWorkStream.Read(nAnimCount, 4);
    n := 0;
    repeat
      SetLength(aItems, n + 1);
      m_pWorkStream.Read(aItems[n].vDimensions, SizeOf(LTVector));
      m_pWorkStream.Read(aItems[n].nNameLength, 2);
      SetLength(aItems[n].szName, aItems[n].nNameLength);
      m_pWorkStream.Read(aItems[n].szName[1], aItems[n].nNameLength);
      m_pWorkStream.Read(aItems[n].nCompressionType, 4);
      m_pWorkStream.Read(aItems[n].nInterpTime, 4);
      m_pWorkStream.Read(aItems[n].nKeyFrames, 4);

      if aItems[n].nKeyFrames > 0 then
      begin
        SetLength(aItems[n].aKeyFrames, aItems[n].nKeyFrames);
        for i := 0 to aItems[n].nKeyFrames - 1 do
        begin
          m_pWorkStream.Read(aItems[n].aKeyFrames[i].nTime, 4);
          m_pWorkStream.Read(aItems[n].aKeyFrames[i].nCmdLen, 2);

          if aItems[n].aKeyFrames[i].nCmdLen > 0 then
          begin
            SetLength(aItems[n].aKeyFrames[i].szCmd, aItems[n].aKeyFrames[i].nCmdLen);
            m_pWorkStream.Read(aItems[n].aKeyFrames[i].szCmd[1], aItems[n].aKeyFrames[i].nCmdLen);
          end;

        end;
      end;

      SetLength(aItems[n].aNodeAnims, Length(m_ABCModel.Nodes.aItems));
      for j := 0 to Length(m_ABCModel.Nodes.aItems) - 1 do
      begin
        SetLength(aItems[n].aNodeAnims[j].avPos, aItems[n].nKeyFrames);
        SetLength(aItems[n].aNodeAnims[j].arRot, aItems[n].nKeyFrames);
        for k := 0 to aItems[n].nKeyFrames - 1 do
        begin
          m_pWorkStream.Read(aItems[n].aNodeAnims[j].avPos[k], SizeOf(LTVector));
          m_pWorkStream.Read(aItems[n].aNodeAnims[j].arRot[k], SizeOf(LTRotation));
        end;
      end;

      Inc(n, 1);
    until n > nAnimCount - 1;
  end;
  //WLog(IntToStr(Length(m_ABCModel.Animation.aItems)));
end;

procedure TABCParser.ReadSockets(nNextOffset: Cardinal; nOffset: Cardinal);
var n: Integer;
begin
  WLog('Sockets chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  with m_ABCModel.Sockets do
  begin
    m_pWorkStream.Read(nSocketsCount, 4);
    n := 0;
    // can be 0
    if nSocketsCount > 0 then
    begin
      repeat
        SetLength(aItems, n + 1);
        m_pWorkStream.Read(aItems[n].nNodeIndex, 4);
        m_pWorkStream.Read(aItems[n].nNameLength, 2);
        SetLength(aItems[n].szName, aItems[n].nNameLength);
        m_pWorkStream.Read(aItems[n].szName[1], aItems[n].nNameLength);
        m_pWorkStream.Read(aItems[n].rQuat, sizeof(LTRotation));
        m_pWorkStream.Read(aItems[n].vPos, sizeof(LTVector));
        Inc(n, 1);
      until n > nSocketsCount - 1;
    end;
  end;
  //WLogSockets;
end;

procedure TABCParser.ReadAnimBindings(nNextOffset: Cardinal; nOffset: Cardinal);
var m, n, i: Cardinal;
begin
  WLog('AnimBindings chunk found, offset: ' + IntToHex(nOffset, 8) + 'h');
  n := 0;
  with m_ABCModel.AnimBindings do
  begin
    for i := 0 to m_ABCModel.ChildModels.nChildModelsCount - 1 do
    begin
      m_pWorkStream.Read(nRealAnimsCount, 4);
      m := 0;
      repeat
        SetLength(aItems, n + 1);
        aItems[n].nAnimsCount := nRealAnimsCount;
        m_pWorkStream.Read(aItems[n].nNameLength, 2);
        SetLength(aItems[n].szName, aItems[n].nNameLength);
        m_pWorkStream.Read(aItems[n].szName[1], aItems[n].nNameLength);
        m_pWorkStream.Read(aItems[n].vDims, sizeof(LTVector));
        m_pWorkStream.Read(aItems[n].vTranslation, sizeof(LTVector));
        Inc(n, 1);
        Inc(m, 1);
      until m > nRealAnimsCount - 1;
    end;
    nRealAnimsCount := n;
  end;
  //WLogAnimBindings;
end;

function TABCParser.ReadModel: Boolean;
var szChunkName: string = '';
    nChunkNameLength: Word;
    nNextPos: Cardinal;
begin

  m_bSmthWentWrong := False;
  nChunkNameLength := 0;
  m_pWorkStream.Position := 0;
  nNextPos := 0;

  repeat

    m_pWorkStream.Read(nChunkNameLength, 2);
    SetLength(szChunkName, nChunkNameLength);
    m_pWorkStream.Read(szChunkName[1], nChunkNameLength);
    m_pWorkStream.Read(nNextPos, 4);

    if szChunkName = 'Header' then
    begin
      ReadHeader(nNextPos, m_pWorkStream.Position);

      if m_ABCModel.Header.nFileVersion <> 12 then
      begin
        WLog('ERROR! File version mismatch!');
        m_bSmthWentWrong := True;
        Exit(False);
      end;
      {if m_ABCModel.Header.nChildModelCount > 1 then
      begin
        WLog('ERROR! Model contains two or more child models, please remove them before loading!');
        m_bSmthWentWrong := True;
        Exit(False);
      end;  }
      if m_ABCModel.Header.nLODCount > 1 then
      begin
        WLog('ERROR! Model contains two or more LODs, please remove them before loading!');
        m_bSmthWentWrong := True;
        Exit(False);
      end;
    end
    else if (szChunkName = 'Pieces') then ReadPieces(nNextPos, m_pWorkStream.Position)
    else if (szChunkName = 'Nodes') then ReadNodes(nNextPos, m_pWorkStream.Position)
    else if (szChunkName = 'ChildModels') then
    begin
      m_anOffsetHolder[ID_CHILDMODELS] := m_pWorkStream.Position;
      ReadChildModels(nNextPos, m_pWorkStream.Position)
    end
    else if (szChunkName = 'Animation') then ReadAnimation(nNextPos, m_pWorkStream.Position)
    else if (szChunkName = 'Sockets') then ReadSockets(nNextPos, m_pWorkStream.Position)
    else if (szChunkName = 'AnimBindings') then ReadAnimBindings(nNextPos, m_pWorkStream.Position);

    m_pWorkStream.Position := nNextPos;

  until nNextPos = $FFFFFFFF;

  Result := True;
end;

function TABCParser.OpenFile(S: string): Boolean;
begin
  m_pWorkStream := TMemoryStream.Create;
  m_pWorkStream.LoadFromFile(S);
  WLog(IntToStr(m_pWorkStream.Size) + ' bytes were read from disk');
  m_nTotalBytes := m_pWorkStream.Size;
  Result := ReadModel;
end;

{procedure TABCParser.FixExportedModel(S: string);
var i,j: Cardinal;
begin
  m_pReadStream := TFileStream.Create(S, fmOpenReadWrite);
  m_pReadStream.Position := m_anOffsetHolder[ID_PIECES];

  for i := 0 to m_ABCModel.Pieces.nPiecesCount - 1 do
  for j := 0 to m_ABCModel.Pieces.aItems[i].nNumVerticles - 1 do
  begin

    nNodeIndex := FindNodeFromTransformIndex(m_ABCModel.Pieces.aItems[i].aVertexList[j].nBoneIndex);

    m_ABCModel.Pieces.aItems[i].aVertexList[j].rUnknownRotation.x :=
    m_ABCModel.Pieces.aItems[i].aVertexList[j].vVertex.x *
    m_ABCModel.Nodes.aItems[nNodeIndex].aMatrix[0].x +
    m_ABCModel.Nodes.aItems[nNodeIndex].aMatrix[0].w;

    m_ABCModel.Pieces.aItems[i].aVertexList[j].rUnknownRotation.y :=
    m_ABCModel.Pieces.aItems[i].aVertexList[j].vVertex.y *
    m_ABCModel.Nodes.aItems[nNodeIndex].aMatrix[1].y +
    m_ABCModel.Nodes.aItems[nNodeIndex].aMatrix[1].w;

    m_ABCModel.Pieces.aItems[i].aVertexList[j].rUnknownRotation.z :=
    m_ABCModel.Pieces.aItems[i].aVertexList[j].vVertex.z *
    m_ABCModel.Nodes.aItems[nNodeIndex].aMatrix[2].z -
    m_ABCModel.Nodes.aItems[nNodeIndex].aMatrix[2].w;


   m_ABCModel.Pieces.aItems[i].aVertexList[j].vVertex.x := 0;
    m_ABCModel.Pieces.aItems[i].aVertexList[j].vVertex.y := 0;
    m_ABCModel.Pieces.aItems[i].aVertexList[j].vVertex.z := 0;

  end;

  WritePieces;
  m_pReadStream.Free;
end; }

procedure TABCParser.WritePieces;
//var i,j: Cardinal;
begin

  {with m_ABCModel.Pieces do
  begin
    m_pReadStream.Write(nUnknownCardinal, 4);
    m_pReadStream.Write(nPiecesCount, 4);
    for i := 0 to nPiecesCount - 1 do
    begin
      with aItems[i] do
      begin
        m_pReadStream.Write(nTextureIndex, 2);
        m_pReadStream.Write(fSpecularPower, 4);
        m_pReadStream.Write(fSpecularScale, 4);
        m_pReadStream.Write(fUnknownFloat1, 4);
        m_pReadStream.Write(nUnknownWord2, 2);
        m_pReadStream.Write(nNameLength, 2);
        m_pReadStream.Write(szName[1], nNameLength);
        m_pReadStream.Write(nNumTris, 4);

        for j := 0 to Int64(nNumTris) * 3 - 1 do
        begin
          m_pReadStream.Write(aUVMap[j].UV, SizeOf(TUVPair));
          m_pReadStream.Write(aUVMap[j].nTriFS, 2);
        end;

        m_pReadStream.Write(nNumVerticles, 4);

        for j := 0 to nNumVerticles - 1 do
        begin
          m_pReadStream.Write(aVertexList[j].nNumWeights, 4);
          //m_pReadStream.Write(aVertexList[j].nBoneIndex, 4);
          //m_pReadStream.Write(aVertexList[j].rUnknownRotation, SizeOf(LTRotation));
          m_pReadStream.Write(aVertexList[j].vVertex, SizeOf(LTVector));
          m_pReadStream.Write(aVertexList[j].vNormal, SizeOf(LTVector));
        end;

      end;
    end;
  end;     }
end;

procedure TABCParser.ABCDataToTreeView(tv: TTreeView);
var HeaderNode: TTreeNode;
    PiecesNode: TTreeNode;
    NodesNode: TTreeNode;
    ChildModelsNode: TTreeNode;
    AnimationNode: TTreeNode;
    SocketsNode: TTreeNode;
    AnimBindingsNode: TTreeNode;
    AnimWeightSetsNode: TTreeNode;
    AnimItemNode: TTreeNode;
    KeyFrameRootNode: TTreeNode;
    NodeAnimsRootNode: TTreeNode;
    PieceItemNode: TTreeNode;
    UVMapRootNode: TTreeNode;
    VertexRootNode: TTreeNode;
    i,j: Cardinal;
begin
  HeaderNode := tv.Items.Add(nil, 'Header');
  HeaderNode.Data := CreateID(ID_HEADER, [0, 0, 0, 0]);

  PiecesNode := tv.Items.Add(nil, 'Pieces');
  PiecesNode.Data := nil;

  if m_ABCModel.Pieces.aItems <> nil then
  begin
    for i := 0 to Length(m_ABCModel.Pieces.aItems) - 1 do
    begin
      PieceItemNode := tv.Items.AddChild(PiecesNode, m_ABCModel.Pieces.aItems[i].szName);
      PieceItemNode.Data := CreateID(ID_PIECES, [i, 0, 0, 0]);

      UVMapRootNode := tv.Items.AddChild(PieceItemNode, 'UVMap');
      UVMapRootNode.Data := CreateID(ID_UVMAP, [i, 0, 0, 0]);

      for j := 0 to Length(m_ABCModel.Pieces.aItems[i].aVertexList) - 1 do
      begin
        VertexRootNode := tv.Items.AddChild(PieceItemNode, 'Vertex#' + IntToStr(j));
        VertexRootNode.Data := CreateID(ID_VERTEX, [i, j, 0, 0]);
        tv.Items.AddChild(VertexRootNode, 'Weights').Data := CreateID(ID_VERTEX_WEIGHT, [i, j, 0, 0]);
      end;
    end;
  end;

  NodesNode := tv.Items.Add(nil, 'Nodes');
  NodesNode.Data := nil;

  if m_ABCModel.Nodes.aItems <> nil then
  begin
    for i := 0 to Length(m_ABCModel.Nodes.aItems) - 1 do
    begin
      tv.Items.AddChild(NodesNode, m_ABCModel.Nodes.aItems[i].szName).Data := CreateID(ID_NODES, [i, 0, 0, 0]);
      //WLog(IntToStr(i));
    end;
  end;


  AnimationNode := tv.Items.Add(nil, 'Animation');
  AnimationNode.Data := nil;

  if m_ABCModel.Animation.nAnimCount > 0 then
  begin
    for i := 0 to m_ABCModel.Animation.nAnimCount - 1 do
    begin
      AnimItemNode := tv.Items.AddChild(AnimationNode, m_ABCModel.Animation.aItems[i].szName);
      AnimItemNode.Data := CreateID(ID_ANIMATION, [i, 0, 0, 0]);;

      KeyFrameRootNode := tv.Items.AddChild(AnimItemNode, 'KeyFrames');
      KeyFrameRootNode.Data := nil;
      NodeAnimsRootNode := tv.Items.AddChild(AnimItemNode, 'NodeAnimList');
      NodeAnimsRootNode.Data := nil;

      for j := 0 to m_ABCModel.Animation.aItems[i].nKeyFrames - 1 do
      begin
        tv.Items.AddChild(KeyFrameRootNode, '#' + IntToStr(j)).Data := CreateID(ID_KEYFRAME, [i, j, 0, 0]);
      end;

      for j := 0 to Length(m_ABCModel.Nodes.aItems) - 1 do
      begin
        tv.Items.AddChild(NodeAnimsRootNode, '#' + IntToStr(j)).Data := CreateID(ID_NODEANIMLIST, [i, j, 0, 0]);
      end;
    end;
  end;

  // can be 0
  if m_ABCModel.Sockets.aItems <> nil then
  begin
    if Length(m_ABCModel.Sockets.aItems) > 0 then
    begin
      SocketsNode := tv.Items.Add(nil, 'Sockets');
      SocketsNode.Data := nil;

      for i := 0 to Length(m_ABCModel.Sockets.aItems) - 1 do
      begin
        tv.Items.AddChild(SocketsNode, m_ABCModel.Sockets.aItems[i].szName).Data := CreateID(ID_SOCKETS, [i, 0, 0, 0]);;
      end;
    end;
  end;


  AnimBindingsNode := tv.Items.Add(nil, 'Animation Bindings');
  AnimBindingsNode.Data := nil;
  //WLog(IntToStr(Length(m_ABCModel.AnimBindings.aItems)));
  if m_ABCModel.AnimBindings.aItems <> nil then
  begin
    for i := 0 to Length(m_ABCModel.AnimBindings.aItems) - 1 do
    begin
      tv.Items.AddChild(AnimBindingsNode, m_ABCModel.AnimBindings.aItems[i].szName).Data := CreateID(ID_ANIMBINDINGS, [i, 0, 0, 0]);;
    end;
  end;

  if m_ABCModel.Nodes.WeightSets.aItems <> nil then
  begin
    if Length(m_ABCModel.Nodes.WeightSets.aItems) > 0 then
    begin
      AnimWeightSetsNode := tv.Items.Add(nil, 'Animation Weight Sets');
      AnimWeightSetsNode.Data := nil;
      for i := 0 to Length(m_ABCModel.Nodes.WeightSets.aItems) - 1 do
      begin
        tv.Items.AddChild(AnimWeightSetsNode, m_ABCModel.Nodes.WeightSets.aItems[i].szName).Data := CreateID(ID_ANIMWEIGHTSETS, [i, 0, 0, 0]);
      end;
    end;
  end;

  if m_ABCModel.ChildModels.aItems <> nil then
  begin
    if Length(m_ABCModel.ChildModels.aItems) > 0 then
    begin
      ChildModelsNode := tv.Items.Add(nil, 'Child Models');
      ChildModelsNode.Data := nil;
      for i := 0 to Length(m_ABCModel.ChildModels.aItems) - 1 do
      begin
        tv.Items.AddChild(ChildModelsNode, m_ABCModel.ChildModels.aItems[i].szName).Data := CreateID(ID_CHILDMODELS, [i, 0, 0, 0]);
      end;
    end;
  end;

end;

constructor TABCParser.Create;
begin
  inherited;
  m_slView := TStringList.Create;
end;

destructor TABCParser.Destroy;
begin
  inherited;
  m_slView.Free;
  DestructorHelper;
  SetLength(m_ABCModel.AnimBindings.aItems, 0);
  SetLength(m_ABCModel.Sockets.aItems, 0);
end;

end.

