unit abcmodeltypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type
  TLogProc = procedure(Msg: string);
  TViewProc = procedure(List: TStringList);

  LTFloat = Single;
  LTVector = record
    x: LTFloat;
    y: LTFloat;
    z: LTFloat;
  end;

  LTRotation = record
    x: LTFloat;
    y: LTFloat;
    z: LTFloat;
    w: LTFloat;
  end;

  TUVPair = record
    a: LTFloat;
    b: LTFloat;
  end;

  LTMatrix = array[0..3, 0..3] of LTFloat;

  PLTVector = ^LTVector;
  PLTRotation = ^LTRotation;
  PUVPair = ^TUVPair;
  PLTMatrix = ^LTMatrix;

  TStringArray = array of string;
  TIntArray = array of Integer;
  TVecArray = array of LTVector;
  TCardinalArray = array of Cardinal;
  TRotArray = array of LTRotation;
  TFloatArray = array of LTFloat;

  TABCHeader = record
    nFileVersion: Cardinal;
    //anUnknownBlock1: array[0..12] of Cardinal; //56 bytes

    nKeyframeCount: Cardinal;
    nAnimsCount: Cardinal;
    nNodesCount: Cardinal;
    nPiecesCount: Cardinal;
    nChildModelCount: Cardinal;
    nTrisCount: Cardinal;
    nVertsCount: Cardinal;
    nVertWeightsCount: Cardinal;
    nLODCount: Cardinal;
    nSocketCount: Cardinal;
    nWeightSetCount: Cardinal;
    nStringCount: Cardinal;
    nStringLengthTotal: Cardinal;

    nCmdLenght: Word;
    szCmdString: string;
    fGlobalRadius: LTFloat;
    nNumLOD: Cardinal;
    //szUnknownBlock2: string;
    anPadding: array[0..59] of uint8;
    afLODDistances: array of LTFloat;
  end;

  TABCPieceUVMapItem = record
    UV: TUVPair;
    nTriFS: Word;
  end;

  PABCWeightListItem = ^TABCWeightListItem;

  TABCWeightListItem = record
    nNodeIndex: Cardinal;
    vLocation: LTVector;
    fBias: LTFloat;
  end;

  TABCWeightList = array of TABCWeightListItem;

  PABCPieceUVMapItem = ^TABCPieceUVMapItem;

  TABCPieceVertex = record
    nNumWeights: Word;
    nUnknownWord: Word;
    //anBoneIndex: TCardinalArray;
    //arUnknownRotation: TRotArray;
    aWeightList: TABCWeightList;
    vVertex: LTVector;
    vNormal: LTVector;
  end;

  PABCPieceVertex = ^TABCPieceVertex;

  TABCPieceUVMapList = array of TABCPieceUVMapItem;
  TABCPieceVertexList = array of TABCPieceVertex;
  PABCPieceUVMapList = ^TABCPieceUVMapList;
  PABCPieceVertexList = ^TABCPieceVertexList;

  TABCPiecesItem = record
    nTextureIndex: Word;
    fSpecularPower: LTFloat;
    fSpecularScale: LTFloat;
    fLODWeight: LTFloat;
    nUnknownWord2: Word;
    nNameLength: Word;
    szName: string;
    nNumTris: Cardinal;
    aUVMap: TABCPieceUVMapList;
    nNumVerticles: Cardinal;
    aVertexList: TABCPieceVertexList;
  end;

  PABCPiecesItem = ^TABCPiecesItem;

  TABCPiecesList = array of TABCPiecesItem;

  TABCPieces = record
    nWeightCount: Cardinal;
    nPiecesCount: Cardinal;
    aItems: TABCPiecesList;
  end;

  TABCNodesItem = record
    nNameLength: Word;
    szName: string;
    nTransformIndex: Word;
    nFlags: Byte;
    aMatrix: array[0..3] of LTRotation;
    nNumChildred: Cardinal;
  end;

  PABCNodesItem = ^TABCNodesItem;
  TABCNodesList = array of TABCNodesItem;

  TABCWeightSetsItem = record
    nNameLength: Word;
    szName: string;
    nNumItems: Cardinal;
    afWeights: TFloatArray;
  end;

  PABCWeightSetsItem = ^TABCWeightSetsItem;
  TABCWeightSetsList = array of TABCWeightSetsItem;

  TABCWeightSets = record
    nNumSets: Cardinal;
    aItems: TABCWeightSetsList;
  end;

  TABCNodes = record
    aItems: TABCNodesList;
    WeightSets: TABCWeightSets;
  end;

  TABCNodeRelationsItem = record
    vPos: LTVector;
    rRot: LTRotation;
  end;

  PABCNodeRelationsItem = ^TABCNodeRelationsItem;
  TABCNodeRelationsList = array of TABCNodeRelationsItem;

  TABCChildModelItem = record
    nUnknownCardinal: Cardinal;
    nNameLength: Word;
    szName: string;
    aRelations: TABCNodeRelationsList;
  end;

  PABCChildModelItemList = ^TABCChildModelItemList;
  TABCChildModelItemList = array of TABCChildModelItem;

  TABCChildModels = record
    nChildModelsCount: Cardinal;
    aItems: TABCChildModelItemList;
  end;

  TABCKeyFrame = record
    nTime: Cardinal;
    nCmdLen: Word;
    szCmd: string;
  end;

  TABCNodeAnim = record
    avPos: TVecArray;
    arRot: TRotArray;
  end;

  TABCKeyFrameList = array of TABCKeyFrame;
  TABCNodeAnimList = array of TABCNodeAnim;

  TABCAnimationItem = record
    vDimensions: LTVector;
    nNameLength: Word;
    szName: string;
    nCompressionType: Cardinal;
    nInterpTime: Cardinal;
    nKeyFrames: Cardinal;
    aKeyFrames: TABCKeyFrameList;
    aNodeAnims: TABCNodeAnimList;
  end;

  PABCAnimationItem = ^TABCAnimationItem;
  TABCAnimationList = array of TABCAnimationItem;

  TABCAnimation = record
    nAnimCount: Cardinal;
    aItems: TABCAnimationList;
  end;

  PABCAnimation = ^TABCAnimation;

  TABCSocketsItem = record
    nNodeIndex: Cardinal;
    nNameLength: Word;
    szName: string;
    rQuat: LTRotation;
    vPos: LTVector;
  end;

  PABCSocketsItem = ^TABCSocketsItem;
  TABCSocketsList = array of TABCSocketsItem;

  TABCSockets = record
    nSocketsCount: Cardinal;
    aItems: TABCSocketsList;
  end;

  TABCAnimBindingsItem = record
    nAnimsCount: Cardinal;
    nNameLength: Word;
    szName: string;
    vDims: LTVector;
    vTranslation: LTVector;
  end;

  PABCAnimBindingsItem = ^TABCAnimBindingsItem;
  TABCAnimBindingsList = array of TABCAnimBindingsItem;
  PABCAnimBindingsList = ^TABCAnimBindingsList;

  TABCAnimBindings = record
    nRealAnimsCount: Cardinal;
    aItems: TABCAnimBindingsList;
  end;

  TABCModel = record
    Header: TABCHeader;
    Pieces: TABCPieces;
    Nodes: TABCNodes;
    ChildModels: TABCChildModels;
    Animation: TABCAnimation;
    Sockets: TABCSockets;
    AnimBindings: TABCAnimBindings;
  end;

  PABCModel = ^TABCModel;


function UVToStrC(U: PUVPair): string;
function LTVectorToStrC(V: PLTVector): string;
function LTRotationToStrC(R: PLTRotation): string;
procedure QuatToEuler(R: PLTRotation; var X: LTFloat; var Y: LTFloat; var Z: LTFloat);

implementation

function UVToStrC(U: PUVPair): string;
begin
  Result := FormatFloat('0.000000', U^.a) + ' ' +
           FormatFloat('0.000000', U^.b);
end;

function LTVectorToStrC(V: PLTVector): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y) + ' ' +
           FormatFloat('0.000000', V^.z);
end;

function LTRotationToStrC(R: PLTRotation): string;
begin
  Result := FormatFloat('0.000000', R^.x) + ' ' +
           FormatFloat('0.000000', R^.y) + ' ' +
           FormatFloat('0.000000', R^.z) + ' ' +
            FormatFloat('0.000000', R^.w);
end;

procedure SetupMatrixEuler(V: PLTVector; M: PLTMatrix);
var yc, pc, rc, ys, ps, rs: LTFloat;
begin
  yc := cos(V^.y);
  ys := sin(V^.y);
  pc := cos(V^.x);
  ps := sin(V^.x);
  rc := cos(V^.z);
  rs := sin(V^.z);

  M^[0][0] := rc*yc + rs*ps*ys;
  M^[0][1] := -rs*yc + rc*ps*ys;
  M^[0][2] := pc*ys;
  M^[0][3] := 0;

  M^[1][0] := rs*pc;
  M^[1][1] := rc*pc;
  M^[1][2] := -ps;
  M^[1][3] := 0;

  M^[2][0] := -rc*ys + rs*ps*yc;
  M^[2][1] := rs*ys + rc*ps*yc;
  M^[2][2] := pc*yc;
  M^[2][3] := 0;

  M^[3][0] := 0;
  M^[3][1] := 0;
  M^[3][2] := 0;
  M^[3][3] := 1;
end;

{
public void set(Quat4d q1) {
	double test = q1.x*q1.y + q1.z*q1.w;
	if (test > 0.499) { // singularity at north pole
		heading = 2 * atan2(q1.x,q1.w);
		attitude = Math.PI/2;
		bank = 0;
		return;
	}
	if (test < -0.499) { // singularity at south pole
		heading = -2 * atan2(q1.x,q1.w);
		attitude = - Math.PI/2;
		bank = 0;
		return;
	}
    double sqx = q1.x*q1.x;
    double sqy = q1.y*q1.y;
    double sqz = q1.z*q1.z;
    heading = atan2(2*q1.y*q1.w-2*q1.x*q1.z , 1 - 2*sqy - 2*sqz);
	attitude = asin(2*test);
	bank = atan2(2*q1.x*q1.w-2*q1.y*q1.z , 1 - 2*sqx - 2*sqz)
}

}

procedure QuatToEuler(R: PLTRotation; var X: LTFloat; var Y: LTFloat; var Z: LTFloat);
var test, sqx, sqy, sqz: LTFloat;
begin
  test := R^.x*R^.y + R^.z*R^.w;
  if test > 0.499 then
  begin
    X := 2 * arctan2(R^.x, R^.w);
    Y := pi / 2;
    Z := 0;
    Exit;
  end;
  if test < -0.499 then
  begin
    X := -2 * arctan2(R^.x, R^.w);
    Y := -pi / 2;
    Z := 0;
    Exit;
  end;
  sqx :=  R^.x*R^.x;
  sqy :=  R^.y*R^.y;
  sqz :=  R^.z*R^.z;
  X := arctan2(2*R^.y*R^.w-2*R^.x*R^.z , 1 - 2*sqy - 2*sqz);
  Y := arcsin(2*test);
  Z := arctan2(2*R^.x*R^.w-2*R^.y*R^.z , 1 - 2*sqx - 2*sqz);
end;

end.

