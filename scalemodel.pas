unit scalemodel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, abcmodeltypes;

type

  { TScaleModelForm }

  TScaleModelForm = class(TForm)
    btnScale: TButton;
    cbOrigin: TComboBox;
    cbxScaleAnims: TCheckBox;
    cbxScaleSockets: TCheckBox;
    cbxScaleSkeleton: TCheckBox;
    cbxScaleDims: TCheckBox;
    cbxScaleMesh: TCheckBox;
    edtScalingFactorX: TEdit;
    edtScalingFactorY: TEdit;
    edtScalingFactorZ: TEdit;
    lblScalingFactorX: TLabel;
    lblOrigin: TLabel;
    lblScalingFactorY: TLabel;
    lblScalingFactorZ: TLabel;
    procedure btnScaleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ScaleModelForm: TScaleModelForm;
  ABCModel: PABCModel;

implementation

uses main;

{$R *.lfm}

{ TScaleModelForm }

procedure TScaleModelForm.btnScaleClick(Sender: TObject);
var vScale: LTVector;
    vOrigin: LTVector;
    i, j, k: Cardinal;
    nNodes: Cardinal;
begin
  if cbOrigin.ItemIndex = 0 then
  begin
    vOrigin.x := 0.0;
    vOrigin.y := 0.0;
    vOrigin.z := 0.0;
  end;
  {else if cbOrigin.ItemIndex = 1 then
  begin
    with ABCModel^.Nodes do
    begin
      vOrigin.x := aItems[0].aMatrix[0].w;
      vOrigin.y := aItems[0].aMatrix[1].w;
      vOrigin.z := aItems[0].aMatrix[2].w;
    end;
  end;}

  vScale.x := StrToFloat(edtScalingFactorX.Text);
  vScale.y := StrToFloat(edtScalingFactorY.Text);
  vScale.z := StrToFloat(edtScalingFactorZ.Text);

  if cbxScaleMesh.Checked then
  begin
    with ABCModel^.Pieces do
    begin
      if nPiecesCount > 0 then
      for i := 0 to nPiecesCount - 1 do
      begin
        for j := 0 to aItems[i].nNumVerticles -1 do
        begin
          aItems[i].aVertexList[j].vVertex.x :=
            (aItems[i].aVertexList[j].vVertex.x - vOrigin.x) * vScale.x;
          aItems[i].aVertexList[j].vVertex.y :=
            (aItems[i].aVertexList[j].vVertex.y - vOrigin.y) * vScale.y;
          aItems[i].aVertexList[j].vVertex.z :=
            (aItems[i].aVertexList[j].vVertex.z - vOrigin.z) * vScale.z;
        end;
      end;
    end;
  end;

  if cbxScaleSkeleton.Checked then
  begin
    with ABCModel^.Nodes do
    begin
      nNodes := Length(aItems);
      for i := 0 to nNodes - 1 do
      begin
        aItems[i].aMatrix[0].w :=
          (aItems[i].aMatrix[0].w - vOrigin.x) * vScale.x;
        aItems[i].aMatrix[1].w :=
          (aItems[i].aMatrix[1].w - vOrigin.y) * vScale.y;
        aItems[i].aMatrix[2].w :=
          (aItems[i].aMatrix[2].w - vOrigin.z) * vScale.z;
      end;
    end;
  end;

  if cbxScaleDims.Checked then
  begin

    with ABCModel^.AnimBindings do
    begin
      for i := 0 to nRealAnimsCount-1 do
      begin
        aItems[i].vDims.x := aItems[i].vDims.x * vScale.x;
        aItems[i].vDims.y := aItems[i].vDims.y * vScale.y;
        aItems[i].vDims.z := aItems[i].vDims.z * vScale.z;
      end;
    end;

    with ABCModel^.Animation do
    begin
      for i := 0 to nAnimCount-1 do
      begin
        aItems[i].vDimensions.x := aItems[i].vDimensions.x * vScale.x;
        aItems[i].vDimensions.y := aItems[i].vDimensions.y * vScale.y;
        aItems[i].vDimensions.z := aItems[i].vDimensions.z * vScale.z;
      end;
    end;

  end;

  if cbxScaleAnims.Checked then
  begin
    nNodes := Length(ABCModel^.Nodes.aItems);
    with ABCModel^.Animation do
    begin
      for i := 0 to nAnimCount-1 do
      for j := 0 to nNodes-1 do
      begin
        for k := 0 to aItems[i].nKeyFrames-1  do
        begin
          aItems[i].aNodeAnims[j].avPos[k].x :=
            (aItems[i].aNodeAnims[j].avPos[k].x - vOrigin.x) * vScale.x;
          aItems[i].aNodeAnims[j].avPos[k].y :=
            (aItems[i].aNodeAnims[j].avPos[k].y - vOrigin.y) * vScale.y;
          aItems[i].aNodeAnims[j].avPos[k].z :=
            (aItems[i].aNodeAnims[j].avPos[k].z - vOrigin.z) * vScale.z;
        end;
      end;
    end;

    with ABCModel^.AnimBindings do
    begin
      for i := 0 to nRealAnimsCount-1 do
      begin
        aItems[i].vTranslation.x :=
          (aItems[i].vTranslation.x - vOrigin.x) * vScale.x;
        aItems[i].vTranslation.y :=
          (aItems[i].vTranslation.y - vOrigin.y) * vScale.y;
        aItems[i].vTranslation.z :=
          (aItems[i].vTranslation.z - vOrigin.z) * vScale.z;
      end;
    end;
  end;

  if cbxScaleSockets.Checked then
  begin
    with ABCModel^.Sockets do
    begin
      if nSocketsCount > 0 then
      for i := 0 to nSocketsCount - 1 do
      begin
        aItems[i].vPos.x := (aItems[i].vPos.x - vOrigin.x) * vScale.x;
        aItems[i].vPos.y := (aItems[i].vPos.y - vOrigin.y) * vScale.y;
        aItems[i].vPos.z := (aItems[i].vPos.z - vOrigin.z) * vScale.z;
      end;
    end;
  end;

  ShowMessage('Scaling done!');
end;

procedure TScaleModelForm.FormShow(Sender: TObject);
begin
  ABCModel := @pABCModel.ABCModel;
end;

end.

