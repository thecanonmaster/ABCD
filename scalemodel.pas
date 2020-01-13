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
    cbxScaleSkeleton: TCheckBox;
    cbxScaleDims: TCheckBox;
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
    i, j, k: Cardinal;
    nNodes: Cardinal;
begin
  // ZERO
  if cbOrigin.ItemIndex = 0 then
  begin
    vScale.x := StrToFloat(edtScalingFactorX.Text);
    vScale.y := StrToFloat(edtScalingFactorY.Text);
    vScale.z := StrToFloat(edtScalingFactorZ.Text);

    with ABCModel^.Pieces do
    begin
      for i := 0 to nPiecesCount - 1 do
      begin
        for j := 0 to aItems[i].nNumVerticles -1 do
        begin
          aItems[i].aVertexList[j].vVertex.x := aItems[i].aVertexList[j].vVertex.x * vScale.x;
          aItems[i].aVertexList[j].vVertex.y := aItems[i].aVertexList[j].vVertex.y * vScale.y;
          aItems[i].aVertexList[j].vVertex.z := aItems[i].aVertexList[j].vVertex.z * vScale.z;
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
          aItems[i].aMatrix[0].w := aItems[i].aMatrix[0].w * vScale.x;
          aItems[i].aMatrix[1].w := aItems[i].aMatrix[1].w * vScale.y;
          aItems[i].aMatrix[2].w := aItems[i].aMatrix[2].w * vScale.z;
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
            aItems[i].aNodeAnims[j].avPos[k].x := aItems[i].aNodeAnims[j].avPos[k].x * vScale.x;
            aItems[i].aNodeAnims[j].avPos[k].y := aItems[i].aNodeAnims[j].avPos[k].y * vScale.y;
            aItems[i].aNodeAnims[j].avPos[k].z := aItems[i].aNodeAnims[j].avPos[k].x * vScale.z;
          end;
        end;

      end;
    end;

    ShowMessage('Scaling done!');
  end;
end;

procedure TScaleModelForm.FormShow(Sender: TObject);
begin
  ABCModel := @pABCModel.ABCModel;
end;

end.

