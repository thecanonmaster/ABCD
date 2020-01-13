unit animmerge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  abcmodeltypes;

type

  { TAnimMergeForm }

  TAnimMergeForm = class(TForm)
    btnMerge: TButton;
    cbAnim1: TComboBox;
    cbAnim2: TComboBox;
    cbMode: TComboBox;
    edtNewAnim: TEdit;
    edtOffset: TEdit;
    lblAnim1: TLabel;
    lblAnim2: TLabel;
    lblMode: TLabel;
    lblNewAnim: TLabel;
    procedure btnMergeClick(Sender: TObject);
    procedure cbModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure MergeFullFirst;
    procedure MergeFullSecond;
    procedure MergeWithOffset;
    { private declarations }
  public
    { public declarations }
  end;

var
  AnimMergeForm: TAnimMergeForm;
  ABCModel: PABCModel;

implementation

uses main;

procedure TAnimMergeForm.MergeFullFirst;
var i, j, nNodes: Cardinal;
begin
  with ABCModel^.AnimBindings do
  begin
    Inc(nRealAnimsCount, 1);
    SetLength(aItems, nRealAnimsCount);
    aItems[nRealAnimsCount-1].nAnimsCount := nRealAnimsCount;
    aItems[nRealAnimsCount-1].nNameLength := Length(edtNewAnim.Text);
    aItems[nRealAnimsCount-1].szName := edtNewAnim.Text;
    aItems[nRealAnimsCount-1].vDims := aItems[cbAnim1.ItemIndex].vDims;
    aItems[nRealAnimsCount-1].vTranslation := aItems[cbAnim1.ItemIndex].vTranslation;
  end;

  with ABCModel^.Animation do
  begin
    Inc(nAnimCount, 1);
    SetLength(aItems, nAnimCount);
    aItems[nAnimCount-1].vDimensions := aItems[cbAnim1.ItemIndex].vDimensions;
    aItems[nAnimCount-1].nNameLength := Length(edtNewAnim.Text);
    aItems[nAnimCount-1].szName := edtNewAnim.Text;
    aItems[nAnimCount-1].nCompressionType := aItems[cbAnim1.ItemIndex].nCompressionType;
    aItems[nAnimCount-1].nInterpTime := aItems[cbAnim1.ItemIndex].nInterpTime;
    aItems[nAnimCount-1].nKeyFrames := Int64(aItems[cbAnim1.ItemIndex].nKeyFrames) + Int64(aItems[cbAnim2.ItemIndex].nKeyFrames) - 1;

    SetLength(aItems[nAnimCount-1].aKeyFrames, aItems[nAnimCount-1].nKeyFrames);
    for i := 0 to aItems[cbAnim1.ItemIndex].nKeyFrames-1 do
    begin
      aItems[nAnimCount-1].aKeyFrames[i].nCmdLen := aItems[cbAnim1.ItemIndex].aKeyFrames[i].nCmdLen;
      aItems[nAnimCount-1].aKeyFrames[i].szCmd := aItems[cbAnim1.ItemIndex].aKeyFrames[i].szCmd;
      aItems[nAnimCount-1].aKeyFrames[i].nTime := aItems[cbAnim1.ItemIndex].aKeyFrames[i].nTime;
    end;
    for i := 1 to aItems[cbAnim2.ItemIndex].nKeyFrames-1 do
    begin
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1+i].nCmdLen := aItems[cbAnim2.ItemIndex].aKeyFrames[i].nCmdLen;
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1+i].szCmd := aItems[cbAnim2.ItemIndex].aKeyFrames[i].szCmd;
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1+i].nTime :=
      aItems[cbAnim1.ItemIndex].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1].nTime + aItems[cbAnim2.ItemIndex].aKeyFrames[i].nTime;
    end;

    nNodes := Length(ABCModel^.Nodes.aItems);
    SetLength(aItems[nAnimCount-1].aNodeAnims, nNodes);
    for i := 0 to nNodes-1 do
    begin
      SetLength(aItems[nAnimCount-1].aNodeAnims[i].avPos, aItems[nAnimCount-1].nKeyFrames);
      SetLength(aItems[nAnimCount-1].aNodeAnims[i].arRot, aItems[nAnimCount-1].nKeyFrames);
      for j := 0 to aItems[cbAnim1.ItemIndex].nKeyFrames-1 do
      begin
        aItems[nAnimCount-1].aNodeAnims[i].avPos[j] := aItems[cbAnim1.ItemIndex].aNodeAnims[i].avPos[j];
        aItems[nAnimCount-1].aNodeAnims[i].arRot[j] := aItems[cbAnim1.ItemIndex].aNodeAnims[i].arRot[j];
      end;
      for j := 1 to aItems[cbAnim2.ItemIndex].nKeyFrames-1 do
      begin
        aItems[nAnimCount-1].aNodeAnims[i].avPos[aItems[cbAnim1.ItemIndex].nKeyFrames-1+j] := aItems[cbAnim2.ItemIndex].aNodeAnims[i].avPos[j];
        aItems[nAnimCount-1].aNodeAnims[i].arRot[aItems[cbAnim1.ItemIndex].nKeyFrames-1+j] := aItems[cbAnim2.ItemIndex].aNodeAnims[i].arRot[j];
      end;
    end;
  end;
end;

procedure TAnimMergeForm.MergeFullSecond;
var i, j, nNodes: Cardinal;
begin
  with ABCModel^.AnimBindings do
  begin
    Inc(nRealAnimsCount, 1);
    SetLength(aItems, nRealAnimsCount);
    aItems[nRealAnimsCount-1].nAnimsCount := nRealAnimsCount;
    aItems[nRealAnimsCount-1].nNameLength := Length(edtNewAnim.Text);
    aItems[nRealAnimsCount-1].szName := edtNewAnim.Text;
    aItems[nRealAnimsCount-1].vDims := aItems[cbAnim1.ItemIndex].vDims;
    aItems[nRealAnimsCount-1].vTranslation := aItems[cbAnim1.ItemIndex].vTranslation;
  end;

  with ABCModel^.Animation do
  begin
    Inc(nAnimCount, 1);
    SetLength(aItems, nAnimCount);
    aItems[nAnimCount-1].vDimensions := aItems[cbAnim1.ItemIndex].vDimensions;
    aItems[nAnimCount-1].nNameLength := Length(edtNewAnim.Text);
    aItems[nAnimCount-1].szName := edtNewAnim.Text;
    aItems[nAnimCount-1].nCompressionType := aItems[cbAnim1.ItemIndex].nCompressionType;
    aItems[nAnimCount-1].nInterpTime := aItems[cbAnim1.ItemIndex].nInterpTime;
    aItems[nAnimCount-1].nKeyFrames := Int64(aItems[cbAnim1.ItemIndex].nKeyFrames) + Int64(aItems[cbAnim2.ItemIndex].nKeyFrames) - 1;

    SetLength(aItems[nAnimCount-1].aKeyFrames, aItems[nAnimCount-1].nKeyFrames);
    for i := 0 to aItems[cbAnim1.ItemIndex].nKeyFrames do
    begin
      aItems[nAnimCount-1].aKeyFrames[i].nCmdLen := aItems[cbAnim1.ItemIndex].aKeyFrames[i].nCmdLen;
      aItems[nAnimCount-1].aKeyFrames[i].szCmd := aItems[cbAnim1.ItemIndex].aKeyFrames[i].szCmd;
      aItems[nAnimCount-1].aKeyFrames[i].nTime := aItems[cbAnim1.ItemIndex].aKeyFrames[i].nTime;
    end;
    for i := 0 to aItems[cbAnim2.ItemIndex].nKeyFrames-1 do
    begin
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1+i].nCmdLen := aItems[cbAnim2.ItemIndex].aKeyFrames[i].nCmdLen;
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1+i].szCmd := aItems[cbAnim2.ItemIndex].aKeyFrames[i].szCmd;
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1+i].nTime :=
      aItems[cbAnim1.ItemIndex].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1].nTime + aItems[cbAnim2.ItemIndex].aKeyFrames[i].nTime;
    end;

    nNodes := Length(ABCModel^.Nodes.aItems);
    SetLength(aItems[nAnimCount-1].aNodeAnims, nNodes);
    for i := 0 to nNodes-1 do
    begin
      SetLength(aItems[nAnimCount-1].aNodeAnims[i].avPos, aItems[nAnimCount-1].nKeyFrames);
      SetLength(aItems[nAnimCount-1].aNodeAnims[i].arRot, aItems[nAnimCount-1].nKeyFrames);
      for j := 0 to aItems[cbAnim1.ItemIndex].nKeyFrames do
      begin
        aItems[nAnimCount-1].aNodeAnims[i].avPos[j] := aItems[cbAnim1.ItemIndex].aNodeAnims[i].avPos[j];
        aItems[nAnimCount-1].aNodeAnims[i].arRot[j] := aItems[cbAnim1.ItemIndex].aNodeAnims[i].arRot[j];
      end;
      for j := 0 to aItems[cbAnim2.ItemIndex].nKeyFrames-1 do
      begin
        aItems[nAnimCount-1].aNodeAnims[i].avPos[aItems[cbAnim1.ItemIndex].nKeyFrames-1+j] := aItems[cbAnim2.ItemIndex].aNodeAnims[i].avPos[j];
        aItems[nAnimCount-1].aNodeAnims[i].arRot[aItems[cbAnim1.ItemIndex].nKeyFrames-1+j] := aItems[cbAnim2.ItemIndex].aNodeAnims[i].arRot[j];
      end;
    end;
  end;
end;

procedure TAnimMergeForm.MergeWithOffset;
var i, j, nNodes: Cardinal;
    nOffsetTime: Cardinal;
begin
  nOffsetTime := StrToInt(edtOffset.Text);
  with ABCModel^.AnimBindings do
  begin
    Inc(nRealAnimsCount, 1);
    SetLength(aItems, nRealAnimsCount);
    aItems[nRealAnimsCount-1].nAnimsCount := nRealAnimsCount;
    aItems[nRealAnimsCount-1].nNameLength := Length(edtNewAnim.Text);
    aItems[nRealAnimsCount-1].szName := edtNewAnim.Text;
    aItems[nRealAnimsCount-1].vDims := aItems[cbAnim1.ItemIndex].vDims;
    aItems[nRealAnimsCount-1].vTranslation := aItems[cbAnim1.ItemIndex].vTranslation;
  end;

  with ABCModel^.Animation do
  begin
    Inc(nAnimCount, 1);
    SetLength(aItems, nAnimCount);
    aItems[nAnimCount-1].vDimensions := aItems[cbAnim1.ItemIndex].vDimensions;
    aItems[nAnimCount-1].nNameLength := Length(edtNewAnim.Text);
    aItems[nAnimCount-1].szName := edtNewAnim.Text;
    aItems[nAnimCount-1].nCompressionType := aItems[cbAnim1.ItemIndex].nCompressionType;
    aItems[nAnimCount-1].nInterpTime := aItems[cbAnim1.ItemIndex].nInterpTime;
    aItems[nAnimCount-1].nKeyFrames := Int64(aItems[cbAnim1.ItemIndex].nKeyFrames) + Int64(aItems[cbAnim2.ItemIndex].nKeyFrames);

    SetLength(aItems[nAnimCount-1].aKeyFrames, aItems[nAnimCount-1].nKeyFrames);
    for i := 0 to aItems[cbAnim1.ItemIndex].nKeyFrames-1 do
    begin
      aItems[nAnimCount-1].aKeyFrames[i].nCmdLen := aItems[cbAnim1.ItemIndex].aKeyFrames[i].nCmdLen;
      aItems[nAnimCount-1].aKeyFrames[i].szCmd := aItems[cbAnim1.ItemIndex].aKeyFrames[i].szCmd;
      aItems[nAnimCount-1].aKeyFrames[i].nTime := aItems[cbAnim1.ItemIndex].aKeyFrames[i].nTime;
    end;
    for i := 0 to aItems[cbAnim2.ItemIndex].nKeyFrames-1 do
    begin
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames+i].nCmdLen := aItems[cbAnim2.ItemIndex].aKeyFrames[i].nCmdLen;
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames+i].szCmd := aItems[cbAnim2.ItemIndex].aKeyFrames[i].szCmd;
      aItems[nAnimCount-1].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames+i].nTime :=
      aItems[cbAnim1.ItemIndex].aKeyFrames[aItems[cbAnim1.ItemIndex].nKeyFrames-1].nTime + nOffsetTime + aItems[cbAnim2.ItemIndex].aKeyFrames[i].nTime;
    end;

    nNodes := Length(ABCModel^.Nodes.aItems);
    SetLength(aItems[nAnimCount-1].aNodeAnims, nNodes);
    for i := 0 to nNodes-1 do
    begin
      SetLength(aItems[nAnimCount-1].aNodeAnims[i].avPos, aItems[nAnimCount-1].nKeyFrames);
      SetLength(aItems[nAnimCount-1].aNodeAnims[i].arRot, aItems[nAnimCount-1].nKeyFrames);
      for j := 0 to aItems[cbAnim1.ItemIndex].nKeyFrames-1 do
      begin
        aItems[nAnimCount-1].aNodeAnims[i].avPos[j] := aItems[cbAnim1.ItemIndex].aNodeAnims[i].avPos[j];
        aItems[nAnimCount-1].aNodeAnims[i].arRot[j] := aItems[cbAnim1.ItemIndex].aNodeAnims[i].arRot[j];
      end;
      for j := 0 to aItems[cbAnim2.ItemIndex].nKeyFrames-1 do
      begin
        aItems[nAnimCount-1].aNodeAnims[i].avPos[aItems[cbAnim1.ItemIndex].nKeyFrames+j] := aItems[cbAnim2.ItemIndex].aNodeAnims[i].avPos[j];
        aItems[nAnimCount-1].aNodeAnims[i].arRot[aItems[cbAnim1.ItemIndex].nKeyFrames+j] := aItems[cbAnim2.ItemIndex].aNodeAnims[i].arRot[j];
      end;
    end;
  end;
end;

{ TAnimMergeForm }

procedure TAnimMergeForm.FormShow(Sender: TObject);
var i: Cardinal;
begin
  ABCModel := @pABCModel.ABCModel;
  for i := 0 to ABCModel^.Animation.nAnimCount - 1 do
  begin
    cbAnim1.Items.Add(ABCModel^.Animation.aItems[i].szName);
    cbAnim2.Items.Add(ABCModel^.Animation.aItems[i].szName);
    cbAnim1.ItemIndex := 0;
    cbAnim2.ItemIndex := 0;
  end;
end;

procedure TAnimMergeForm.btnMergeClick(Sender: TObject);
begin
  if cbMode.ItemIndex = 0 then MergeFullFirst
  else if cbMode.ItemIndex = 0 then MergeFullSecond
  else MergeWithOffset;

  MainForm.tvData.Items.Clear;
  pABCModel.ABCDataToTreeView(MainForm.tvData);

  ShowMessage('Merging done!');
end;

procedure TAnimMergeForm.cbModeChange(Sender: TObject);
begin
  if cbMode.ItemIndex = 2 then edtOffset.Enabled := True
  else
  begin
    edtOffset.Enabled := False;
    edtOffset.Text := '0';
  end;
end;

{$R *.lfm}

end.

