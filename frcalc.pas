unit frcalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  abcmodeltypes, strutils;

type

  { TFRateCalcForm }

  TFRateCalcForm = class(TForm)
    btnCalc: TButton;
    cbAnim: TComboBox;
    edtDesRate: TEdit;
    lblDesRate: TLabel;
    lblAnim: TLabel;
    lblFireRate: TLabel;
    lblResLength: TLabel;
    lblMsgCount: TLabel;
    lblLength: TLabel;
    lblRatio: TLabel;
    procedure btnCalcClick(Sender: TObject);
    procedure ReadInfo;
    function CountMsg(pAnim: PABCAnimationItem): Cardinal;
    procedure cbAnimChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FRateCalcForm: TFRateCalcForm;
  ABCModel: PABCModel;
  nTime, nMsgCount: Cardinal;
  fRate: LTFloat;

implementation

uses main;

{ TFRateCalcForm }

procedure TFRateCalcForm.ReadInfo;
begin
  nTime := ABCModel^.Animation.aItems[cbAnim.ItemIndex].aKeyFrames[ABCModel^.Animation.aItems[cbAnim.ItemIndex].nKeyFrames-1].nTime;
  nMsgCount := CountMsg(@ABCModel^.Animation.aItems[cbAnim.ItemIndex]);
  fRate := 60000 * nMsgCount / nTime;
  lblLength.Caption := 'Length: ' + IntToStr(nTime) + ' ms';
  lblMsgCount.Caption := 'Msg count: ' + IntToStr(nMsgCount);
  lblFireRate.Caption := 'Fire rate: ' + FloatToStr(fRate);
  //edtDesRate.Text := '';
  lblResLength.Caption := 'Resulted length: N/A';
  lblRatio.Caption := 'Ratio: N/A';
end;

procedure TFRateCalcForm.btnCalcClick(Sender: TObject);
var fLength: LTFloat;
begin
  fLength := fRate * nTime / StrToInt(edtDesRate.Text);
  lblResLength.Caption := 'Resulted length: ' + FloatToStr(fLength);
  lblRatio.Caption := 'Ratio: ' + FloatToStr(fLength / nTime);
end;

function TFRateCalcForm.CountMsg(pAnim: PABCAnimationItem): Cardinal;
var i: Cardinal;
begin
  Result := 0;
  for i := 0 to pAnim^.nKeyFrames-1 do
  begin
    if AnsiContainsText(pAnim^.aKeyFrames[i].szCmd, 'fire_key') then Inc(Result, 1);
  end;
end;

procedure TFRateCalcForm.cbAnimChange(Sender: TObject);
begin
  ReadInfo;
end;

procedure TFRateCalcForm.FormShow(Sender: TObject);
var i: Cardinal;
begin
  ABCModel := @pABCModel.ABCModel;
  for i := 0 to ABCModel^.Animation.nAnimCount-1 do
  begin
    cbAnim.Items.Add(ABCModel^.Animation.aItems[i].szName);
  end;
  cbAnim.ItemIndex := 0;
  ReadInfo;
end;

{$R *.lfm}

end.

