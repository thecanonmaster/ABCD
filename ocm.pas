unit ocm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  abcmodeltypes;

type

  { TOCMForm }

  TOCMForm = class(TForm)
    btnOffset: TButton;
    cbxNode: TComboBox;
    cbxlCM: TCheckListBox;
    edtY: TEdit;
    edtZ: TEdit;
    edtX: TEdit;
    lblNode: TLabel;
    lblNode1: TLabel;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    procedure btnOffsetClick(Sender: TObject);
    procedure Clear;
    procedure FillCMIndices(var A: TIntArray);
  private

  public

  end;

var
  OCMForm: TOCMForm;

implementation

{$R *.lfm}

{ TOCMForm }

procedure TOCMForm.Clear;
begin
  cbxNode.Clear;
  cbxlCM.Clear;
  edtX.Text := '0.0';
  edtY.Text := '0.0';
  edtZ.Text := '0.0';
end;

procedure TOCMForm.FillCMIndices(var A: TIntArray);
var
  i: Integer;
  nChecked: Integer = 0;
begin
  SetLength(A, cbxlCM.Items.Count);
  for i := 0 to cbxlCM.Items.Count - 1 do
  begin
    if cbxlCM.Checked[i] then
    begin
      A[nChecked] := i;
      nChecked := nChecked + 1;
    end;
  end;
  SetLength(A, nChecked);
end;

procedure TOCMForm.btnOffsetClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

end.

