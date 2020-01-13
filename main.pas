unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, StdCtrls, Grids, abcmodel, hl1smd, ltaexporter, abcmodeltypes;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ExitItem: TMenuItem;
    DecompileItem: TMenuItem;
    FirerateItem: TMenuItem;
    AnimMergeItem: TMenuItem;
    ExportChildModelsItem: TMenuItem;
    ImportChildModelItem: TMenuItem;
    ScaleModelItem: TMenuItem;
    OpenDialogSCM: TOpenDialog;
    SeparatorItem3: TMenuItem;
    SeparatorItem2: TMenuItem;
    SMDFixItem: TMenuItem;
    SMDItem: TMenuItem;
    ToolsMenu: TMenuItem;
    mmoLog: TMemo;
    OpenDialogABC: TOpenDialog;
    SeparatorItem1: TMenuItem;
    OpenABCItem: TMenuItem;
    StatusBar1: TStatusBar;
    sgData: TStringGrid;
    tvData: TTreeView;
    procedure AnimMergeItemClick(Sender: TObject);
    procedure DecompileItemClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure ExportChildModelsItemClick(Sender: TObject);
    procedure FirerateItemClick(Sender: TObject);
    procedure FixItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpMenuClick(Sender: TObject);
    procedure ImportChildModelItemClick(Sender: TObject);
    procedure OpenABCItemClick(Sender: TObject);
    procedure ScaleModelItemClick(Sender: TObject);
    procedure SMDFixItemClick(Sender: TObject);
    procedure SMDItemClick(Sender: TObject);
    procedure tvDataChange(Sender: TObject; {%H-}Node: TTreeNode);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  szCurrentDir: string;
  szCurrentFile: string;
  pABCModel: TABCParser;

procedure WLog(S: string);

implementation

uses frcalc, animmerge, scalemodel;

{$R *.lfm}

{ TMainForm }

procedure WLog(S: string);
begin
  MainForm.mmoLog.Lines.Add(S);
end;

function GetLeftStr(S: string): string;
var n: Integer;
begin
  n := Pos('|', S);
  Result := Copy(S, 1, n - 1);
end;

function GetRightStr(S: string): string;
var n: Integer;
begin
  n := Pos('|', S);
  Result := Copy(S, n + 1, Length(S));
end;

procedure Viewer(sl: TStringList);
var i: Integer;
begin
  MainForm.sgData.RowCount := sl.Count + 1;
  for i := 1 to sl.Count do
  begin
    MainForm.sgData.Cells[0, i] := GetLeftStr(sl.Strings[i - 1]);
    MainForm.sgData.Cells[1, i] := GetRightStr(sl.Strings[i - 1]);
  end;
end;

procedure TMainForm.ExitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.ExportChildModelsItemClick(Sender: TObject);
var pStream: TMemoryStream;
    szFilename: string;
    i: Cardinal;
begin
  pStream := TMemoryStream.Create;
  pStream.Write('SCM', 3);
  with pABCModel.ABCModel.ChildModels do
  begin
    pStream.WriteDWord(nChildModelsCount);
    pStream.WriteDWord(aItems[0].nUnknownCardinal);
    pStream.WriteDWord(Length(aItems[0].aRelations));
    for i := 0 to Length(aItems[0].aRelations)-1 do
    begin
      pStream.Write(aItems[0].aRelations[i].vPos, SizeOf(LTVector));
      pStream.Write(aItems[0].aRelations[i].rRot, SizeOf(LTRotation));
    end;
  end;
  szFilename := ExtractFileNameWithoutExt(szCurrentFile) + '.scm';
  pStream.SaveToFile(szFilename);
  WLog('Exported SELF child model file: ' + szFilename);
  pStream.Free;
end;

procedure TMainForm.FirerateItemClick(Sender: TObject);
begin
  FRateCalcForm.cbAnim.Clear;
  FRateCalcForm.ShowModal;
end;

procedure TMainForm.DecompileItemClick(Sender: TObject);
var LTAExporter: TLTAExporter;
    szFilename: string;
begin
  LTAExporter := TLTAExporter.Create(@pABCModel.ABCModel);
  szFilename := ExtractFileNameWithoutExt(szCurrentFile) + '.lta';
  LTAExporter.ExportText(szFilename);
  WLog('Decompiled file: ' + szFilename);
  LTAExporter.Free;
end;

procedure TMainForm.AnimMergeItemClick(Sender: TObject);
begin
  AnimMergeForm.cbAnim1.Clear;
  AnimMergeForm.cbAnim2.Clear;
  AnimMergeForm.edtNewAnim.Clear;
  AnimMergeForm.ShowModal;
end;

procedure TMainForm.FixItemClick(Sender: TObject);
var s: string;
begin
  s := szCurrentFile;
  Insert('_fix', s, Pos('.', s));
  CopyFile(szCurrentFile, s);
  //pABCModel.FixExportedModel(s);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(pABCModel)
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FormatSettings.DecimalSeparator := '.';
  getdir(0, szCurrentDir);
  OpenDialogABC.InitialDir := szCurrentDir;
end;

procedure TMainForm.HelpMenuClick(Sender: TObject);
begin
  ShowMessage('ABCD v0.13');
end;

procedure TMainForm.ImportChildModelItemClick(Sender: TObject);
var s: string;
begin
  if OpenDialogSCM.Execute then
  begin
    s := szCurrentFile;
    Insert('_SCMFIX', s, Pos('.', s));
    pABCModel.ImportSelfChildModel(s, OpenDialogSCM.FileName);
  end;
end;

procedure TMainForm.OpenABCItemClick(Sender: TObject);
begin
  if OpenDialogABC.Execute then
  begin
    mmoLog.Clear;
    tvData.Items.Clear;
    sgData.RowCount := 1;
    if pABCModel <> nil then FreeAndNil(pABCModel);
    szCurrentFile := OpenDialogABC.FileName;
    WLog('Opening ABC file ' + OpenDialogABC.FileName);
    pABCModel := TABCParser.Create;
    pABCModel.LogProc := @WLog;
    pABCModel.ViewProc := @Viewer;
    if not pABCModel.OpenFile(OpenDialogABC.FileName) then
    begin
      ShowMessage('Something went wrong, check log for additional info!');
      Exit;
    end;
    pABCModel.ABCDataToTreeView(tvData);

  end;
end;

procedure TMainForm.ScaleModelItemClick(Sender: TObject);
begin
  ScaleModelForm.ShowModal;
end;

procedure TMainForm.SMDFixItemClick(Sender: TObject);
begin
  ABCToHL1SMDFix(@pABCModel.ABCModel);
end;

procedure TMainForm.SMDItemClick(Sender: TObject);
begin
  ABCToHL1SMD(@pABCModel.ABCModel);
end;

procedure TMainForm.tvDataChange(Sender: TObject; Node: TTreeNode);
begin
  if tvData.Selected <> nil then
    pABCModel.ViewTVNode(tvData.Selected.Data);
end;


end.

