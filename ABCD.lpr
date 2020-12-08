program ABCD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, abcmodel, abcmodeltypes, ms3d, hl1smd, ltaexporter, frcalc, 
animmerge, scalemodel
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFRateCalcForm, FRateCalcForm);
  Application.CreateForm(TAnimMergeForm, AnimMergeForm);
  Application.CreateForm(TScaleModelForm, ScaleModelForm);
  Application.Run;
end.

