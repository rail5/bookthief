program bookthief;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}
	cthreads,
	{$ENDIF}
	{$IFDEF HASAMIGA}
	athreads,
	{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, MainWindowUnit, AdvancedWindowUnit, ProgressWindowUnit
	;

{$R *.res}

begin
	RequireDerivedFormResource:=True;
	Application.Title:='BookThief';
	Application.Scaled:=True;
	{$PUSH}{$WARN 5044 OFF}
	Application.MainFormOnTaskbar:=True;
	{$POP}
	Application.Initialize;
	Application.CreateForm(TMainWindow, MainWindow);
	Application.CreateForm(TAdvancedWindow, AdvancedWindow);
	Application.CreateForm(TProgressWindow, ProgressWindow);
	Application.Run;
end.

