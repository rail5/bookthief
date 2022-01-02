program bookthief(input, output, stdErr);

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Unit2, Unit3, Unit4
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='BookThief';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  
  if paramCount()>0 then
  begin
    Form1.openfile := paramStr(1);
  end;
  Application.CreateForm(TForm3, Form3);  
  Application.CreateForm(TForm4, Form4);  
  Application.Run;
  
end.


