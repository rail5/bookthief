unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, BaseUnix;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Process1: TProcess;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    function SpawnLiesel(comd : string) : boolean;
    procedure Timer1Timer(Sender: TObject);
  private
    FormActivated: Boolean;
  public

  end;

var
  Form2: TForm2;

implementation

uses Unit1;

{$R *.lfm}

{ TForm2 }

function IsNumeric(haystack : string) : boolean;
var
  C: char;
begin
  for C in haystack do
  begin
    Result := C in ['0'..'9'];
    if not Result then Break;
  end;
end;

function StreamToString(const AStream: TStream) : string;
begin
  Result := '';
  with TStringStream.Create() do
  try
    LoadFromStream(AStream);
    Result := DataString;
  finally
    Free;
  end;
end;

function TForm2.SpawnLiesel(comd : string) : boolean;
var
  OutputStream : TStream;
  BytesRead : longint;
  Buffer : array[1..1] of byte;
  progstring : string;
  progaccumulator : string;
  progint : integer;
  checkcomd : string;
  ErrStream : TStream;
  ErrBytesRead : longint;
  ErrBuffer : array[1..2048] of byte;
  ErrString : string;
begin
  Process1 := TProcess.Create(nil);

  checkcomd := comd + ' -c';

  Process1.CommandLine := checkcomd; // Run the 'check' command to verify everything's good to go

  WriteLn(checkcomd);

  Process1.Options := [poUsePipes];

  Process1.Execute;

  OutputStream := TMemoryStream.Create;

  BytesRead := Process1.Output.Read(Buffer, 2048);
  OutputStream.Write(Buffer, BytesRead);

  ErrStream := TMemoryStream.Create;
  ErrBytesRead := Process1.Stderr.Read(ErrBuffer, 2048);
  ErrStream.Write(ErrBuffer, ErrBytesRead);

  progstring := StreamToString(OutputStream);
  if progstring <> 'OK' then
    begin
      ErrString := StreamToString(ErrStream);
      WriteLn(ErrString);
      ShowMessage(ErrString); // If the Liesel -c check command doesn't output "OK" to stdout, read from stderr and pipe to a message box
      Form2.Close();
      FormActivated := false;
      Exit(false);
    end;

  WriteLn('Command verified OK');

  WriteLn('Running command: ' + comd + LineEnding);
  progaccumulator := '';

  Process1 := TProcess.Create(nil);

  Process1.CommandLine := comd;

  Process1.Options:= [poUsePipes];

  Process1.Execute;

  OutputStream := TMemoryStream.Create;

  repeat
    Application.ProcessMessages();
    BytesRead := Process1.Output.Read(Buffer, 1);
    OutputStream.Write(Buffer, BytesRead);
    progstring := StreamToString(OutputStream);
    if progstring = '%' then
      begin
        ProgressBar1.Position := StrToInt(progaccumulator);
        Update();
        Write(progaccumulator + '%...');
        progaccumulator := '';
      end
    else if IsNumeric(progstring) then
      begin
        progaccumulator := progaccumulator + progstring;
      end
    else if progstring = 'D' then
      begin
        ProgressBar1.Position := 100;
        Write('Done!' + LineEnding);
        ShowMessage('Done!');
        Form2.Close;
        FormActivated := false;
      end;
    OutputStream := TMemoryStream.Create;
  until BytesRead = 0;

  Process1.Free;

  OutputStream.Free;

  SpawnLiesel := true;
end;


procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  Update();
  SpawnLiesel(Form1.currentcomd);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  FpKill(Process1.ProcessID, SIGINT);
  Write('Cancelled' + LineEnding);
  ShowMessage('Cancelled');
  ProgressBar1.Position := 0;
  Form2.Close;
  FormActivated := false;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  if not FormActivated then begin
    FormActivated := true;
    Timer1.Enabled := true;
  end;
end;

end.


