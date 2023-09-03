unit Unit2;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs,
	ComCtrls, StdCtrls,
	{$IFDEF LINUX}
	BaseUnix,
	{$ENDIF}
	{$IFDEF WINDOWS}
	Windows,
	{$ENDIF}
	ExtCtrls;

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
		function SpawnLiesel() : boolean;
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

{$IFDEF WINDOWS}
function ProcKill(ProcessID: Cardinal) : boolean;
var
	hProcess : THandle;
begin
	Result := False;
	hProcess := OpenProcess(PROCESS_TERMINATE,False,ProcessID);
	if hProcess > 0 then
		try
			Result := Win32Check(Windows.TerminateProcess(hProcess,0));
		finally
			CloseHandle(hProcess);
		end;
end;
{$ENDIF}

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

function TForm2.SpawnLiesel() : boolean;
var
	comd : string;
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
	finalcomd : TStringArray;
	i : integer;
begin
	comd := Form1.GenerateCommand(true);
	finalcomd := Form1.CreateCommand(false, true);

	Process1 := TProcess.Create(nil);

	Process1.Executable := Form1.LieselPath;

	Process1.ShowWindow := swoHide;

	for i := 0 to (Length(finalcomd)-1) do
	begin
		Process1.Parameters.Add(finalcomd[i]);
	end;

	Process1.Parameters.Add('-c'); // Run the 'check' command to verify everything's good to go

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
			{$IFDEF LINUX} WriteLn(ErrString); {$ENDIF}
			ShowMessage(ErrString); // If the Liesel -c check command doesn't output "OK" to stdout, read from stderr and pipe to a message box
			Form2.Close();
			FormActivated := false;
			Exit(false);
		end;

	{$IFDEF LINUX} WriteLn('Command verified OK'); {$ENDIF}

	{$IFDEF LINUX} WriteLn('Running command: ' + comd + LineEnding); {$ENDIF}
	progaccumulator := '';

	Process1 := TProcess.Create(nil);

	Process1.Executable := Form1.LieselPath;

	Process1.ShowWindow := swoHide;

	for i := 0 to (Length(finalcomd)-1) do
	begin
		Process1.Parameters.Add(finalcomd[i]);
	end;

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
				{$IFDEF LINUX} Write(progaccumulator + '%...'); {$ENDIF}
				progaccumulator := '';
			end
		else if IsNumeric(progstring) then
			begin
				progaccumulator := progaccumulator + progstring;
			end
		else if progstring = 'D' then
			begin
				ProgressBar1.Position := 100;
				{$IFDEF LINUX} Write('Done!' + LineEnding); {$ENDIF}
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
	SpawnLiesel();
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
	{$IFDEF LINUX} FpKill(Process1.ProcessID, SIGINT); {$ENDIF}
	{$IFDEF WINDOWS} ProcKill(Process1.ProcessID); {$ENDIF}
	{$IFDEF LINUX} Write('Cancelled' + LineEnding); {$ENDIF}
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


