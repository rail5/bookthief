unit ProgressWindowUnit;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
	StdCtrls;

type

	{ TProgressWindow }

	TProgressWindow = class(TForm)
		CancelButton: TButton;
		CancelButtonPanel: TPanel;
		ProgressLabel: TLabel;
		ProgressBar1: TProgressBar;
		procedure CancelButtonClick(Sender: TObject);
	private
		FCancelFlag: LongInt;
		FFinished: Boolean;
	public
		procedure ResetForJob;
		procedure SetProgress(Percent: Cardinal; const Msg: string);
		procedure MarkDone(const Msg: string);
		procedure MarkCancelled(const Msg: string);
		procedure MarkError(const Msg: string);
		procedure RequestCancel;
		function CancelRequested: Boolean;
	end;

var
	ProgressWindow: TProgressWindow;

implementation

{$R *.lfm}

{ TProgressWindow }

procedure TProgressWindow.CancelButtonClick(Sender: TObject);
begin
	RequestCancel;
end;

procedure TProgressWindow.ResetForJob;
begin
	InterlockedExchange(FCancelFlag, 0);
	FFinished := False;
	ProgressBar1.Min := 0;
	ProgressBar1.Max := 100;
	ProgressBar1.Position := 0;
	ProgressLabel.Caption := 'Working...';
	CancelButton.Enabled := True;
end;

procedure TProgressWindow.SetProgress(Percent: Cardinal; const Msg: string);
var
	p: Integer;
begin
	p := Percent;
	if p < 0 then p := 0;
	if p > 100 then p := 100;
	ProgressBar1.Position := p;
	if Msg <> '' then
		ProgressLabel.Caption := Msg
	else
		ProgressLabel.Caption := Format('%d%%', [p]);
end;

procedure TProgressWindow.MarkDone(const Msg: string);
var
	MsgText: string;
begin
	if FFinished then Exit;
	FFinished := True;

	ProgressBar1.Position := 100;
	if Msg <> '' then ProgressLabel.Caption := Msg else ProgressLabel.Caption := 'Done.';
	CancelButton.Enabled := False;

	if Msg <> '' then MsgText := Msg else MsgText := 'Done.';
	Hide;
	MessageDlg('BookThief', MsgText, mtInformation, [mbOK], 0);
end;

procedure TProgressWindow.MarkCancelled(const Msg: string);
var
	MsgText: string;
begin
	if FFinished then Exit;
	FFinished := True;
	CancelButton.Enabled := False;

	if Msg <> '' then
	begin
		MsgText := Msg;
		ProgressLabel.Caption := Msg;
	end
	else
	begin
		MsgText := 'Cancelled.';
		ProgressLabel.Caption := 'Cancelled.';
	end;

	Hide;
	MessageDlg('BookThief', MsgText, mtInformation, [mbOK], 0);
end;

procedure TProgressWindow.MarkError(const Msg: string);
var
	MsgText: string;
begin
	if FFinished then Exit;
	FFinished := True;

	if Msg <> '' then
	begin
		MsgText := Msg;
		ProgressLabel.Caption := 'Error: ' + Msg
	end
	else
	begin
		MsgText := 'Unknown error.';
		ProgressLabel.Caption := 'Error.';
	end;
	CancelButton.Enabled := False;

	Hide;
	MessageDlg('BookThief', MsgText, mtError, [mbOK], 0);
end;

procedure TProgressWindow.RequestCancel;
begin
	if FFinished then Exit;
	InterlockedExchange(FCancelFlag, 1);
	CancelButton.Enabled := False;
	ProgressLabel.Caption := 'Cancelling...';
end;

function TProgressWindow.CancelRequested: Boolean;
begin
	Result := (FCancelFlag <> 0);
end;

end.

