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

	public

	end;

var
	ProgressWindow: TProgressWindow;

implementation

{$R *.lfm}

{ TProgressWindow }

procedure TProgressWindow.CancelButtonClick(Sender: TObject);
begin

end;

end.

