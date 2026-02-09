unit ImportExportCommandModalUnit;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLType;

type
	TImportExportMode = (ieImport, ieExport);
	{ TImportExportCommandModal }

	TImportExportCommandModal = class(TForm)
		ImportExportButton: TButton;
		LieselCommandInputTextbox: TEdit;
		ImportExportLabel: TLabel;
		ImportExportButtonPanel: TPanel;
		procedure SetMode(const AMode: TImportExportMode);
		procedure CenterImportExportButton;
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormShow(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure ImportExportButtonClick(Sender: TObject);
	private
		FMode: TImportExportMode;
		function GetCommandText: string;
		procedure SetCommandText(const AValue: string);

	public
		property CommandText: string read GetCommandText write SetCommandText;

	end;

var
	ImportExportCommandModal: TImportExportCommandModal;

implementation

{$R *.lfm}

{ TImportExportCommandModal }

procedure TImportExportCommandModal.SetMode(const AMode: TImportExportMode);
begin
	FMode := AMode;
	case FMode of
		ieImport:
			begin
				ImportExportLabel.Caption := 'Import Liesel Command';
				ImportExportButton.Caption := 'Import Settings';
				LieselCommandInputTextbox.ReadOnly := False;
			end;
		ieExport:
			begin
				ImportExportLabel.Caption := 'Your exported command is:';
				ImportExportButton.Caption := 'OK';
				LieselCommandInputTextbox.ReadOnly := True;
			end;
	end;
	// Ensure button activation always yields mrOk.
	ImportExportButton.ModalResult := mrOk;
	ImportExportButton.Default := True;
	CenterImportExportButton;
end;

function TImportExportCommandModal.GetCommandText: string;
begin
	if LieselCommandInputTextbox <> nil then
		Result := LieselCommandInputTextbox.Text
	else
		Result := '';
end;

procedure TImportExportCommandModal.SetCommandText(const AValue: string);
begin
	if LieselCommandInputTextbox <> nil then
		LieselCommandInputTextbox.Text := AValue;
end;

procedure TImportExportCommandModal.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
	begin
		ModalResult := mrCancel;
		Key := 0;
		Exit;
	end
	else if Key = VK_RETURN then
		ImportExportButtonClick(Self);
end;

procedure TImportExportCommandModal.CenterImportExportButton;
begin
	ImportExportButton.Left := (ImportExportButtonPanel.ClientWidth - ImportExportButton.Width) div 2;
end;

procedure TImportExportCommandModal.FormShow(Sender: TObject);
begin
	KeyPreview := True; // Enable form-level key handling
	ModalResult := mrNone;
	// Defensive: ensure these are set even if SetMode wasn't called for some reason.
	ImportExportButton.ModalResult := mrOk;
	ImportExportButton.Default := True;
	CenterImportExportButton;
	if LieselCommandInputTextbox <> nil then
	begin
		LieselCommandInputTextbox.SetFocus;
		if FMode = ieExport then
			LieselCommandInputTextbox.SelectAll;
	end;
end;

procedure TImportExportCommandModal.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TImportExportCommandModal.FormResize(Sender: TObject);
begin
	CenterImportExportButton;
end;

procedure TImportExportCommandModal.ImportExportButtonClick(Sender: TObject);
begin
	ModalResult := mrOk;
	// Do not call Close here; for modal forms, setting ModalResult is sufficient
	// and calling Close can cause the LCL to override to mrCancel.
end;

end.

