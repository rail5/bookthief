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
		procedure FormShow(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure ImportExportButtonClick(Sender: TObject);
	private
		FMode: TImportExportMode;

	public

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
			end;
		ieExport:
			begin
				ImportExportLabel.Caption := 'Your exported command is:';
				ImportExportButton.Caption := 'OK';
			end;
	end;
	CenterImportExportButton;
end;

procedure TImportExportCommandModal.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		Close;
end;

procedure TImportExportCommandModal.CenterImportExportButton;
begin
	ImportExportButton.Left := (ImportExportButtonPanel.ClientWidth - ImportExportButton.Width) div 2;
end;

procedure TImportExportCommandModal.FormShow(Sender: TObject);
begin
	KeyPreview := True; // Enable form-level key handling
	CenterImportExportButton;
end;

procedure TImportExportCommandModal.FormResize(Sender: TObject);
begin
	CenterImportExportButton;
end;

procedure TImportExportCommandModal.ImportExportButtonClick(Sender: TObject);
begin
	{ Handle import/export based on FMode }
	{ Then, close the modal }
	Close;
end;

end.

