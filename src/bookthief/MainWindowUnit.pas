unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Types, Math, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
	ComCtrls, ExtCtrls, LCLType, Spin, LCLIntf,
	ImportExportCommandModalUnit, AdvancedWindowUnit, ProgressWindowUnit,
	MinSizeFormUnit;

const
	BORDER_SPACING_BOTTOM = 40;

type

	{ TMainWindow }

	TMainWindow = class(TMinSizeForm)
		BottomPanel: TPanel;
		BasicControlsPanel: TPanel;
		AdvancedButtonPanel: TPanel;
		OpenDialog: TOpenDialog;
		RescaleDropdownBox: TComboBox;
		RescaleInputPanel: TPanel;
		SaveDialog: TSaveDialog;
		SegmentInputLabel: TLabel;
		SegmentInputPanel: TPanel;
		RangeInputTextbox: TEdit;
		RangeInputLabel: TLabel;
		RangeInputPanel: TPanel;
		GreyscalePanel: TPanel;
		RescalePanel: TPanel;
		SegmentPanel: TPanel;
		RangePanel: TPanel;
		QualitySliderLabel: TLabel;
		SaveButton: TButton;
		RescaleCheckbox: TCheckBox;
		SegmentCheckbox: TCheckBox;
		RangeCheckbox: TCheckBox;
		GreyscaleCheckbox: TCheckBox;
		FileInputButton: TButton;
		MainMenu1: TMainMenu;
		FileMenu: TMenuItem;
		HelpMenu: TMenuItem;
		ImportCommandButton: TMenuItem;
		ExportCommandButton: TMenuItem;
		LoadSettingsButton: TMenuItem;
		SaveSettingsButton: TMenuItem;
		QuitButton: TMenuItem;
		AboutButton: TMenuItem;
		LibgenButton: TMenuItem;
		Separator1: TMenuItem;
		Separator2: TMenuItem;
		Separator3: TMenuItem;
		Separator4: TMenuItem;
		QualitySlider: TTrackBar;
		AdvancedButton: TToggleBox;
		SegmentInputBox: TSpinEdit;
		AboutDialog: TTaskDialog;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure AboutButtonClick(Sender: TObject);
		procedure AdvancedButtonChange(Sender: TObject);
		procedure ExportCommandButtonClick(Sender: TObject);
		procedure FileInputButtonClick(Sender: TObject);
		procedure GreyscaleCheckboxChange(Sender: TObject);
		procedure ImportCommandButtonClick(Sender: TObject);
		procedure LibgenButtonClick(Sender: TObject);
		procedure LoadSettingsButtonClick(Sender: TObject);
		procedure QuitButtonClick(Sender: TObject);
		procedure RangeCheckboxChange(Sender: TObject);
		procedure RangesTextboxKeyPress(Sender: TObject; var Key: char);
		procedure RescaleCheckboxChange(Sender: TObject);
		procedure SaveButtonClick(Sender: TObject);
		procedure SaveSettingsButtonClick(Sender: TObject);
		procedure SegmentCheckboxChange(Sender: TObject);
		procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
			MaxWidth, MaxHeight: TConstraintSize);
	private
		procedure AdvancedWindowClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure LayoutChanged; // call after any show/hide or size-affecting change
	protected
		procedure GetRequiredClientSize(out ReqClientW, ReqClientH: Integer); override;
	public
	end;

var
	MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.GetRequiredClientSize(out ReqClientW, ReqClientH: Integer);

	function RowWidth(LeftCtrl, RightCtrl: TControl): Integer;
	begin
		Result := 0;
		if (LeftCtrl <> nil) and LeftCtrl.Visible then
			Result := Result + LeftCtrl.Width;
		if (RightCtrl <> nil) and RightCtrl.Visible then
			Result := Result + RightCtrl.Width;
	end;

begin
	// Width: ensure any "left + right" row can fit simultaneously.
	ReqClientW := 0;

	ReqClientW := Max(ReqClientW, FileInputButton.Width);
	ReqClientW := Max(ReqClientW, SaveButton.Width);
	ReqClientW := Max(ReqClientW, QualitySlider.Width);

	ReqClientW := Max(ReqClientW, RowWidth(RangeCheckbox, RangeInputPanel));
	ReqClientW := Max(ReqClientW, RowWidth(SegmentCheckbox, SegmentInputPanel));
	ReqClientW := Max(ReqClientW, RowWidth(RescaleCheckbox, RescaleInputPanel));
	ReqClientW := Max(ReqClientW, GreyscaleCheckbox.Width);
	ReqClientW := Max(ReqClientW, AdvancedButton.Width);

	// Height: stacked layout (top button + autosizing middle + autosizing bottom).
	ReqClientH := 0;
	if FileInputButton <> nil then
		ReqClientH := ReqClientH + FileInputButton.Height;
	if BasicControlsPanel <> nil then
		ReqClientH := ReqClientH + BasicControlsPanel.Height;
	if BottomPanel <> nil then
		ReqClientH := ReqClientH + BottomPanel.Height;

	// Basic sanity minimum to avoid collapsing too far.
	ReqClientW := Max(ReqClientW, 320);
	ReqClientH := Max(ReqClientH, 240);
end;

procedure TMainWindow.LayoutChanged;
begin
	DisableAutoSizing;
	try
		if BasicControlsPanel <> nil then BasicControlsPanel.AdjustSize;
		if BottomPanel <> nil then BottomPanel.AdjustSize;
	finally
		EnableAutoSizing;
	end;

	EnsureSizeAtLeastMinimum;
end;

procedure TMainWindow.FormConstrainedResize(Sender: TObject; var MinWidth,
	MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
	ApplyMinConstraintsToResize(MinWidth, MinHeight);
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin
	RangeCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	SegmentCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	RescaleCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	GreyscaleCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;

	EnsureSizeAtLeastMinimum;
end;

procedure TMainWindow.FormShow(Sender: TObject);
begin
	// All auto-created forms are guaranteed to exist here
	if Assigned(AdvancedWindow) then
	begin
		AdvancedWindow.OnClose := @AdvancedWindowClose;
		AdvancedWindow.Hide;
	end;

	// On first show, set the window size to exactly the minimum required size.
	SnapToMinimumSize(2);
end;

procedure TMainWindow.AdvancedWindowClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	{ Uncheck the AdvancedButton when the AdvancedWindow is closed }
	CloseAction := caHide;
	AdvancedButton.Checked := False;
end;

procedure TMainWindow.GreyscaleCheckboxChange(Sender: TObject);
begin

end;

procedure TMainWindow.FileInputButtonClick(Sender: TObject);
begin
	if OpenDialog.Execute then
	begin
		{ Set the button caption to the file BASENAME (not full path) }
		FileInputButton.Caption := ExtractFileName(OpenDialog.FileName);
		{ OpenDialog.FileName (the full path) should be stored to pass to Liesel later }
	end;
end;

procedure TMainWindow.AboutButtonClick(Sender: TObject);
begin
	{ Display AboutDialog }
	AboutDialog.Execute;
end;

procedure TMainWindow.AdvancedButtonChange(Sender: TObject);
begin
	if AdvancedButton.Checked then
		AdvancedWindow.Show
	else
		AdvancedWindow.Hide;
end;

procedure TMainWindow.ImportCommandButtonClick(Sender: TObject);
begin
	ImportExportCommandModal := TImportExportCommandModal.Create(Self);
	ImportExportCommandModal.SetMode(ieImport);
	try
		ImportExportCommandModal.ShowModal;
	finally
		ImportExportCommandModal.Free;
	end;
	{ After modal is closed, retrieve the command string from the textbox and apply settings }
end;

procedure TMainWindow.ExportCommandButtonClick(Sender: TObject);
begin
	ImportExportCommandModal := TImportExportCommandModal.Create(Self);
	ImportExportCommandModal.SetMode(ieExport);
	try
		{ Generate export command string from current settings and set it to the textbox }
		// ImportExportCommandModal.LieselCommandInputTextbox.Text := GenerateExportCommandString();
		ImportExportCommandModal.ShowModal;
	finally
		ImportExportCommandModal.Free;
	end;
end;

procedure TMainWindow.LibgenButtonClick(Sender: TObject);
begin
	{ Open Library Genesis website in default browser }
	OpenURL('http://libgen.li');
end;

procedure TMainWindow.LoadSettingsButtonClick(Sender: TObject);
begin

end;

procedure TMainWindow.QuitButtonClick(Sender: TObject);
begin
	Close;
end;

procedure TMainWindow.RangeCheckboxChange(Sender: TObject);
begin
	RangeInputPanel.Visible := RangeCheckbox.Checked;
	LayoutChanged;
end;

procedure TMainWindow.RangesTextboxKeyPress(Sender: TObject; var Key: char);
begin
	{ Validate: Input can contain only digits, commas, and hyphens }
	if not (Key in ['0'..'9', ',', '-', Char(VK_BACK)]) then Key := #0;
end;

procedure TMainWindow.RescaleCheckboxChange(Sender: TObject);
begin
	RescaleInputPanel.Visible := RescaleCheckbox.Checked;
	LayoutChanged;
end;

procedure TMainWindow.SaveButtonClick(Sender: TObject);
begin
	if SaveDialog.Execute then
	begin
		// Implement saving functionality here using SaveDialog.FileName
		// DEMO: Display ProgressWindow
		if not Assigned(ProgressWindow) then
			ProgressWindow := TProgressWindow.Create(Self);
		ProgressWindow.Show;
	end;
end;

procedure TMainWindow.SaveSettingsButtonClick(Sender: TObject);
begin

end;

procedure TMainWindow.SegmentCheckboxChange(Sender: TObject);
begin
	SegmentInputPanel.Visible := SegmentCheckbox.Checked;
	LayoutChanged;
end;

end.

