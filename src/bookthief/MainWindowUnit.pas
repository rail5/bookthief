unit MainWindowUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Types, Math, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
	ComCtrls, ExtCtrls, LCLType, Spin, LCLIntf,
	ImportExportCommandModalUnit, AdvancedWindowUnit, ProgressWindowUnit,
	MinSizeFormUnit, LieselUnit, LieselABIUnit;

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
		procedure QualitySliderChange(Sender: TObject);
		procedure RescaleDropdownBoxChange(Sender: TObject);
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
		FInputPdfPath: string;
		FOutputPdfPath: string;
		FPreviewDebounceTimer: TTimer;
		FPreviewLatestSeq: Cardinal;
		FPreviewInFlight: Boolean;
		FApplyingSettings: Boolean;
		procedure SetInputPdfPathFromUi(const APath: string);
		function TryGetCliInputPdfPath(out APath: string): Boolean;
		procedure AdvancedWindowClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure AdvancedSettingsChanged(Sender: TObject);
		procedure RequestPreviewUpdateDebounced;
		procedure PreviewDebounceTimerTick(Sender: TObject);
		procedure LayoutChanged; // call after any show/hide or size-affecting change
	protected
		procedure GetRequiredClientSize(out ReqClientW, ReqClientH: Integer); override;
	public
	end;

var
	MainWindow: TMainWindow;

implementation

{$R *.lfm}

type
	TPrintJobThread = class(TThread)
	private
		FInputPath: string;
		FOutputPath: string;
		FDpiDensity: Cardinal;
		FUsePageRanges: Boolean;
		FPageRanges: string;
		FGreyscale: Boolean;
		FUseSegments: Boolean;
		FSegmentSize: Cardinal;
		FUseRescale: Boolean;
		FRescaleSize: string;
		FLib: TLieselLib;
		FCtx: TLieselContext;
		FBook: TLieselBook;

		FUiPercent: Cardinal;
		FUiMessage: string;
		FUiDone: Boolean;
		FUiError: string;
		FUiCancelledMsg: string;

		procedure UiProgress;
		procedure UiDone;
		procedure UiCancelled;
		procedure UiError;
		procedure HandleProgress(Sender: TObject; Event: TLieselBookProgressEvent;
			SegmentIndex, PageIndex, Percent: Cardinal; const MessageUtf8: string);
		function HandleCancel(Sender: TObject): Boolean;
	protected
		procedure Execute; override;
	public
		constructor Create(const AInputPath, AOutputPath: string;
			ADpiDensity: Cardinal;
			AUsePageRanges: Boolean; const APageRanges: string;
			AGreyscale: Boolean;
			AUseSegments: Boolean; ASegmentSize: Cardinal;
			AUseRescale: Boolean; const ARescaleSize: string);
		destructor Destroy; override;
	end;

	TPreviewSnapshot = record
		InputPath: string;
		PreviewEnabled: Boolean;
		PreviewPageIndex0: Cardinal;
		DpiDensity: Cardinal;
		Greyscale: Boolean;
		UseRescale: Boolean;
		RescaleSize: string;
		DividePages: Boolean;
		Booklet: Boolean;
		ThresholdEnabled: Boolean;
		ThresholdLevel: Byte;
		WidenCenterMargin: Boolean;
		WidenCenterMarginAmount: Cardinal;
		CropEnabled: Boolean;
		CropL: Byte;
		CropR: Byte;
		CropT: Byte;
		CropB: Byte;
	end;

	TPreviewRenderThread = class(TThread)
	private
		FSeq: Cardinal;
		FSnap: TPreviewSnapshot;
		FPageCount: Cardinal;
		FJpeg: TBytes;
		FError: string;
		procedure UiApply;
	protected
		procedure Execute; override;
	public
		constructor Create(ASeq: Cardinal; const ASnap: TPreviewSnapshot);
	end;

constructor TPreviewRenderThread.Create(ASeq: Cardinal; const ASnap: TPreviewSnapshot);
begin
	inherited Create(True);
	FreeOnTerminate := True;
	FSeq := ASeq;
	FSnap := ASnap;
	FPageCount := 0;
	SetLength(FJpeg, 0);
	FError := '';
	Start;
end;

procedure TPreviewRenderThread.Execute;
var
	lib: TLieselLib;
	ctx: TLieselContext;
	book: TLieselBook;
	doLoadPdf: Boolean;
	doPreview: Boolean;
begin
	lib := nil;
	ctx := nil;
	book := nil;
	doLoadPdf := True;
	doPreview := True;
	try
		if Trim(FSnap.InputPath) = '' then doLoadPdf := False;
		doPreview := doLoadPdf and FSnap.PreviewEnabled;

		if doLoadPdf then
		begin
			lib := TLieselLib.Create;
			lib.Load;
			ctx := TLieselContext.Create(lib);
			book := ctx.CreateBook;

			book.SetInputPdfPath(FSnap.InputPath);
			book.LoadPdf;
			FPageCount := book.GetPdfPageCount;

			if doPreview then
			begin
				// Batch all settings without generating previews, then enable previewing and
				// trigger a single render.
				book.SetPreviewing(False);
				book.SetDpiDensity(FSnap.DpiDensity);

				// Settings from both MainWindow and AdvancedWindow
				book.SetGreyscale(FSnap.Greyscale);
				book.SetDivide(FSnap.DividePages);
				book.SetBooklet(FSnap.Booklet);

				if FSnap.UseRescale and (Trim(FSnap.RescaleSize) <> '') then
					book.SetRescaleSize(FSnap.RescaleSize)
				else
					book.ClearRescaleSize;

				if FSnap.ThresholdEnabled then
					book.SetThresholdLevel(FSnap.ThresholdLevel)
				else
					book.ClearThresholdLevel;

				if FSnap.WidenCenterMargin then
					book.SetWidenMarginsAmount(FSnap.WidenCenterMarginAmount)
				else
					book.SetWidenMarginsAmount(0);

				if FSnap.CropEnabled then
					book.SetCropPercentagesLRBT(FSnap.CropL, FSnap.CropR, FSnap.CropT, FSnap.CropB)
				else
					book.SetCropPercentagesLRBT(0, 0, 0, 0);

				// Clamp preview page index to a valid range.
				if FPageCount > 0 then
				begin
					// Booklet mode cannot preview the last page (it needs page+1).
					if FSnap.Booklet and (FPageCount >= 2) then
					begin
						if FSnap.PreviewPageIndex0 > (FPageCount - 2) then
							FSnap.PreviewPageIndex0 := FPageCount - 2;
					end
					else
					begin
						if FSnap.PreviewPageIndex0 > (FPageCount - 1) then
							FSnap.PreviewPageIndex0 := FPageCount - 1;
					end;
				end
				else
					FSnap.PreviewPageIndex0 := 0;

				book.SetPreviewPage(FSnap.PreviewPageIndex0);
				book.SetPreviewing(True);
				FJpeg := book.GetPreviewJpegBytes;
			end
			else
			begin
				SetLength(FJpeg, 0);
			end;
		end
		else
		begin
			FPageCount := 0;
			SetLength(FJpeg, 0);
		end;
	except
		on E: Exception do
			FError := E.Message;
	end;
	try
		Synchronize(@UiApply);
	finally
		FreeAndNil(book);
		FreeAndNil(ctx);
		FreeAndNil(lib);
	end;
end;

procedure TPreviewRenderThread.UiApply;
begin
	// Always clear the in-flight flag; otherwise a stale render can permanently
	// block future preview updates (common when dragging sliders).
	if MainWindow <> nil then
		MainWindow.FPreviewInFlight := False;

	// Drop stale renders, but still trigger a follow-up if newer changes exist.
	if (MainWindow = nil) then Exit;
	if (MainWindow.FPreviewLatestSeq <> FSeq) then
	begin
		if (MainWindow.FPreviewDebounceTimer <> nil) and (AdvancedWindow <> nil) and AdvancedWindow.Visible then
		begin
			MainWindow.FPreviewDebounceTimer.Enabled := False;
			MainWindow.FPreviewDebounceTimer.Enabled := True;
		end;
		Exit;
	end;

	if (AdvancedWindow = nil) or (not AdvancedWindow.Visible) then Exit;

	// Update UI with the current PDF page count (used by LastPageButton).
	AdvancedWindow.SetPdfPageCount(FPageCount);

	if (FError <> '') then
		AdvancedWindow.SetPreviewJpegBytes(nil)
	else
		AdvancedWindow.SetPreviewJpegBytes(FJpeg);
end;

constructor TPrintJobThread.Create(const AInputPath, AOutputPath: string;
	ADpiDensity: Cardinal;
	AUsePageRanges: Boolean; const APageRanges: string;
	AGreyscale: Boolean;
	AUseSegments: Boolean; ASegmentSize: Cardinal;
	AUseRescale: Boolean; const ARescaleSize: string);
begin
	inherited Create(True);
	FreeOnTerminate := True;
	FInputPath := AInputPath;
	FOutputPath := AOutputPath;
	FDpiDensity := ADpiDensity;
	FUsePageRanges := AUsePageRanges;
	FPageRanges := APageRanges;
	FGreyscale := AGreyscale;
	FUseSegments := AUseSegments;
	FSegmentSize := ASegmentSize;
	FUseRescale := AUseRescale;
	FRescaleSize := ARescaleSize;
	FLib := nil;
	FCtx := nil;
	FBook := nil;
	FUiPercent := 0;
	FUiMessage := '';
	FUiDone := False;
	FUiError := '';
	Start;
end;

destructor TPrintJobThread.Destroy;
begin
	FreeAndNil(FBook);
	FreeAndNil(FCtx);
	FreeAndNil(FLib);
	inherited Destroy;
end;

procedure TPrintJobThread.UiProgress;
begin
	if Assigned(ProgressWindow) then
		ProgressWindow.SetProgress(FUiPercent, FUiMessage);
end;

procedure TPrintJobThread.UiDone;
begin
	if Assigned(ProgressWindow) then
		ProgressWindow.MarkDone('Done.');
end;

procedure TPrintJobThread.UiCancelled;
begin
	if Assigned(ProgressWindow) then
		ProgressWindow.MarkCancelled(FUiCancelledMsg);
end;

procedure TPrintJobThread.UiError;
begin
	if Assigned(ProgressWindow) then
		ProgressWindow.MarkError(FUiError);
end;

procedure TPrintJobThread.HandleProgress(Sender: TObject; Event: TLieselBookProgressEvent;
	SegmentIndex, PageIndex, Percent: Cardinal; const MessageUtf8: string);
begin
	FUiPercent := Percent;
	FUiMessage := MessageUtf8;
	// NOTE: This thread is FreeOnTerminate. Using Queue here can schedule UiProgress
	// after the thread object has been freed, causing heap corruption.
	// Synchronize avoids that use-after-free.
	Synchronize(@UiProgress);
end;

function TPrintJobThread.HandleCancel(Sender: TObject): Boolean;
begin
	if Assigned(ProgressWindow) then
		Result := ProgressWindow.CancelRequested
	else
		Result := False;
end;

procedure TPrintJobThread.Execute;
begin
	try
		FLib := TLieselLib.Create;
		FLib.Load;
		FCtx := TLieselContext.Create(FLib);
		FBook := FCtx.CreateBook;

		FBook.OnProgress := @HandleProgress;
		FBook.OnCancel := @HandleCancel;

		FBook.SetInputPdfPath(FInputPath);
		FBook.SetOutputPdfPath(FOutputPath);
		FBook.SetDpiDensity(FDpiDensity);
		FBook.SetGreyscale(FGreyscale);

		if FUseSegments then
			FBook.SetSegmentSize(FSegmentSize)
		else
			FBook.ClearSegmentSize;

		if FUseRescale then
		begin
			if Trim(FRescaleSize) <> '' then
				FBook.SetRescaleSize(FRescaleSize)
			else
				FBook.ClearRescaleSize;
		end
		else
			FBook.ClearRescaleSize;

		if FUsePageRanges then
		begin
			if Trim(FPageRanges) <> '' then
				FBook.SetPageRanges(FPageRanges)
			else
				FBook.ClearPageRanges;
		end
		else
			FBook.ClearPageRanges;

		FBook.LoadPdf;
		FBook.Print;

		FUiDone := True;
		Synchronize(@UiDone);
	except
		on E: ELieselError do
		begin
			if E.Status = LIESEL_E_CANCELLED then
			begin
				FUiCancelledMsg := 'Cancelled.';
				Synchronize(@UiCancelled);
			end
			else
			begin
				FUiError := E.Message;
				Synchronize(@UiError);
			end;
		end;
		on E: Exception do
		begin
			FUiError := E.Message;
			Synchronize(@UiError);
		end;
	end;
end;

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
	FInputPdfPath := '';
	FOutputPdfPath := '';
	FPreviewLatestSeq := 0;
	FPreviewInFlight := False;
	FApplyingSettings := False;

	RangeCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	SegmentCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	RescaleCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	GreyscaleCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;

	// Debounced async preview updates
	FPreviewDebounceTimer := TTimer.Create(Self);
	FPreviewDebounceTimer.Enabled := False;
	FPreviewDebounceTimer.Interval := 150;
	FPreviewDebounceTimer.OnTimer := @PreviewDebounceTimerTick;

	QualitySlider.OnChange := @QualitySliderChange;
	RescaleDropdownBox.OnChange := @RescaleDropdownBoxChange;

	EnsureSizeAtLeastMinimum;
end;

procedure TMainWindow.FormShow(Sender: TObject);
begin
	// All auto-created forms are guaranteed to exist here
	if Assigned(AdvancedWindow) then
	begin
		AdvancedWindow.OnClose := @AdvancedWindowClose;
		AdvancedWindow.OnSettingsChanged := @AdvancedSettingsChanged;
		AdvancedWindow.Hide;
	end;

	// Optional: if a PDF path was passed on the command line, auto-select it.
	// We do this on FormShow so controls exist and sizing can account for the
	// updated button caption.
	if Trim(FInputPdfPath) = '' then
	begin
		if TryGetCliInputPdfPath(FInputPdfPath) then
			SetInputPdfPathFromUi(FInputPdfPath);
	end;

	// On first show, set the window size to exactly the minimum required size.
	SnapToMinimumSize(2);
end;

procedure TMainWindow.SetInputPdfPathFromUi(const APath: string);
begin
	FInputPdfPath := APath;
	if Trim(FInputPdfPath) <> '' then
	begin
		// Set the button caption to the file BASENAME (not full path)
		FileInputButton.Caption := ExtractFileName(FInputPdfPath);
		// Provide a helpful default output name.
		SaveDialog.FileName := ChangeFileExt(ExtractFileName(FInputPdfPath), '') + '-out.pdf';
		// Also seed the open dialog so re-opening starts in the right place.
		OpenDialog.FileName := FInputPdfPath;
	end;
	RequestPreviewUpdateDebounced;
end;

function TMainWindow.TryGetCliInputPdfPath(out APath: string): Boolean;
var
	i: Integer;
	cand: string;
begin
	Result := False;
	APath := '';
	// Scan args and pick the first existing file. This avoids accidentally
	// treating flags like "--help" as a path.
	for i := 1 to ParamCount do
	begin
		cand := Trim(ParamStr(i));
		if cand = '' then Continue;
		if FileExists(cand) then
		begin
			APath := cand;
			Exit(True);
		end;
	end;
end;

procedure TMainWindow.AdvancedWindowClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	{ Uncheck the AdvancedButton when the AdvancedWindow is closed }
	CloseAction := caHide;
	AdvancedButton.Checked := False;
end;

procedure TMainWindow.GreyscaleCheckboxChange(Sender: TObject);
begin
	RequestPreviewUpdateDebounced;
end;

procedure TMainWindow.FileInputButtonClick(Sender: TObject);
begin
	if OpenDialog.Execute then
	begin
		SetInputPdfPathFromUi(OpenDialog.FileName);
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
	RequestPreviewUpdateDebounced;
end;

procedure TMainWindow.QualitySliderChange(Sender: TObject);
begin
	RequestPreviewUpdateDebounced;
end;

procedure TMainWindow.RescaleDropdownBoxChange(Sender: TObject);
begin
	RequestPreviewUpdateDebounced;
end;

procedure TMainWindow.AdvancedSettingsChanged(Sender: TObject);
begin
	RequestPreviewUpdateDebounced;
end;

procedure TMainWindow.RequestPreviewUpdateDebounced;
begin
	if FApplyingSettings then Exit;
	if (AdvancedWindow = nil) or (not AdvancedWindow.Visible) then Exit;
	if Trim(FInputPdfPath) = '' then
	begin
		// No input -> clear preview.
		AdvancedWindow.SetPreviewJpegBytes(nil);
		AdvancedWindow.SetPdfPageCount(0);
		Exit;
	end;
	// Preview explicitly disabled -> clear preview, but still update page count.
	if not AdvancedWindow.EnableDisablePreviewCheckbox.Checked then
		AdvancedWindow.SetPreviewJpegBytes(nil);

	Inc(FPreviewLatestSeq);
	FPreviewDebounceTimer.Enabled := False;
	FPreviewDebounceTimer.Enabled := True;
end;

procedure TMainWindow.PreviewDebounceTimerTick(Sender: TObject);
var
	seq: Cardinal;
	snap: TPreviewSnapshot;
begin
	FPreviewDebounceTimer.Enabled := False;
	if (AdvancedWindow = nil) or (not AdvancedWindow.Visible) then Exit;
	if FPreviewInFlight then
	begin
		// A render is currently running; try again shortly.
		FPreviewDebounceTimer.Enabled := True;
		Exit;
	end;

	seq := FPreviewLatestSeq;

	// Snapshot all state on the UI thread.
	snap := Default(TPreviewSnapshot);
	snap.InputPath := FInputPdfPath;
	snap.DpiDensity := Cardinal(QualitySlider.Position);
	snap.Greyscale := GreyscaleCheckbox.Checked;
	snap.UseRescale := RescaleCheckbox.Checked;
	snap.RescaleSize := RescaleDropdownBox.Text;

	snap.PreviewEnabled := AdvancedWindow.EnableDisablePreviewCheckbox.Checked;
	snap.PreviewPageIndex0 := Cardinal(AdvancedWindow.LeftRightNavigation.Position);

	snap.DividePages := AdvancedWindow.SplitPagesCheckbox.Checked;
	snap.Booklet := not AdvancedWindow.NoBookletCheckbox.Checked;

	snap.ThresholdEnabled := AdvancedWindow.ColorThresholdCheckbox.Checked;
	snap.ThresholdLevel := Byte(AdvancedWindow.ColorThresholdSlider.Position);

	snap.WidenCenterMargin := AdvancedWindow.CenterMarginCheckbox.Checked;
	snap.WidenCenterMarginAmount := Cardinal(AdvancedWindow.CenterMarginSlider.Position);

	snap.CropEnabled := AdvancedWindow.CropCheckbox.Checked;
	snap.CropL := Byte(AdvancedWindow.LeftCropSlider.Position);
	snap.CropR := Byte(AdvancedWindow.RightCropSlider.Position);
	snap.CropT := Byte(AdvancedWindow.TopCropSlider.Position);
	snap.CropB := Byte(AdvancedWindow.BottomCropSlider.Position);

	TPreviewRenderThread.Create(seq, snap);
	FPreviewInFlight := True;
end;

procedure TMainWindow.ImportCommandButtonClick(Sender: TObject);
var
	cmd: string;
	lib: TLieselLib;
	ctx: TLieselContext;
	book: TLieselBook;
	segSize: Cardinal;
	rescaleSize: string;
	ranges: string;
	thresholdLevel: Byte;
	cropL, cropR, cropT, cropB: Byte;
	widenAmt: Cardinal;
	bookletEnabled: Boolean;
	divideEnabled: Boolean;
	importedInput: string;
begin
	ImportExportCommandModal := TImportExportCommandModal.Create(Self);
	ImportExportCommandModal.SetMode(ieImport);
	try
		if ImportExportCommandModal.ShowModal <> mrOk then Exit;
		cmd := Trim(ImportExportCommandModal.CommandText);
	finally
		ImportExportCommandModal.Free;
	end;

	if cmd = '' then Exit;

	lib := nil;
	ctx := nil;
	book := nil;
	try
		lib := TLieselLib.Create;
		lib.Load;
		ctx := TLieselContext.Create(lib);
		book := ctx.CreateBook;

		book.ImportSettingsFromCommandString(cmd);

		// Update the selected input file too, so preview/printing reflect the imported command.
		// (This also makes Import visibly "do something" even if only paths differ.)
		if book.TryGetInputPdfPath(importedInput) and (Trim(importedInput) <> '') then
		begin
			FInputPdfPath := importedInput;
			FileInputButton.Caption := ExtractFileName(importedInput);
			SaveDialog.FileName := ChangeFileExt(ExtractFileName(FInputPdfPath), '') + '-out.pdf';
		end;

		FApplyingSettings := True;
		try
			// MainWindow settings
			QualitySlider.Position := EnsureRange(Integer(book.GetDpiDensity), QualitySlider.Min, QualitySlider.Max);
			GreyscaleCheckbox.Checked := book.GetGreyscale;

			if book.TryGetSegmentSize(segSize) then
			begin
				SegmentCheckbox.Checked := True;
				SegmentInputBox.Value := Integer(segSize);
			end
			else
				SegmentCheckbox.Checked := False;

			if book.TryGetRescaleSize(rescaleSize) and (Trim(rescaleSize) <> '') then
			begin
				RescaleCheckbox.Checked := True;
				RescaleDropdownBox.Text := rescaleSize;
			end
			else
				RescaleCheckbox.Checked := False;

			if book.TryGetPageRanges(ranges) and (Trim(ranges) <> '') then
			begin
				RangeCheckbox.Checked := True;
				RangeInputTextbox.Text := ranges;
			end
			else
			begin
				RangeCheckbox.Checked := False;
				RangeInputTextbox.Text := '';
			end;

			// Apply panel visibility/layout updates without triggering previews mid-batch.
			RangeCheckboxChange(Self);
			SegmentCheckboxChange(Self);
			RescaleCheckboxChange(Self);

			// AdvancedWindow settings
			if Assigned(AdvancedWindow) then
			begin
				divideEnabled := book.GetDivide;
				bookletEnabled := book.GetBooklet;
				AdvancedWindow.SplitPagesCheckbox.Checked := divideEnabled;
				AdvancedWindow.NoBookletCheckbox.Checked := not bookletEnabled;

				if book.TryGetThresholdLevel(thresholdLevel) then
				begin
					AdvancedWindow.ColorThresholdCheckbox.Checked := True;
					AdvancedWindow.ColorThresholdSlider.Position := Integer(thresholdLevel);
				end
				else
					AdvancedWindow.ColorThresholdCheckbox.Checked := False;

				widenAmt := book.GetWidenMarginsAmount;
				AdvancedWindow.CenterMarginCheckbox.Checked := widenAmt > 0;
				AdvancedWindow.CenterMarginSlider.Position := Integer(widenAmt);

				if book.TryGetCropPercentagesLRBT(cropL, cropR, cropT, cropB) then
				begin
					AdvancedWindow.CropCheckbox.Checked := True;
					AdvancedWindow.LeftCropSlider.Position := Integer(cropL);
					AdvancedWindow.RightCropSlider.Position := Integer(cropR);
					AdvancedWindow.TopCropSlider.Position := Integer(cropT);
					AdvancedWindow.BottomCropSlider.Position := Integer(cropB);
				end
				else
					AdvancedWindow.CropCheckbox.Checked := False;
			end;
		finally
			FApplyingSettings := False;
		end;

		RequestPreviewUpdateDebounced;
	except
		on E: Exception do
			MessageDlg('Import failed', E.Message, mtError, [mbOK], 0);
	end;

	FreeAndNil(book);
	FreeAndNil(ctx);
	FreeAndNil(lib);
end;

procedure TMainWindow.ExportCommandButtonClick(Sender: TObject);
var
	lib: TLieselLib;
	ctx: TLieselContext;
	book: TLieselBook;
	cmd: string;
begin
	lib := nil;
	ctx := nil;
	book := nil;
	cmd := '';
	try
		lib := TLieselLib.Create;
		lib.Load;
		ctx := TLieselContext.Create(lib);
		book := ctx.CreateBook;

		// MainWindow settings
		if Trim(FInputPdfPath) <> '' then
			book.SetInputPdfPath(FInputPdfPath);
		book.SetDpiDensity(Cardinal(QualitySlider.Position));
		book.SetGreyscale(GreyscaleCheckbox.Checked);

		if SegmentCheckbox.Checked then
			book.SetSegmentSize(Cardinal(SegmentInputBox.Value))
		else
			book.ClearSegmentSize;

		if RescaleCheckbox.Checked and (Trim(RescaleDropdownBox.Text) <> '') then
			book.SetRescaleSize(RescaleDropdownBox.Text)
		else
			book.ClearRescaleSize;

		if RangeCheckbox.Checked and (Trim(RangeInputTextbox.Text) <> '') then
			book.SetPageRanges(RangeInputTextbox.Text)
		else
			book.ClearPageRanges;

		// AdvancedWindow settings
		if Assigned(AdvancedWindow) then
		begin
			book.SetDivide(AdvancedWindow.SplitPagesCheckbox.Checked);
			book.SetBooklet(not AdvancedWindow.NoBookletCheckbox.Checked);

			if AdvancedWindow.ColorThresholdCheckbox.Checked then
				book.SetThresholdLevel(Byte(AdvancedWindow.ColorThresholdSlider.Position))
			else
				book.ClearThresholdLevel;

			if AdvancedWindow.CenterMarginCheckbox.Checked then
				book.SetWidenMarginsAmount(Cardinal(AdvancedWindow.CenterMarginSlider.Position))
			else
				book.SetWidenMarginsAmount(0);

			if AdvancedWindow.CropCheckbox.Checked then
				book.SetCropPercentagesLRBT(
					Byte(AdvancedWindow.LeftCropSlider.Position),
					Byte(AdvancedWindow.RightCropSlider.Position),
					Byte(AdvancedWindow.TopCropSlider.Position),
					Byte(AdvancedWindow.BottomCropSlider.Position)
				)
			else
				book.SetCropPercentagesLRBT(0, 0, 0, 0);
		end;

		cmd := book.ExportSettingsAsCommandString;

		ImportExportCommandModal := TImportExportCommandModal.Create(Self);
		ImportExportCommandModal.SetMode(ieExport);
		ImportExportCommandModal.CommandText := cmd;
		try
			ImportExportCommandModal.ShowModal;
		finally
			ImportExportCommandModal.Free;
		end;
	except
		on E: Exception do
			MessageDlg('Export failed', E.Message, mtError, [mbOK], 0);
	end;

	FreeAndNil(book);
	FreeAndNil(ctx);
	FreeAndNil(lib);
end;

procedure TMainWindow.LibgenButtonClick(Sender: TObject);
begin
	{ Open Library Genesis website in default browser }
	OpenURL('http://libgen.li');
end;

procedure TMainWindow.LoadSettingsButtonClick(Sender: TObject);
var
	cfgPath: string;
	sl: TStringList;
	cmd: string;
	lib: TLieselLib;
	ctx: TLieselContext;
	book: TLieselBook;
	segSize: Cardinal;
	rescaleSize: string;
	ranges: string;
	thresholdLevel: Byte;
	widenAmt: Cardinal;
	cropL, cropR, cropT, cropB: Byte;
	importedInput: string;
begin
	// Load settings from ~/.bookthiefrc (fail silently on any error)
	cfgPath := IncludeTrailingPathDelimiter(GetUserDir) + '.bookthiefrc';
	if not FileExists(cfgPath) then Exit;

	sl := nil;
	lib := nil;
	ctx := nil;
	book := nil;
	try
		sl := TStringList.Create;
		sl.LoadFromFile(cfgPath);
		cmd := Trim(sl.Text);
		if cmd = '' then Exit;

		lib := TLieselLib.Create;
		lib.Load;
		ctx := TLieselContext.Create(lib);
		book := ctx.CreateBook;

		book.ImportSettingsFromCommandString(cmd);

		// If the saved command included an input path and it was accepted, update the GUI.
		if book.TryGetInputPdfPath(importedInput) and (Trim(importedInput) <> '') then
		begin
			FInputPdfPath := importedInput;
			FileInputButton.Caption := ExtractFileName(importedInput);
			SaveDialog.FileName := ChangeFileExt(ExtractFileName(FInputPdfPath), '') + '-out.pdf';
		end;

		FApplyingSettings := True;
		try
			// MainWindow settings
			QualitySlider.Position := EnsureRange(Integer(book.GetDpiDensity), QualitySlider.Min, QualitySlider.Max);
			GreyscaleCheckbox.Checked := book.GetGreyscale;

			if book.TryGetSegmentSize(segSize) then
			begin
				SegmentCheckbox.Checked := True;
				SegmentInputBox.Value := Integer(segSize);
			end
			else
				SegmentCheckbox.Checked := False;

			if book.TryGetRescaleSize(rescaleSize) and (Trim(rescaleSize) <> '') then
			begin
				RescaleCheckbox.Checked := True;
				RescaleDropdownBox.Text := rescaleSize;
			end
			else
				RescaleCheckbox.Checked := False;

			if book.TryGetPageRanges(ranges) and (Trim(ranges) <> '') then
			begin
				RangeCheckbox.Checked := True;
				RangeInputTextbox.Text := ranges;
			end
			else
			begin
				RangeCheckbox.Checked := False;
				RangeInputTextbox.Text := '';
			end;

			// Apply panel visibility/layout updates without triggering previews mid-batch.
			RangeCheckboxChange(Self);
			SegmentCheckboxChange(Self);
			RescaleCheckboxChange(Self);

			// AdvancedWindow settings
			if Assigned(AdvancedWindow) then
			begin
				AdvancedWindow.SplitPagesCheckbox.Checked := book.GetDivide;
				AdvancedWindow.NoBookletCheckbox.Checked := not book.GetBooklet;

				if book.TryGetThresholdLevel(thresholdLevel) then
				begin
					AdvancedWindow.ColorThresholdCheckbox.Checked := True;
					AdvancedWindow.ColorThresholdSlider.Position := Integer(thresholdLevel);
				end
				else
					AdvancedWindow.ColorThresholdCheckbox.Checked := False;

				widenAmt := book.GetWidenMarginsAmount;
				AdvancedWindow.CenterMarginCheckbox.Checked := widenAmt > 0;
				AdvancedWindow.CenterMarginSlider.Position := Integer(widenAmt);

				if book.TryGetCropPercentagesLRBT(cropL, cropR, cropT, cropB) then
				begin
					AdvancedWindow.CropCheckbox.Checked := True;
					AdvancedWindow.LeftCropSlider.Position := Integer(cropL);
					AdvancedWindow.RightCropSlider.Position := Integer(cropR);
					AdvancedWindow.TopCropSlider.Position := Integer(cropT);
					AdvancedWindow.BottomCropSlider.Position := Integer(cropB);
				end
				else
					AdvancedWindow.CropCheckbox.Checked := False;
			end;
		finally
			FApplyingSettings := False;
		end;

		RequestPreviewUpdateDebounced;
	except
		// Fail silently
	end;

	FreeAndNil(book);
	FreeAndNil(ctx);
	FreeAndNil(lib);
	FreeAndNil(sl);
end;

procedure TMainWindow.QuitButtonClick(Sender: TObject);
begin
	Close;
end;

procedure TMainWindow.RangeCheckboxChange(Sender: TObject);
begin
	RangeInputPanel.Visible := RangeCheckbox.Checked;
	LayoutChanged;
	RequestPreviewUpdateDebounced;
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
	RequestPreviewUpdateDebounced;
end;

procedure TMainWindow.SaveButtonClick(Sender: TObject);
var
	dpiDensity: Cardinal;
	useRanges: Boolean;
	ranges: string;
	greyscale: Boolean;
	useSegments: Boolean;
	segmentSize: Cardinal;
	useRescale: Boolean;
	rescaleSize: string;
begin
	if FInputPdfPath = '' then
	begin
		MessageDlg('No input selected', 'Please choose an input PDF first.', mtWarning, [mbOK], 0);
		Exit;
	end;

	if SaveDialog.Execute then
	begin
		FOutputPdfPath := SaveDialog.FileName;

		// Quality slider maps directly to Liesel DPI density.
		dpiDensity := Cardinal(QualitySlider.Position);

		useRanges := RangeCheckbox.Checked;
		ranges := RangeInputTextbox.Text;

		greyscale := GreyscaleCheckbox.Checked;

		useSegments := SegmentCheckbox.Checked;
		segmentSize := Cardinal(SegmentInputBox.Value);

		useRescale := RescaleCheckbox.Checked;
		rescaleSize := RescaleDropdownBox.Text;

		if Assigned(ProgressWindow) then
		begin
			ProgressWindow.ResetForJob;
			ProgressWindow.Show;
			ProgressWindow.BringToFront;
		end;

		// Run in background so the UI stays responsive.
		TPrintJobThread.Create(
			FInputPdfPath, FOutputPdfPath,
			dpiDensity,
			useRanges, ranges,
			greyscale,
			useSegments, segmentSize,
			useRescale, rescaleSize
		);
	end;
end;

procedure TMainWindow.SaveSettingsButtonClick(Sender: TObject);
var
	cfgPath: string;
	lib: TLieselLib;
	ctx: TLieselContext;
	book: TLieselBook;
	sl: TStringList;
	cmd: string;
	inputEscaped: string;
	inputPart: string;
begin
	// Export current settings to ~/.bookthiefrc (show an error dialog on save failure)
	cfgPath := IncludeTrailingPathDelimiter(GetUserDir) + '.bookthiefrc';
	lib := nil;
	ctx := nil;
	book := nil;
	sl := nil;
	try
		lib := TLieselLib.Create;
		lib.Load;
		ctx := TLieselContext.Create(lib);
		book := ctx.CreateBook;

		// MainWindow settings (intentionally do not bind to a specific input/output path)
		book.SetDpiDensity(Cardinal(QualitySlider.Position));
		book.SetGreyscale(GreyscaleCheckbox.Checked);

		if SegmentCheckbox.Checked then
			book.SetSegmentSize(Cardinal(SegmentInputBox.Value))
		else
			book.ClearSegmentSize;

		if RescaleCheckbox.Checked and (Trim(RescaleDropdownBox.Text) <> '') then
			book.SetRescaleSize(RescaleDropdownBox.Text)
		else
			book.ClearRescaleSize;

		if RangeCheckbox.Checked and (Trim(RangeInputTextbox.Text) <> '') then
			book.SetPageRanges(RangeInputTextbox.Text)
		else
			book.ClearPageRanges;

		// AdvancedWindow settings
		if Assigned(AdvancedWindow) then
		begin
			book.SetDivide(AdvancedWindow.SplitPagesCheckbox.Checked);
			book.SetBooklet(not AdvancedWindow.NoBookletCheckbox.Checked);

			if AdvancedWindow.ColorThresholdCheckbox.Checked then
				book.SetThresholdLevel(Byte(AdvancedWindow.ColorThresholdSlider.Position))
			else
				book.ClearThresholdLevel;

			if AdvancedWindow.CenterMarginCheckbox.Checked then
				book.SetWidenMarginsAmount(Cardinal(AdvancedWindow.CenterMarginSlider.Position))
			else
				book.SetWidenMarginsAmount(0);

			if AdvancedWindow.CropCheckbox.Checked then
				book.SetCropPercentagesLRBT(
					Byte(AdvancedWindow.LeftCropSlider.Position),
					Byte(AdvancedWindow.RightCropSlider.Position),
					Byte(AdvancedWindow.TopCropSlider.Position),
					Byte(AdvancedWindow.BottomCropSlider.Position)
				)
			else
				book.SetCropPercentagesLRBT(0, 0, 0, 0);
		end;

		cmd := Trim(book.ExportSettingsAsCommandString);

		// Also persist the currently-selected input path. Do not call book.SetInputPdfPath
		// here, because Book may validate file existence; for a config file we just want
		// to remember the path string.
		if Trim(FInputPdfPath) <> '' then
		begin
			inputEscaped := StringReplace(FInputPdfPath, '\\', '\\\\', [rfReplaceAll]);
			inputEscaped := StringReplace(inputEscaped, '"', '\\"', [rfReplaceAll]);
			inputPart := ' "' + inputEscaped + '"';
			if (Length(cmd) >= 5) and (Copy(cmd, 1, 5) = 'liesel') then
				cmd := 'liesel' + inputPart + Copy(cmd, 6, MaxInt)
			else
				cmd := 'liesel' + inputPart + ' ' + cmd;
		end;

		sl := TStringList.Create;
		sl.Text := cmd + LineEnding;
		sl.SaveToFile(cfgPath);
	except
		on E: Exception do
			MessageDlg('Save settings failed', E.Message, mtError, [mbOK], 0);
	end;

	FreeAndNil(sl);
	FreeAndNil(book);
	FreeAndNil(ctx);
	FreeAndNil(lib);
end;

procedure TMainWindow.SegmentCheckboxChange(Sender: TObject);
begin
	SegmentInputPanel.Visible := SegmentCheckbox.Checked;
	LayoutChanged;
	RequestPreviewUpdateDebounced;
end;

end.

