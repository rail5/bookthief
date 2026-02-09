unit AdvancedWindowUnit;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Types, Math, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	ComCtrls, StdCtrls, LCLType,
	MinSizeFormUnit;

type
	TAdvancedWindowClosedEvent = procedure(Sender: TObject) of object;

	{ TAdvancedWindow }

	TAdvancedWindow = class(TMinSizeForm)
		NoBookletCheckbox: TCheckBox;
		NoBookletPanel: TPanel;
		SplitPagesCheckbox: TCheckBox;
		CropCheckbox: TCheckBox;
		ColorThresholdCheckbox: TCheckBox;
		CenterMarginCheckbox: TCheckBox;
		CenterMarginPanel: TPanel;
		CenterMarginSlider: TTrackBar;
		EnableDisablePreviewCheckbox: TCheckBox;
		FirstPageButton: TButton;
		AdvancedControlsPanel: TPanel;
		ColorThresholdPanel: TPanel;
		CropPanel: TPanel;
		CropSlidersPanel: TPanel;
		LeftRightSlidersPanel: TPanel;
		SplitPagesPanel: TPanel;
		TopBottomSlidersPanel: TPanel;
		PreviewingPageLabel: TLabel;
		LastPageButton: TButton;
		ImageAreaPanel: TPanel;
		ImagePreview: TImage;
		ImagePreviewPanel: TPanel;
		ImagePreviewControlsPanel: TPanel;
		ControlsOuterPanel: TPanel;
		ControlsInnerPanel: TPanel;
		LeftRightNavigation: TUpDown;
		EnableDisablePanel: TPanel;
		PreviewBox: TPanel;
		PreviewPlaceholder: TPaintBox;
		ColorThresholdSlider: TTrackBar;
		LeftCropSlider: TTrackBar;
		RightCropSlider: TTrackBar;
		TopCropSlider: TTrackBar;
		BottomCropSlider: TTrackBar;
		procedure BottomCropSliderChange(Sender: TObject);
		procedure CenterMarginSliderChange(Sender: TObject);
		procedure ColorThresholdCheckboxChange(Sender: TObject);
		procedure CenterMarginCheckboxChange(Sender: TObject);
		procedure ColorThresholdSliderChange(Sender: TObject);
		procedure CropCheckboxChange(Sender: TObject);
		procedure CropPanelClick(Sender: TObject);
		procedure EnableDisablePreviewCheckboxChange(Sender: TObject);
		procedure FirstPageButtonClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormResize(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure LastPageButtonClick(Sender: TObject);
		procedure LeftCropSliderChange(Sender: TObject);
		procedure LeftRightNavigationClick(Sender: TObject; Button: TUDBtnType);
		procedure NoBookletCheckboxChange(Sender: TObject);
		procedure PreviewPlaceholderPaint(Sender: TObject);
		procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
			MaxWidth, MaxHeight: TConstraintSize);
		procedure RightCropSliderChange(Sender: TObject);
		procedure SplitPagesCheckboxChange(Sender: TObject);
		procedure TopCropSliderChange(Sender: TObject);
	private
		FOnSettingsChanged: TNotifyEvent;

		// Fixed-width containers for right-aligned sliders (prevents “width 30” collapse)
		FColorThresholdHost: TPanel;
		FCenterMarginHost: TPanel;

		// Fixed-width containers for crop sliders
		FCropLeftHost: TPanel;
		FCropRightHost: TPanel;
		FCropTopHost: TPanel;
		FCropBottomHost: TPanel;

		procedure BuildRightSliderHost(var Host: TPanel; OwnerPanel: TPanel; Slider: TTrackBar; HostWidth: Integer);
		procedure BuildSideSliderHost(var Host: TPanel; OwnerPanel: TPanel; Slider: TTrackBar;
			AAlign: TAlign; HostWidth: Integer; const LabelText: string);

		procedure UpdatePreviewBoxBounds;
		procedure UpdatePreviewVisibility;
		function HasPreviewImage: Boolean;
		procedure NotifySettingsChanged;

		procedure LayoutChanged; // call after any show/hide or size-affecting change
	protected
		procedure GetRequiredClientSize(out ReqClientW, ReqClientH: Integer); override;
		procedure AfterEnsureSizePass; override;
	public
		// Fired whenever any advanced setting that affects output/preview changes.
		property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged write FOnSettingsChanged;
		procedure SetPreviewJpegBytes(const Jpeg: TBytes);
	end;

var
	AdvancedWindow: TAdvancedWindow;

implementation

{$R *.lfm}

uses
	FPImage, FPReadJPEG;

const
	// To be tuned later (displayed bounds, not original image size)
	PREVIEW_MIN_W = 640;
	PREVIEW_MIN_H = 480;
	PREVIEW_MAX_W = 1200;
	PREVIEW_MAX_H = 900;

	BORDER_SPACING_BOTTOM = 20;

	SLIDER_ROW_W = 295;
	CROP_SLIDER_W = 140;
	CROP_LABEL_H = 18;

{ TAdvancedWindow }

procedure TAdvancedWindow.AfterEnsureSizePass;
begin
	// Advanced window relies on aligned controls; force an align pass between snaps.
	ReAlign;
end;

procedure TAdvancedWindow.BuildRightSliderHost(var Host: TPanel; OwnerPanel: TPanel;
	Slider: TTrackBar; HostWidth: Integer);
begin
	if (OwnerPanel = nil) or (Slider = nil) then Exit;
	if Host <> nil then Exit;

	Host := TPanel.Create(Self);
	Host.Parent := OwnerPanel;
	Host.Align := alRight;
	Host.BevelOuter := bvNone;
	Host.Caption := '';
	Host.AutoSize := False;
	Host.Width := HostWidth;
	Host.Constraints.MinWidth := HostWidth;
	Host.Constraints.MaxWidth := HostWidth;

	// Reparent slider into host so the host enforces width.
	Slider.Parent := Host;
	Slider.Align := alClient;

	// Slider should normally always be "visible"; the host controls visibility.
	Slider.Visible := True;
end;

procedure TAdvancedWindow.BuildSideSliderHost(var Host: TPanel; OwnerPanel: TPanel;
	Slider: TTrackBar; AAlign: TAlign; HostWidth: Integer; const LabelText: string);
var
	L: TLabel;
	minH: Integer;
begin
	if (OwnerPanel = nil) or (Slider = nil) then Exit;
	if Host <> nil then Exit;

	Host := TPanel.Create(Self);
	Host.Parent := OwnerPanel;
	Host.Align := AAlign; // alLeft or alRight
	Host.BevelOuter := bvNone;
	Host.Caption := '';
	Host.AutoSize := False;

	// Fixed width so the trackbar can never collapse to ~30px.
	Host.Width := HostWidth;
	Host.Constraints.MinWidth := HostWidth;
	Host.Constraints.MaxWidth := HostWidth;

	// Label under slider
	L := TLabel.Create(Self);
	L.Parent := Host;
	L.Align := alBottom;
	L.AutoSize := False;
	L.Height := CROP_LABEL_H;
	L.Alignment := taCenter;
	L.Layout := tlCenter;
	L.Caption := LabelText;

	// Slider above label
	Slider.Parent := Host;
	Slider.Align := alClient;
	Slider.Visible := True;

	// Ensure host is tall enough to show both slider + label.
	minH := Slider.Height + L.Height;
	Host.Constraints.MinHeight := minH;
	if Host.Height < minH then Host.Height := minH;
end;

procedure TAdvancedWindow.GetRequiredClientSize(out ReqClientW, ReqClientH: Integer);
var
	previewH: Integer;
begin
	// Initialize out parameters to prevent garbage values
	ReqClientW := 0;
	ReqClientH := 0;
	// Minimum width requirements based on “left+right aligned fixed-width” rows.
	ReqClientW := PREVIEW_MIN_W;

	// Preview controls row: EnableDisablePanel (alLeft) + ControlsOuterPanel (alRight)
	if (EnableDisablePanel <> nil) and (ControlsOuterPanel <> nil) then
		ReqClientW := Max(ReqClientW, EnableDisablePanel.Width + ControlsOuterPanel.Width);

	// Rows with optional right-side slider hosts (fixed width)
	if (ColorThresholdCheckbox <> nil) and (FColorThresholdHost <> nil) and FColorThresholdHost.Visible then
		ReqClientW := Max(ReqClientW, ColorThresholdCheckbox.Width + FColorThresholdHost.Width);

	if (CenterMarginCheckbox <> nil) and (FCenterMarginHost <> nil) and FCenterMarginHost.Visible then
		ReqClientW := Max(ReqClientW, CenterMarginCheckbox.Width + FCenterMarginHost.Width);

	// Crop sliders panel has a fixed width when visible (alRight)
	if (CropSlidersPanel <> nil) and CropSlidersPanel.Visible then
		ReqClientW := Max(ReqClientW, CropSlidersPanel.Width);

	// Height is the sum of the stacked alTop panels.
	ReqClientH := 0;

	if ImagePreviewPanel <> nil then
	begin
		previewH := Max(ImagePreviewPanel.Height, ImagePreviewPanel.Constraints.MinHeight);
		ReqClientH := ReqClientH + previewH;
	end;

	if AdvancedControlsPanel <> nil then
		ReqClientH := ReqClientH + AdvancedControlsPanel.Height;

	// Safety minimum so the preview area never collapses
	ReqClientH := Max(ReqClientH, PREVIEW_MIN_H + ImagePreviewControlsPanel.Height);

end;

procedure TAdvancedWindow.LayoutChanged;
begin
	DisableAlign;
	try
		if LeftRightSlidersPanel <> nil then
		begin
			LeftRightSlidersPanel.InvalidatePreferredSize;
			LeftRightSlidersPanel.AdjustSize;
		end;

		if TopBottomSlidersPanel <> nil then
		begin
			TopBottomSlidersPanel.InvalidatePreferredSize;
			TopBottomSlidersPanel.AdjustSize;
		end;

		if CropPanel <> nil then
		begin
			CropPanel.InvalidatePreferredSize;
			CropPanel.AdjustSize;
		end;

		if AdvancedControlsPanel <> nil then
		begin
			AdvancedControlsPanel.InvalidatePreferredSize;
			AdvancedControlsPanel.AdjustSize;
		end;

		InvalidatePreferredSize;
	finally
		EnableAlign;
	end;
	ReAlign;

	// Converge to a stable minimum size.
	SnapToMinimumSize(3);
end;

procedure TAdvancedWindow.FormConstrainedResize(Sender: TObject; var MinWidth,
	MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
	ApplyMinConstraintsToResize(MinWidth, MinHeight);
end;

procedure TAdvancedWindow.NotifySettingsChanged;
begin
	if Assigned(FOnSettingsChanged) then
		FOnSettingsChanged(Self);
end;

procedure TAdvancedWindow.SetPreviewJpegBytes(const Jpeg: TBytes);
var
	ms: TMemoryStream;
begin
	try
		if Length(Jpeg) = 0 then
		begin
			ImagePreview.Picture.Clear;
			UpdatePreviewVisibility;
			Exit;
		end;

		ms := TMemoryStream.Create;
		try
			ms.WriteBuffer(Jpeg[0], Length(Jpeg));
			ms.Position := 0;
			ImagePreview.Picture.LoadFromStream(ms);
		finally
			ms.Free;
		end;
		UpdatePreviewVisibility;
	except
		// If decoding fails, fall back to placeholder.
		ImagePreview.Picture.Clear;
		UpdatePreviewVisibility;
	end;
end;

procedure TAdvancedWindow.RightCropSliderChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.SplitPagesCheckboxChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.TopCropSliderChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.FormCreate(Sender: TObject);
begin
	// Make sure the preview area doesn't disappear when empty.
	ImagePreviewPanel.AutoSize := False;
	ImagePreviewPanel.Constraints.MinHeight := PREVIEW_MIN_H + ImagePreviewControlsPanel.Height;

	ImagePreview.Stretch := True;
	ImagePreview.Proportional := True;
	ImagePreview.Center := True;

	// Build fixed-width slider hosts (threshold/margin)
	BuildRightSliderHost(FColorThresholdHost, ColorThresholdPanel, ColorThresholdSlider, SLIDER_ROW_W);
	BuildRightSliderHost(FCenterMarginHost, CenterMarginPanel, CenterMarginSlider, SLIDER_ROW_W);

	// Build fixed-width hosts for crop sliders + labels underneath
	BuildSideSliderHost(FCropLeftHost, LeftRightSlidersPanel, LeftCropSlider, alLeft, CROP_SLIDER_W, 'Left');
	BuildSideSliderHost(FCropRightHost, LeftRightSlidersPanel, RightCropSlider, alRight, CROP_SLIDER_W, 'Right');
	BuildSideSliderHost(FCropTopHost, TopBottomSlidersPanel, TopCropSlider, alLeft, CROP_SLIDER_W, 'Top');
	BuildSideSliderHost(FCropBottomHost, TopBottomSlidersPanel, BottomCropSlider, alRight, CROP_SLIDER_W, 'Bottom');

	// Make sure the crop group can never become narrower than the two hosts.
	if CropSlidersPanel <> nil then
		CropSlidersPanel.Constraints.MinWidth := 2 * CROP_SLIDER_W;

	// Host visibility follows checkbox state (not the slider’s Visible flag).
	if FColorThresholdHost <> nil then
		FColorThresholdHost.Visible := ColorThresholdCheckbox.Checked;
	if FCenterMarginHost <> nil then
		FCenterMarginHost.Visible := CenterMarginCheckbox.Checked;

	// (Crop hosts are always present; CropSlidersPanel.Visible controls the whole group)

	UpdatePreviewBoxBounds;
	UpdatePreviewVisibility;

	{ BorderSpacing for checkboxes }
	ColorThresholdCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	CenterMarginCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	CropCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	SplitPagesCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;
	NoBookletCheckbox.BorderSpacing.Bottom := BORDER_SPACING_BOTTOM;

	// Recompute layout (including new label heights) and snap to minimum.
	LayoutChanged;
end;

procedure TAdvancedWindow.FormShow(Sender: TObject);
begin
	KeyPreview := True;
	SnapToMinimumSize(3);
end;

procedure TAdvancedWindow.FormResize(Sender: TObject);
begin
	UpdatePreviewBoxBounds;
end;

procedure TAdvancedWindow.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		Close;
end;

function TAdvancedWindow.HasPreviewImage: Boolean;
begin
	Result :=
		(ImagePreview.Picture <> nil) and
		(ImagePreview.Picture.Graphic <> nil) and
		(not ImagePreview.Picture.Graphic.Empty);
end;

procedure TAdvancedWindow.UpdatePreviewVisibility;
begin
	// Only show the actual TImage when it has content;
	// otherwise show a painted placeholder.
	PreviewPlaceholder.Visible := not HasPreviewImage;
	PreviewBox.Visible := HasPreviewImage;

	if PreviewPlaceholder.Visible then
		PreviewPlaceholder.Invalidate;
end;

procedure TAdvancedWindow.UpdatePreviewBoxBounds;
var
	availW, availH: Integer;
	targetW, targetH: Integer;
begin
	if (ImageAreaPanel = nil) or (PreviewBox = nil) then Exit;

	availW := ImageAreaPanel.ClientWidth;
	availH := ImageAreaPanel.ClientHeight;

	// Clamp desired preview size within min/max, but never exceed available space.
	targetW := Min(availW, PREVIEW_MAX_W);
	targetH := Min(availH, PREVIEW_MAX_H);

	targetW := Max(targetW, PREVIEW_MIN_W);
	targetH := Max(targetH, PREVIEW_MIN_H);

	// If the window is smaller than the minimum, fall back to available space.
	targetW := Min(targetW, availW);
	targetH := Min(targetH, availH);

	PreviewBox.SetBounds(
		(availW - targetW) div 2,
		(availH - targetH) div 2,
		targetW,
		targetH
	);
end;

procedure TAdvancedWindow.PreviewPlaceholderPaint(Sender: TObject);
var
	r: TRect;
	s: string;
	x, y: Integer;
begin
	r := PreviewPlaceholder.ClientRect;

	PreviewPlaceholder.Canvas.Brush.Color := clBtnFace;
	PreviewPlaceholder.Canvas.FillRect(r);

	// Border
	PreviewPlaceholder.Canvas.Pen.Color := clGray;
	PreviewPlaceholder.Canvas.Rectangle(r);

	// Diagonal hint lines
	PreviewPlaceholder.Canvas.Pen.Color := clSilver;
	PreviewPlaceholder.Canvas.Line(r.Left, r.Top, r.Right, r.Bottom);
	PreviewPlaceholder.Canvas.Line(r.Left, r.Bottom, r.Right, r.Top);

	// Centered text
	s := 'No image loaded';
	x := (r.Width - PreviewPlaceholder.Canvas.TextWidth(s)) div 2;
	y := (r.Height - PreviewPlaceholder.Canvas.TextHeight(s)) div 2;
	PreviewPlaceholder.Canvas.Font.Color := clGrayText;
	PreviewPlaceholder.Canvas.TextOut(r.Left + x, r.Top + y, s);
end;

procedure TAdvancedWindow.FirstPageButtonClick(Sender: TObject);
begin
	LeftRightNavigation.Position := 0;
	PreviewingPageLabel.Caption := 'Previewing page: 1';
	NotifySettingsChanged;
	UpdatePreviewVisibility;
end;

procedure TAdvancedWindow.EnableDisablePreviewCheckboxChange(Sender: TObject);
begin
	if not EnableDisablePreviewCheckbox.Checked then
		SetPreviewJpegBytes(nil);
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.ColorThresholdCheckboxChange(Sender: TObject);
begin
	if FColorThresholdHost <> nil then
		FColorThresholdHost.Visible := ColorThresholdCheckbox.Checked;
	LayoutChanged;
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.BottomCropSliderChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.CenterMarginSliderChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.CenterMarginCheckboxChange(Sender: TObject);
begin
	if FCenterMarginHost <> nil then
		FCenterMarginHost.Visible := CenterMarginCheckbox.Checked;
	LayoutChanged;
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.ColorThresholdSliderChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.CropCheckboxChange(Sender: TObject);
begin
	CropSlidersPanel.Visible := CropCheckbox.Checked;
	LayoutChanged;
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.CropPanelClick(Sender: TObject);
begin

end;

procedure TAdvancedWindow.LastPageButtonClick(Sender: TObject);
begin
	// We don't currently know the PDF page count here; caller can clamp.
	NotifySettingsChanged;
	UpdatePreviewVisibility;
end;

procedure TAdvancedWindow.LeftCropSliderChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

procedure TAdvancedWindow.LeftRightNavigationClick(Sender: TObject; Button: TUDBtnType);
begin
	PreviewingPageLabel.Caption := Format('Previewing page: %d', [LeftRightNavigation.Position + 1]);
	NotifySettingsChanged;
	UpdatePreviewVisibility;
end;

procedure TAdvancedWindow.NoBookletCheckboxChange(Sender: TObject);
begin
	NotifySettingsChanged;
end;

end.

