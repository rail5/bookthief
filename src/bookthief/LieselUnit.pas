unit LieselUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, ctypes, LieselABIUnit;

type
	ELieselError = class(Exception)
	private
		FStatus: TLieselStatus;
	public
		constructor CreateStatus(AStatus: TLieselStatus; const AMsg: string);
		property Status: TLieselStatus read FStatus;
	end;

	TLieselBookProgressEvent = (
		lpeInfo,
		lpeRenderPage,
		lpeSegmentDone,
		lpePrintDone
	);

	TLieselProgressEventHandler = procedure(
		Sender: TObject;
		Event: TLieselBookProgressEvent;
		SegmentIndex: Cardinal;
		PageIndex: Cardinal;
		Percent: Cardinal;
		const MessageUtf8: string
	) of object;

	TLieselCancelHandler = function(Sender: TObject): Boolean of object;

	TLieselContext = class;

	{ TLieselBook }

	TLieselBook = class
	private
		FContext: TLieselContext;
		FHandle: PLieselBookHandle;
		FCancelled: Boolean;
		FOnProgress: TLieselProgressEventHandler;
		FOnCancel: TLieselCancelHandler;

		procedure RaiseOnStatus(AStatus: TLieselStatus; const Context: string);
		function LastErrorUtf8: string;
	public
		constructor Create(AContext: TLieselContext);
		destructor Destroy; override;

		procedure Cancel;

		procedure ImportSettingsFromCommandString(const OptionsStrUtf8: string);
		function ExportSettingsAsCommandString: string;

		function GetVerbose: Boolean;
		function GetGreyscale: Boolean;
		function GetDivide: Boolean;
		function GetBooklet: Boolean;
		function GetLandscape: Boolean;
		function GetDpiDensity: Cardinal;

		function TryGetThresholdLevel(out Level0To100: Byte): Boolean;
		function TryGetSegmentSize(out PagesPerSegment: Cardinal): Boolean;
		function TryGetRescaleSize(out SizeUtf8: string): Boolean;
		function TryGetPageRanges(out RangesUtf8: string): Boolean;
		function GetWidenMarginsAmount: Cardinal;
		function TryGetCropPercentagesLRBT(out L, R, T, B: Byte): Boolean;
		function TryGetInputPdfPath(out PathUtf8: string): Boolean;
		function TryGetOutputPdfPath(out PathUtf8: string): Boolean;

		procedure SetInputPdfPath(const PathUtf8: string);
		procedure SetOutputPdfPath(const PathUtf8: string);

		procedure SetVerbose(Enabled: Boolean);
		procedure SetGreyscale(Enabled: Boolean);
		procedure SetDivide(Enabled: Boolean);
		procedure SetBooklet(Enabled: Boolean);
		procedure SetLandscape(Enabled: Boolean);

		procedure SetDpiDensity(Dpi: Cardinal);
		procedure SetThresholdLevel(Level0To100: Byte);
		procedure ClearThresholdLevel;

		procedure SetSegmentSize(PagesPerSegment: Cardinal);
		procedure ClearSegmentSize;
		procedure SetWidenMarginsAmount(Amount: Cardinal);

		procedure SetRescaleSize(const SizeUtf8: string);
		procedure ClearRescaleSize;

		procedure SetPageRanges(const RangesUtf8: string);
		procedure ClearPageRanges;

		procedure SetCropPercentages(const CropUtf8: string);
		procedure SetCropPercentagesLRBT(L, R, T, B: Byte);

		procedure LoadPdf;
		function GetPdfPageCount: Cardinal;
		procedure Print;

		// Preview (GUI support)
		procedure SetPreviewing(Enabled: Boolean);
		procedure SetPreviewPage(PageIndex0Based: Cardinal);
		function GetPreviewJpegBytes: TBytes;

		property OnProgress: TLieselProgressEventHandler read FOnProgress write FOnProgress;
		property OnCancel: TLieselCancelHandler read FOnCancel write FOnCancel;
	end;

	{ TLieselContext }

	TLieselContext = class
	private
		FLib: TLieselLib;
		FHandle: PLieselHandle;
		function LastErrorUtf8: string;
	public
		constructor Create(ALib: TLieselLib);
		destructor Destroy; override;

		function CreateBook: TLieselBook;
		function VersionUtf8: string;

		property Lib: TLieselLib read FLib;
	end;

implementation

{ ELieselError }

constructor ELieselError.CreateStatus(AStatus: TLieselStatus; const AMsg: string);
begin
	FStatus := AStatus;
	inherited Create(AMsg);
end;

function StatusToString(AStatus: TLieselStatus): string;
begin
	case AStatus of
		LIESEL_OK: Result := 'OK';
		LIESEL_E_INVALID_ARG: Result := 'INVALID_ARG';
		LIESEL_E_PARSE: Result := 'PARSE';
		LIESEL_E_IO: Result := 'IO';
		LIESEL_E_RUNTIME: Result := 'RUNTIME';
		LIESEL_E_CANCELLED: Result := 'CANCELLED';
	else
		Result := 'UNKNOWN';
	end;
end;

function AbiEventToEvent(E: LieselABIUnit.TLieselProgressEvent): TLieselBookProgressEvent;
begin
	case E of
		LIESEL_PROGRESS_INFO: Result := lpeInfo;
		LIESEL_PROGRESS_RENDER_PAGE: Result := lpeRenderPage;
		LIESEL_PROGRESS_SEGMENT_DONE: Result := lpeSegmentDone;
		LIESEL_PROGRESS_PRINT_DONE: Result := lpePrintDone;
	else
		Result := lpeInfo;
	end;
end;

procedure LieselProgressThunk(
	userdata: Pointer;
	event: LieselABIUnit.TLieselProgressEvent;
	segment_index: cuint32;
	page_index: cuint32;
	percent: cuint32;
	message_utf8: PChar
); cdecl;
var
	book: TLieselBook;
	msg: string;
	pasEvent: TLieselBookProgressEvent;
begin
	book := TLieselBook(userdata);
	if (book = nil) or (not Assigned(book.FOnProgress)) then Exit;

	if message_utf8 <> nil then msg := StrPas(message_utf8) else msg := '';
	pasEvent := AbiEventToEvent(event);

	// Never let exceptions cross the C ABI boundary.
	try
		book.FOnProgress(book, pasEvent, segment_index, page_index, percent, msg);
	except
		// swallow
	end;
end;

function LieselCancelThunk(userdata: Pointer): cint; cdecl;
var
	book: TLieselBook;
	doCancel: Boolean;
begin
	book := TLieselBook(userdata);
	if book = nil then Exit(0);

	if book.FCancelled then Exit(1);

	doCancel := False;
	if Assigned(book.FOnCancel) then
	begin
		try
			doCancel := book.FOnCancel(book);
		except
			doCancel := False;
		end;
	end;

	if doCancel then Result := 1 else Result := 0;
end;

{ TLieselContext }

constructor TLieselContext.Create(ALib: TLieselLib);
begin
	inherited Create;
	FLib := ALib;
	if (FLib = nil) then raise Exception.Create('TLieselContext requires a TLieselLib');
	if not FLib.Loaded then FLib.Load;

	FHandle := FLib.liesel_create();
	if FHandle = nil then
		raise Exception.Create('liesel_create() failed');
end;

destructor TLieselContext.Destroy;
begin
	if (FHandle <> nil) and (FLib <> nil) and FLib.Loaded then
		FLib.liesel_destroy(FHandle);
	FHandle := nil;
	inherited Destroy;
end;

function TLieselContext.LastErrorUtf8: string;
var
	p: PChar;
begin
	Result := '';
	if (FLib = nil) or not FLib.Loaded or (FHandle = nil) then Exit;
	p := FLib.liesel_last_error(FHandle);
	if p <> nil then Result := StrPas(p);
end;

function TLieselContext.VersionUtf8: string;
var
	p: PChar;
begin
	p := FLib.liesel_version();
	if p = nil then Exit('');
	Result := StrPas(p);
end;

function TLieselContext.CreateBook: TLieselBook;
begin
	Result := TLieselBook.Create(Self);
end;

{ TLieselBook }

constructor TLieselBook.Create(AContext: TLieselContext);
begin
	inherited Create;
	FContext := AContext;
	if (FContext = nil) then raise Exception.Create('TLieselBook requires a TLieselContext');

	FHandle := FContext.Lib.liesel_book_create(FContext.FHandle);
	if FHandle = nil then
		raise Exception.CreateFmt('liesel_book_create() failed: %s', [FContext.LastErrorUtf8]);

	FCancelled := False;
end;

destructor TLieselBook.Destroy;
begin
	if (FHandle <> nil) and (FContext <> nil) and (FContext.Lib <> nil) and FContext.Lib.Loaded then
		FContext.Lib.liesel_book_destroy(FHandle);
	FHandle := nil;
	inherited Destroy;
end;

procedure TLieselBook.Cancel;
begin
	FCancelled := True;
end;

procedure TLieselBook.ImportSettingsFromCommandString(const OptionsStrUtf8: string);
var
	s: UTF8String;
begin
	s := UTF8String(OptionsStrUtf8);
	RaiseOnStatus(FContext.Lib.liesel_book_import_settings_from_commandstring(FHandle, PChar(s)), 'import_settings_from_commandstring');
end;

function TLieselBook.ExportSettingsAsCommandString: string;
var
	p: PChar;
	st: TLieselStatus;
begin
	p := nil;
	st := FContext.Lib.liesel_book_export_settings_as_commandstring(FHandle, @p);
	RaiseOnStatus(st, 'export_settings_as_commandstring');
	Result := '';
	if p <> nil then
	begin
		Result := StrPas(p);
		FContext.Lib.liesel_free(p);
	end;
end;

function TLieselBook.GetVerbose: Boolean;
var
	enabled: cint;
begin
	enabled := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_verbose(FHandle, @enabled), 'get_verbose');
	Result := enabled <> 0;
end;

function TLieselBook.GetGreyscale: Boolean;
var
	enabled: cint;
begin
	enabled := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_greyscale(FHandle, @enabled), 'get_greyscale');
	Result := enabled <> 0;
end;

function TLieselBook.GetDivide: Boolean;
var
	enabled: cint;
begin
	enabled := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_divide(FHandle, @enabled), 'get_divide');
	Result := enabled <> 0;
end;

function TLieselBook.GetBooklet: Boolean;
var
	enabled: cint;
begin
	enabled := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_booklet(FHandle, @enabled), 'get_booklet');
	Result := enabled <> 0;
end;

function TLieselBook.GetLandscape: Boolean;
var
	enabled: cint;
begin
	enabled := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_landscape(FHandle, @enabled), 'get_landscape');
	Result := enabled <> 0;
end;

function TLieselBook.GetDpiDensity: Cardinal;
var
	dpi: cuint32;
begin
	dpi := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_dpi_density(FHandle, @dpi), 'get_dpi_density');
	Result := Cardinal(dpi);
end;

function TLieselBook.TryGetThresholdLevel(out Level0To100: Byte): Boolean;
var
	isSet: cint;
	level: cuint8;
begin
	Level0To100 := 0;
	isSet := 0;
	level := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_threshold_level(FHandle, @isSet, @level), 'get_threshold_level');
	Result := isSet <> 0;
	if Result then Level0To100 := Byte(level);
end;

function TLieselBook.TryGetSegmentSize(out PagesPerSegment: Cardinal): Boolean;
var
	isSet: cint;
	seg: cuint32;
begin
	PagesPerSegment := 0;
	isSet := 0;
	seg := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_segment_size(FHandle, @isSet, @seg), 'get_segment_size');
	Result := isSet <> 0;
	if Result then PagesPerSegment := Cardinal(seg);
end;

function TLieselBook.GetWidenMarginsAmount: Cardinal;
var
	amt: cuint32;
begin
	amt := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_widen_margins_amount(FHandle, @amt), 'get_widen_margins_amount');
	Result := Cardinal(amt);
end;

function TLieselBook.TryGetRescaleSize(out SizeUtf8: string): Boolean;
var
	isSet: cint;
	p: Pointer;
begin
	SizeUtf8 := '';
	isSet := 0;
	p := nil;
	RaiseOnStatus(FContext.Lib.liesel_book_get_rescale_size(FHandle, @isSet, @p), 'get_rescale_size');
	Result := isSet <> 0;
	if (p <> nil) then
	begin
		SizeUtf8 := StrPas(PChar(p));
		FContext.Lib.liesel_free(p);
	end;
end;

function TLieselBook.TryGetPageRanges(out RangesUtf8: string): Boolean;
var
	isSet: cint;
	p: Pointer;
begin
	RangesUtf8 := '';
	isSet := 0;
	p := nil;
	RaiseOnStatus(FContext.Lib.liesel_book_get_page_ranges(FHandle, @isSet, @p), 'get_page_ranges');
	Result := isSet <> 0;
	if (p <> nil) then
	begin
		RangesUtf8 := StrPas(PChar(p));
		FContext.Lib.liesel_free(p);
	end;
end;

function TLieselBook.TryGetCropPercentagesLRBT(out L, R, T, B: Byte): Boolean;
var
	enabled: cint;
	lv, rv, tv, bv: cuint8;
begin
	L := 0;
	R := 0;
	T := 0;
	B := 0;
	enabled := 0;
	lv := 0;
	rv := 0;
	tv := 0;
	bv := 0;
	RaiseOnStatus(
		FContext.Lib.liesel_book_get_crop_percentages_lrbt(FHandle, @enabled, @lv, @rv, @tv, @bv),
		'get_crop_percentages_lrbt'
	);
	Result := enabled <> 0;
	L := Byte(lv);
	R := Byte(rv);
	T := Byte(tv);
	B := Byte(bv);
end;

function TLieselBook.TryGetInputPdfPath(out PathUtf8: string): Boolean;
var
	isSet: cint;
	p: Pointer;
begin
	PathUtf8 := '';
	isSet := 0;
	p := nil;
	RaiseOnStatus(FContext.Lib.liesel_book_get_input_pdf_path(FHandle, @isSet, @p), 'get_input_pdf_path');
	Result := isSet <> 0;
	if p <> nil then
	begin
		PathUtf8 := StrPas(PChar(p));
		FContext.Lib.liesel_free(p);
	end;
end;

function TLieselBook.TryGetOutputPdfPath(out PathUtf8: string): Boolean;
var
	isSet: cint;
	p: Pointer;
begin
	PathUtf8 := '';
	isSet := 0;
	p := nil;
	RaiseOnStatus(FContext.Lib.liesel_book_get_output_pdf_path(FHandle, @isSet, @p), 'get_output_pdf_path');
	Result := isSet <> 0;
	if p <> nil then
	begin
		PathUtf8 := StrPas(PChar(p));
		FContext.Lib.liesel_free(p);
	end;
end;

function TLieselBook.LastErrorUtf8: string;
var
	p: PChar;
begin
	Result := '';
	if (FContext = nil) or (FContext.Lib = nil) or not FContext.Lib.Loaded or (FHandle = nil) then Exit;
	p := FContext.Lib.liesel_book_last_error(FHandle);
	if p <> nil then Result := StrPas(p);
end;

procedure TLieselBook.RaiseOnStatus(AStatus: TLieselStatus; const Context: string);
var
	msg: string;
begin
	if AStatus = LIESEL_OK then Exit;
	msg := Format('%s failed (%s): %s', [Context, StatusToString(AStatus), LastErrorUtf8]);
	raise ELieselError.CreateStatus(AStatus, msg);
end;

procedure TLieselBook.SetInputPdfPath(const PathUtf8: string);
var
	s: UTF8String;
begin
	s := UTF8String(PathUtf8);
	RaiseOnStatus(FContext.Lib.liesel_book_set_input_pdf_path(FHandle, PChar(s)), 'set_input_pdf_path');
end;

procedure TLieselBook.SetOutputPdfPath(const PathUtf8: string);
var
	s: UTF8String;
begin
	s := UTF8String(PathUtf8);
	RaiseOnStatus(FContext.Lib.liesel_book_set_output_pdf_path(FHandle, PChar(s)), 'set_output_pdf_path');
end;

procedure TLieselBook.SetVerbose(Enabled: Boolean);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_verbose(FHandle, Ord(Enabled)), 'set_verbose');
end;

procedure TLieselBook.SetGreyscale(Enabled: Boolean);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_greyscale(FHandle, Ord(Enabled)), 'set_greyscale');
end;

procedure TLieselBook.SetDivide(Enabled: Boolean);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_divide(FHandle, Ord(Enabled)), 'set_divide');
end;

procedure TLieselBook.SetBooklet(Enabled: Boolean);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_booklet(FHandle, Ord(Enabled)), 'set_booklet');
end;

procedure TLieselBook.SetLandscape(Enabled: Boolean);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_landscape(FHandle, Ord(Enabled)), 'set_landscape');
end;

procedure TLieselBook.SetDpiDensity(Dpi: Cardinal);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_dpi_density(FHandle, Dpi), 'set_dpi_density');
end;

procedure TLieselBook.SetThresholdLevel(Level0To100: Byte);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_threshold_level(FHandle, Level0To100), 'set_threshold_level');
end;

procedure TLieselBook.ClearThresholdLevel;
begin
	RaiseOnStatus(FContext.Lib.liesel_book_clear_threshold_level(FHandle), 'clear_threshold_level');
end;

procedure TLieselBook.SetSegmentSize(PagesPerSegment: Cardinal);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_segment_size(FHandle, PagesPerSegment), 'set_segment_size');
end;

procedure TLieselBook.ClearSegmentSize;
begin
	RaiseOnStatus(FContext.Lib.liesel_book_clear_segment_size(FHandle), 'clear_segment_size');
end;

procedure TLieselBook.SetWidenMarginsAmount(Amount: Cardinal);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_widen_margins_amount(FHandle, Amount), 'set_widen_margins_amount');
end;

procedure TLieselBook.SetRescaleSize(const SizeUtf8: string);
var
	s: UTF8String;
begin
	s := UTF8String(SizeUtf8);
	RaiseOnStatus(FContext.Lib.liesel_book_set_rescale_size(FHandle, PChar(s)), 'set_rescale_size');
end;

procedure TLieselBook.SetPreviewing(Enabled: Boolean);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_previewing(FHandle, Ord(Enabled)), 'set_previewing');
end;

procedure TLieselBook.SetPreviewPage(PageIndex0Based: Cardinal);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_preview_page(FHandle, PageIndex0Based), 'set_preview_page');
end;

function TLieselBook.GetPreviewJpegBytes: TBytes;
var
	p: Pointer;
	len: csize_t;
	st: TLieselStatus;
begin
	SetLength(Result, 0);
	p := nil;
	len := 0;

	st := FContext.Lib.liesel_book_get_preview_jpeg(FHandle, @p, @len);
	RaiseOnStatus(st, 'get_preview_jpeg');

	if (p <> nil) and (len > 0) then
	begin
		SetLength(Result, len);
		Move(p^, Result[0], len);
		FContext.Lib.liesel_free(p);
	end
	else if (p <> nil) then
	begin
		// Defensive: free even if len==0
		FContext.Lib.liesel_free(p);
	end;
end;

procedure TLieselBook.ClearRescaleSize;
begin
	RaiseOnStatus(FContext.Lib.liesel_book_clear_rescale_size(FHandle), 'clear_rescale_size');
end;

procedure TLieselBook.SetPageRanges(const RangesUtf8: string);
var
	s: UTF8String;
begin
	s := UTF8String(RangesUtf8);
	RaiseOnStatus(FContext.Lib.liesel_book_set_page_ranges(FHandle, PChar(s)), 'set_page_ranges');
end;

procedure TLieselBook.ClearPageRanges;
begin
	RaiseOnStatus(FContext.Lib.liesel_book_clear_page_ranges(FHandle), 'clear_page_ranges');
end;

procedure TLieselBook.SetCropPercentages(const CropUtf8: string);
var
	s: UTF8String;
begin
	s := UTF8String(CropUtf8);
	RaiseOnStatus(FContext.Lib.liesel_book_set_crop_percentages(FHandle, PChar(s)), 'set_crop_percentages');
end;

procedure TLieselBook.SetCropPercentagesLRBT(L, R, T, B: Byte);
begin
	RaiseOnStatus(FContext.Lib.liesel_book_set_crop_percentages_lrbt(FHandle, L, R, T, B), 'set_crop_percentages_lrbt');
end;

procedure TLieselBook.LoadPdf;
begin
	RaiseOnStatus(FContext.Lib.liesel_book_load_pdf(FHandle), 'load_pdf');
end;

function TLieselBook.GetPdfPageCount: Cardinal;
var
	count: cuint32;
begin
	count := 0;
	RaiseOnStatus(FContext.Lib.liesel_book_get_pdf_page_count(FHandle, @count), 'get_pdf_page_count');
	Result := Cardinal(count);
end;

procedure TLieselBook.Print;
var
	st: TLieselStatus;
begin
	FCancelled := False;
	st := FContext.Lib.liesel_book_print(FHandle, @LieselProgressThunk, Pointer(Self), @LieselCancelThunk, Pointer(Self));
	RaiseOnStatus(st, 'print');
end;

end.
