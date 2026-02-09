unit LieselABIUnit;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, dynlibs, ctypes, VersionInfoUnit;

type
	PLieselHandle = Pointer;
	PLieselBookHandle = Pointer;

	TLieselStatus = cint;

const
	LIESEL_OK = 0;
	LIESEL_E_INVALID_ARG = 1;
	LIESEL_E_PARSE = 2;
	LIESEL_E_IO = 3;
	LIESEL_E_RUNTIME = 4;
	LIESEL_E_CANCELLED = 5;

type
	TLieselProgressEvent = cint;

const
	LIESEL_PROGRESS_INFO = 0;
	LIESEL_PROGRESS_RENDER_PAGE = 1;
	LIESEL_PROGRESS_SEGMENT_DONE = 2;
	LIESEL_PROGRESS_PRINT_DONE = 3;

type
	TLieselProgressCallback = procedure(
		userdata: Pointer;
		event: TLieselProgressEvent;
		segment_index: cuint32;
		page_index: cuint32;
		percent: cuint32;
		message_utf8: PChar
	); cdecl;

	TLieselCancelCallback = function(userdata: Pointer): cint; cdecl;

	TLieselLib = class
	private
		FLib: TLibHandle;
		function GetLoaded: Boolean;
		function TryGetProc(const ProcName: AnsiString): Pointer;
		procedure RequireProc(ProcPtr: Pointer; const ProcName: string);
	public
		// Function pointers (cdecl)
		liesel_create: function: PLieselHandle; cdecl;
		liesel_destroy: procedure(h: PLieselHandle); cdecl;
		liesel_version: function: PChar; cdecl;
		liesel_major_version: function: cint; cdecl;
		liesel_minor_version: function: cint; cdecl;
		liesel_patch_version: function: cint; cdecl;
		liesel_last_error: function(h: PLieselHandle): PChar; cdecl;
		liesel_free: procedure(p: Pointer); cdecl;

		liesel_book_create: function(h: PLieselHandle): PLieselBookHandle; cdecl;
		liesel_book_destroy: procedure(b: PLieselBookHandle); cdecl;
		liesel_book_last_error: function(b: PLieselBookHandle): PChar; cdecl;

		liesel_book_set_input_pdf_path: function(b: PLieselBookHandle; path_utf8: PChar): TLieselStatus; cdecl;
		liesel_book_set_output_pdf_path: function(b: PLieselBookHandle; path_utf8: PChar): TLieselStatus; cdecl;

		liesel_book_set_verbose: function(b: PLieselBookHandle; enabled: cint): TLieselStatus; cdecl;
		liesel_book_set_greyscale: function(b: PLieselBookHandle; enabled: cint): TLieselStatus; cdecl;
		liesel_book_set_divide: function(b: PLieselBookHandle; enabled: cint): TLieselStatus; cdecl;
		liesel_book_set_booklet: function(b: PLieselBookHandle; enabled: cint): TLieselStatus; cdecl;
		liesel_book_set_landscape: function(b: PLieselBookHandle; enabled: cint): TLieselStatus; cdecl;

		liesel_book_set_dpi_density: function(b: PLieselBookHandle; dpi: cuint32): TLieselStatus; cdecl;
		liesel_book_set_threshold_level: function(b: PLieselBookHandle; level_0_100: cuint8): TLieselStatus; cdecl;
		liesel_book_clear_threshold_level: function(b: PLieselBookHandle): TLieselStatus; cdecl;

		liesel_book_set_segment_size: function(b: PLieselBookHandle; pages_per_segment: cuint32): TLieselStatus; cdecl;
		liesel_book_clear_segment_size: function(b: PLieselBookHandle): TLieselStatus; cdecl;
		liesel_book_set_widen_margins_amount: function(b: PLieselBookHandle; amount: cuint32): TLieselStatus; cdecl;

		liesel_book_set_rescale_size: function(b: PLieselBookHandle; size_utf8: PChar): TLieselStatus; cdecl;
		liesel_book_clear_rescale_size: function(b: PLieselBookHandle): TLieselStatus; cdecl;

		liesel_book_set_page_ranges: function(b: PLieselBookHandle; range_utf8: PChar): TLieselStatus; cdecl;
		liesel_book_clear_page_ranges: function(b: PLieselBookHandle): TLieselStatus; cdecl;

		liesel_book_set_crop_percentages: function(b: PLieselBookHandle; crop_utf8: PChar): TLieselStatus; cdecl;
		liesel_book_set_crop_percentages_lrbt: function(b: PLieselBookHandle; l, r, t, bt: cuint8): TLieselStatus; cdecl;

		liesel_book_load_pdf: function(b: PLieselBookHandle): TLieselStatus; cdecl;
		liesel_book_get_pdf_page_count: function(b: PLieselBookHandle; out_page_count: pcuint32): TLieselStatus; cdecl;

		// Preview (GUI support)
		liesel_book_set_previewing: function(b: PLieselBookHandle; enabled: cint): TLieselStatus; cdecl;
		liesel_book_set_preview_page: function(b: PLieselBookHandle; page_index: cuint32): TLieselStatus; cdecl;
		liesel_book_get_preview_jpeg: function(b: PLieselBookHandle; out_bytes: PPointer; out_len: Pcsize_t): TLieselStatus; cdecl;

		liesel_book_print: function(
			b: PLieselBookHandle;
			progress_cb: TLieselProgressCallback;
			progress_userdata: Pointer;
			cancel_cb: TLieselCancelCallback;
			cancel_userdata: Pointer
		): TLieselStatus; cdecl;

		constructor Create;
		destructor Destroy; override;

		procedure Load(const ExplicitPath: string = '');
		procedure Unload;

		property Loaded: Boolean read GetLoaded;
		property LibHandle: TLibHandle read FLib;
	end;

function DefaultLieselSoPath: string;

implementation

function DefaultLieselSoPath: string;
var
	exeDir: string;
	candidate: string;
begin
	// Prefer the library in the same directory as the running executable.
	exeDir := ExtractFilePath(ParamStr(0));
	candidate := IncludeTrailingPathDelimiter(exeDir) + 'libliesel.so';
	Result := candidate;
end;

{ TLieselLib }

constructor TLieselLib.Create;
begin
	inherited Create;
	FLib := 0;
end;

destructor TLieselLib.Destroy;
begin
	Unload;
	inherited Destroy;
end;

function TLieselLib.GetLoaded: Boolean;
begin
	Result := FLib <> 0;
end;

function TLieselLib.TryGetProc(const ProcName: AnsiString): Pointer;
begin
	if FLib = 0 then Exit(nil);
	Result := GetProcedureAddress(FLib, ProcName);
end;

procedure TLieselLib.RequireProc(ProcPtr: Pointer; const ProcName: string);
begin
	if ProcPtr = nil then
		raise Exception.CreateFmt('libliesel ABI missing symbol: %s', [ProcName]);
end;

procedure TLieselLib.Load(const ExplicitPath: string);
var
	path: string;
	fromEnv: string;
	tryName: string;

	procedure BindAll;
	begin
		Pointer(liesel_version) := TryGetProc('liesel_version');
		Pointer(liesel_major_version) := TryGetProc('liesel_major_version');
		Pointer(liesel_minor_version) := TryGetProc('liesel_minor_version');
		Pointer(liesel_patch_version) := TryGetProc('liesel_patch_version');

		// Verify that the major version matches
		if liesel_major_version() <> VersionInfoUnit.MAJOR_VERSION then
		begin
			raise Exception.CreateFmt(
				'libliesel major version mismatch: expected %d, got %d',
				[VersionInfoUnit.MAJOR_VERSION, liesel_major_version()]
			);
		end;

		Pointer(liesel_create) := TryGetProc('liesel_create');
		Pointer(liesel_destroy) := TryGetProc('liesel_destroy');
		Pointer(liesel_last_error) := TryGetProc('liesel_last_error');
		Pointer(liesel_free) := TryGetProc('liesel_free');

		Pointer(liesel_book_create) := TryGetProc('liesel_book_create');
		Pointer(liesel_book_destroy) := TryGetProc('liesel_book_destroy');
		Pointer(liesel_book_last_error) := TryGetProc('liesel_book_last_error');

		Pointer(liesel_book_set_input_pdf_path) := TryGetProc('liesel_book_set_input_pdf_path');
		Pointer(liesel_book_set_output_pdf_path) := TryGetProc('liesel_book_set_output_pdf_path');

		Pointer(liesel_book_set_verbose) := TryGetProc('liesel_book_set_verbose');
		Pointer(liesel_book_set_greyscale) := TryGetProc('liesel_book_set_greyscale');
		Pointer(liesel_book_set_divide) := TryGetProc('liesel_book_set_divide');
		Pointer(liesel_book_set_booklet) := TryGetProc('liesel_book_set_booklet');
		Pointer(liesel_book_set_landscape) := TryGetProc('liesel_book_set_landscape');

		Pointer(liesel_book_set_dpi_density) := TryGetProc('liesel_book_set_dpi_density');
		Pointer(liesel_book_set_threshold_level) := TryGetProc('liesel_book_set_threshold_level');
		Pointer(liesel_book_clear_threshold_level) := TryGetProc('liesel_book_clear_threshold_level');

		Pointer(liesel_book_set_segment_size) := TryGetProc('liesel_book_set_segment_size');
		Pointer(liesel_book_clear_segment_size) := TryGetProc('liesel_book_clear_segment_size');
		Pointer(liesel_book_set_widen_margins_amount) := TryGetProc('liesel_book_set_widen_margins_amount');

		Pointer(liesel_book_set_rescale_size) := TryGetProc('liesel_book_set_rescale_size');
		Pointer(liesel_book_clear_rescale_size) := TryGetProc('liesel_book_clear_rescale_size');

		Pointer(liesel_book_set_page_ranges) := TryGetProc('liesel_book_set_page_ranges');
		Pointer(liesel_book_clear_page_ranges) := TryGetProc('liesel_book_clear_page_ranges');

		Pointer(liesel_book_set_crop_percentages) := TryGetProc('liesel_book_set_crop_percentages');
		Pointer(liesel_book_set_crop_percentages_lrbt) := TryGetProc('liesel_book_set_crop_percentages_lrbt');

		Pointer(liesel_book_load_pdf) := TryGetProc('liesel_book_load_pdf');
		Pointer(liesel_book_get_pdf_page_count) := TryGetProc('liesel_book_get_pdf_page_count');
		Pointer(liesel_book_print) := TryGetProc('liesel_book_print');

		Pointer(liesel_book_set_previewing) := TryGetProc('liesel_book_set_previewing');
		Pointer(liesel_book_set_preview_page) := TryGetProc('liesel_book_set_preview_page');
		Pointer(liesel_book_get_preview_jpeg) := TryGetProc('liesel_book_get_preview_jpeg');

		RequireProc(Pointer(liesel_create), 'liesel_create');
		RequireProc(Pointer(liesel_destroy), 'liesel_destroy');
		RequireProc(Pointer(liesel_version), 'liesel_version');
		RequireProc(Pointer(liesel_last_error), 'liesel_last_error');
		RequireProc(Pointer(liesel_free), 'liesel_free');

		RequireProc(Pointer(liesel_book_create), 'liesel_book_create');
		RequireProc(Pointer(liesel_book_destroy), 'liesel_book_destroy');
		RequireProc(Pointer(liesel_book_last_error), 'liesel_book_last_error');

		RequireProc(Pointer(liesel_book_set_input_pdf_path), 'liesel_book_set_input_pdf_path');
		RequireProc(Pointer(liesel_book_set_output_pdf_path), 'liesel_book_set_output_pdf_path');

		RequireProc(Pointer(liesel_book_set_verbose), 'liesel_book_set_verbose');
		RequireProc(Pointer(liesel_book_set_greyscale), 'liesel_book_set_greyscale');
		RequireProc(Pointer(liesel_book_set_divide), 'liesel_book_set_divide');
		RequireProc(Pointer(liesel_book_set_booklet), 'liesel_book_set_booklet');
		RequireProc(Pointer(liesel_book_set_landscape), 'liesel_book_set_landscape');

		RequireProc(Pointer(liesel_book_set_dpi_density), 'liesel_book_set_dpi_density');
		RequireProc(Pointer(liesel_book_set_threshold_level), 'liesel_book_set_threshold_level');
		RequireProc(Pointer(liesel_book_clear_threshold_level), 'liesel_book_clear_threshold_level');

		RequireProc(Pointer(liesel_book_set_segment_size), 'liesel_book_set_segment_size');
		RequireProc(Pointer(liesel_book_clear_segment_size), 'liesel_book_clear_segment_size');
		RequireProc(Pointer(liesel_book_set_widen_margins_amount), 'liesel_book_set_widen_margins_amount');

		RequireProc(Pointer(liesel_book_set_rescale_size), 'liesel_book_set_rescale_size');
		RequireProc(Pointer(liesel_book_clear_rescale_size), 'liesel_book_clear_rescale_size');

		RequireProc(Pointer(liesel_book_set_page_ranges), 'liesel_book_set_page_ranges');
		RequireProc(Pointer(liesel_book_clear_page_ranges), 'liesel_book_clear_page_ranges');

		RequireProc(Pointer(liesel_book_set_crop_percentages), 'liesel_book_set_crop_percentages');
		RequireProc(Pointer(liesel_book_set_crop_percentages_lrbt), 'liesel_book_set_crop_percentages_lrbt');

		RequireProc(Pointer(liesel_book_load_pdf), 'liesel_book_load_pdf');
			RequireProc(Pointer(liesel_book_get_pdf_page_count), 'liesel_book_get_pdf_page_count');
		RequireProc(Pointer(liesel_book_print), 'liesel_book_print');

		RequireProc(Pointer(liesel_book_set_previewing), 'liesel_book_set_previewing');
		RequireProc(Pointer(liesel_book_set_preview_page), 'liesel_book_set_preview_page');
		RequireProc(Pointer(liesel_book_get_preview_jpeg), 'liesel_book_get_preview_jpeg');
	end;

begin
	if Loaded then Exit;

	path := ExplicitPath;
	if path <> '' then
	begin
		FLib := SafeLoadLibrary(path);
		if FLib = 0 then raise Exception.CreateFmt('Failed to load %s', [path]);
		BindAll;
		Exit;
	end;

	fromEnv := GetEnvironmentVariable('LIESEL_LIB');
	if fromEnv <> '' then
	begin
		FLib := SafeLoadLibrary(fromEnv);
		if FLib <> 0 then
		begin
			BindAll;
			Exit;
		end;
	end;

	// Try same directory as executable
	tryName := DefaultLieselSoPath;
	FLib := SafeLoadLibrary(tryName);
	if FLib <> 0 then
	begin
		BindAll;
		Exit;
	end;

	// Fall back to the dynamic loader search path
	FLib := SafeLoadLibrary('libliesel.so');
	if FLib <> 0 then
	begin
		BindAll;
		Exit;
	end;

	raise Exception.Create('Failed to load libliesel.so. Set LIESEL_LIB or place libliesel.so next to the executable.');
end;

procedure TLieselLib.Unload;
begin
	if FLib <> 0 then
	begin
		UnloadLibrary(FLib);
		FLib := 0;
	end;
end;

end.
