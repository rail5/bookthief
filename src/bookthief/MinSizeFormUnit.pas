unit MinSizeFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, LMessages, LCLIntf;

type
  { TMinSizeForm
	Base class for forms that enforce a minimum size derived from currently-visible controls.

	Descendants must implement GetRequiredClientSize(...) which returns the minimum *client*
	size needed. This unit then converts that to a minimum *window* size via non-client
	chrome measurements (when available). }
  TMinSizeForm = class(TForm)
  private
	FEnsuringMinSize: Boolean;
	FPendingSnapPasses: Integer;
	procedure WMDeferredSnap(var Msg: TLMessage); message LM_USER + 1;
  protected
	procedure GetRequiredClientSize(out ReqClientW, ReqClientH: Integer); virtual; abstract;
	procedure UpdateMinConstraints;
	procedure AfterEnsureSizePass; virtual; // for descendants that need an align pass after resizing
  public
	procedure EnsureSizeAtLeastMinimum;

	// Snap window size exactly to current minimum. Multiple passes help converge when
	// aligned controls change size after resizing.
	procedure SnapToMinimumSize(APasses: Integer = 1);

	// Helper for OnConstrainedResize handlers.
	// NOTE: Do NOT recompute constraints here; only apply the last computed values.
	procedure ApplyMinConstraintsToResize(var MinWidth, MinHeight: TConstraintSize);
  end;

implementation

procedure TMinSizeForm.UpdateMinConstraints;
var
  reqClientW, reqClientH: Integer;
  nonClientW, nonClientH: Integer;
begin
  GetRequiredClientSize(reqClientW, reqClientH);

  // Convert “required client size” -> “required form size”.
  // When the handle isn't allocated yet, ClientWidth/Height may be 0; avoid bogus math.
  nonClientW := 0;
  nonClientH := 0;
  if HandleAllocated then
  begin
	if ClientWidth > 0 then nonClientW := Width - ClientWidth;
	if ClientHeight > 0 then nonClientH := Height - ClientHeight;
  end;

  Constraints.MinWidth := reqClientW + nonClientW;
  Constraints.MinHeight := reqClientH + nonClientH;
end;

procedure TMinSizeForm.AfterEnsureSizePass;
begin
  // default: no-op
end;

procedure TMinSizeForm.EnsureSizeAtLeastMinimum;
begin
  if FEnsuringMinSize then Exit;
  FEnsuringMinSize := True;
  try
	UpdateMinConstraints;

	if Width < Constraints.MinWidth then Width := Constraints.MinWidth;
	if Height < Constraints.MinHeight then Height := Constraints.MinHeight;
  finally
	FEnsuringMinSize := False;
  end;
end;

procedure TMinSizeForm.SnapToMinimumSize(APasses: Integer);
var
  i: Integer;
  targetW, targetH: Integer;
begin
  if APasses < 1 then APasses := 1;

  if FEnsuringMinSize then Exit;
  FEnsuringMinSize := True;
  try
	// First pass: clear constraints and resize immediately.
	Constraints.MinWidth := 0;
	Constraints.MinHeight := 0;

	UpdateMinConstraints;
	targetW := Constraints.MinWidth;
	targetH := Constraints.MinHeight;

	// Temporarily drop constraints so SetBounds can actually shrink the window.
	Constraints.MinWidth := 0;
	Constraints.MinHeight := 0;

	SetBounds(Left, Top, targetW, targetH);

	// Reapply the correct minimum constraints after resizing.
	Constraints.MinWidth := targetW;
	Constraints.MinHeight := targetH;

	AfterEnsureSizePass;

	// Post a deferred message for remaining convergence passes.
	// This lets the WM process the size change before we try again.
	if APasses > 1 then
	begin
	  FPendingSnapPasses := APasses - 1;
	  PostMessage(Handle, LM_USER + 1, 0, 0);
	end;
  finally
	FEnsuringMinSize := False;
  end;
end;

procedure TMinSizeForm.WMDeferredSnap(var Msg: TLMessage);
var
  targetW, targetH: Integer;
begin
  if FPendingSnapPasses <= 0 then Exit;

  FEnsuringMinSize := True;
  try
	Constraints.MinWidth := 0;
	Constraints.MinHeight := 0;

	UpdateMinConstraints;
	targetW := Constraints.MinWidth;
	targetH := Constraints.MinHeight;

	if (Width <> targetW) or (Height <> targetH) then
	begin
	  // Temporarily drop constraints so SetBounds can actually shrink the window.
	  Constraints.MinWidth := 0;
	  Constraints.MinHeight := 0;

	  SetBounds(Left, Top, targetW, targetH);

	  // Reapply the correct minimum constraints after resizing.
	  Constraints.MinWidth := targetW;
	  Constraints.MinHeight := targetH;

	  AfterEnsureSizePass;
	end;

	Dec(FPendingSnapPasses);
	if FPendingSnapPasses > 0 then
	  PostMessage(Handle, LM_USER + 1, 0, 0);
  finally
	FEnsuringMinSize := False;
  end;
end;

procedure TMinSizeForm.ApplyMinConstraintsToResize(var MinWidth, MinHeight: TConstraintSize);
begin
  MinWidth := Constraints.MinWidth;
  MinHeight := Constraints.MinHeight;
end;

end.
