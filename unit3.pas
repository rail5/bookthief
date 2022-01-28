unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Process, Types;

type

  { TForm3 }

  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Timer1: TTimer;
    Timer2: TTimer;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    TrackBar6: TTrackBar;
    TrackBar7: TTrackBar;
    UpDown1: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    function ExportLiesel(Options : TStringArray) : boolean;
    procedure Timer2Timer(Sender: TObject);
    procedure TrackBar2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBar2MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    FormActivated : boolean;

  public

  end;

var
  Form3: TForm3;

implementation

uses Unit1;

{$R *.lfm}

{ TForm3 }

function TForm3.ExportLiesel(Options : TStringArray) : boolean;
var
  ReturnInfo : string;
begin
  Shape1.Visible := true;
  Label4.Visible := true;
  Update();


  if RunCommand('liesel', Options, ReturnInfo, [], swoHide) then
  begin
    Form3.Image1.Picture.LoadFromFile(Form1.systmpdir + '/bookthief-temp-preview.jpeg');
    Shape1.Visible := false;
    Label4.Visible := false;
    UpDown1.Max := Form1.currentpagecount - 1;
    Update();
  end;
  ExportLiesel := true;
end;

procedure TForm3.Timer2Timer(Sender: TObject);
begin
  Timer2.Enabled := false;
  if Form1.Button1.Caption <> '(None)' then
    begin
      if CheckBox6.Checked then
        begin
          ExportLiesel(Form1.CreateCommand(true, true));
        end;
    end;
end;

procedure TForm3.TrackBar2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Timer2.Enabled := true;
end;

procedure TForm3.TrackBar2MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Timer2.Enabled := true;
end;

procedure TForm3.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  Label3.Caption := UpDown1.Position.ToString();
  Timer2.Enabled := true;
end;

procedure TForm3.FormActivate(Sender: TObject);
begin
  if not FormActivated then begin
    FormActivated := true;
    Timer1.Enabled := true;
  end;
end;

procedure TForm3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormActivated := false;
end;

procedure TForm3.FormHide(Sender: TObject);
begin
  FormActivated := false;
end;

procedure TForm3.Label9Click(Sender: TObject);
var
  helptext : string;
begin
  helptext := 'This option widens the center margins progressively throughout the booklet, so that the center margins are wider in the middle, and narrower at the ends' + LineEnding;
  helptext := helptext + LineEnding + 'This may be useful when printing very large booklets';
  ShowMessage(helptext);
end;

procedure TForm3.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    begin
      TrackBar2.Visible := true;
      Timer2.Enabled := true;
    end
  else
    begin
      TrackBar2.Visible := false;
      Timer2.Enabled := true;
    end;
end;

procedure TForm3.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.Checked then
    begin
      TrackBar1.Visible := true;
      TrackBar3.Visible := true;
      TrackBar4.Visible := true;
      TrackBar5.Visible := true;
      Label5.Visible := true;
      Label6.Visible := true;
      Label7.Visible := true;
      Label8.Visible := true;
    end
  else
    begin
      TrackBar1.Visible := false;
      TrackBar3.Visible := false;
      TrackBar4.Visible := false;
      TrackBar5.Visible := false;
      Label5.Visible := false;
      Label6.Visible := false;
      Label7.Visible := false;
      Label8.Visible := false;
    end;
    Timer2.Enabled := true;
end;

procedure TForm3.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.Checked then
    begin
      TrackBar6.Visible := true;
    end
  else
    begin
      TrackBar6.Visible := false;
    end;
    Timer2.Enabled := true;
end;

procedure TForm3.CheckBox4Change(Sender: TObject);
var
  changingcount: integer;
begin
  if Form1.OpenDialog1.Filename <> '' then
    begin
      if CheckBox4.Checked then
        begin
          changingcount := Form1.currentpagecount * 2;
          Button2.Caption := changingcount.ToString();
          UpDown1.Max := changingcount;
          UpDown1.Increment := 1;
        end
      else
        begin
      Button2.Caption := Form1.currentpagecount.ToString();
      UpDown1.Max := Form1.currentpagecount;
      UpDown1.Increment := 2;
        end;
    end;


  Timer2.Enabled := true;
end;

procedure TForm3.CheckBox5Change(Sender: TObject);
begin
  if Checkbox5.Checked then
    begin
      TrackBar7.Visible := true;
      Label10.Visible := true;
      Label11.Visible := true;
    end
  else
    begin
      TrackBar7.Visible := false;
      Label10.Visible := false;
      Label11.Visible := false;
    end;
end;

procedure TForm3.CheckBox6Change(Sender: TObject);
begin
  if CheckBox6.Checked then
    begin
      Shape2.Visible := false;
      Label1.Visible := false;
      Update();
      Timer2.Enabled := true;
    end
  else
    begin
      Shape2.Visible := true;
      Label1.Visible := true;
      Update();
    end;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  UpDown1.Position := 1;
  Label3.Caption := UpDown1.Position.ToString();
  Timer2.Enabled := true;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  UpDown1.Position := Form1.currentpagecount - 1;
  Label3.Caption := UpDown1.Position.ToString();
  Timer2.Enabled := true;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  CheckBox6.Checked := true;
  Update();
  if Form1.Button1.Caption <> '(None)' then
    begin
      ExportLiesel(Form1.CreateCommand(true, true));
    end;
end;

end.

