unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, Spin, LazFileUtils, LCLType, LCLIntf,
  XMLPropStorage, Unit2, Unit3, Unit4, Types, StrUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    XMLPropStorage1: TXMLPropStorage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure Edit3KeyPress(Sender: TObject; var Key: char);
    procedure Form1Activate(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    function GetTransform() : string;
    function GetFlip() : boolean;
    function CreateCommand(preview, outrequired : boolean) : TStringArray;
    function GenerateCommand(outrequired : boolean) : string;
    procedure TrackBar1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TrackBar1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    Form1Activated: Boolean;
  public
    currentcomd : string;
    openfile: string;
    currentpagecount: LongInt;
    currentpdf: string;
    systmpdir: string;
    LieselPath: string;

  end;

var
  Form1: TForm1;



implementation

{$R *.lfm}

{ TForm1 }

function SanitizeFilename(filenm : string) : string;
var
  outresult : string;
begin
  outresult := StringReplace(filenm, '\', '\\', [rfReplaceAll]);
  outresult := StringReplace(outresult, '"', '\"', [rfReplaceAll]);
  SanitizeFilename := outresult;
end;

function TForm1.GetTransform() : string;
var
  outresult : string;
begin
  if ComboBox1.Text = 'custom' then
    begin
      outresult := Edit3.Text + 'x' + Edit4.Text;
    end
  else
    begin
      outresult := ComboBox1.Text;
    end;
    GetTransform := outresult;
end;

function TForm1.GetFlip() : boolean;
begin
  GetFlip := (ComboBox2.Text = 'long-edge flip');
end;

function TForm1.CreateCommand(preview, outrequired : boolean) : TStringArray;
var
  comd : string;
  theresult : TStringArray;
  cropstring : string;
begin
  comd := '-b -i "' + OpenDialog1.Filename + '"';

  SetLength(theresult, Length(theresult)+3);
  theresult[Length(theresult)-3] := '-b';
  theresult[Length(theresult)-2] := '-i';
  theresult[Length(theresult)-1] := '' + currentpdf + '';

  if CheckBox1.Checked then
    begin
      SetLength(theresult, Length(theresult)+1);
      theresult[Length(theresult)-1] := '-g';
    end;

  if (CheckBox2.Checked) and (preview = false) then
    begin
      SetLength(theresult, Length(theresult)+2);
      theresult[Length(theresult)-2] := '-r';
      theresult[Length(theresult)-1] := Edit1.Text;
    end;

  if (CheckBox3.Checked) and (preview = false) then
    begin
      SetLength(theresult, Length(theresult)+2);
      theresult[Length(theresult)-2] := '-s';
      theresult[Length(theresult)-1] := SpinEdit1.Value.ToString();
    end;

  if CheckBox4.Checked then
    begin
      SetLength(theresult, Length(theresult)+2);
      if ComboBox1.Text = 'custom' then
        begin
          theresult[Length(theresult)-2] := '-t';
          theresult[Length(theresult)-1] := Edit3.Text + 'x' + Edit4.Text;
        end
      else
        begin
          theresult[Length(theresult)-2] := '-t';
          theresult[Length(theresult)-1] := ComboBox1.Text;
        end;
    end;

  if ComboBox2.Text = 'long-edge flip' then
    begin
      SetLength(theresult, Length(theresult)+1);
      theresult[Length(theresult)-1] := '-l';
    end;

  if CheckBox5.Checked then
    begin
      if Form3.CheckBox1.Checked then
        begin
          SetLength(theresult, Length(theresult)+2);
          theresult[Length(theresult)-2] := '-k';
          theresult[Length(theresult)-1] := Form3.TrackBar2.Position.ToString();
        end;

      if Form3.CheckBox2.Checked then
        begin
          SetLength(theresult, Length(theresult)+2);
          theresult[Length(theresult)-2] := '-C';
          cropstring := Form3.TrackBar1.Position.ToString() + ',';
          cropstring := cropstring + Form3.TrackBar3.Position.ToString() + ',';
          cropstring := cropstring + Form3.TrackBar4.Position.ToString() + ',';
          cropstring := cropstring + Form3.TrackBar5.Position.ToString();
          theresult[Length(theresult)-1] := cropstring;
        end;

      if Form3.CheckBox3.Checked then
        begin
          SetLength(theresult, Length(theresult)+2);
          theresult[Length(theresult)-2] := '-w';
          theresult[Length(theresult)-1] := Form3.TrackBar6.Position.ToString();
        end;

      if Form3.CheckBox5.Checked then
        begin
          SetLength(theresult, Length(theresult)+2);
          theresult[Length(theresult)-2] := '-a';
          theresult[Length(theresult)-1] := Form3.TrackBar7.Position.ToString();
        end;

      if Form3.CheckBox4.Checked then
        begin
          SetLength(theresult, Length(theresult)+1);
          theresult[Length(theresult)-1] := '-D';
        end;

      if Form3.CheckBox7.Checked then
        begin
          SetLength(theresult, Length(theresult)+1);
          theresult[Length(theresult)-1] := '-N';
        end;

    end;

  SetLength(theresult, Length(theresult)+2);
  theresult[Length(theresult)-2] :=  '-d';
  theresult[Length(theresult)-1] := TrackBar1.Position.ToString();

  if preview then
    begin
      SetLength(theresult, Length(theresult)+2);
      theresult[Length(theresult)-2] := '-e';

      if (Form3.CheckBox4.Checked or Form3.CheckBox7.Checked) then
        begin
          theresult[Length(theresult)-1] := Form3.UpDown1.Position.Tostring() + ',' + Form3.UpDown1.Position.ToString();
        end
      else
        begin
          theresult[Length(theresult)-1] := Form3.UpDown1.Position.ToString() + ',' + (Form3.UpDown1.Position + 1).ToString();
        end;

      SetLength(theresult, Length(theresult)+2);
      theresult[Length(theresult)-2] := '-o';
      theresult[Length(theresult)-1] := systmpdir + '/bookthief-temp-preview.jpeg';
    end
  else
    begin

      if outrequired then
        begin
          SetLength(theresult, Length(theresult)+2);
          theresult[Length(theresult)-2] := '-o';
          theresult[Length(theresult)-1] := '' + SaveDialog1.Filename + '';
        end
      else
        begin
          SetLength(theresult, Length(theresult)+1);
          theresult[Length(theresult)-1] := '-O';
        end;

    end;

    SetLength(theresult, Length(theresult)+1);
    theresult[Length(theresult)-1] := '-f';

    CreateCommand := theresult;
end;

procedure TForm1.TrackBar1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if CheckBox5.Checked then
    Form3.Timer2.Enabled := true;
end;

procedure TForm1.TrackBar1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if CheckBox5.Checked then
    Form3.Timer2.Enabled := true;
end;

function TForm1.GenerateCommand(outrequired : boolean): string;
var
  comd: string;
  escaped: string;
  precomd: TStringArray;
  i: integer;
begin
  comd := 'liesel';
  escaped := '';

  precomd := Form1.CreateCommand(false, outrequired);

  for i := 0 to (Length(precomd)-1) do
  begin
    if AnsiContainsStr(precomd[i], ' ') then
      begin
        escaped := '"';
      end;
    comd := comd + ' ' + escaped + precomd[i] + escaped;
    escaped := '';
  end;

  GenerateCommand := comd;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pgstring : string;
begin
  if OpenDialog1.Execute then
    begin
      if fileExists(OpenDialog1.Filename) then
        begin
          Button1.Caption:=ExtractFileNameOnly(OpenDialog1.Filename) + '.pdf';
          currentpdf := OpenDialog1.Filename;
          if RunCommand(LieselPath, ['-p', OpenDialog1.Filename], pgstring, [], swoHide) then
            begin
              currentpagecount := StrToInt(pgstring);
              Form3.Button2.Caption := pgstring;
            end;
          if CheckBox5.Checked then
            Form3.ExportLiesel(CreateCommand(true, true));
        end;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if fileExists(OpenDialog1.Filename) then
    begin
      if SaveDialog1.Execute then
        begin
          currentcomd := GenerateCommand(true);
          Form2.ShowModal();

        end;
    end
  else
    ShowMessage('Error: Please select an input PDF');
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox5.Checked then
    Form3.Timer2.Enabled := true;
end;

procedure TForm1.CheckBox2Change(Sender: TObject);
begin
  if CheckBox2.Checked then
    begin
      Label3.Visible:=true;
      Edit1.Visible:=true;
      end
  else
    begin
      Label3.Visible:=false;
      Edit1.Visible:=false;;
    end
end;

procedure TForm1.CheckBox3Change(Sender: TObject);
begin
  if CheckBox3.Checked then
    begin
      SpinEdit1.Visible:=true;
      Label4.Visible:=true;
    end
  else
    begin
      SpinEdit1.Visible:=false;
      Label4.Visible:=false;
    end;
end;

procedure TForm1.CheckBox4Change(Sender: TObject);
begin
  if Checkbox4.Checked then
    begin
      ComboBox1.Visible:=true;
      if ComboBox1.Text = 'custom' then
        begin
          Edit3.Visible := true;
          Edit4.Visible := true;
          Label5.Visible := true;
          Label6.Visible := true;
        end;
    end
  else
    begin
      ComboBox1.Visible:=false;
      Edit3.Visible := false;
      Edit4.Visible := false;
      Label5.Visible := false;
      Label6.Visible := false;
    end;

    if CheckBox5.Checked then
      Form3.Timer2.Enabled := true;
end;

procedure TForm1.CheckBox5Change(Sender: TObject);
begin
  if CheckBox5.Checked then
    begin
      Form3.Show;
    end
  else
    begin
      Form3.Hide;
    end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if ComboBox1.Text = 'custom' then
    begin
      Edit3.Visible := true;
      Edit4.Visible := true;
      Label5.Visible := true;
      Label6.Visible := true;
    end
  else
    begin
      Edit3.Visible := false;
      Edit4.Visible := false;
      Label5.Visible := false;
      Label6.Visible := false;
    end;

  if CheckBox5.Checked then
    Form3.Timer2.Enabled := true;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', '-', ',', Char(VK_BACK)]) then Key := #0;
end;

procedure TForm1.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9', Char(VK_BACK), Char(VK_DELETE), CHAR(VK_DECIMAL), CHAR(VK_SEPARATOR)]) then Key := #0;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
var
  exportedcomd : string;
  saneinfile : string;
  saneoutfile : string;
begin
  if fileExists(OpenDialog1.Filename) then
    begin
      saneinfile := SanitizeFilename(OpenDialog1.Filename);
      exportedcomd := GenerateCommand(false);
      Form4.mode := 1;
      Form4.exportedcomd := exportedcomd;
      Form4.Timer1.Enabled := true;
      Form4.ShowModal();
    end
  else
    begin
      ShowMessage('Error: Please select an input PDF');
    end;

end;

procedure TForm1.MenuItem4Click(Sender: TObject);
var
  ReturnInfo: ansistring;
  Opts: TStringArray;
begin
  if RunCommand(LieselPath, ['-q'], ReturnInfo, [], swoHide) then
    begin
      ShowMessage(ReturnInfo);
    end
  else
    begin
      ShowMessage('Error: is Liesel installed?');
    end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  OpenURL('http://gen.lib.rus.ec');
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  OpenURL('http://1lib.domains');
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  pgstring : string;
begin
  Timer1.Enabled := false;
  Update();

  if openfile.EndsWith('.pdf', true) then
    begin
      OpenDialog1.Filename := openfile;
      if fileExists(OpenDialog1.Filename) then
        begin
          Button1.Caption := ExtractFileNameOnly(OpenDialog1.Filename) + '.pdf';
          currentpdf := openfile;
          if RunCommand(LieselPath, ['-p', openfile], pgstring, [], swoHide) then
          begin
            currentpagecount := StrToInt(pgstring);
            Form3.Button2.Caption := pgstring;
          end;
        end;
    end;
end;

procedure TForm1.Form1Activate(Sender: TObject);
var
  lieselVersion : string;
  BTVersion : string;
begin
  if not Form1Activated then begin
    Form1Activated := true;
    
    {$IFDEF DARWIN}
    LieselPath := ExtractFilePath(Application.ExeName) + '/liesel';
    {$ENDIF}
    
    {$IFNDEF DARWIN}
    LieselPath := 'liesel';
    {$ENDIF}

    if RunCommand(LieselPath, ['-V'], lieselVersion, [], swoHide) then
      begin
        MenuItem15.Caption := 'Liesel version: ' + lieselVersion;
      end
    else
      begin
        MenuItem15.Caption := 'ERROR finding Liesel';
      end;

      BTVersion := '11.2';

      MenuItem16.Caption := 'BookThief version: ' + BTVersion;

    {$IFDEF WINDOWS}
    XMLPropStorage1.FileName := GetUserDir() + '\bookthief.xml';
    XMLPropStorage1.Restore;
    {$ENDIF}
    systmpdir := GetTempDir(true);
    Timer1.Enabled := true;
  end;
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  Form4.mode := 0;
  Form4.exportedcomd := '';
  Form4.Timer1.Enabled := true;
  Form4.ShowModal;
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  Form4.LoadSettingsFromXML();
end;

procedure TForm1.MenuItem13Click(Sender: TObject);
begin
  Form4.WriteSettingsToXML(GenerateCommand(false));
end;

end.


