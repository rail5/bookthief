unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, Spin, LazFileUtils, LCLType, LCLIntf, Unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
    procedure Edit3KeyPress(Sender: TObject; var Key: char);
    procedure Form1Activate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    function GetTransform() : string;
    function GetFlip() : boolean;
  private
    Form1Activated: Boolean;
  public
    currentcomd : string;
    openfile: string;

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

function TForm1.GetFlip : boolean;
begin
  GetFlip := (ComboBox2.Text = 'long-edge flip');
end;

function GenerateCommand(grayscale, rangeprinting, segprinting, rescaling, longedgeflip: boolean; range: string; segsize, quality: integer; infile, outfile, rescaler: string): string;
var
  comd: string;
begin
  comd := 'liesel -b -i "' + infile + '"';
  if grayscale then
    begin
      comd := comd + ' -g';
    end;
  if rangeprinting then
    begin
      comd := comd + ' -r ' + range;
    end;
  if segprinting then
    begin
      comd := comd + ' -s ' + segsize.ToString();
    end;
  if rescaling then
    begin
      comd := comd + ' -t ' + rescaler;
    end;
  if longedgeflip then
    begin
      comd := comd + ' -l';
    end;
  comd := comd + ' -d ' + quality.ToString() + ' -o "' + outfile + '"' + ' -f';

  GenerateCommand := comd;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      if fileExists(OpenDialog1.Filename) then
        Button1.Caption:=ExtractFileNameOnly(OpenDialog1.Filename) + '.pdf';
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if fileExists(OpenDialog1.Filename) then
    begin
      if SaveDialog1.Execute then
        begin
          currentcomd := GenerateCommand(CheckBox1.Checked, CheckBox2.Checked, CheckBox3.Checked, CheckBox4.Checked, GetFlip(), Edit1.Text, SpinEdit1.Value, TrackBar1.Position, OpenDialog1.Filename, SaveDialog1.Filename, GetTransform());
          Form2.ShowModal();

        end;
    end
  else
    ShowMessage('Error: Please select an input PDF');
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
      if SaveDialog1.Execute then
        begin
          saneinfile := SanitizeFilename(OpenDialog1.Filename);
          saneoutfile := SanitizeFilename(SaveDialog1.Filename); // Only necessary for 'Export Command,' as the TProc call to Liesel doesn't run in an ordinary shell
          exportedcomd := GenerateCommand(CheckBox1.Checked, CheckBox2.Checked, CheckBox3.Checked, CheckBox4.Checked, GetFlip(), Edit1.Text, SpinEdit1.Value, TrackBar1.Position, saneinfile, saneoutfile, GetTransform());
          ShowMessage('Your exported command is: ' + LineEnding + LineEnding + exportedcomd);
        end;

    end
  else
    begin
      ShowMessage('Error: Please select an input PDF');
    end;

end;

procedure TForm1.MenuItem4Click(Sender: TObject);
var
  ReturnInfo: ansistring;
begin
  if RunCommand('liesel', ['-q'], ReturnInfo) then
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
begin
  Timer1.Enabled := false;
  Update();

  if openfile.EndsWith('.pdf', true) then
    begin
      OpenDialog1.Filename := openfile;
      if fileExists(OpenDialog1.Filename) then
        begin
          Button1.Caption := ExtractFileNameOnly(OpenDialog1.Filename) + '.pdf';
        end;
    end;
end;

procedure TForm1.Form1Activate(Sender: TObject);
begin
  if not Form1Activated then begin
    Form1Activated := true;
    Timer1.Enabled := true;
  end;
end;

end.


