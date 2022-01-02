unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  StrUtils, Process, LazFileUtils;

type

  { TForm4 }

  TForm4 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    mode : integer;
    exportedcomd : string;
  end;

var
  Form4: TForm4;

implementation
  Uses Unit1, Unit3;

{$R *.lfm}

{ TForm4 }

procedure TForm4.Button1Click(Sender: TObject);
var
  comd : string;
  ReturnInfo : string;
  parameters : TStringArray;
  i : integer;
  cropvalues : TStringArray;
  transform : TStringArray;
  pgstring : string;
  previewonoff : boolean;
begin
  if mode = 1 then
    begin
      Form4.Close;
      Exit();
    end;
  comd := Edit1.Text;
  if AnsiStartsStr('liesel ', comd) then
    begin
      comd := StringReplace(comd, 'liesel ', 'liesel -B ', []);
    end
  else
    begin
      ShowMessage('Error: Invalid Liesel Command');
    end;

  if RunCommand(comd, ReturnInfo) then
    begin
      parameters := SplitString(ReturnInfo, LineEnding);
      for i := 0 to Length(parameters)-1 do
        begin
          Delete(parameters[i], 1, 3);
        end;

      // The following should be changed if liesel -B returns more params in the future
      // At the moment, the structure is:
      // 1. Automargin value
      // 2. ProgressCounter y/n (ignored)
      // 3. Crop values
      // 4. Quality
      // 5. Divide pages y/n
      // 6. Force overwrites y/n (ignored)
      // 7. Grayscale y/n
      // 8. Input file path
      // 9. Color threshold value
      // 10. Long-edge flip y/n (ignored)
      // 11. Minimum segment size (ignored)
      // 12. Output file path (ignored)
      // 13. Range value
      // 14. Segment size
      // 15. Transform/rescale value
      // 16. Widen margins value
      //
      // All options are marked '0' when disabled, apart from:
      // Crop, which is marked 0,0,0,0
      // And Transform, which is marked 0x0
      //
      // Genuinely, I really don't like it being this specific and inflexible
      // You know, having this Liesel -B option
      // It feels flimsy and hacky, like a Windows program
      // But apart from teaching Pascal to parse GNU GetOpt arguments,
      // Which would be a project worthy of its own repo altogether,
      // I'm not sure how else to do this

      previewonoff := Form3.CheckBox6.Checked; // Store current "live preview" enabled/disabled
      Form3.CheckBox6.Checked := false; // Temporarily disable live preview
      Update();

      Form3.CheckBox5.Checked := (parameters[0] <> '0');
      Form3.TrackBar7.Position := StrToInt(parameters[0]);
      if Form3.CheckBox5.Checked then
        Form1.CheckBox5.Checked := true;

      Form3.CheckBox2.Checked := (parameters[2] <> '0,0,0,0');
      if Form3.CheckBox2.Checked then
        begin
        Form1.CheckBox5.Checked := true;
        cropvalues := SplitString(parameters[2], ',');
        Form3.TrackBar1.Position := StrToInt(cropvalues[0]);
        Form3.TrackBar3.Position := StrToInt(cropvalues[1]);
        Form3.TrackBar4.Position := StrToInt(cropvalues[2]);
        Form3.TrackBar5.Position := StrToInt(cropvalues[3]);
        end;

      Form1.TrackBar1.Position := StrToInt(parameters[3]);

      Form3.CheckBox4.Checked := (parameters[4] <> '0');
      if Form3.CheckBox4.Checked then
        Form1.CheckBox5.Checked := true;

      Form1.CheckBox1.Checked := (parameters[6] <> '0');

      // patch infile check here as well later

      if AnsiEndsStr('.pdf', LowerCase(parameters[7])) then
        begin
          if FileExists(parameters[7]) then
            begin
              Form1.OpenDialog1.Filename := parameters[7];
              Form1.Button1.Caption := ExtractFileNameOnly(Form1.OpenDialog1.Filename) + '.pdf';
              Form1.currentpdf := Form1.OpenDialog1.Filename;
              if RunCommand('liesel', ['-p', Form1.OpenDialog1.Filename], pgstring, [], swoHide) then
                begin
                  Form1.currentpagecount := StrToInt(pgstring);
                  Form3.Button2.Caption := pgstring;
                end;
              if Form1.CheckBox5.Checked then
                 Form3.ExportLiesel(Form1.CreateCommand(true));
            end;
        end;

      Form3.CheckBox1.Checked := (parameters[8] <> '0');
      if Form3.CheckBox1.Checked then
        begin
        Form3.TrackBar2.Position := StrToInt(parameters[8]);
        Form1.CheckBox5.Checked := true;
        end;

      Form1.CheckBox2.Checked := (parameters[12] <> '0');
      if Form1.CheckBox2.Checked then
        Form1.Edit1.Text := parameters[12];

      Form1.CheckBox3.Checked := (parameters[13] <> '0');
      if Form1.CheckBox3.Checked then
        Form1.SpinEdit1.Value := StrToInt(parameters[13]);

      Form1.CheckBox4.Checked := (parameters[14] <> '0x0');
      if Form1.CheckBox4.Checked then
        begin
          transform := SplitString(parameters[14], 'x');
          Form1.ComboBox1.Text := 'custom';
          Form1.ComboBox1Change(TObject.Create);
          Form1.Edit3.Text := transform[0];
          Form1.Edit4.Text := transform[1];
        end;

      Form3.CheckBox3.Checked := (parameters[15] <> '0');
      if Form3.CheckBox3.Checked then
        begin
        Form3.TrackBar6.Position := StrToInt(parameters[15]);
        Form1.CheckBox5.Checked := true;
        end;

      Update();

      Form3.CheckBox6.Checked := previewonoff; // Restore previous "live preview" setting
      Form4.Close;
    end
  else
    begin
      ShowMessage('Error: Invalid command');
    end;
end;

procedure TForm4.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  if mode = 0 then
    begin
      Label1.Caption := 'Import Liesel Command';
      Edit1.Text := '';
      Button1.Caption := 'Import Settings';
    end
  else if mode = 1 then
    begin
      Label1.Caption := 'Your exported command is:';
      Edit1.Text := exportedcomd;
      Button1.Caption := 'OK';
    end;
end;

end.

