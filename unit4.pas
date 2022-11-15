unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  StrUtils, Process, LazFileUtils, XMLRead, XMLUtils, DOM;

type

  { TForm4 }

  TForm4 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Timer1: TTimer;
    procedure LoadSettingsFromXML();
    function WriteSettingsToXML(comd : string) : boolean;
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

procedure TForm4.LoadSettingsFromXML();
var
  pgstring : string;
  previewonoff : boolean;
  XML : TXMLDocument;
  NodeInfile : TDOMNode;
  NodeAutomargin : TDOMNode;
  NodeWiden : TDOMNode;
  NodeCrop : TDOMNode;
  cropvalues : TStringArray;
  NodeDivide : TDOMNode;
  NodeGrayscale : TDOMNode;
  NodeThreshold : TDOMNode;
  NodeRange : TDOMNode;
  NodeSegment : TDOMNode;
  NodeTransform : TDOMNode;
  transformvalues : TStringArray;
  NodeQuality : TDOMNode;
  NodeLinear : TDOMNode;
  advancedview : boolean;
  settingsfile : string;
begin
  advancedview := Form1.CheckBox5.Checked;

      settingsfile := Form1.systmpdir + '/bookthief-temp-settings.xml';

      ReadXMLFile(XML, settingsfile);

      NodeInfile := XML.DocumentElement.FindNode('infile');
      NodeAutomargin := XML.DocumentElement.FindNode('automargin');
      NodeWiden := XML.DocumentElement.FindNode('widen');
      NodeCrop := XML.DocumentElement.FindNode('crop');
      NodeDivide := XML.DocumentElement.FindNode('divide');
      NodeGrayscale := XML.DocumentElement.FindNode('grayscale');
      NodeThreshold := XML.DocumentElement.FindNode('threshold');
      NodeRange := XML.DocumentElement.FindNode('range');
      NodeSegment := XML.DocumentElement.FindNode('segment');
      NodeTransform := XML.DocumentElement.FindNode('transform');
      NodeQuality := XML.DocumentElement.FindNode('quality');
      NodeLinear := XML.DocumentElement.FindNode('linear');

      previewonoff := Form3.CheckBox6.Checked; // Store current "live preview" enabled/disabled
      Form3.CheckBox6.Checked := false; // Temporarily disable live preview
      Update();

      // Action goes here

      if (NodeInfile <> nil) then
        begin
          if AnsiEndsStr('.pdf', NodeInfile.TextContent) then
            begin
              if FileExists(NodeInfile.TextContent) then
                begin
                  Form1.OpenDialog1.Filename := NodeInfile.TextContent;
                  Form1.Button1.Caption := ExtractFileNameOnly(Form1.OpenDialog1.Filename) + '.pdf';
                  Form1.currentpdf := Form1.OpenDialog1.Filename;
                  if RunCommand('liesel', ['-p', Form1.OpenDialog1.Filename], pgstring, [], swoHide) then
                    begin
                      Form1.currentpagecount := StrToInt(pgstring);
                      Form3.Button2.Caption := pgstring;
                    end;
                  if Form1.CheckBox5.Checked then
                    Form3.ExportLiesel(Form1.CreateCommand(true, true));  // I think this should go at the end of the whole process, not here
                end;
            end;
        end;


      Form3.CheckBox5.Checked := (NodeAutomargin <> nil);
        if (NodeAutomargin <> nil) then
          begin
            Form3.TrackBar7.Position := StrToInt(NodeAutomargin.TextContent);
            advancedview := true;
          end;

        Form3.CheckBox3.Checked := (NodeWiden <> nil);
        if (NodeWiden <> nil) then
          begin
            Form3.TrackBar6.Position := StrToInt(NodeWiden.TextContent);
            advancedview := true;
          end;

        Form3.CheckBox2.Checked := (NodeCrop <> nil);
        if (NodeCrop <> nil) then
          begin
            advancedview := true;
            cropvalues := SplitString(NodeCrop.TextContent, ',');
            Form3.TrackBar1.Position := StrToInt(cropvalues[0]);
            Form3.TrackBar3.Position := StrToInt(cropvalues[1]);
            Form3.TrackBar4.Position := StrToInt(cropvalues[2]);
            Form3.TrackBar5.Position := StrToInt(cropvalues[3]);
          end;

        Form3.CheckBox1.Checked := (NodeThreshold <> nil);
        if (NodeThreshold <> nil) then
          begin
            Form3.TrackBar2.Position := StrToInt(NodeThreshold.TextContent);
            advancedview := true;
          end;

        Form1.CheckBox2.Checked := (NodeRange <> nil);
        if (NodeRange <> nil) then
          begin
            Form1.Edit1.Text := NodeRange.TextContent;
          end;

        Form1.CheckBox3.Checked := (NodeSegment <> nil);
        if (NodeSegment <> nil) then
          begin
            Form1.SpinEdit1.Value := StrToInt(NodeSegment.TextContent);
          end;

        Form1.CheckBox4.Checked := (NodeTransform <> nil);
        if (NodeTransform <> nil) then
          begin
            transformvalues := SplitString(NodeTransform.TextContent, 'x');
            Form1.ComboBox1.Text := 'custom';
            Form1.ComboBox1Change(TObject.Create);
            Form1.Edit3.Text := transformvalues[0];
            Form1.Edit4.Text := transformvalues[1];
          end;

        Form3.CheckBox4.Checked := (NodeDivide <> nil);
        Form1.CheckBox1.Checked := (NodeGrayscale <> nil);

        Form3.CheckBox7.Checked := (NodeLinear <> nil);

        if (NodeQuality <> nil) then
          begin
            Form1.TrackBar1.Position := StrToInt(NodeQuality.TextContent);
          end;

        Form1.CheckBox5.Checked := advancedview;

      Update();

      Form3.CheckBox6.Checked := previewonoff; // Restore previous "live preview" setting
end;

function TForm4.WriteSettingsToXML(comd : string) : boolean;
var
  settingsfile : string;
  ReturnInfo : string;
  BullshitThatIHaveToPutUpWith : TextFile;
begin
  if AnsiStartsStr('liesel ', comd) then
    begin
      comd := StringReplace(comd, 'liesel ', 'liesel -B ', []);
    end
  else
    begin
      ShowMessage('Error: Invalid Liesel Command');
      WriteSettingsToXML := false;
      Exit();
    end;

  if RunCommand(comd, ReturnInfo) then
    begin
      settingsfile := Form1.systmpdir + '/bookthief-temp-settings.xml';

      AssignFile(BullshitThatIHaveToPutUpWith, settingsfile);
      Rewrite(BullshitThatIHaveToPutUpWith);
      WriteLn(BullshitThatIHaveToPutUpWith, ReturnInfo);
      CloseFile(BullshitThatIHaveToPutUpWith);
      WriteSettingsToXML := true;
    end
  else
    begin
      ShowMessage('Error: Invalid command');
      WriteSettingsToXML := false;
    end;
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  comd : string;
begin
  if mode = 1 then
    begin
      Form4.Close;
      Exit();
    end;
  comd := Edit1.Text;
  if (WriteSettingsToXML(comd)) then
    LoadSettingsFromXML();

  Form4.Close;
  Exit();
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

