/***************************************************************
 * Name:	  bookthiefMain.cpp
 * Purpose:   Code for Application Frame
 * Author:	rail5 (andrew@rail5.org)
 * Created:   2021-10-10
 * Copyright: rail5 (https://rail5.org)
 * License:
 **************************************************************/

#include "bookthiefMain.h"
#include <wx/msgdlg.h>
#include <wx/valtext.h>
#include <wx/app.h>
#include <wx/filedlg.h>
#include <wx/textdlg.h>
#include <array>
#include <thread>
#include <functional>

#include "functions/is_number.h"
#include "functions/itoa.h"
#include "functions/file_exists.h"
#include "functions/has_ending.h"
#include "functions/truncate.h"
#include "functions/replaceall.h"
#include "functions/custom_popen.h"

//(*InternalHeaders(bookthiefFrame)
#include <wx/intl.h>
#include <wx/string.h>
//*)

//helper functions
enum wxbuildinfoformat {
	short_f, long_f };

wxString wxbuildinfo(wxbuildinfoformat format)
{
	wxString wxbuild(wxVERSION_STRING);

	if (format == long_f )
	{
#if defined(__WXMSW__)
		wxbuild << _T("-Windows");
#elif defined(__UNIX__)
		wxbuild << _T("-Linux");
#endif

#if wxUSE_UNICODE
		wxbuild << _T("-Unicode build");
#else
		wxbuild << _T("-ANSI build");
#endif // wxUSE_UNICODE
	}

	return wxbuild;
}

//(*IdInit(bookthiefFrame)
const long bookthiefFrame::ID_FILEPICKERCTRL1 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX1 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX2 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT1 = wxNewId();
const long bookthiefFrame::ID_TEXTCTRL1 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT2 = wxNewId();
const long bookthiefFrame::ID_TEXTCTRL2 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX3 = wxNewId();
const long bookthiefFrame::ID_SPINCTRL1 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT3 = wxNewId();
const long bookthiefFrame::ID_HYPERLINKCTRL1 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX4 = wxNewId();
const long bookthiefFrame::ID_CHOICE1 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT4 = wxNewId();
const long bookthiefFrame::ID_SLIDER1 = wxNewId();
const long bookthiefFrame::ID_BUTTON2 = wxNewId();
const long bookthiefFrame::ID_MENUITEM1 = wxNewId();
const long bookthiefFrame::idMenuAbout = wxNewId();
const long bookthiefFrame::idMenuExport = wxNewId();
const long bookthiefFrame::idMenuLink1 = wxNewId();
const long bookthiefFrame::idMenuLink2 = wxNewId();
const long bookthiefFrame::ID_PROGRESSDIALOG1 = wxNewId();
//*)

BEGIN_EVENT_TABLE(bookthiefFrame,wxFrame)
	//(*EventTable(bookthiefFrame)
	//*)
END_EVENT_TABLE()

wxString infile;

bookthiefFrame::bookthiefFrame(wxWindow* parent,wxWindowID id)
{
	//(*Initialize(bookthiefFrame)
	wxBoxSizer* BoxSizer10;
	wxBoxSizer* BoxSizer11;
	wxBoxSizer* BoxSizer12;
	wxBoxSizer* BoxSizer13;
	wxBoxSizer* BoxSizer14;
	wxBoxSizer* BoxSizer15;
	wxBoxSizer* BoxSizer16;
	wxBoxSizer* BoxSizer17;
	wxBoxSizer* BoxSizer18;
	wxBoxSizer* BoxSizer19;
	wxBoxSizer* BoxSizer1;
	wxBoxSizer* BoxSizer20;
	wxBoxSizer* BoxSizer21;
	wxBoxSizer* BoxSizer22;
	wxBoxSizer* BoxSizer23;
	wxBoxSizer* BoxSizer24;
	wxBoxSizer* BoxSizer2;
	wxBoxSizer* BoxSizer3;
	wxBoxSizer* BoxSizer4;
	wxBoxSizer* BoxSizer5;
	wxBoxSizer* BoxSizer6;
	wxBoxSizer* BoxSizer7;
	wxBoxSizer* BoxSizer8;
	wxBoxSizer* BoxSizer9;
	wxMenu* Menu1;
	wxMenu* Menu2;
	wxMenuBar* MenuBar1;
	wxMenuItem* MenuItem1;
	wxMenuItem* MenuItem2;
	wxMenuItem* MenuItem3;
	wxMenuItem* MenuItem4;
	wxMenuItem* MenuItem5;

	Create(parent, wxID_ANY, _("BookThief"), wxDefaultPosition, wxDefaultSize, wxDEFAULT_FRAME_STYLE, _T("wxID_ANY"));
	BoxSizer1 = new wxBoxSizer(wxVERTICAL);
	BoxSizer2 = new wxBoxSizer(wxHORIZONTAL);
	FilePickerCtrl1 = new wxFilePickerCtrl(this, ID_FILEPICKERCTRL1, wxEmptyString, wxEmptyString, _T("*.pdf"), wxDefaultPosition, wxDefaultSize, wxFLP_FILE_MUST_EXIST|wxFLP_OPEN, wxDefaultValidator, _T("ID_FILEPICKERCTRL1"));
	BoxSizer2->Add(FilePickerCtrl1, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_TOP, 5);
	BoxSizer1->Add(BoxSizer2, 0, wxEXPAND, 5);
	BoxSizer4 = new wxBoxSizer(wxVERTICAL);
	BoxSizer5 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer10 = new wxBoxSizer(wxHORIZONTAL);
	CheckBox1 = new wxCheckBox(this, ID_CHECKBOX1, _("Convert to grayscale"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX1"));
	CheckBox1->SetValue(true);
	BoxSizer10->Add(CheckBox1, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer5->Add(BoxSizer10, 1, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer4->Add(BoxSizer5, 0, wxALIGN_LEFT, 5);
	BoxSizer6 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer7 = new wxBoxSizer(wxVERTICAL);
	CheckBox2 = new wxCheckBox(this, ID_CHECKBOX2, _("Print only within range"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX2"));
	CheckBox2->SetValue(true);
	BoxSizer7->Add(CheckBox2, 1, wxALL|wxALIGN_LEFT, 5);
	BoxSizer6->Add(BoxSizer7, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer8 = new wxBoxSizer(wxVERTICAL);
	BoxSizer18 = new wxBoxSizer(wxVERTICAL);
	StaticText1 = new wxStaticText(this, ID_STATICTEXT1, _("Print only pages:"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT1"));
	BoxSizer18->Add(StaticText1, 0, wxALIGN_CENTER_HORIZONTAL, 5);
	BoxSizer9 = new wxBoxSizer(wxHORIZONTAL);
	TextCtrl1 = new wxTextCtrl(this, ID_TEXTCTRL1, _("1"), wxDefaultPosition, wxSize(50,35), 0, wxTextValidator(wxFILTER_DIGITS), _T("ID_TEXTCTRL1"));
	BoxSizer9->Add(TextCtrl1, 0, wxTOP|wxBOTTOM|wxLEFT|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL|wxSHAPED, 5);
	StaticText2 = new wxStaticText(this, ID_STATICTEXT2, _("  to  "), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT2"));
	BoxSizer9->Add(StaticText2, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	TextCtrl2 = new wxTextCtrl(this, ID_TEXTCTRL2, _("100"), wxDefaultPosition, wxSize(50,35), 0, wxTextValidator(wxFILTER_DIGITS), _T("ID_TEXTCTRL2"));
	BoxSizer9->Add(TextCtrl2, 0, wxTOP|wxBOTTOM|wxRIGHT|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL|wxSHAPED, 5);
	BoxSizer18->Add(BoxSizer9, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer8->Add(BoxSizer18, 1, wxALL|wxALIGN_RIGHT, 5);
	BoxSizer6->Add(BoxSizer8, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer4->Add(BoxSizer6, 0, wxEXPAND, 5);
	BoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer12 = new wxBoxSizer(wxVERTICAL);
	CheckBox3 = new wxCheckBox(this, ID_CHECKBOX3, _("Print in segments"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX3"));
	CheckBox3->SetValue(true);
	BoxSizer12->Add(CheckBox3, 0, wxALL|wxALIGN_LEFT, 5);
	BoxSizer11->Add(BoxSizer12, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer13 = new wxBoxSizer(wxVERTICAL);
	BoxSizer17 = new wxBoxSizer(wxVERTICAL);
	BoxSizer19 = new wxBoxSizer(wxHORIZONTAL);
	SpinCtrl1 = new wxSpinCtrl(this, ID_SPINCTRL1, _T("40"), wxDefaultPosition, wxSize(126,34), 0, 0, 100, 40, _T("ID_SPINCTRL1"));
	SpinCtrl1->SetValue(_T("40"));
	BoxSizer19->Add(SpinCtrl1, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer17->Add(BoxSizer19, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	StaticText3 = new wxStaticText(this, ID_STATICTEXT3, _("Pages per segment"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT3"));
	BoxSizer17->Add(StaticText3, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer13->Add(BoxSizer17, 1, wxALL|wxALIGN_RIGHT, 5);
	BoxSizer11->Add(BoxSizer13, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer4->Add(BoxSizer11, 0, wxEXPAND, 5);
	BoxSizer14 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer15 = new wxBoxSizer(wxHORIZONTAL);
	CheckBox4 = new wxCheckBox(this, ID_CHECKBOX4, _("Rescale to paper size"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX4"));
	CheckBox4->SetValue(true);
	BoxSizer15->Add(CheckBox4, 1, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer14->Add(BoxSizer15, 0, wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer16 = new wxBoxSizer(wxVERTICAL);
	BoxSizer21 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer22 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer24 = new wxBoxSizer(wxHORIZONTAL);
	BoxSizer20 = new wxBoxSizer(wxVERTICAL);
	Choice1 = new wxChoice(this, ID_CHOICE1, wxDefaultPosition, wxSize(106,30), 0, 0, 0, wxDefaultValidator, _T("ID_CHOICE1"));
	Choice1->SetSelection( Choice1->Append(_("us-letter")) );
	Choice1->Append(_("a4"));
	BoxSizer20->Add(Choice1, 1, wxALL, 5);
	BoxSizer24->Add(BoxSizer20, 1, wxALL, 5);
	BoxSizer22->Add(BoxSizer24, 1, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer21->Add(BoxSizer22, 1, wxALL|wxALIGN_CENTER_VERTICAL, 5);
	BoxSizer16->Add(BoxSizer21, 1, wxALL|wxALIGN_RIGHT, 5);
	BoxSizer14->Add(BoxSizer16, 1, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
	BoxSizer4->Add(BoxSizer14, 0, wxEXPAND, 5);
	BoxSizer1->Add(BoxSizer4, 0, wxEXPAND, 5);
	BoxSizer3 = new wxBoxSizer(wxVERTICAL);
	StaticText4 = new wxStaticText(this, ID_STATICTEXT4, _("Quality"), wxDefaultPosition, wxDefaultSize, 0, _T("ID_STATICTEXT4"));
	BoxSizer3->Add(StaticText4, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 5);
	Slider1 = new wxSlider(this, ID_SLIDER1, 100, 0, 200, wxDefaultPosition, wxSize(279,50), wxSL_VALUE_LABEL, wxDefaultValidator, _T("ID_SLIDER1"));
	BoxSizer3->Add(Slider1, 0, wxALL|wxEXPAND, 5);
	BoxSizer1->Add(BoxSizer3, 0, wxEXPAND, 0);
	BoxSizer23 = new wxBoxSizer(wxHORIZONTAL);
	Button2 = new wxButton(this, ID_BUTTON2, _("Save as"), wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, _T("ID_BUTTON2"));
	BoxSizer23->Add(Button2, 1, 0, 0);
	BoxSizer1->Add(BoxSizer23, 0, wxALIGN_CENTER_HORIZONTAL, 0);
	SetSizer(BoxSizer1);
	MenuBar1 = new wxMenuBar();
	Menu1 = new wxMenu();
	MenuItem3 = new wxMenuItem(Menu1, idMenuExport, _("Export Command\tF12"), _("Export the generated Liesel command"), wxITEM_NORMAL);
	MenuItem1 = new wxMenuItem(Menu1, ID_MENUITEM1, _("Quit\tAlt-F4"), _("Quit the application"), wxITEM_NORMAL);
	Menu1->Append(MenuItem3);
	Menu1->Append(MenuItem1);
	MenuBar1->Append(Menu1, _("&File"));
	Menu2 = new wxMenu();
	MenuItem2 = new wxMenuItem(Menu2, idMenuAbout, _("About\tF1"), _("Show info about this application"), wxITEM_NORMAL);
	MenuItem4 = new wxMenuItem(Menu2, idMenuLink1, _("Library Genesis"), _("Link to Library Genesis"), wxITEM_NORMAL);
	MenuItem5 = new wxMenuItem(Menu2, idMenuLink2, _("ZLibrary"), _("Link to Zlibrary"), wxITEM_NORMAL);
	Menu2->Append(MenuItem2);
	Menu2->AppendSeparator();
	Menu2->Append(MenuItem4);
	Menu2->Append(MenuItem5);
	MenuBar1->Append(Menu2, _("Help"));
	SetMenuBar(MenuBar1);
	BoxSizer1->Fit(this);
	BoxSizer1->SetSizeHints(this);
	
	const wxSize &minSize = this->GetClientSize() + wxSize(100,100);
	
	this->SetMinClientSize(minSize);
	this->SetClientSize(minSize);
	this->SetMaxClientSize(minSize);
	
	BoxSizer2->SetMinSize(BoxSizer2->GetMinSize());
	BoxSizer3->SetMinSize(BoxSizer3->GetMinSize());
	BoxSizer4->SetMinSize(BoxSizer4->GetMinSize());
	BoxSizer5->SetMinSize(BoxSizer5->GetMinSize());
	BoxSizer6->SetMinSize(BoxSizer6->GetMinSize());
	BoxSizer7->SetMinSize(BoxSizer7->GetMinSize());
	BoxSizer8->SetMinSize(BoxSizer8->GetMinSize());
	BoxSizer9->SetMinSize(BoxSizer9->GetMinSize());
	BoxSizer10->SetMinSize(BoxSizer10->GetMinSize());
	BoxSizer11->SetMinSize(BoxSizer11->GetMinSize());
	BoxSizer12->SetMinSize(BoxSizer12->GetMinSize());
	BoxSizer13->SetMinSize(BoxSizer13->GetMinSize());
	BoxSizer14->SetMinSize(BoxSizer14->GetMinSize());
	BoxSizer15->SetMinSize(BoxSizer15->GetMinSize());
	BoxSizer16->SetMinSize(BoxSizer16->GetMinSize());
	BoxSizer17->SetMinSize(BoxSizer17->GetMinSize());
	BoxSizer18->SetMinSize(BoxSizer18->GetMinSize());
	BoxSizer19->SetMinSize(BoxSizer19->GetMinSize());
	BoxSizer20->SetMinSize(BoxSizer20->GetMinSize());
	BoxSizer21->SetMinSize(BoxSizer21->GetMinSize());
	BoxSizer22->SetMinSize(BoxSizer22->GetMinSize());
	BoxSizer23->SetMinSize(BoxSizer23->GetMinSize());


	Connect(ID_CHECKBOX2,wxEVT_COMMAND_CHECKBOX_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnCheckBox2Click);
	Connect(ID_CHECKBOX3,wxEVT_COMMAND_CHECKBOX_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnCheckBox3Click);
	Connect(ID_CHECKBOX4,wxEVT_COMMAND_CHECKBOX_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnCheckBox4Click);
	Connect(ID_MENUITEM1,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnQuit);
	Connect(idMenuAbout,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnAbout);
	Connect(idMenuExport,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnExport);
	Connect(idMenuLink1,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnLink1);
	Connect(idMenuLink2,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnLink2);
	Connect(ID_FILEPICKERCTRL1,wxEVT_COMMAND_FILEPICKER_CHANGED,(wxObjectEventFunction)&bookthiefFrame::OnFilePickerCtrl1FileChanged);
	Connect(ID_BUTTON2,wxEVT_COMMAND_BUTTON_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnButton2Click);


	
	
	if (wxTheApp->argc > 1) {
		if (file_exists(wxTheApp->argv[1]) && has_ending(wxTheApp->argv[1], ".pdf")) {
			infile = wxTheApp->argv[1];
			wxString infilenm = infile.substr(infile.find_last_of("/\\") + 1);
			wxSetWorkingDirectory(replaced(infilenm, "", infile));
			FilePickerCtrl1->SetFileName(infile);
		}
	} else {
		infile = "";
	}
	
	
//	OnOpen();
	
	//*)
}

bookthiefFrame::~bookthiefFrame()
{
	//(*Destroy(bookthiefFrame)
	//*)
}

void bookthiefFrame::OnQuit(wxCommandEvent& event)
{
	Close();
}

int lieselproc;

std::string bookthiefFrame::exec(const char* cmd, bool nonprogress) {

	if (nonprogress == true) {
		std::array<char, 128> buffer;
		std::string result;
		std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
		if (!pipe) {
			throw std::runtime_error("popen() failed!");
		}
		while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
			result += buffer.data();
		}
		return result;
	}

	char checkcmd[4096] = {""};
	strcat(checkcmd, cmd);
	strcat(checkcmd, " -c");
	std::array<char, 128> buffer;
	std::string result;
	std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(checkcmd, "r"), pclose);
	if (!pipe) {
		throw std::runtime_error("popen() failed!");
	}
	while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
		result += buffer.data();
	}
	if (result != "OK") {
		ProgressDialog1->Update(100);
		return result;
	}
	char buf[5];

	int progcounter = 10;

	int compensate = 0; // for some reason, and I don't understand why, the ProgressDialog->Update()
				// function seems to always lag behind one step
				// ie, exec() sends Update(10); and nothing happens,
				// then exec() sends Update(20); and it moves to 10
				// then Update(30); and it moves to 20, etc
				// So for now, a "compensate" int is calculated on first input
				// And added to every subsequent input.
				// I'll be ecstatic to figure out why the delay happens and how to fix it
				// so that I can get rid of this janky "compensate" patch

	bool firstinput = true;



	FILE *fp = popen2(cmd, "r", lieselproc);
	std::cout << "Spawned Liesel with PID " << lieselproc+1 << "\n\n";
	setvbuf(fp, NULL, _IONBF, 0);

	while (fgets(buf, 5, fp) != NULL) {
		ProgressDialog1->Update(progcounter);


		char *buffer = buf;
		buffer[strlen(buf) - 2] = '\0';

		if (is_number(buffer)) {
			progcounter = atoi(buffer);
			if (firstinput == true) {
				compensate = progcounter; // establish the compensate value, as explained above
			}
			if ((progcounter + compensate) < 100) {
				progcounter = progcounter + compensate; // minor bug found where wxWidgets reported an error on being asked to update to 101, 103 etc
			}
			if (ProgressDialog1->Update(progcounter) == false) {
				// user pressed Cancel
				kill(lieselproc+1, SIGTERM);
				ProgressDialog1->Hide();
				return "Canceled";
			}
		}
		firstinput = false;
	}

	if (pclose2(fp, lieselproc)) {
		return "";
	}

	ProgressDialog1->Update(100);
	return "Done!";



}

wxString bookthiefFrame::gencommand() {
	wxString saneinfile = infile;
	replaceAll("\\", "\\\\", saneinfile);
	replaceAll("\"", "\\\"", saneinfile);

	if (!has_ending(saneinfile, ".pdf")) {
		wxMessageBox("Error: Please select a valid input PDF", "BookThief");
		return "fail";
	}

	wxFileDialog saveFileDialog(this, ("Save PDF file"), "", "", "PDF files (*.pdf)|*.pdf", wxFD_SAVE);
	if (saveFileDialog.ShowModal() == wxID_CANCEL) {
		return "fail";
	}

	wxString outfile = saveFileDialog.GetPath();
	replaceAll("\\", "\\\\", outfile);
	replaceAll("\"", "\\\"", outfile);

	if (outfile == "") {
		wxMessageBox("Error: Please pick a file for the new PDF", "BookThief");
		return "fail";
	}

	char command[8320] = "liesel -b -i \"";

	strcat(command, saneinfile);
	strcat(command, "\"");

	if (CheckBox1->GetValue()  == true) {
		strcat(command, " -g");
	}

	if (CheckBox2->GetValue() == true) {
		strcat(command, " -r ");
		strcat(command, TextCtrl1->GetValue());
		strcat(command, "-");
		strcat(command, TextCtrl2->GetValue());
	}

	if (CheckBox3->GetValue() == true) {
		char segsize[33];
		itoa(SpinCtrl1->GetValue(), segsize, 10);
		strcat(command, " -s ");
		strcat(command, segsize);
	}

	if (CheckBox4->GetValue() == true) {
		strcat(command, " -t ");
		strcat(command, Choice1->GetStringSelection());
	}

	char quality[33];
	int qualint = Slider1->GetValue();
	if (qualint < 0) {
		qualint = 0;
	}
	itoa(qualint, quality, 10);

	strcat(command, " -d ");
	strcat(command, quality);

	strcat(command, " -o \"");
	strcat(command, outfile);
	strcat(command, "\"");
	
	return command;
}


void bookthiefFrame::OnAbout(wxCommandEvent& event)
{
	wxString info = exec("liesel -q", true);
	wxMessageBox(info, "BookThief");
}

void bookthiefFrame::OnExport(wxCommandEvent& event)
{
	wxString comd = gencommand();
	if (comd == "fail") {
		return;
	}

	wxTextEntryDialog returncomd(this, ("Your exported command is:"), "BookThief", comd);
	returncomd.ShowModal();
}

void bookthiefFrame::OnLink1(wxCommandEvent& event)
{
	wxLaunchDefaultBrowser("http://gen.lib.rus.ec");
}

void bookthiefFrame::OnLink2(wxCommandEvent& event)
{
	wxLaunchDefaultBrowser("http://1lib.domains");
}

void bookthiefFrame::OnFilePickerCtrl1FileChanged(wxFileDirPickerEvent& event)
{
	infile = FilePickerCtrl1->GetPath();
}

void bookthiefFrame::OnButton2Click(wxCommandEvent& event)
{
	wxString comd = gencommand();
	if (comd == "fail") {
		return;
	}
	const char* command = (const char*)comd.mb_str();
	std::cout << "Running:\n" << command << "\n\n";
	ProgressDialog1 = new wxProgressDialog(_("BookThief"), _("Building..."), 100, this, wxPD_APP_MODAL|wxPD_AUTO_HIDE|wxPD_CAN_ABORT);
	wxString messageone = exec(command, false);
	wxMessageBox(messageone, "BookThief");

}

void bookthiefFrame::OnCheckBox2Click(wxCommandEvent& event) {
	if (CheckBox2->GetValue() == true) {
		StaticText1->Show();
		TextCtrl2->Show();
		StaticText2->Show();
		TextCtrl1->Show();
	} else {
		StaticText1->Hide();
		TextCtrl2->Hide();
		StaticText2->Hide();
		TextCtrl1->Hide();
	}
}

void bookthiefFrame::OnCheckBox3Click(wxCommandEvent& event) {

	if (CheckBox3->GetValue() == true) {
		StaticText3->Show();
		SpinCtrl1->Show();
	} else {
		StaticText3->Hide();
		SpinCtrl1->Hide();
	}
}

void bookthiefFrame::OnCheckBox4Click(wxCommandEvent& event) {
	if (CheckBox4->GetValue() == true) {
		Choice1->Show();
	} else {
		Choice1->Hide();

	}
}
