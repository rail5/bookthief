/***************************************************************
 * Name:	BookThief
 * Version:	4.0
 * Purpose:	GUI Frontend for Liesel
 * Author:	rail5 (andrew@rail5.org)
 * Created:	2021-08-23
 * Copyright:	rail5 (https://rail5.org)
 * License:	GNU GPL V3
 **************************************************************/

#include "bookthiefMain.h"
#include <wx/msgdlg.h>
#include <wx/valtext.h>
#include <wx/app.h>
#include <wx/filedlg.h>

#include <string.h>
#include <math.h>

#include <memory>
#include <stdexcept>
#include <array>
#include <thread>
#include <functional>

#include "functions/is_number.h"
#include "functions/itoa.h"
#include "functions/file_exists.h"
#include "functions/has_ending.h"
#include "functions/truncate.h"
#include "functions/replaceall.h"

//(*InternalHeaders(bookthiefFrame)
#include <wx/artprov.h>
#include <wx/bitmap.h>
#include <wx/icon.h>
#include <wx/image.h>
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
const long bookthiefFrame::ID_BUTTON1 = wxNewId();
const long bookthiefFrame::ID_HYPERLINKCTRL1 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX1 = wxNewId();
const long bookthiefFrame::ID_SPINCTRL1 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT1 = wxNewId();
const long bookthiefFrame::ID_TEXTCTRL1 = wxNewId();
const long bookthiefFrame::ID_TEXTCTRL2 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT2 = wxNewId();
const long bookthiefFrame::ID_HYPERLINKCTRL2 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT3 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT4 = wxNewId();
const long bookthiefFrame::ID_HYPERLINKCTRL3 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX2 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX3 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT6 = wxNewId();
const long bookthiefFrame::ID_BUTTON2 = wxNewId();
const long bookthiefFrame::ID_STATICBITMAP1 = wxNewId();
const long bookthiefFrame::ID_STATICBITMAP2 = wxNewId();
const long bookthiefFrame::idMenuQuit = wxNewId();
const long bookthiefFrame::idMenuAbout = wxNewId();
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
	wxMenu* Menu1;
	wxMenu* Menu2;
	wxMenuBar* MenuBar1;
	wxMenuItem* MenuItem1;
	wxMenuItem* MenuItem2;

	Create(parent, wxID_ANY, _("BookThief"), wxDefaultPosition, wxDefaultSize, wxDEFAULT_FRAME_STYLE, _T("wxID_ANY"));
	SetClientSize(wxSize(400,350));
	SetMinSize(wxSize(400,350));
	SetMaxSize(wxSize(400,350));
	{
		wxIcon FrameIcon;
		FrameIcon.CopyFromBitmap(wxArtProvider::GetBitmap(wxART_MAKE_ART_ID_FROM_STR(_T("wxART_FILE_SAVE")),wxART_OTHER));
		SetIcon(FrameIcon);
	}
	Button1 = new wxButton(this, ID_BUTTON1, _("Save as"), wxPoint(152,240), wxDefaultSize, 0, wxDefaultValidator, _T("ID_BUTTON1"));
	HyperlinkCtrl1 = new wxHyperlinkCtrl(this, ID_HYPERLINKCTRL1, _("Library Genesis"), _("http://gen.lib.rus.ec/"), wxPoint(96,288), wxDefaultSize, wxHL_CONTEXTMENU|wxHL_ALIGN_CENTRE, _T("ID_HYPERLINKCTRL1"));
	CheckBox1 = new wxCheckBox(this, ID_CHECKBOX1, _("Convert to grayscale"), wxPoint(8,56), wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX1"));
	CheckBox1->SetValue(false);
	SpinCtrl1 = new wxSpinCtrl(this, ID_SPINCTRL1, _T("40"), wxPoint(248,144), wxSize(136,34), 0, 0, 100, 40, _T("ID_SPINCTRL1"));
	SpinCtrl1->SetValue(_T("40"));
	SpinCtrl1->Hide();
	StaticText1 = new wxStaticText(this, ID_STATICTEXT1, _("Pages per segment"), wxPoint(256,184), wxDefaultSize, 0, _T("ID_STATICTEXT1"));
	StaticText1->Hide();
	TextCtrl1 = new wxTextCtrl(this, ID_TEXTCTRL1, _("1"), wxPoint(248,104), wxSize(56,27), 0, wxDefaultValidator, _T("ID_TEXTCTRL1"));
	TextCtrl1->Hide();
	TextCtrl2 = new wxTextCtrl(this, ID_TEXTCTRL2, _("10"), wxPoint(336,104), wxSize(48,27), 0, wxDefaultValidator, _T("ID_TEXTCTRL2"));
	TextCtrl2->Hide();
	StaticText2 = new wxStaticText(this, ID_STATICTEXT2, _("to"), wxPoint(312,112), wxDefaultSize, 0, _T("ID_STATICTEXT2"));
	StaticText2->Hide();
	HyperlinkCtrl2 = new wxHyperlinkCtrl(this, ID_HYPERLINKCTRL2, _("ZLibrary"), _("https://1lib.domains"), wxPoint(216,288), wxDefaultSize, wxHL_CONTEXTMENU|wxHL_ALIGN_CENTRE, _T("ID_HYPERLINKCTRL2"));
	StaticText3 = new wxStaticText(this, ID_STATICTEXT3, _("Looking for books\? Try:"), wxPoint(128,280), wxDefaultSize, 0, _T("ID_STATICTEXT3"));
	StaticText4 = new wxStaticText(this, ID_STATICTEXT4, _("or"), wxPoint(208,296), wxDefaultSize, 0, _T("ID_STATICTEXT4"));
	HyperlinkCtrl3 = new wxHyperlinkCtrl(this, ID_HYPERLINKCTRL3, _("\?"), wxEmptyString, wxPoint(288,192), wxDefaultSize, wxHL_CONTEXTMENU|wxHL_ALIGN_CENTRE, _T("ID_HYPERLINKCTRL3"));
	HyperlinkCtrl3->Hide();
	CheckBox2 = new wxCheckBox(this, ID_CHECKBOX2, _("Print only within range"), wxPoint(8,104), wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX2"));
	CheckBox2->SetValue(false);
	CheckBox3 = new wxCheckBox(this, ID_CHECKBOX3, _("Print in segments"), wxPoint(8,152), wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX3"));
	CheckBox3->SetValue(false);
	StaticText6 = new wxStaticText(this, ID_STATICTEXT6, _("Print only pages:"), wxPoint(264,88), wxDefaultSize, 0, _T("ID_STATICTEXT6"));
	StaticText6->Hide();
	Button2 = new wxButton(this, ID_BUTTON2, _("(None)"), wxPoint(8,8), wxSize(384,34), 0, wxDefaultValidator, _T("ID_BUTTON2"));
	StaticBitmap1 = new wxStaticBitmap(this, ID_STATICBITMAP1, wxArtProvider::GetBitmap(wxART_MAKE_ART_ID_FROM_STR(_T("wxART_FILE_OPEN")),wxART_OTHER), wxPoint(368,16), wxDefaultSize, 0, _T("ID_STATICBITMAP1"));
	StaticBitmap2 = new wxStaticBitmap(this, ID_STATICBITMAP2, wxBitmap(wxImage(_T("./pdf-16x16-cc0.png"))), wxPoint(8,8), wxDefaultSize, 0, _T("ID_STATICBITMAP2"));
	StaticBitmap2->Hide();
	MenuBar1 = new wxMenuBar();
	Menu1 = new wxMenu();
	MenuItem1 = new wxMenuItem(Menu1, idMenuQuit, _("Quit\tAlt-F4"), _("Quit the application"), wxITEM_NORMAL);
	Menu1->Append(MenuItem1);
	MenuBar1->Append(Menu1, _("&File"));
	Menu2 = new wxMenu();
	MenuItem2 = new wxMenuItem(Menu2, idMenuAbout, _("About\tF1"), _("Show info about this application"), wxITEM_NORMAL);
	Menu2->Append(MenuItem2);
	MenuBar1->Append(Menu2, _("Help"));
	SetMenuBar(MenuBar1);

	Connect(ID_BUTTON1,wxEVT_COMMAND_BUTTON_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnButton1Click1);
    Connect(ID_HYPERLINKCTRL3,wxEVT_COMMAND_HYPERLINK,(wxObjectEventFunction)&bookthiefFrame::OnHyperlinkCtrl3Click1);
	Connect(ID_CHECKBOX2,wxEVT_COMMAND_CHECKBOX_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnCheckBox2Click);
	Connect(ID_CHECKBOX3,wxEVT_COMMAND_CHECKBOX_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnCheckBox3Click);
	Connect(ID_BUTTON2,wxEVT_COMMAND_BUTTON_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnButton2Click);
	Connect(idMenuQuit,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnQuit);
	Connect(idMenuAbout,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnAbout);

	if (wxTheApp->argc > 1) {
		if (file_exists(wxTheApp->argv[1]) && has_ending(wxTheApp->argv[1], ".pdf")) {
			infile = wxTheApp->argv[1];
			StaticBitmap2->Show();
			wxString infilenm = infile.substr(infile.find_last_of("/\\") + 1);
			Button2->SetLabel(truncate(infilenm, 40, true));
			wxSetWorkingDirectory(replaced(infilenm, "", infile));
		}
    } else {
        infile = "";
    }

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

	FILE *fp = popen(cmd, "r");
	setvbuf(fp, NULL, _IONBF, 0);

	while (fgets(buf, 5, fp) != NULL) {
		ProgressDialog1->Update(progcounter);


		char *buffer = buf;
		buffer[strlen(buf) - 2] = '\0';

		if (is_number(buffer)) {
			progcounter = atoi(buffer);
			if (firstinput == true) {
				compensate = progcounter;
				progcounter = progcounter + compensate;
			} else {
				progcounter = progcounter + compensate;
			}
			ProgressDialog1->Update(progcounter);
		}
		firstinput = false;
	}

	if (pclose(fp)) {
		return "0";
	}

	ProgressDialog1->Update(100);
	return "Done!";



}


void bookthiefFrame::OnAbout(wxCommandEvent& event)
{
	wxString info = exec("liesel -q", true);
	wxMessageBox(info, "BookThief");
}

void bookthiefFrame::OnButton1Click1(wxCommandEvent& event)
{
	replaceAll("\"", "\\\"", infile);
	replaceAll("\\", "\\\\", infile);

	wxFileDialog saveFileDialog(this, ("Save PDF file"), "", "", "PDF files (*.pdf)|*.pdf", wxFD_SAVE);
	if (saveFileDialog.ShowModal() == wxID_CANCEL) {
		return;
	}

	wxString outfile = saveFileDialog.GetPath();
	replaceAll("\"", "\\\"", outfile);
	replaceAll("\\", "\\\\", outfile);

	if (outfile == "") {
		wxMessageBox("Error: Please pick a file for the new PDF", "BookThief");
		return;
	}

	char command[1024] = "liesel -i \"";

	strcat(command, infile);
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

	strcat(command, " -o \"");
	strcat(command, outfile);
	strcat(command, "\"");
	ProgressDialog1 = new wxProgressDialog(_("BookThief"), _("Building..."), 100, this, wxPD_APP_MODAL|wxPD_AUTO_HIDE);
	wxString messageone = exec(command, false);
	wxMessageBox(messageone, "BookThief");


}

void bookthiefFrame::OnHyperlinkCtrl3Click1(wxCommandEvent& event)
{
	wxMessageBox("This option will produce multiple PDFs\n\n\nNote that there are 4 'pages' to 1 sheet of paper\n(2 pages per side)\n40 'pages' therefore print on 10 sheets of paper", "BookThief");

}

void bookthiefFrame::OnCheckBox2Click(wxCommandEvent& event)
{
		if (CheckBox2->GetValue() == true) {
			StaticText6->Show();
			TextCtrl1->Show();
			TextCtrl2->Show();
			StaticText2->Show();
		} else {
			StaticText6->Hide();
			TextCtrl1->Hide();
			TextCtrl2->Hide();
			StaticText2->Hide();
		}
}

void bookthiefFrame::OnCheckBox3Click(wxCommandEvent& event)
{
	if (CheckBox3->GetValue() == true) {
		SpinCtrl1->Show();
		StaticText1->Show();
		HyperlinkCtrl3->Show();
	} else {
		SpinCtrl1->Hide();
		StaticText1->Hide();
		HyperlinkCtrl3->Hide();
	}
}



void bookthiefFrame::OnButton2Click(wxCommandEvent& event)
{
    wxFileDialog openFileDialog(this, ("Choose PDF file"), "", "", "PDF files (*.pdf)|*.pdf", wxFD_OPEN);
	if (openFileDialog.ShowModal() == wxID_CANCEL) {
        return;
	}
	StaticBitmap2->Show();
	Button2->SetLabel(truncate(openFileDialog.GetFilename(), 40, true));
	infile = openFileDialog.GetPath();
	wxSetWorkingDirectory(openFileDialog.GetDirectory());
}