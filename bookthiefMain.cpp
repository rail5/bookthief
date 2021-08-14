/***************************************************************
 * Name:      bookthiefMain.cpp
 * Purpose:   Code for Application Frame
 * Author:    rail5 (andrew@rail5.org)
 * Created:   2021-08-14
 * Copyright: rail5 (https://rail5.org)
 * License:   GNU GPL v3.0
 **************************************************************/

#include "bookthiefMain.h"
#include <wx/msgdlg.h>
#include <string.h>
#include <math.h>
#include <memory>
#include <stdexcept>
#include <array>

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
const long bookthiefFrame::ID_FILEPICKERCTRL1 = wxNewId();
const long bookthiefFrame::ID_CHECKBOX1 = wxNewId();
const long bookthiefFrame::ID_CHOICE1 = wxNewId();
const long bookthiefFrame::ID_SPINCTRL1 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT1 = wxNewId();
const long bookthiefFrame::ID_TEXTCTRL1 = wxNewId();
const long bookthiefFrame::ID_TEXTCTRL2 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT2 = wxNewId();
const long bookthiefFrame::ID_HYPERLINKCTRL2 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT3 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT4 = wxNewId();
const long bookthiefFrame::ID_DIRPICKERCTRL1 = wxNewId();
const long bookthiefFrame::ID_STATICTEXT5 = wxNewId();
const long bookthiefFrame::ID_HYPERLINKCTRL3 = wxNewId();
const long bookthiefFrame::idMenuQuit = wxNewId();
const long bookthiefFrame::idMenuAbout = wxNewId();
const long bookthiefFrame::ID_STATUSBAR1 = wxNewId();
const long bookthiefFrame::ID_PROGRESSDIALOG1 = wxNewId();
//*)

BEGIN_EVENT_TABLE(bookthiefFrame,wxFrame)
    //(*EventTable(bookthiefFrame)
    //*)
END_EVENT_TABLE()

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
    Button1 = new wxButton(this, ID_BUTTON1, _("Build"), wxPoint(152,200), wxDefaultSize, 0, wxDefaultValidator, _T("ID_BUTTON1"));
    HyperlinkCtrl1 = new wxHyperlinkCtrl(this, ID_HYPERLINKCTRL1, _("Library Genesis"), _("http://gen.lib.rus.ec/"), wxPoint(96,256), wxDefaultSize, wxHL_CONTEXTMENU|wxHL_ALIGN_CENTRE, _T("ID_HYPERLINKCTRL1"));
    FilePickerCtrl1 = new wxFilePickerCtrl(this, ID_FILEPICKERCTRL1, wxEmptyString, wxEmptyString, wxEmptyString, wxPoint(8,8), wxSize(384,34), wxFLP_FILE_MUST_EXIST|wxFLP_OPEN, wxDefaultValidator, _T("ID_FILEPICKERCTRL1"));
    CheckBox1 = new wxCheckBox(this, ID_CHECKBOX1, _("Convert to grayscale"), wxPoint(8,96), wxDefaultSize, 0, wxDefaultValidator, _T("ID_CHECKBOX1"));
    CheckBox1->SetValue(false);
    Choice1 = new wxChoice(this, ID_CHOICE1, wxPoint(8,56), wxSize(208,30), 0, 0, 0, wxDefaultValidator, _T("ID_CHOICE1"));
    Choice1->SetSelection( Choice1->Append(_("Print entire book")) );
    Choice1->Append(_("Print book in segments"));
    Choice1->Append(_("Print only within range"));
    SpinCtrl1 = new wxSpinCtrl(this, ID_SPINCTRL1, _T("40"), wxPoint(248,56), wxSize(136,34), 0, 0, 100, 40, _T("ID_SPINCTRL1"));
    SpinCtrl1->SetValue(_T("40"));
    SpinCtrl1->Hide();
    StaticText1 = new wxStaticText(this, ID_STATICTEXT1, _("Pages per segment"), wxPoint(248,96), wxDefaultSize, 0, _T("ID_STATICTEXT1"));
    StaticText1->Hide();
    TextCtrl1 = new wxTextCtrl(this, ID_TEXTCTRL1, _("1"), wxPoint(248,56), wxSize(56,27), 0, wxDefaultValidator, _T("ID_TEXTCTRL1"));
    TextCtrl1->Hide();
    TextCtrl2 = new wxTextCtrl(this, ID_TEXTCTRL2, _("10"), wxPoint(336,56), wxSize(48,27), 0, wxDefaultValidator, _T("ID_TEXTCTRL2"));
    TextCtrl2->Hide();
    StaticText2 = new wxStaticText(this, ID_STATICTEXT2, _("to"), wxPoint(312,64), wxDefaultSize, 0, _T("ID_STATICTEXT2"));
    StaticText2->Hide();
    HyperlinkCtrl2 = new wxHyperlinkCtrl(this, ID_HYPERLINKCTRL2, _("ZLibrary"), _("https://1lib.domains"), wxPoint(216,256), wxDefaultSize, wxHL_CONTEXTMENU|wxHL_ALIGN_CENTRE, _T("ID_HYPERLINKCTRL2"));
    StaticText3 = new wxStaticText(this, ID_STATICTEXT3, _("Looking for books\? Try:"), wxPoint(128,248), wxDefaultSize, 0, _T("ID_STATICTEXT3"));
    StaticText4 = new wxStaticText(this, ID_STATICTEXT4, _("or"), wxPoint(208,264), wxDefaultSize, 0, _T("ID_STATICTEXT4"));
    DirPickerCtrl1 = new wxDirPickerCtrl(this, ID_DIRPICKERCTRL1, wxEmptyString, wxEmptyString, wxPoint(8,160), wxDefaultSize, wxDIRP_DIR_MUST_EXIST, wxDefaultValidator, _T("ID_DIRPICKERCTRL1"));
    StaticText5 = new wxStaticText(this, ID_STATICTEXT5, _("Save in folder:"), wxPoint(8,144), wxDefaultSize, 0, _T("ID_STATICTEXT5"));
    HyperlinkCtrl3 = new wxHyperlinkCtrl(this, ID_HYPERLINKCTRL3, _("\?"), wxEmptyString, wxPoint(280,104), wxDefaultSize, wxHL_CONTEXTMENU|wxHL_ALIGN_CENTRE, _T("ID_HYPERLINKCTRL3"));
    HyperlinkCtrl3->Hide();
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
    StatusBar1 = new wxStatusBar(this, ID_STATUSBAR1, 0, _T("ID_STATUSBAR1"));
    int __wxStatusBarWidths_1[1] = { -1 };
    int __wxStatusBarStyles_1[1] = { wxSB_NORMAL };
    StatusBar1->SetFieldsCount(1,__wxStatusBarWidths_1);
    StatusBar1->SetStatusStyles(1,__wxStatusBarStyles_1);
    SetStatusBar(StatusBar1);

    Connect(ID_BUTTON1,wxEVT_COMMAND_BUTTON_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnButton1Click1);
    Connect(ID_CHECKBOX1,wxEVT_COMMAND_CHECKBOX_CLICKED,(wxObjectEventFunction)&bookthiefFrame::OnCheckBox1Click);
    Connect(ID_CHOICE1,wxEVT_COMMAND_CHOICE_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnChoice1Select);
    Connect(ID_TEXTCTRL1,wxEVT_COMMAND_TEXT_UPDATED,(wxObjectEventFunction)&bookthiefFrame::OnTextCtrl1Text);
    Connect(ID_HYPERLINKCTRL3,wxEVT_COMMAND_HYPERLINK,(wxObjectEventFunction)&bookthiefFrame::OnHyperlinkCtrl3Click1);
    Connect(idMenuQuit,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnQuit);
    Connect(idMenuAbout,wxEVT_COMMAND_MENU_SELECTED,(wxObjectEventFunction)&bookthiefFrame::OnAbout);
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

void bookthiefFrame::OnAbout(wxCommandEvent& event)
{
    wxMessageBox("BookThief is free software, distributed under the GNU GPL v3.0 License\r\nYou are free to use, modify, and redistribute this program under certain conditions\r\nBasically: as long as you keep it free.", "BookThief");
}
int style = 0;
void bookthiefFrame::OnChoice1Select(wxCommandEvent& event)
{
    if (event.GetString() == "Print book in segments") {
        TextCtrl1->Hide();
        TextCtrl2->Hide();
        StaticText2->Hide();
        SpinCtrl1->Show();
        StaticText1->Show();
        HyperlinkCtrl3->Show();
        style = 2;
    } else if (event.GetString() == "Print only within range") {
        SpinCtrl1->Hide();
        StaticText1->Hide();
        TextCtrl1->Show();
        TextCtrl2->Show();
        StaticText2->Show();
        HyperlinkCtrl3->Hide();
        style = 1;
    } else {
        SpinCtrl1->Hide();
        StaticText1->Hide();
        TextCtrl1->Hide();
        TextCtrl2->Hide();
        StaticText2->Hide();
        HyperlinkCtrl3->Hide();
        style = 0;
    }

}

void bookthiefFrame::OnSpinCtrl1Change(wxSpinEvent& event)
{
}

void bookthiefFrame::OnTextCtrl1Text(wxCommandEvent& event)
{
}

std::string exec(const char* cmd) {
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


char* itoa(int value, char* result, int base) {
        /**
	 * C++ version 0.4 char* style "itoa":
	 * Written by Lukás Chmela
	 * Released under GPLv3.

	 */
	// check that the base if valid
    if (base < 2 || base > 36) { *result = '\0'; return result; }

    char* ptr = result, *ptr1 = result, tmp_char;
    int tmp_value;

    do {
        tmp_value = value;
        value /= base;
        *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqrstuvwxyz" [35 + (tmp_value - value * base)];
    } while ( value );

    // Apply negative sign
    if (tmp_value < 0) *ptr++ = '-';
    *ptr-- = '\0';
    while(ptr1 < ptr) {
        tmp_char = *ptr;
        *ptr--= *ptr1;
        *ptr1++ = tmp_char;
    }
    return result;
}

void replaceAll(const wxString& from, const wxString& to, wxString& source)
{
    wxString newString;
    newString.reserve(source.length());  //avoids a few memory allocations

    wxString::size_type lastPos = 0;
    wxString::size_type findPos;

    while(wxString::npos != (findPos = source.find(from, lastPos)))
    {
        newString.append(source, lastPos, findPos - lastPos);
        newString += to;
        lastPos = findPos + from.length();
    }

    newString += source.substr(lastPos);

    source.swap(newString);
}


void bookthiefFrame::OnButton1Click1(wxCommandEvent& event)
{
    wxString infile = FilePickerCtrl1->GetPath();
    replaceAll("\"", "\\\"", infile);

    wxString outfile = DirPickerCtrl1->GetPath();
    replaceAll("\"", "\\\"", outfile);
    if (outfile == "") {
        wxMessageBox("Error: Please pick a folder to save the new PDF in", "BookThief");
        return;
    }

    wxString finalfile = FilePickerCtrl1->GetPath().substr(FilePickerCtrl1->GetPath().find_last_of("/\\") + 1);
    replaceAll("\"", "\\\"", finalfile);

    char command[1024] = "liesel -i \"";
    strcat(command, infile);
    strcat(command, "\"");

    if (CheckBox1->IsChecked()  == true) {
        strcat(command, " -g");
    }

    if (style == 1) {
        strcat(command, " -r ");
        strcat(command, TextCtrl1->GetValue());
        strcat(command, "-");
        strcat(command, TextCtrl2->GetValue());
        strcat(command, " -o \"");
        strcat(command, outfile);
        strcat(command, "/");
        strcat(command, finalfile);
        strcat(command, ".bookthief.pdf\"");
        wxString messageone = exec(command);
        wxMessageBox(messageone, "BookThief");
    } else if (style == 2) {
        char checkpages[1024] = "liesel -p \"";
        strcat(checkpages, infile);
        strcat(checkpages, "\"");
        const char* messagetwo = exec(checkpages).c_str();
        int pagecount = atoi(messagetwo);

        int segsizechanged = 0;
        int oldsegsize;

        if (SpinCtrl1->GetValue() > pagecount) {
            segsizechanged = 1;
            oldsegsize = SpinCtrl1->GetValue();
            SpinCtrl1->SetValue(pagecount);
        }
        char segm[33];
        double segmint = (double)pagecount/SpinCtrl1->GetValue();
        int segman = ceil(segmint);
        int finsegman = pagecount % SpinCtrl1->GetValue();
        if (finsegman == 0) {
            finsegman = SpinCtrl1->GetValue();
        } else if (finsegman < 4) {
            segman = segman - 1;
            finsegman = finsegman + SpinCtrl1->GetValue();
        }
        char finseg[33];
        itoa(finsegman, finseg, 10);

        itoa(segman, segm, 10);

        ProgressDialog1 = new wxProgressDialog(_("BookThief"), _("Building..."), 100, this, wxPD_APP_MODAL|wxPD_AUTO_HIDE);
        int i = 1;
        while (i <= segman) {

            int firstpage;
            int lastpage;

            if (i == 1) {
                firstpage = 1;
                lastpage = firstpage+(SpinCtrl1->GetValue()-1);
            } else if (i > 1 && i < segman) {
                firstpage = (SpinCtrl1->GetValue()*(i-1))+1;
                lastpage = firstpage+(SpinCtrl1->GetValue()-1);
            } else if (i == segman) {
                firstpage = (SpinCtrl1->GetValue()*(i-1))+1;
                lastpage = firstpage+(finsegman-1);
            }

            char firstpgch[33];
            char lastpgch[33];
            char segnumber[33];
            itoa(firstpage, firstpgch, 10);
            itoa(lastpage, lastpgch, 10);
            itoa(i, segnumber, 10);
            wxString newcomd = (wxString)command + " -r " + (wxString)firstpgch + "-" + (wxString)lastpgch;
            newcomd = newcomd + " -o \"" + outfile + "/" + finalfile + ".bookthief-part" + (wxString)segnumber + ".pdf\"";
            char fincomd[1024];
            strncpy(fincomd, (const char*)newcomd.mb_str(wxConvUTF8), 1023);

            double progsofar = (double)100/segman;
            int progconvert = floor(progsofar);
            progconvert = progconvert*i;

            wxString messagethree = exec(fincomd);
            ProgressDialog1->Update(progconvert);
            i++;

        }
        if (segsizechanged == 1) {
            SpinCtrl1->SetValue(oldsegsize); //So the users aren't too bugged by us lowering their input values in the spinctrl
        }
        ProgressDialog1->Update(100);
        wxMessageBox("Done", "BookThief");
    } else if (style == 0) {
        strcat(command, " -o \"");
        strcat(command, outfile);
        strcat(command, "/");
        strcat(command, finalfile);
        strcat(command, ".bookthief.pdf\"");
        wxString messagefour = exec(command);
        wxMessageBox(messagefour, "BookThief");
    } else {
        wxMessageBox("error", "BookThief");
    }


}

void bookthiefFrame::OnHyperlinkCtrl3Click1(wxCommandEvent& event)
{
    wxMessageBox("Note that there are 4 'pages' to 1 sheet of paper\r\n(2 pages per side)\r\n40 'pages' therefore print on 10 sheets of paper", "BookThief");

}
void bookthiefFrame::OnCheckBox1Click(wxCommandEvent& event)
{
}
