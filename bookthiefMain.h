/***************************************************************
 * Name:	  bookthiefMain.h
 * Purpose:   Defines Application Frame
 * Author:	rail5 (andrew@rail5.org)
 * Created:   2021-10-05
 * Copyright: rail5 (https://rail5.org)
 * License:   GNU GPL v3.0
 **************************************************************/

#ifndef BOOKTHIEFMAIN_H
#define BOOKTHIEFMAIN_H

//(*Headers(bookthiefFrame)
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/frame.h>
#include <wx/hyperlink.h>
#include <wx/menu.h>
#include <wx/progdlg.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/statbmp.h>
#include <wx/stattext.h>
#include <wx/statusbr.h>
#include <wx/textctrl.h>
//*)

class bookthiefFrame: public wxFrame
{
	public:

		bookthiefFrame(wxWindow* parent,wxWindowID id = -1);
		virtual ~bookthiefFrame();

	private:

		//(*Handlers(bookthiefFrame)
		void OnQuit(wxCommandEvent& event);
		void OnAbout(wxCommandEvent& event);
		void OnExport(wxCommandEvent& event);
		void OnButton1Click(wxCommandEvent& event);
		void OnChoice1Select(wxCommandEvent& event);
		void OnSpinCtrl1Change(wxSpinEvent& event);
		void OnButton1Click1(wxCommandEvent& event);
		void OnHyperlinkCtrl3Click(wxCommandEvent& event);
		void OnHyperlinkCtrl3Click1(wxCommandEvent& event);
		void OnCheckBox2Click(wxCommandEvent& event);
		void OnCheckBox3Click(wxCommandEvent& event);
		std::string exec(const char* cmd, bool nonprogress = false);
		void OnButton2Click(wxCommandEvent& event);
		void OnCheckBox4Click(wxCommandEvent& event);
		wxString gencommand();
		//*)

		//(*Identifiers(bookthiefFrame)
		static const long ID_BUTTON1;
		static const long ID_HYPERLINKCTRL1;
		static const long ID_CHECKBOX1;
		static const long ID_SPINCTRL1;
		static const long ID_STATICTEXT1;
		static const long ID_TEXTCTRL1;
		static const long ID_TEXTCTRL2;
		static const long ID_STATICTEXT2;
		static const long ID_HYPERLINKCTRL2;
		static const long ID_STATICTEXT3;
		static const long ID_STATICTEXT4;
		static const long ID_HYPERLINKCTRL3;
		static const long ID_CHECKBOX2;
		static const long ID_CHECKBOX3;
		static const long ID_STATICTEXT6;
		static const long ID_BUTTON2;
		static const long ID_STATICBITMAP1;
		static const long ID_STATICBITMAP2;
		static const long ID_CHECKBOX4;
		static const long ID_CHOICE1;
		static const long ID_SLIDER1;
		static const long ID_STATICTEXT5;
		static const long ID_STATICTEXT7;
		static const long ID_STATICTEXT8;
		static const long ID_STATICTEXT9;
		static const long idMenuQuit;
		static const long idMenuAbout;
		static const long idMenuExport;
		static const long ID_STATUSBAR1;
		static const long ID_PROGRESSDIALOG1;
		//*)

		//(*Declarations(bookthiefFrame)
		wxButton* Button1;
		wxButton* Button2;
		wxCheckBox* CheckBox1;
		wxCheckBox* CheckBox2;
		wxCheckBox* CheckBox3;
		wxCheckBox* CheckBox4;
		wxChoice* Choice1;
		wxHyperlinkCtrl* HyperlinkCtrl1;
		wxHyperlinkCtrl* HyperlinkCtrl2;
		wxHyperlinkCtrl* HyperlinkCtrl3;
		wxProgressDialog* ProgressDialog1;
		wxSlider* Slider1;
		wxSpinCtrl* SpinCtrl1;
		wxStaticBitmap* StaticBitmap1;
		wxStaticBitmap* StaticBitmap2;
		wxStaticText* StaticText1;
		wxStaticText* StaticText2;
		wxStaticText* StaticText3;
		wxStaticText* StaticText4;
		wxStaticText* StaticText5;
		wxStaticText* StaticText6;
		wxStaticText* StaticText7;
		wxStaticText* StaticText8;
		wxStaticText* StaticText9;
		wxStatusBar* StatusBar1;
		wxTextCtrl* TextCtrl1;
		wxTextCtrl* TextCtrl2;
		//*)

		DECLARE_EVENT_TABLE()
};

#endif // BOOKTHIEFMAIN_H
