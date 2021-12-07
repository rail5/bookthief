/***************************************************************
 * Name:	  bookthiefMain.h
 * Purpose:   Defines Application Frame
 * Author:	rail5 (andrew@rail5.org)
 * Created:   2021-10-10
 * Copyright: rail5 (https://rail5.org)
 * License:
 **************************************************************/

#ifndef bookthiefMAIN_H
#define bookthiefMAIN_H

//(*Headers(bookthiefFrame)
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/filepicker.h>
#include <wx/frame.h>
#include <wx/hyperlink.h>
#include <wx/menu.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
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
		void OnCheckBox2Click(wxCommandEvent& event);
		void OnFilePickerCtrl1FileChanged(wxFileDirPickerEvent& event);
		void OnButton2Click(wxCommandEvent& event);
		void OnCheckBox3Click(wxCommandEvent& event);
		void OnCheckBox4Click(wxCommandEvent& event);
		std::string exec(std::string cmd, bool nonprogress);
		wxString gencommand();
		void OnExport(wxCommandEvent& event);
		void OnLink1(wxCommandEvent& event);
		void OnLink2(wxCommandEvent& event);
		//*)

		//(*Identifiers(bookthiefFrame)
		static const long ID_FILEPICKERCTRL1;
		static const long ID_CHECKBOX1;
		static const long ID_CHECKBOX2;
		static const long ID_STATICTEXT1;
		static const long ID_TEXTCTRL1;
		static const long ID_STATICTEXT2;
		static const long ID_TEXTCTRL2;
		static const long ID_CHECKBOX3;
		static const long ID_SPINCTRL1;
		static const long ID_STATICTEXT3;
		static const long ID_HYPERLINKCTRL1;
		static const long ID_CHECKBOX4;
		static const long ID_CHOICE1;
		static const long ID_STATICTEXT4;
		static const long ID_SLIDER1;
		static const long ID_BUTTON2;
		static const long ID_MENUITEM1;
		static const long idMenuAbout;
		static const long idMenuExport;
		static const long idMenuLink1;
		static const long idMenuLink2;
		static const long ID_PROGRESSDIALOG1;
		//*)

		//(*Declarations(bookthiefFrame)
		wxButton* Button2;
		wxCheckBox* CheckBox1;
		wxCheckBox* CheckBox2;
		wxCheckBox* CheckBox3;
		wxCheckBox* CheckBox4;
		wxChoice* Choice1;
		wxFilePickerCtrl* FilePickerCtrl1;
		wxHyperlinkCtrl* HyperlinkCtrl1;
		wxProgressDialog* ProgressDialog1;
		wxSlider* Slider1;
		wxSpinCtrl* SpinCtrl1;
		wxStaticText* StaticText1;
		wxStaticText* StaticText2;
		wxStaticText* StaticText3;
		wxStaticText* StaticText4;
		wxTextCtrl* TextCtrl1;
		wxTextCtrl* TextCtrl2;
		//*)

		DECLARE_EVENT_TABLE()
};

#endif // bookthiefMAIN_H
