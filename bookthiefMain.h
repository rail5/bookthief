/***************************************************************
 * Name:      bookthiefMain.h
 * Purpose:   Defines Application Frame
 * Author:    rail5 (andrew@rail5.org)
 * Created:   2021-08-04
 * Copyright: rail5 (https://rail5.org)
 * License:   GNU GPL v3.0
 **************************************************************/

#ifndef BOOKTHIEFMAIN_H
#define BOOKTHIEFMAIN_H

//(*Headers(bookthiefFrame)
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/filepicker.h>
#include <wx/frame.h>
#include <wx/hyperlink.h>
#include <wx/menu.h>
#include <wx/progdlg.h>
#include <wx/spinctrl.h>
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
        void OnButton1Click(wxCommandEvent& event);
        void OnChoice1Select(wxCommandEvent& event);
        void OnSpinCtrl1Change(wxSpinEvent& event);
        void OnTextCtrl1Text(wxCommandEvent& event);
        void OnButton1Click1(wxCommandEvent& event);
        void OnCheckBox1Click(wxCommandEvent& event);
        void OnHyperlinkCtrl3Click(wxCommandEvent& event);
        void OnHyperlinkCtrl3Click1(wxCommandEvent& event);
        //*)

        //(*Identifiers(bookthiefFrame)
        static const long ID_BUTTON1;
        static const long ID_HYPERLINKCTRL1;
        static const long ID_FILEPICKERCTRL1;
        static const long ID_CHECKBOX1;
        static const long ID_CHOICE1;
        static const long ID_SPINCTRL1;
        static const long ID_STATICTEXT1;
        static const long ID_TEXTCTRL1;
        static const long ID_TEXTCTRL2;
        static const long ID_STATICTEXT2;
        static const long ID_HYPERLINKCTRL2;
        static const long ID_STATICTEXT3;
        static const long ID_STATICTEXT4;
        static const long ID_DIRPICKERCTRL1;
        static const long ID_STATICTEXT5;
        static const long ID_HYPERLINKCTRL3;
        static const long idMenuQuit;
        static const long idMenuAbout;
        static const long ID_STATUSBAR1;
        static const long ID_PROGRESSDIALOG1;
        //*)

        //(*Declarations(bookthiefFrame)
        wxButton* Button1;
        wxCheckBox* CheckBox1;
        wxChoice* Choice1;
        wxDirPickerCtrl* DirPickerCtrl1;
        wxFilePickerCtrl* FilePickerCtrl1;
        wxHyperlinkCtrl* HyperlinkCtrl1;
        wxHyperlinkCtrl* HyperlinkCtrl2;
        wxHyperlinkCtrl* HyperlinkCtrl3;
        wxProgressDialog* ProgressDialog1;
        wxSpinCtrl* SpinCtrl1;
        wxStaticText* StaticText1;
        wxStaticText* StaticText2;
        wxStaticText* StaticText3;
        wxStaticText* StaticText4;
        wxStaticText* StaticText5;
        wxStatusBar* StatusBar1;
        wxTextCtrl* TextCtrl1;
        wxTextCtrl* TextCtrl2;
        //*)

        DECLARE_EVENT_TABLE()
};

#endif // BOOKTHIEFMAIN_H
