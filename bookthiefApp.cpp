/***************************************************************
 * Name:      bookthiefApp.cpp
 * Purpose:   Code for Application Class
 * Author:    rail5 (andrew@rail5.org)
 * Created:   2021-08-04
 * Copyright: rail5 (https://rail5.org)
 * License:
 **************************************************************/

#include "bookthiefApp.h"

//(*AppHeaders
#include "bookthiefMain.h"
#include <wx/image.h>
//*)

IMPLEMENT_APP(bookthiefApp);

bool bookthiefApp::OnInit()
{
    //(*AppInitialize
    bool wxsOK = true;
    wxInitAllImageHandlers();
    
    if ( wxsOK )
    {
    	bookthiefFrame* Frame = new bookthiefFrame(0);
    	Frame->Show();
    	SetTopWindow(Frame);
    }
    //*)
    return wxsOK;

}
