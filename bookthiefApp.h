/***************************************************************
 * Name:      bookthiefApp.h
 * Purpose:   Defines Application Class
 * Author:    rail5 (andrew@rail5.org)
 * Created:   2021-08-04
 * Copyright: rail5 (https://rail5.org)
 * License:   GNU GPL v3.0
 **************************************************************/

#ifndef BOOKTHIEFAPP_H
#define BOOKTHIEFAPP_H

#include <wx/app.h>

class bookthiefApp : public wxApp
{
    public:
        virtual bool OnInit();
};

#endif // BOOKTHIEFAPP_H
