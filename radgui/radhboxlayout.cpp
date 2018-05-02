/**********************************************************************************
    Copyright (C) 2014 Michael Korns.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
												RadHBoxLayout for GUI

CHANGE HISTORY
Version	Date		Who		Change
1.0000	8/4/2014	mfk 	First experiments with remote GUI objects.
												 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>
#include <QtGui/QIcon>
#include <QtGui/QMainWindow>

#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QMessageBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QSlider>
#include <QtGui/QSpinBox>
#include <QtGui/QTextDocumentFragment>

#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "radtabwidget.h"
#include "radvboxlayout.h"
#include "radhboxlayout.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadHBoxLayout

Constructor for the RadIde RadHBoxLayout. 

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadHBoxLayout::RadHBoxLayout()
	: QHBoxLayout(NULL)
{
	cpWrpHBoxLayout = new WrpHBoxLayout(this,(QWidget*)this,"WrpHBoxLayout");
}

/********************************************************************************************
RadHBoxLayout::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadLineEdit object.

Examples:
(addLayout: RadLayout))				;; Add the specified RadVBoxLayout or RadHBoxLayout to this RadHBoxLayout.
(addWidget: RadWidget))				;; Add the specified RadWidget to this RadHBoxLayout.
(setStretchFactor: RadWidgetPtr factor))	;; Set the integer stretch factor for specified RadWidget/RadHBoxLayout/RadVBoxLayout derived object.

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadHBoxLayout::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	struct					FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	NUM						n;
	LpCHAR					messagePtr;
	LpCHAR					textPtr;
	QString					textQString;
    QByteArray				textBArray;
	QTextCursor				cursor;
	QTextDocumentFragment	fragment;
	TVAL					prmv[2];
	StartFrame
	DeclareTVAL(ec);
	DeclareTVAL(result);
	EndFrame

	// **********************************************
	// We always return *result at the last section of this function
	// Note: The default return valid is void.
	// **********************************************
    *result = gCP->Tval_VOID;
			
	// **********************************************
	// The first argument must be a symbol message 
	// **********************************************
	if ((argc<1)||((argv[0].Tag!=TYSYMBOL)&&(argv[0].Tag!=TYQUOTEDSYMBOL)))
	{
		InvalidMessage:
		*result = TERROR("!RadHBoxLayout: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"addLayout") == 0)
	{
		// (addLayout: RadLayout))				;; Add the specified RadVBoxLayout or RadHBoxLayout to this RadHBoxLayout.

		// Check for valid arguments 
		if ((argc!=2)||(argv[1].Tag!=TYPOINTER))
		{
			InvalidArgument:
			*ec = TERROR("!RadHBoxLayout.addLayout: 1st argument must be RadVBoxLayout or RadHBoxLayout!");
			FrameExit(*ec);
		}

		// Load and Identify the layout argument
		RadObject* radObjectPtr = (RadObject*)argv[1].u.Pointer;
		if (radObjectPtr->cpWrpClassName == "WrpVBoxLayout")
		{
			RadVBoxLayout* aRadVBoxLayout = (RadVBoxLayout*)radObjectPtr->cpWrpQWidget;
			addLayout((QVBoxLayout*)aRadVBoxLayout);
		} else if (radObjectPtr->cpWrpClassName == "WrpHBoxLayout") 
		{
			RadHBoxLayout* aRadHBoxLayout = (RadHBoxLayout*)radObjectPtr->cpWrpQWidget;
			addLayout((QHBoxLayout*)aRadHBoxLayout);
		} else 
			goto InvalidArgument;

		goto Last;
	}
	else if (strcmp(messagePtr,"addWidget") == 0)
	{
		// (addWidget: RadWidget))				;; Add the specified RadWidget to this RadHBoxLayout.

		// check for valid arguments 
		if ((argc!=2)||(argv[1].Tag!=TYPOINTER))
		{
			*ec = TERROR("!RadHBoxLayout.addWidget: 1st argument must be RadWidget derived class!");
			FrameExit(*ec);
		}

		// Load and Identify the layout argument
		RadObject* radObjectPtr = (RadObject*)argv[1].u.Pointer;
		RadWidget* aRadWidget = (RadWidget*)radObjectPtr->cpWrpQWidget;
		addWidget((QWidget*)aRadWidget);

		goto Last;
	}
	else if (strcmp(messagePtr,"setStretchFactor") == 0)
	{
		// (setStretchFactor: RadWidgetPtr factor))	;; Set the integer stretch factor for specified RadWidget/RadHBoxLayout/RadVBoxLayout derived object.

		// check for valid arguments 
		if ((argc<2)||(argv[1].Tag!=TYPOINTER))
		{
			*ec = TERROR("!RadHBoxLayout.setStretchFactor: 1st argument must be RadWidget/RadHBoxLayout/RadVBoxLayout derived class!");
			FrameExit(*ec);
		}
		else if ((argc<3)||(argv[2].Tag!=TYNUM))
		{
			*ec = TERROR("!RadHBoxLayout.setStretchFactor: 2nd argument must be an Integer stretch factor!");
			FrameExit(*ec);
		}

		// Load and Identify the layout argument
		RadObject* radObjectPtr = (RadObject*)argv[1].u.Pointer;
		if (radObjectPtr->cpWrpClassName == "WrpVBoxLayout")
		{
			RadVBoxLayout* aRadVBoxLayout = (RadVBoxLayout*)radObjectPtr->cpWrpQWidget;
			setStretchFactor(aRadVBoxLayout,argv[2].u.Int);
		}
		else
		if (radObjectPtr->cpWrpClassName == "WrpHBoxLayout")
		{
			RadHBoxLayout* aRadHBoxLayout = (RadHBoxLayout*)radObjectPtr->cpWrpQWidget;
			setStretchFactor(aRadHBoxLayout,argv[2].u.Int);
		}
		else
		{
			RadWidget* aRadWidget = (RadWidget*)radObjectPtr->cpWrpQWidget;
			setStretchFactor((QWidget*)aRadWidget,argv[2].u.Int);
		}

		goto Last;
	}
	else
	{
		/* Invalid message */
		goto InvalidMessage;
	}

	Last:
	Funcs->_Host_Escape = RADGlue_Escape;
	RADGlue_ProcessEvents(gCP,gTP);
	FrameExit(*result);
}

/********************************************************************************************
lispNew

Lisp object creation method for the RadHBoxLayout class. Implements the RadIde RadHBoxLayout. 

Example:
	(setq radHBoxLayoutPtr (qt new: RadHBoxLayout:))	;; Create a new RadHBoxLayout object
					...
	(setq radHBoxLayoutPtr (qt delete: radHBoxLayoutPtr)) ;; Delete a radHBoxLayoutPtr object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadWidget:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.


********************************************************************************************/
TVAL RadHBoxLayout::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	RadHBoxLayout*	radHBoxLayoutPtr = new RadHBoxLayout();
	LpCHAR			textPtr;
	TVAL			manager;
	TVAL			result;

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radHBoxLayoutPtr->cpWrpHBoxLayout;
	return(result);
}

