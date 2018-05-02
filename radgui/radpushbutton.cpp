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
												RadPushButton for GUI

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
#include "radpushbutton.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadPushButton 

Constructor for the RadIde RadPushButton. RadPushButtons come standard with support 
for Lisp call back managers for mouse press and for rich text.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadPushButton::RadPushButton()
	: QPushButton(NULL)
{
	// A RadLineEdit object comes with a default font.

	cpWrpPushButton = new WrpPushButton(this,(QWidget*)this,"WrpPushButton");
}

/********************************************************************************************
RadPushButton::event

Manager for the RadIde RadPushButton event.

********************************************************************************************/
bool RadPushButton::event(QEvent *e)
{
	bool result = QPushButton::event(e);
	TVAL ret;
	QString lispCommand;


	if (e->type() == QEvent::MouseButtonRelease) 
	{

		// If there is no button manager, then run the command as is.
		if (cpPressMember.Tag != TYVOID)
		{
			// If there is a console manager, then call (buttonMgr).
			RADGlue_RunLambdaMember(cpPushButtonManager,cpPressMember,0,NULL,FALSE);
		}
		else
		if (cpPushButtonManager.Tag != TYVOID)
		{
			RADGlue_RunLambda(cpPushButtonManager, 0, NULL, FALSE);
		}

	}

	return(result);
}

/********************************************************************************************
RadPushButton::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadLineEdit object.

Examples:
(setq myText (getText:))			;; Get the entire contents of the RadPushButton display pane.
(setText: myText)					;; Set the entire contents of the RadPushButton display pane.
(setMgr: mgrLambda)					;; Set the RadPushButton manager - where (defun mgrLambda() ...lisp code...).

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadPushButton::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadPushButton: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"getText") == 0)
	{
		// (setq myText (getText:))			;; Get the entire contents of the RadPushButton display pane.
		/* Get the entire contents of the RadPushButton display pane */
		textQString = text();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"setMgr") == 0)
	{
		// (setMgr: mgrLambda)					;; Set the RadPushButton manager - where (defun mgrLambda() ...lisp code...).
		// Get the name of the RadPushButton manager Lambda in the main context 
		if (argc!=2)
		{
			*ec = TERROR("!RadPushButton.setMgr: 1st argument must be Lambda!");
			FrameExit(*ec);
		}

		cpPushButtonManager = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
		cpPressMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gpSym_press);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setText") == 0)
	{
		// (setText: myText)					;; Set the entire contents of the RadPushButton display pane.
		/* Get the entire contents of the RadPushButton display pane */
		if (argc!=2) goto InvalidSetDisplayTextArgument;
		switch (argv[1].Tag) 
		{
			case TYTEXT: 
				textPtr = argv[1].u.Text; 
				break;
			case TYSTRING:
				textPtr = CharArray(argv[1]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				textPtr = SymbolArray(argv[1]); 
				break;
			default:
			InvalidSetDisplayTextArgument:
			*ec = TERROR("!RadPushButton.setText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadTextEdit display pane.
		// Note: We don't allow HTML in the RadTextEdit like we do in the Demo Window.
		textQString = textPtr;
		textQString = textQString.left(RADMAXPUSHBUTTONSIZE);
		setText(textQString);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
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

Lisp object creation method for the RadPushButton class. Implements the RadIde PushButton. 

Example:
	(setq radPushButtonPtr (qt new: RadPushButton: text manager))	;; Create a new RadPushButton object
					...
	(setq radPushButtonPtr (qt delete: radPushButtonPtr)) ;; Delete a radPushButtonPtr object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadPushButton: title manager))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.
	
	Note: RadPushButton was test, with above code, and NO memory leaks were evident.

********************************************************************************************/
TVAL RadPushButton::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	RadPushButton*	radPushButtonPtr = new RadPushButton();
	LpCHAR			textPtr;
	TVAL			manager;
	TVAL			result;

	// Were text and manager Lambda arguments passed?
	if (argc>1)
	{
		switch(argv[0].Tag)
		{
			case TYTEXT:
			textPtr = &argv[0].u.Text[0];
			break;

			case TYSTRING:
			textPtr = CharArray(argv[0]);
			break;

			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
			textPtr = SymbolArray(argv[0]);
			break;

			default:
			InvalidArgument:
			result = TERROR("!RadPushButton.new: expecting a text and a Lambda argument!");
			return(result);
			break;
		}
	}
	else
		goto InvalidArgument;

	// Initialize the RadPushButton object
	radPushButtonPtr->setText(textPtr);
	radPushButtonPtr->cpPushButtonManager = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
	radPushButtonPtr->cpPressMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,radPushButtonPtr->cpPushButtonManager,gpSym_press);

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radPushButtonPtr->cpWrpPushButton;
	return(result);
}

