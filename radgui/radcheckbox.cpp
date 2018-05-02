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
												RadCheckBox for GUI

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
#include <QtGui/QCheckBox>

#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "radcheckbox.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadCheckBox 

Constructor for the RadIde RadCheckBox.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadCheckBox::RadCheckBox()
	: QCheckBox(NULL)
{
	cpWrpCheckBox = new WrpCheckBox(this,(QWidget*)this,"WrpCheckBox");
}

/********************************************************************************************
RadCheckBox::event

Manager for the RadIde RadCheckBox event.

********************************************************************************************/
bool RadCheckBox::event(QEvent *e)
{
	bool result = QCheckBox::event(e);
	TVAL ret;

	if (e->type() == QEvent::MouseButtonRelease) 
	{
		// If there is a check box manager AND we are responsive, then call it.
		if ((cpResponsiveSW == TRUE) && (cpPressMember.Tag != TYVOID))
		{
			RADGlue_RunLambdaMember(cpCheckBoxManager, cpPressMember, 0, NULL, FALSE);
		}
		else
		if ((cpResponsiveSW == TRUE) && (cpCheckBoxManager.Tag != TYVOID))
		{
			RADGlue_RunLambda(cpCheckBoxManager, 0, NULL, FALSE);
		}
	}

	return(result);
}

/********************************************************************************************
RadCheckBox::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadCheckBox object.

Examples:
(setq state (getCheckState:))		;; Get the checked state of the RadCheckBox (0=unchecked,1=partially checked,2=checked).
(setq myText (getText:))			;; Get the entire contents of the RadCheckBox display pane.
(setCheckState: state)				;; Set the checked state of the RadCheckBox (0=unchecked,1=partially checked,2=checked).
(setText: myText)					;; Set the entire contents of the RadCheckBox display pane.
(setMgr: mgrLambda)					;; Set the RadCheckBox manager - where (defun mgrLambda() ...lisp code...).

Programmer Notes:
These Lisp messages were meant for RadCheckBox objects returned from (setq aRadCheckBox (qt new: RadCheckBox: text manager)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadCheckBox::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadCheckBox: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"getCheckState") == 0)
	{
		// (setq state (getCheckState:))		;; Get the checked state of the RadCheckBox (0=unchecked,1=partially checked,2=checked).
		result->Tag = TYNUM;
		result->u.Int = (Qt::CheckState)checkState();
		goto Last;
	}
	else if (strcmp(messagePtr,"getText") == 0)
	{
		// (setq myText (getText:))			;; Get the entire contents of the RadCheckBox display pane.
		textQString = text();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"setCheckState") == 0)
	{
		// (setCheckState: state)				;; Set the checked state of the RadCheckBox (0=unchecked,1=partially checked,2=checked).
		if ((argc!=2)||(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadCheckBox.setCheckState: mandatory 2nd argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the check state of the RadCheckBox.
		cpResponsiveSW = FALSE;
		setCheckState((Qt::CheckState)argv[1].u.Int);
		cpResponsiveSW = TRUE;

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setMgr") == 0)
	{
		// (setMgr: mgrLambda)					;; Set the RadCheckBox manager - where (defun mgrLambda() ...lisp code...).
		if (argc!=2)
		{
			*ec = TERROR("!RadCheckBox.setMgr: 1st argument must be Lambda!");
			FrameExit(*ec);
		}

		cpCheckBoxManager = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
		cpPressMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gpSym_press);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setText") == 0)
	{
		// (setText: myText)					;; Set the entire contents of the RadCheckBox display pane.
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
			*ec = TERROR("!RadCheckBox.setText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadCheckBox display pane.
		textQString = textPtr;
		textQString = textQString.left(RADMAXCHECKBOXSIZE);
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

Lisp object creation method for the RadCheckBox class.

Example:
	(setq radCheckBoxPtr (qt new: RadCheckBox: text manager))	;; Create a new RadCheckBox object
					...
	(setq radCheckBoxPtr (qt delete: radCheckBoxPtr)) ;; Delete a radCheckBoxPtr object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadCheckBox: title manager))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

	Note: RadCheckBox tested, with above code, and NO memory leaks are evident.


********************************************************************************************/
TVAL RadCheckBox::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	RadCheckBox*	radCheckBoxPtr = new RadCheckBox();
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
			result = TERROR("!RadCheckBox.new: expecting a text and a Lambda argument!");
			return(result);
			break;
		}
	}
	else
		goto InvalidArgument;

	// Initialize the RadCheckBox object
	radCheckBoxPtr->setText(textPtr);
	radCheckBoxPtr->cpCheckBoxManager = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
	radCheckBoxPtr->cpPressMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gpSym_press);
	radCheckBoxPtr->cpResponsiveSW = true;


	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radCheckBoxPtr->cpWrpCheckBox;
	return(result);
}

