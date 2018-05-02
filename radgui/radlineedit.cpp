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
												RadLineEdit for GUI

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
#include "radlineedit.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadLineEdit 

Constructor for the RadIde RadLineEdit. RadLineEdit panes come standard with support 
for Lisp call back managers for key strokes.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadLineEdit::RadLineEdit()
	: QLineEdit()
{
	// A RadLineEdit object comes with a default font.
	QFont font("Courier",10,1,false);
	font.setBold(true);
	QLineEdit::setReadOnly(false);
	QLineEdit::setFont(font);

	cpWrpLineEdit = new WrpLineEdit(this,this,"WrpLineEdit");
}

/********************************************************************************************
RadLineEdit::keyReleaseEvent

Manager for the RadIde RadLineEdit keyReleaseEvent.

********************************************************************************************/
void RadLineEdit::keyReleaseEvent(QKeyEvent *e)
{
	int			key = e->key();
	TVAL		argv[1];

	// Manage a Return or Enter key press
	if ((key == Qt::Key_Return)||(key == Qt::Key_Enter)) 
	{
		// If there is a RadTextEdit manager, then call (mgr.fkey key).
		if (cpFKeyMember.Tag != TYVOID)
		{
			// If there is a line edit manager, then call (mgr).
			RADGlue_RunLambdaMember(cpLineEditLambda,cpFKeyMember,0,argv,FALSE);
		}
		else
			if (cpLineEditLambda.Tag != TYVOID)
		{
			RADGlue_RunLambda(cpLineEditLambda, 0, argv, FALSE);
		}


	}
}

/********************************************************************************************
RadLinetEdit::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadLineEdit object.

Examples:
(setq myText (getText:))			;; Get the entire contents of the RadLineEdit display pane.
(selectAll:)						;; Select ALL the RadLineEdit text and highlight in the GUI.
(setText: myText)					;; Set the entire contents of the RadLineEdit display pane.
(setFocus:)							;; Set the RadLineEdit to be the GUI focus.
(setMgr: mgrLambda)					;; Set the RadLineEdit manager - where (defun mgrLambda() ...lisp code...).
(setReadOnly: readOnlySW)			;; Set the RadLineEdit pane to read only mode (TRUE).

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadLineEdit::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadLineEdit: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"getText") == 0)
	{
		// (setq myText (getText:))			;; Get the entire contents of the RadLineEdit display pane.
		/* Get the entire contents of the RadTextEdit display pane */
		textQString = text();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr, "selectAll") == 0)
	{
		// (selectAll:)	;; Select ALL the RadLineEdit text and highlight in the GUI.
		selectAll();

		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr, "setFocus") == 0)
	{
		// (setFocus:)	;; Set the RadLineEdit to be the GUI focus.
		setFocus();

		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr, "setMgr") == 0)
	{
		// (setMgr: mgrLambda)					;; Set the RadLineEdit manager - where (defun mgrLambda() ...lisp code...).
		// Get the name of the RadLineEdit manager Lambda in the main context 
		if (argc!=2)
		{
			*ec = TERROR("!RadLineEdit.setMgr: 1st argument must be Lambda!");
			FrameExit(*ec);
		}

		cpLineEditLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
		cpFKeyMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gpSym_fkey);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setReadOnly") == 0)
	{
		// (setReadOnly: readOnlySW) ;; Set the RadLineEdit pane to read only mode (IFF TRUE).

		/* Invalid message */
		if ((argc<2)||(argv[1].Tag!=TYBOLE))
		{
			*ec = TERROR("!RadLineEdit.setReadOnly: 1st argument must be boolean!");
			FrameExit(*ec);
		}
		
		cpReadOnlySW = argv[1].u.Bool;
		setReadOnly(argv[1].u.Bool);
		goto Last;
	}
	else if (strcmp(messagePtr,"setText") == 0)
	{
		// (setText: myText)					;; Set the entire contents of the RadLineEdit display pane.
		/* Get the entire contents of the RadLineEdit display pane */
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
			*ec = TERROR("!RadLineEdit.setText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadTextEdit display pane.
		// Note: We don't allow HTML in the RadTextEdit like we do in the Demo Window.
		textQString = textPtr;
		textQString = textQString.right(RADMAXLINEEDITSIZE);
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

Lisp object creation method for the RadLineEdit class. Implements the RadIde Line Edit pane. 
RadTextEdit panes come standard with a default fixed font and support for Lisp call back managers 
for key strokes.


Example:
	(setq radLineEditPtr (qt new: RadLineEdit: (Optional)mgrLambda))	;; Create a new RadLineEdit object
					...
	(setq RadLineEditPtr (qt delete: radLineEditPtr)) ;; Delete a RadLineEdit object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadLineEdit:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

	Note: The RadLineEdit class was tested, using the above code, and NO memory leaks were evident.

********************************************************************************************/
TVAL RadLineEdit::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	TVAL			mgrLambda;
	TVAL			result;
	RadLineEdit*	radLineEditPtr = new RadLineEdit();

	// Was an optional manager Lambda argument passed?
	if (argc>0)
		mgrLambda = argv[0];
	else
		mgrLambda.Tag = TYVOID;

	// Initialize the RadLineEdit Lisp call back Lambdas.
	radLineEditPtr->cpLineEditLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gCP->Tval_VOID);
	radLineEditPtr->cpFKeyMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gpSym_fkey);

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radLineEditPtr->cpWrpLineEdit;
	return(result);
}

