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
												RadLabel for GUI

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
#include "radlabel.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadLabel 

Constructor for the RadIde RadLabel. RadLabel panes come standard with support 
for rich text.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadLabel::RadLabel(const QString &text)
	: QLabel(text,NULL,(Qt::WindowFlags)NULL)
{
	// A RadLabel object comes with a default text contents.
	QLabel::setOpenExternalLinks(true); 
	QFont font("Courier",10,1,false);
	font.setBold(true);
	QLabel::setFont(font);
	setTextFormat(Qt::RichText);
	setTextInteractionFlags(Qt::TextSelectableByMouse);
	cpWrpLabel = new WrpLabel(this,(QWidget*)this,"WrpLabel");
}


/********************************************************************************************
RadLabel::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadLineEdit object.

Examples:
(setq myText (getText:))			;; Get the entire contents of the RadLabel display pane.
(setText: myText)					;; Set the entire contents of the RadLabel display pane.
(setTextFormat: formatInt)			;; Set the text formatting rules for the RadLabel display pane(0=PlainText,1=RichText).

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadLabel::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadLabel: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"getText") == 0)
	{
		// (setq myText (getText:))			;; Get the entire contents of the RadLabel display pane.
		/* Get the entire contents of the RadLabel display pane */
		textQString = text();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"setText") == 0)
	{
		// (setText: myText)					;; Set the entire contents of the RadLabel display pane.
		/* Get the entire contents of the RadLabel display pane */
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
			*ec = TERROR("!RadLabel.setText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadLabel display pane.
		textQString = textPtr;
		textQString = textQString.left(RADMAXTEXTLABELSIZE);
		setText(textQString);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setTextFormat") == 0)
	{
		// (setTextFormat: formatInt)			;; Set the text formatting rules for the RadLabel display pane(0=PlainText,1=RichText).
		if ((argc!=2) || (argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadLabel.setTextFormat: mandatory 2nd argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the text formatting rules for the RadLabel display pane.
		// Note: (0=PlainText,1=RichText).
		setTextFormat((Qt::TextFormat)argv[1].u.Int);

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

Lisp object creation method for the RadLabel class. Implements the RadIde Label pane. 
RadLabel panes come standard with a support for rich text.


Example:
	(setq radLabelPtr (qt new: RadLabel: text))	;; Create a new RadLabel object
					...
	(setq radLabelPtr (qt delete: radLabelPtr)) ;; Delete a radLabelPtr object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadWidget:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.


********************************************************************************************/
TVAL RadLabel::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	LpCHAR			textPtr;
	TVAL			result;

	// (setq radLabelPtr (qt new: RadLabel: text))	;; Create a new RadLabel object
	// Was an optional manager Lambda argument passed?
	if (argc>0)
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
			result = TERROR("!RadLabel.new: 1st argument must be RadLabel text!");
			return(result);
			break;
		}
	}
	else
		goto InvalidArgument;

	// Initialize the RadLabel object
	RadLabel* radLabelPtr = new RadLabel(textPtr);

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radLabelPtr->cpWrpLabel;
	return(result);
}

