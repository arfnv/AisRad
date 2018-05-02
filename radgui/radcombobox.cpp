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
#include <QtGui/QComboBox>

#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "radlineedit.h"
#include "radcombobox.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadComboBox 

Constructor for the RadIde RadComboBox. 

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadComboBox::RadComboBox()
	: QComboBox(NULL)
{
	cpWrpComboBox = new WrpComboBox(this,this,"WrpComboBox");
}

/********************************************************************************************
RadComboBox::keyReleaseEvent

Manager for the RadIde RadComboBox keyReleaseEvent.

********************************************************************************************/
void RadComboBox::keyReleaseEvent(QKeyEvent *e)
{
	int			key = e->key();
	TVAL		argv[1];

	// Manage a Return or Enter key press
	if ((key == Qt::Key_Return)||(key == Qt::Key_Enter)) 
	{
		// If there is a RadComboBox manager, then call it.
		if (cpFKeyMember.Tag != TYVOID)
		{
			RADGlue_RunLambdaMember(cpComboBoxLambda,cpFKeyMember,0,argv,FALSE);
		}
		else
			if (cpComboBoxLambda.Tag != TYVOID)
		{
			RADGlue_RunLambda(cpComboBoxLambda, 0, argv, FALSE);
		}
	}
}

/********************************************************************************************
RadComboBox::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadLineEdit object.

Examples:
(addItem: "Text")					;; Adds the specified text to the item list of the RadComboBox.
(setq myText (getItemText: index))	;; Get the text of the specified of the RadComboBox item list.
(setq myText (getText:))			;; Get the entire contents of the RadComboBox line edit.
(removeItem: index)					;; Remove the specified item from the RadComboBox item list.
(setFocus:)							;; Set the RadComboBox to be the GUI focus.
(setItemText: index "Text")			;; Sets the specified text in the item list of the RadComboBox.
(setMgr: mgrLambda)					;; Set the RadComboBox manager - where (defun mgrLambda() ...lisp code...).
(setReadOnly: readOnlySW)			;; Set the RadComboBox line edit to read only mode (TRUE).
(setText: myText)					;; Set the entire contents of the RadComboBox line edit.

Programmer Notes:
These Lisp messages were meant for RadComboBox objects returned from (setq aRadComboBox (qt new: RadComboBox:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadComboBox::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadComboBox: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"addItem") == 0)
	{
		// (addItem: "Text")		;; Adds the specified text to the item list of the RadComboBox.
		if (argc!=2) goto InvalidSetAddItemTextArgument;
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
			InvalidSetAddItemTextArgument:
			*ec = TERROR("!RadComboBoxt.addItem: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Add an item to the contents of the RadComboBox combo list.
		textQString = textPtr;
		textQString = textQString.right(RADMAXLINEEDITSIZE);
		addItem(textQString);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"getItemText") == 0)
	{
		// (setq myText (getItemText: index))		;; Get the text of the specified of the RadComboBox item list.
		if ((argc!=2)||(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadComboBoxt.getItemText: mandatory 2nd argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		textQString = itemText(argv[1].u.Int);
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"getText") == 0)
	{
		// (setq myText (getText:))			;; Get the entire contents of the RadComboBox line edit.
		textQString = lineEdit()->text();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"removeItem") == 0)
	{
		// (removeItem: index)		;; Remove the specified item from the RadComboBox item list.
		if ((argc!=2)||(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadComboBox.removeItem: mandatory 2nd argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Remove an item from the contents of the RadComboBox item list.
		removeItem(argv[1].u.Int);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setFocus") == 0)
	{
		// (setFocus:)				;; Set the RadComboBox to be the GUI focus.
		setFocus();

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setItemText") == 0) 
	{
		// //(setItemText: index "Text")			;; Sets the specified text in the item list of the RadComboBox.
		if ((argc!=3)||(argv[1].Tag!=TYNUM)) goto InvalidSetItemTextArgument;
		switch (argv[2].Tag) 
		{
			case TYTEXT: 
				textPtr = argv[2].u.Text; 
				break;
			case TYSTRING:
				textPtr = CharArray(argv[2]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				textPtr = SymbolArray(argv[2]); 
				break;
			default:
			InvalidSetItemTextArgument:
			*ec = TERROR("!RadComboBoxt.setItemText: expecting and 2nd Integer argument and a 3rd argument of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Add an item to the contents of the RadComboBox combo list.
		textQString = textPtr;
		textQString = textQString.right(RADMAXLINEEDITSIZE);
		addItem(textQString);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setMgr") == 0)
	{
		// (setMgr: mgrLambda)		;; Set the RadComboBox manager - where (defun mgrLambda() ...lisp code...).
		if (argc!=2)
		{
			*ec = TERROR("!RadComboBox.setMgr: 1st argument must be Lambda!");
			FrameExit(*ec);
		}

		cpComboBoxLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
		cpFKeyMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gpSym_fkey);

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setReadOnly") == 0)
	{
		// (setReadOnly: readOnlySW)	;; Set the RadComboBox line edit to read only mode (TRUE).
		/* Invalid message */
		if ((argc<2)||(argv[1].Tag!=TYBOLE))
		{
			*ec = TERROR("!RadComboBox.setReadOnly: 1st argument must be boolean!");
			FrameExit(*ec);
		}
		
		cpReadOnlySW = argv[1].u.Bool;
		lineEdit()->setReadOnly(argv[1].u.Bool);
		goto Last;
	}
	else if (strcmp(messagePtr,"setText") == 0)
	{
		// (setText: myText)		;; Set the entire contents of the RadComboBox line edit.
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
			*ec = TERROR("!RadComboBoxt.setText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadComboBox display pane.
		textQString = textPtr;
		textQString = textQString.right(RADMAXLINEEDITSIZE);
		lineEdit()->setText(textQString);

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

Lisp object creation method for the RadComboBox class. Implements the RadIde ComboBox. 
RadComboBox panes come standard with a default fixed font and support for Lisp call back managers 
for key strokes.


Example:
	(setq radComboBoxPtr (qt new: RadComboBox: (Optional)mgrLambda))	;; Create a new RadComboBox object
					...
	(setq RadComboBoxPtr (qt delete: radComboBoxPtr)) ;; Delete a RadComboBox object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadLineEdit: manager))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

	Note: The RadComboBox class was tested, using the above code, and a memory leak IS evident.
		  Creating and deleting the RadComboBox causes significant memory leaks in the QT heap.

********************************************************************************************/
TVAL RadComboBox::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	TVAL			mgrLambda;
	TVAL			result;
	RadComboBox*	radComboBoxPtr = new RadComboBox();

	// Was an optional manager Lambda argument passed?
	if (argc>0)
		mgrLambda = argv[0];
	else
		mgrLambda.Tag = TYVOID;

	// Set the combo box editable
	radComboBoxPtr->setEditable(true);
	QFont font("Courier",10,1,false);
	font.setBold(true);
	radComboBoxPtr->lineEdit()->setReadOnly(false);
	radComboBoxPtr->lineEdit()->setFont(font);


	// Initialize the RadLineEdit Lisp call back Lambdas.
	radComboBoxPtr->cpComboBoxLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gCP->Tval_VOID);
	radComboBoxPtr->cpFKeyMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gpSym_fkey);

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radComboBoxPtr->cpWrpComboBox;
	return(result);
}

