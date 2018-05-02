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
												RadTextEdit for GUI

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
#include <QtGui/QScrollBar>

#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "radtabwidget.h"
#include "radtextedit.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadTextEdit 

Constructor for the RadIde RadTextEdit.

Notes:
Implements the RadIde Text Edit pane. RadTextEdit panes come standard with horizontal word wrap
at the pane boundry WYSIWYG and vertical scroll bars, full rich text capabilities, and support 
for Lisp call back managers for both key strokes and mouse clicks.

********************************************************************************************/
RadTextEdit::RadTextEdit()
	: QTextEdit()
{
	// A RadTextEdit object comes with both vertical and horizontal scroll bars,
	//  is able to receive rich text, and has a default font.
	QFont font("Courier", 10, 1, false);
	font.setBold(true);
	QTextEdit::setAcceptRichText(true);
	QTextEdit::setReadOnly(false);
	QTextEdit::setAutoFormatting(QTextEdit::AutoNone);
	cpReadOnlySW = false;
	cpTabsOffSW = false;
	QTextEdit::setFont(font);

	cpWrpTextEdit = new WrpTextEdit(this,this,"WrpTextEdit");
}

/********************************************************************************************
RadTextEdit::keyPressEvent

Manager for the RadIde RadTextEdit keyPressEvent.

********************************************************************************************/
void RadTextEdit::keyPressEvent(QKeyEvent *e)
{
	int						key = e->key();
	Qt::KeyboardModifiers	keyState = e->modifiers();
	bool					altKey = (keyState == Qt::AltModifier);		
	bool					ctrlKey = (keyState == Qt::ControlModifier);
	bool					noModifiers = (keyState == Qt::NoModifier);
	TVAL					argv[1];

	// We do NOT allow Alt key presses to pass through (they are designated RadTextEdit Function keys).
	// Note: If tabs are OFF, We do NOT allow tabs to be entered into our documents.
	if ((altKey == true) || (cpTabsOffSW==TRUE)&&(key == Qt::Key_Tab)) 
	{
		QKeyEvent* event = new QKeyEvent(QEvent::KeyPress,0,Qt::NoModifier);
		QTextEdit::keyPressEvent(event);
		return;
	}
	else
	{
		QTextEdit::keyPressEvent(e);
	}
}

/********************************************************************************************
RadTextEdit::keyReleaseEvent

Manager for the RadIde RadTextEdit keyReleaseEvent.

********************************************************************************************/
void RadTextEdit::keyReleaseEvent(QKeyEvent *e)
{
	int						key = e->key();
	int						fkey;
	Qt::KeyboardModifiers	keyState = e->modifiers();
	bool					altKey = (keyState == Qt::AltModifier);		
	bool					ctrlKey = (keyState == Qt::ControlModifier);	
	bool					noModifiers = (keyState == Qt::NoModifier);
	TVAL					argv[1];

	// If there is a RadTextEdit manager (AND a function key has been pressed) then call (mgr.fkey key).
	// Note1: Only certain key combinations are treated as function keys.
	// Note2: These special key combinations are given Function Key Codes as follows in this code - RADGlue_FunctionKeyCode.
	if (cpFKeyMember.Tag != TYVOID)
	{
		fkey = RADGlue_FunctionKeyCode(e);
		if (fkey > 0)
		{
			argv[0].Tag = TYNUM;
			argv[0].u.Int = fkey;
			RADGlue_RunLambdaMember(cpTextEditLambda,cpFKeyMember,1,argv,FALSE);
		}
	}
}

/********************************************************************************************
RadTextEdit::mousePressEvent

Manager for the RadIde RadTextEdit mousePressEvent.

********************************************************************************************/
void RadTextEdit::mousePressEvent(QMouseEvent *e)
{
	QTextCursor	cursor;
	int			click = e->button();
	int			type = e->type();
	QPoint		pos = e->pos();
	int			x = pos.x();
	int			y = pos.y();
	int			oldScrollbarValue = this->horizontalScrollBar()->value();
	TVAL		argv[1];

	// Turn OFF right click if in read only mode.
	if ((cpReadOnlySW == true)&&(click == Qt::RightButton))
	{
		return;
	}

	// Perform the normal mouse press event processing
	QTextEdit::mousePressEvent(e);

	// Is this RadTextEdit object set to read only mode?
	if (cpReadOnlySW == true)
	{
		// Select the whole line on any mouse press event.
		cursor = textCursor();
		cursor.select(QTextCursor::LineUnderCursor);
		setTextCursor(cursor);
		this->horizontalScrollBar()->setValue(oldScrollbarValue);
	}

	// Manage a mouse click
	if ((type!=QEvent::MouseButtonDblClick)&&((click == Qt::LeftButton)||(click == Qt::RightButton)))
	{
		// If there is a RadTextEdit manager, then call (mgr.singleclick click).
		if (cpMouseSingleClickMember.Tag != TYVOID)
		{
			// If there is a text edit manager, then call (mgr.singleclick click).
			argv[0].Tag = TYNUM;argv[0].u.Int = click;
			RADGlue_RunLambdaMember(cpTextEditLambda,cpMouseSingleClickMember,1,argv,FALSE);
		}
	}
}

/********************************************************************************************
RadTextEdit::mouseDoubleClickEvent

Manager for the RadIde RadTextEdit mouseDoubleClickEvent.

********************************************************************************************/
void RadTextEdit::mouseDoubleClickEvent(QMouseEvent *e)
{
	QTextCursor	cursor;
	int			click = e->button();
	int			type = e->type();
	QPoint		pos = e->pos();
	int			x = pos.x();
	int			y = pos.y();
	int			oldScrollbarValue = this->horizontalScrollBar()->value();
	TVAL		argv[1];

	// Turn OFF right click if in read only mode.
	if ((cpReadOnlySW == true)&&(click == Qt::RightButton))
	{
		return;
	}

	// Is this RadTextEdit object set to read only mode?
	if (cpReadOnlySW == true)
	{
		// Select the whole line on any mouse press event.
		cursor = textCursor();
		cursor.select(QTextCursor::LineUnderCursor);
		setTextCursor(cursor);
		this->horizontalScrollBar()->setValue(oldScrollbarValue);
	}
	else
	{
		// Perform the normal double click event processing
		QTextEdit::mouseDoubleClickEvent(e);
	}

	// Manage a mouse double click
	if ((click == Qt::LeftButton)||(click == Qt::RightButton))
	{
		// If there is a RadTextEdit manager, then call (mgr.dblclick click).
		if (cpMouseDoubleClickMember.Tag != TYVOID)
		{
			// If there is a text edit manager, then call (mgr.dblclick click).
			argv[0].Tag = TYNUM;argv[0].u.Int = click;
			RADGlue_RunLambdaMember(cpTextEditLambda,cpMouseDoubleClickMember,1,argv,FALSE);
		}
	}
}

/********************************************************************************************
RadTextEdit::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadTextEdit object.

Examples:
(copy:)								;; Copy selected text to the mainWindow clipboard.
(setq result (find: text caseInSensitiveSW backWardsSW wholeWordSW))	;; Find the next instance of the text in the RadTextEdit display pane and select (if found returns true).
(setq myText (getHtmlText:))		;; Get the entire contents of the RadTextEdit display pane.
(setq myText (getText:))			;; Get the entire contents of the RadTextEdit display pane.
(setq myText (getSelectedText:))	;; Get the entire contents of currently selected text in the RadTextEdit display pane.
(setq pos (getSelectionEnd:))		;; Get the cursor selection END position in the RadTextEdit display pane.
(setq pos (getSelectionStart:))		;; Get the cursor selection START position in the RadTextEdit display pane.
(paste:)							;; Paste the text in the mainWindow clipboard into the RadTextEdit display pane.
(selectAll:)						;; Select ALL the RadTextEdit text and highlight in the GUI.
(setAcceptRichText: boolean)		;; Set the RadTextEdit to accept rich text (true) or not (false).
(setFocus:)							;; Set the RadTextEdit to be the GUI focus.
(setHtmlText: myText)				;; Set the entire contents of the RadTextEdit display pane.
(setMgr: mgrLambda)					;; Set the RadTextEdit manager - where (defun mgrLambda(command) ...lisp code...).
(setReadOnly: readOnlySW)			;; Set the RadTextEdit pane to read only mode (TRUE).
(setSelectedText: command)			;; Set the currently selected text based on the command (all, line, word).
(setSelectedText: start end)		;; Set the currently selected text specified cursor start and end locations.
(setTabsOFF:)						;; Set the RadEditText NOT to accept Tabs.
(setTabsON:)						;; Set the RadEditText to accept Tabs.
(setText: myText)					;; Set the entire contents of the RadTextEdit display pane.
(setWordWrapOFF:)					;; Set the RadTextEdit so that words do NOT wrap at the display width of the pane.
(setWordWrapON:)					;; Set the RadTextEdit to wrap words at the display width of the pane.

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadTextEdit::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadTextEdit: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"copy") == 0)
	{
		// (copy:)	;; Copy selected text to the mainWindow clipboard.
		copy();

		goto Last;
	}
	else if (strcmp(messagePtr,"find") == 0)
	{
		//(setq result (find: text caseInSensitiveSW backWardsSW wholeWordSW))	;; Find the next instance of the text in the RadTextEdit display pane and select (if found returns true).
		// Check the arguments for validity
		if ((argc!=5)||(argv[2].Tag!=TYBOLE)||(argv[3].Tag!=TYBOLE)||(argv[4].Tag!=TYBOLE)) goto InvalidFindTextArgument;
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
			InvalidFindTextArgument:
			*ec = TERROR("!RadTextEdit.find: expecting 4 arguments must be text caseInSensitiveSW backWardsSW wholeWordSW!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Isolate the target text to be "found" in the RadTextEdit display pane.
		textQString = textPtr;
		textQString = textQString.left(RADMAXTEXTEDITSIZE);

		// Move the cursor to the end of the text in the RadTextEdit pane.
		QTextDocument::FindFlags findOptions = 0;
		result->Tag = TYBOLE;

		// caseInSensitiveSW backWardsSW wholeWordSW
		if (argv[2].u.Bool==(BOLE)FALSE) findOptions |= QTextDocument::FindCaseSensitively;
		if (argv[3].u.Bool==(BOLE)TRUE) findOptions |= QTextDocument::FindBackward;
		if (argv[4].u.Bool==(BOLE)TRUE) findOptions |= QTextDocument::FindWholeWords;

		result->u.Bool = find(textQString,findOptions);

		goto Last;
	}
	else if (strcmp(messagePtr,"getHtmlText") == 0)
	{
		// (setq myText (getHtmlText:))		;; Get the entire contents of the RadTextEdit display pane.
		/* Get the entire contents of the RadTextEdit display pane */
		textQString = toHtml();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"getText") == 0)
	{
		// (setq myText (getText:))			;; Get the entire contents of the RadTextEdit display pane.
		/* Get the entire contents of the RadTextEdit display pane */
		textQString = toPlainText();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"getSelectionEnd") == 0)
	{
		// (setq pos (getSelectionEnd:))		;; Get the cursor selection END position in the RadTextEdit display pane.
		// Get the cursor selection END position in the RadTextEdit display pane.
		cursor = textCursor();
		n = cursor.selectionEnd();
		result->Tag = TYNUM;
		result->u.Int = n;
		goto Last;
	}
	else if (strcmp(messagePtr,"getSelectionStart") == 0)
	{
		// (setq pos (getSelectionStart:))		;; Get the cursor selection START position in the RadTextEdit display pane.
		// Get the cursor selection END position in the RadTextEdit display pane.
		cursor = textCursor();
		n = cursor.selectionStart();
		result->Tag = TYNUM;
		result->u.Int = n;
		goto Last;
	}
	else if (strcmp(messagePtr,"getSelectedText") == 0)
	{
		// (setq myText (getSelectedText:))	;; Get the entire contents of currently selected text in the RadTextEdit display pane.
		/* Get the entire contents of the selected text in the RadTextEdit display pane */
		cursor = textCursor();
		fragment = cursor.selection();
		textQString = fragment.toPlainText();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"paste") == 0)
	{
		// (paste:)   ;; Paste the text in the mainWindow clipboard into the RadTextEdit display pane.
		paste();

		goto Last;
	}	
	else if (strcmp(messagePtr, "selectAll") == 0)
	{
		// (selectAll:)	;; Select ALL the RadTextEdit text and highlight in the GUI.
		selectAll();

		goto Last;
	}
	else if (strcmp(messagePtr, "setAcceptRichText") == 0)
	{
		// (setAcceptRichText: boolean)		;; Set the RadTextEdit to accept rich text (true) or not (false).
		if ((argc<2)||(argv[1].Tag!=TYBOLE))
		{
			*ec = TERROR("!RadTextEdit.setAcceptRichText: 2nd argument must be a boolean!");
			FrameExit(*ec);
		}

		setAcceptRichText(argv[1].u.Bool);

		goto Last;
	}
	else if (strcmp(messagePtr,"setFocus") == 0)
	{
		// (setFocus:)	;; Set the RadTextEdit to be the GUI focus.
		setFocus();

		goto Last;
	}
	else if (strcmp(messagePtr,"setHtmlText") == 0)
	{
		// (setHtmlTest: myText)	;; Set the entire contents of the RadTextEdit display pane.
		/* Set the entire contents of the RadTextEdit display pane */
		if (argc!=2) goto InvalidSetHtmlTextArgument;
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
			InvalidSetHtmlTextArgument:
			*ec = TERROR("!RadTextEdit.setHtmlText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadTextEdit display pane.
		textQString = textPtr;
		if (RADMAXTEXTEDITSIZE < textQString.length())
		{
			textQString = textQString.left(RADMAXTEXTEDITSIZE);
			QMessageBox::information(NULL,"Warning","Text exceeded maximum and was truncated",QMessageBox::Ok);
		}

		setText(textQString);

		// Move the cursor to the end of the text in the RadTextEdit pane.
		cursor = textCursor();
		cursor.movePosition(QTextCursor::End, QTextCursor::MoveAnchor);
		setTextCursor(cursor);
		setFocus();

		goto Last;
	} 
	else if (strcmp(messagePtr,"setMgr") == 0)
	{
		// (setMgr: mgrLambda)					;; Set the RadTextEdit manager - where (defun mgrLambda(command) ...lisp code...).
		/* Get the name of the RadTextEdit manager Lambda in the main context */
		if (argc!=2)
		{
			*ec = TERROR("!RadTextEdit.setMgr: 1st argument must be Lambda!");
			FrameExit(*ec);
		}

		cpTextEditLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
		cpFKeyMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpTextEditLambda,gpSym_fkey);
		cpMouseSingleClickMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpTextEditLambda,gpSym_singleclick);
		cpMouseDoubleClickMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpTextEditLambda,gpSym_dblclick);

		goto Last;
	}
	else if (strcmp(messagePtr,"setReadOnly") == 0)
	{
		// (setReadOnly: readOnlySW) ;; Set the RadTextEdit pane to read only mode (IFF TRUE).

		/* Invalid message */
		if ((argc<2)||(argv[1].Tag!=TYBOLE))
		{
			*ec = TERROR("!RadTextEdit.setReadOnly: 1st argument must be boolean!");
			FrameExit(*ec);
		}
		
		cpReadOnlySW = argv[1].u.Bool;
		setReadOnly(argv[1].u.Bool);
		goto Last;
	}
	else if (strcmp(messagePtr,"setSelectedText") == 0)
	{
		// (setSelectedText: command)		;; Set the currently selected text based on the command (all, line, word).
		// (setSelectedText: start end)		;; Set the currently selected text specified cursor start and end locations.

		// Set the currently selected text based on the command (all, block, line, word).
		// Get the command which tells how to set the current selection
		// Note: (setSelectedText command)
		if (argc==2)
		{
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
				InvalidSetSelectedTextArgument:
					*ec = TERROR("!RadTextEdit.setSelectedText: mandatory 2nd argument must be all: block: line: or word:!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Translate the command into an integer for QTextCursor.
			if (strcmp(textPtr,"all") == 0) n = QTextCursor::Document;
			else if (strcmp(textPtr,"line") == 0) n = QTextCursor::LineUnderCursor;
			else if (strcmp(textPtr,"word") == 0) n = QTextCursor::WordUnderCursor;
			else goto InvalidSetSelectedTextArgument;

			// Set the current text selection as commanded.
			cursor = textCursor();
			cursor.select((QTextCursor::SelectionType)n);
			setTextCursor(cursor);
			goto Last;
		} 
		else if (argc==3)
		{
			// (setSelectedText: start end)		;; Set the currently selected text specified cursor start and end locations.
			if ((argv[1].Tag != TYNUM)||(argv[2].Tag != TYNUM)) 
			{
				*ec = TERROR("!RadTextEdit.setSelectedText: 2nd and 3rd arguments must be Integers!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the current text selection as commanded.
			cursor = textCursor();
			cursor.setPosition(argv[1].u.Int,QTextCursor::MoveAnchor);
			cursor.setPosition(argv[2].u.Int,QTextCursor::KeepAnchor);
			setTextCursor(cursor);
			goto Last;
		}
		else
		{
			*ec = TERROR("!RadTextEdit.setSelectedText: invalid number of arguments!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

	} 
	else if (strcmp(messagePtr,"setTabsOFF") == 0)
	{
		//(setTabsOFF:)		;; Set the RadEditText NOT to accept Tabs (TRUE) or to accept Tabs (FALSE).
		cpTabsOffSW = TRUE;
		goto Last;
	}
	else if (strcmp(messagePtr,"setTabsON") == 0)
	{
		//(setTabsON:)		;; Set the RadEditText to accept Tabs.
		cpTabsOffSW = FALSE;
		goto Last;
	}
	else if (strcmp(messagePtr,"setText") == 0)
	{
		// (setTest: myText)					;; Set the entire contents of the RadTextEdit display pane.
		/* Get the entire contents of the RadTextEdit display pane */
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
			*ec = TERROR("!RadTextEdit.setText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadTextEdit display pane.
		// Note: We don't allow HTML in the RadTextEdit like we do in the Demo Window.
		textQString = textPtr;
		if (RADMAXTEXTEDITSIZE < textQString.length())
		{
			textQString = textQString.left(RADMAXTEXTEDITSIZE);
			QMessageBox::information(NULL,"Warning","Text exceeded maximum and was truncated",QMessageBox::Ok);
		}
		setPlainText(textQString);

		// Move the cursor to the end of the text in the RadTextEdit pane.
		cursor = textCursor();
		cursor.movePosition(QTextCursor::Start, QTextCursor::MoveAnchor);
		setTextCursor(cursor);
		setFocus();

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
	}
	else if (strcmp(messagePtr,"setWordWrapOFF") == 0)
	{
		//(setWordWrapOFF:);; Set the RadTextEdit so that words do NOT wrap at the display width of the pane.
		setLineWrapMode(QTextEdit::NoWrap);

		goto Last;
	}
	else if (strcmp(messagePtr,"setWordWrapON") == 0)
	{
		//(setWordWrapON:);; Set the RadTextEdit to wrap words at the display width of the pane.
		setLineWrapMode(QTextEdit::WidgetWidth);

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

Lisp object creation method for the RadTextEdit class. Implements the RadIde Text Edit pane. 
RadTextEdit panes come standard with horizontal word wrap at the pane boundry WYSIWYG and 
vertical scroll bars, full rich text capabilities, and support for Lisp call back managers 
for both key strokes and mouse clicks.


Example:
	(setq radTextEditPtr (qt new: RadTextEdit: (Optional)mgrLambda))	;; Create a new RadTextEdit object
					...
	(setq RadTextEditPtr (qt delete: radTextEditPtr)) ;; Delete a RadTextEdit object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadTextEdit:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

	Note: The RadTextEdit class was tested, using the above code, and a memory leak IS evident.
		  Creating and deleting the RadTextEdit causes significant memory leaks in the QT heap.


********************************************************************************************/
TVAL RadTextEdit::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	TVAL			mgrLambda;
	TVAL			result;
	RadTextEdit*	radTextEditPtr = new RadTextEdit();

	// Was an optional manager Lambda argument passed?
	if (argc>0)
		mgrLambda = argv[0];
	else
		mgrLambda.Tag = TYVOID;

	// Initialize the RadTextEdit Lisp call back Lambdas.
	radTextEditPtr->cpTextEditLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gCP->Tval_VOID);
	radTextEditPtr->cpFKeyMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gpSym_fkey);
	radTextEditPtr->cpMouseSingleClickMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gpSym_singleclick);
	radTextEditPtr->cpMouseDoubleClickMember = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gpSym_dblclick);

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radTextEditPtr->cpWrpTextEdit;
	return(result);
}

