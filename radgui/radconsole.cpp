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
aisdev/aradgui/radconsole.h
												Console Display System

CHANGE HISTORY
Version	Date		Who		Change
1.0000	2/14/2013	mfk 	First experiments with rad console window object.
												 
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

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------
RadConsole*		gRadConsole;


//	------------------------------------------------------ METHODS -------------------------------------------------------------
/********************************************************************************************
RadConsole 

Constructor for the RadIde main console display system. RadIde is an acronym for...
				Rapid Analytic Demo Integrated Development Environment.

This is an in process GUI console object meant to be manipulated by local AIS Lisp Lambdas
in the Main Smartbase context running on this local thread.

Notes:	
		This is the RadIde main console display system. It is the GUI from which Lisp commands
		can be sent to the Smartbase engine running on the main workspace.

		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Cabinet, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Cabinet, Editor, 
		and Debugger tabs, leaving only the single/multiple AppWindow tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to display this Main console object, then this
		Rad Main console will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
RadConsole::RadConsole()
	: QWidget()
{
	QFont font("Courier",10,1,false);
	font.setBold(true);

	gRadConsole = this;

	cpConsoleInstallStatus= FALSE;
	cpConsoleMainTabIndex = -1;
	cpConsoleTitle = "Console";

	cpConsoleMgr.Tag = TYVOID;
	cpClearEventMgr.Tag = TYVOID;
	cpDisplayEventMgr.Tag = TYVOID;
	cpFKeyEventMgr.Tag = TYVOID;
	cpRunEventMgr.Tag = TYVOID;
	cpMouseSingleClickMgr.Tag = TYVOID;
	cpMouseDoubleClickMgr.Tag = TYVOID;

	cpConsoleWorkSpace = new QWidget;
	cpConsoleLog = "";
	cpCommand = new QComboBox;
	cpCommand->setMinimumContentsLength(RADMAXCOMMANDSIZE);
	cpCommand->setLineEdit(new QLineEdit);
	cpCommand->setEditable(true);
	cpCommand->setFont(font);
	cpClearButton = new RadConsoleClearButton("Clear");
	cpRunButton = new RadConsoleRunButton(FALSE);
	QGridLayout *commandLayout = new QGridLayout;
	commandLayout->addWidget(cpClearButton,0,0,Qt::AlignLeft);
	commandLayout->addWidget(cpRunButton,0,1,Qt::AlignLeft);
	commandLayout->addWidget(cpCommand, 0, 2, Qt::AlignLeft);
	commandLayout->setColumnStretch(2, 1);

	QScrollArea *scrollArea = new QScrollArea;
	cpConsole = new RadConsoleTextEdit();
	scrollArea->setWidget(cpConsole);
	scrollArea->setWidgetResizable(true);
	QVBoxLayout *mainLayout = new QVBoxLayout;
	mainLayout->addLayout(commandLayout);
	mainLayout->addWidget(scrollArea);
	cpConsoleWorkSpace->setLayout(mainLayout);

//	createMenu();
}

/********************************************************************************************
RadConsole::processEvents 

Console event processing function.

********************************************************************************************/
NUM	RadConsole::processEvents(LpXCONTEXT gCP,LpTHREAD gTP)
{
	cpRunButton->SetConsoleBusy(gTP->busySW);

	return(0);
}

/********************************************************************************************
RadConsole::displayOnConsole 

Display the specified text at at the end of the curent console text.

********************************************************************************************/
NUM	RadConsole::displayOnConsole(LpCHAR displayMe,NUM newline)
{
	TVAL	ret;
	NUM		displayLen = strlen((char *)displayMe);
	QString lispCommand;
	QString displayString;
	QString	oldDisplayBfr;
	QString	newDisplayBfr;

	// Make sure that the String does NOT exceed the RAD Console display size.
	if (displayLen >= RADMAXCONSOLESIZE)
	{
		displayMe = &displayMe[displayLen-RADMAXCONSOLESIZE];
	}
	else
	{		
		QString	consoleText	= cpConsole->toPlainText();
		if (consoleText.length() >= (RADMAXCONSOLESIZE*2.0))
		{
			cpConsole->setPlainText(consoleText.right(RADMAXCONSOLESIZE));
		}
	}

	QTextCursor cursor(this->cpConsole->textCursor());
	cursor.movePosition(QTextCursor::End, QTextCursor::MoveAnchor);
	if (newline == 0)
		cursor.insertText(displayMe);
	else
	{
		cursor.insertText(displayMe);
		cursor.insertText("\n");
	}

	this->cpConsole->setTextCursor(cursor);
	this->cpConsole->setFocus();

	return(0);
}

/********************************************************************************************
RadConsole::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the console is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the console.

Examples:
(addCommandItem: myText)	    		;; Add a new text item to the console command drop down list.
(setq result (find: text caseInSensitiveSW backWardsSW wholeWordSW))	;; Find the next instance of the text in the console display pane and select (if found returns true).
(setq index (getCommandCount:))			;; Get the number of items in the console command drop down list.
(setq myText (getCommandItem: index))	;; Get the entire contents of the specified console command drop down item.
(setq myText (getCommandText:))			;; Get the entire contents of the console command line.
(setq myText (getConsoleLog:))			;; Get the entire contents of the console log and reset to "".
(setq myText (getDisplayHtml:))			;; Get the entire contents of the console display pane.
(setq myText (getDisplayText:))			;; Get the entire contents of the console display pane.
(setq myText (getSelectedText:))		;; Get the entire contents of currently selected text in the console display pane.
(setq pos (getSelectionEnd:))			;; Get the cursor selection END position in the console display pane.
(setq pos (getSelectionStart:))			;; Get the cursor selection START position in the console display pane.
(maximize: (Optional)title)				;; Show console display system maximized in main window and optionally set window title.
(minimize: (Optional)title)				;; Show console display system minimized in main window and optionally set window title.
(paste:)								;; Paste the text in the mainWindow clipboard into the console display pane.
(removeCommandItem: index)	    		;; Remove the specified text item from the console command drop down list.
(setCommandItem: index myText)			;; Set the contents of the specified console command drop down item.
(setCommandText: myText)				;; Set the entire contents of the console command line.
(setDisplayHtml: myText)				;; Set the entire contents of the console display pane as HTML.
(setDisplayText: myText)				;; Set the entire contents of the console display pane.
(setConsoleMgr: mgrLambdaName)			;; Set the console manager name - where (defun mgrLambdaName(command) ...lisp code...).
(setTabIndex: tabIndex)					;; Set the index of the RAD Main Tab index to which the console display system is assigned (sent by RadMainWindow, etc.).
(setSelectedText: command)				;; Set the currently selected text based on the command (all, line, word).
(setSelectedText: start end)			;; Set the currently selected text specified cursor start and end locations.
(setTitle: title)						;; Set the main window title on the console display system BUT do not display

********************************************************************************************/
TVAL RadConsole::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
	DeclareTVAL(result);	*result = gCP->Tval_VOID;
	EndFrame

// **********************************************
// The first argument must be a symbol message 
// **********************************************
if ((argc<1)||((argv[0].Tag!=TYSYMBOL)&&(argv[0].Tag!=TYQUOTEDSYMBOL)))
{
	InvalidMessage:
	*ec = TERROR("!console: first argument must be a valid message symbol!");
	FrameExit(*ec);
}

// **********************************************
// Install the console display system (if necessary) 
// **********************************************
if (cpConsoleInstallStatus == FALSE)
{
	cpConsoleInstallStatus = TRUE;
	cpConsoleMainTabIndex = gpRadMainWindow->cpMainTabWidget->addRadTab(cpConsoleWorkSpace,cpConsoleTitle,cpConsoleMgr);
}


// **********************************************
// Manage each distinct message
// **********************************************
messagePtr = SymbolArray(argv[0]);
if (strcmp(messagePtr, "addCommandItem") == 0)
{
	/* Add a new text item to the console command drop down list. */
	if (argc!=2) goto InvalidAddCommandItemArgument;
	switch (argv[1].Tag)
	{
	case TYTEXT:
		textPtr = argv[1].u.Text;
		break;
	case TYSTRING:
		textPtr = CharArray(argv[1]);
		break;
	case TYQUOTEDSYMBOL:
	case TYSYMBOL:
		textPtr = SymbolArray(argv[1]);
		break;
	default:
	InvalidAddCommandItemArgument :
		*ec = TERROR("!console.AddCommandItem: mandatory 2nd argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

	// Set the entire contents of the console command line.
	textQString = textPtr;
	textQString = textQString.right(RADMAXCOMMANDSIZE);
	cpCommand->addItem(textQString);

	RADGlue_ProcessEvents(gCP, gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr, "find") == 0)
{
	// (setq result (find: text caseInSensitiveSW backWardsSW wholeWordSW))	;; Find the next instance of the text in the console display pane and select (if found returns true).
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
		*ec = TERROR("!console.find: expecting 4 arguments must be text caseInSensitiveSW backWardsSW wholeWordSW!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
	}

	// Isolate the target text to be "found" in the RadTextEdit display pane.
	textQString = textPtr;
	textQString = textQString.left(RADMAXCONSOLESIZE);

	// Move the cursor to the end of the text in the RadTextEdit pane.
	QTextDocument::FindFlags findOptions = 0;
	result->Tag = TYBOLE;

	// caseInSensitiveSW backWardsSW wholeWordSW
	if (argv[2].u.Bool==(BOLE)FALSE) findOptions |= QTextDocument::FindCaseSensitively;
	if (argv[3].u.Bool==(BOLE)TRUE) findOptions |= QTextDocument::FindBackward;
	if (argv[4].u.Bool==(BOLE)TRUE) findOptions |= QTextDocument::FindWholeWords;

	result->u.Bool = cpConsole->find(textQString,findOptions);

	goto Last;
}
else if (strcmp(messagePtr, "getCommandCount") == 0)
{
	/* Get the number of items in the console command drop down list. */
	n = cpCommand->count();
	*result = TINT(n);
	goto Last;
}
else if (strcmp(messagePtr, "getCommandItem") == 0)
{
	/* Get the contents of the specified command drop down item. */
	if (argc == 2)
	{
		/* Get the entire contents of the specified command drop down item. */
		if (argv[1].Tag != TYNUM)
		{
			*ec = TERROR("!console.getCommandItem: optional 1st argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP, gTP);
			FrameExit(*ec);
		}
		textQString = cpCommand->itemText(argv[1].u.Int);
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else
	{
		*ec = TERROR("!console.getCommandItem: invalid number of arguments!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

}
else if (strcmp(messagePtr, "getCommandText") == 0)
{
	/* Get the entire contents of the console command line or the specified command drop down item. */
	if (argc == 0)
	{
		/* Get the entire contents of the console command line. */
		textQString = cpCommand->currentText();
		textBArray = textQString.toLocal8Bit();
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else
	{
		*ec = TERROR("!console.getCommandText: invalid number of argiuments!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

}
else if (strcmp(messagePtr, "getConsoleLog") == 0)
{
	/* Get the entire contents of the console display pane */
	textQString = cpConsoleLog;
    textBArray = textQString.toLocal8Bit();
	textPtr = textBArray.data();
	*result = TSTRING(textPtr);
	cpConsoleLog = "";
	goto Last;
}
else if (strcmp(messagePtr,"getDisplayHtml") == 0)
{
	/* Get the entire contents of the console display pane */
	textQString = cpConsole->toHtml();
    textBArray = textQString.toLocal8Bit();
	textPtr = textBArray.data();
	*result = TSTRING(textPtr);
	goto Last;
}
else if (strcmp(messagePtr,"getDisplayText") == 0)
{
	/* Get the entire contents of the console display pane */
	textQString = cpConsole->toPlainText();
    textBArray = textQString.toLocal8Bit();
	textPtr = textBArray.data();
	*result = TSTRING(textPtr);
	goto Last;
}
else if (strcmp(messagePtr,"getSelectionEnd") == 0)
{
	// Get the cursor selection END position in the console display pane.
	cursor = cpConsole->textCursor();
	n = cursor.selectionEnd();
	result->Tag = TYNUM;
	result->u.Int = n;
	goto Last;
}
else if (strcmp(messagePtr,"getSelectionStart") == 0)
{
	// Get the cursor selection END position in the console display pane.
	cursor = cpConsole->textCursor();
	n = cursor.selectionStart();
	result->Tag = TYNUM;
	result->u.Int = n;
	goto Last;
}
else if (strcmp(messagePtr,"getSelectedText") == 0)
{
	/* Get the entire contents of the selected text in the console display pane */
	cursor = cpConsole->textCursor();
	fragment = cursor.selection();
	textQString = fragment.toPlainText();
    textBArray = textQString.toLocal8Bit();
	textPtr = textBArray.data();
	*result = TSTRING(textPtr);
	goto Last;
}
else if (strcmp(messagePtr,"maximize") == 0)
{
	// Is an implied setTitle being requested?
	if (argc>=2)
	{
		// The optional second argument must be the new console window title.
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
			*ec = TERROR("!console.maximize: optional 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the title of the console window to the specified string.
		textQString = textPtr;
		textQString = textQString.right(100);
		gpRadMainWindow->setWindowTitle(textQString);
	}

	// Show the entire console window maximized
	gpRadMainWindow->showMaximized();
	Funcs->_Host_Escape = RADGlue_Escape;
	Funcs->_Host_Display = RADGlue_DisplayOnConsoleFromLisp;
	gCP->_Host_Display = (LpHOST_DISPLAY)(*Funcs->_Host_Display);
	gCP->_Host_Escape = (LpHOST_ESCAPE)(*Funcs->_Host_Escape);
	goto Last;
}
else if (strcmp(messagePtr,"minimize") == 0)
{
	// Is an implied setTitle being requested?
	if (argc>=2)
	{
		// The optional second argument must be the new console window title.
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
			*ec = TERROR("!console.minimize: optional 2nd argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the title of the console window to the specified string.
		textQString = textPtr;
		textQString = textQString.right(100);
		gpRadMainWindow->setWindowTitle(textQString);
	}

	// Show the entire console window minimized
	gpRadMainWindow->showMinimized();
	Funcs->_Host_Escape = RADGlue_Escape;
	Funcs->_Host_Display = RADGlue_DisplayOnConsoleFromLisp;
	gCP->_Host_Display = (LpHOST_DISPLAY)(*Funcs->_Host_Display);
	gCP->_Host_Escape = (LpHOST_ESCAPE)(*Funcs->_Host_Escape);
	goto Last;
}
else if (strcmp(messagePtr,"paste") == 0)
{
	// (paste:)   ;; Paste the text in the mainWindow clipboard into the console display pane.
	cpConsole->paste();

	goto Last;
}	
else if (strcmp(messagePtr, "removeCommandItem") == 0)
{
	/* Remove the specified text item from the console command drop down list. */
	if (argc!=2) goto InvalidRemoveCommandItemArgument;
	switch (argv[1].Tag)
	{
	case TYNUM:
		n = argv[1].u.Int;
		break;
	default:
	InvalidRemoveCommandItemArgument :
		*ec = TERROR("!console.RemoveCommandItem: mandatory 2nd argument must be of type Integer only!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

	// Remove the specified text item from the console command drop down list.
	cpCommand->removeItem(n);

	RADGlue_ProcessEvents(gCP, gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr, "setCommandItem") == 0)
{
	/* Set the contents of the specified console command drop down item. */
	if ((argc!=3)) goto InvalidSetCommandItemArgument;
	switch (argv[1].Tag)
	{
	case TYNUM:
		n = argv[1].u.Int;
		break;
	default:
		*ec = TERROR("!console.setCommandItem: mandatory 2nd argument must be of type Integer only!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

	switch (argv[2].Tag)
	{
	case TYTEXT:
		textPtr = argv[2].u.Text;
		break;
	case TYSTRING:
		textPtr = CharArray(argv[2]);
		break;
	case TYQUOTEDSYMBOL:
	case TYSYMBOL:
		textPtr = SymbolArray(argv[2]);
		break;
	default:
	InvalidSetCommandItemArgument :
		*ec = TERROR("!console.setCommandItem: mandatory 3rd argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

	// Set the  contents of the specified console command drop down item.
	textQString = textPtr;
	textQString = textQString.right(RADMAXCOMMANDSIZE);
	cpCommand->setItemText(n, textQString);

	RADGlue_ProcessEvents(gCP, gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr, "setCommandText") == 0)
{
	/* Set the entire contents of the console command line or of the specified console command drop down list. */
	if ((argc!=2)) goto InvalidSetCommandTextArgument;
	switch (argv[1].Tag)
	{
	case TYTEXT:
		textPtr = argv[1].u.Text;
		break;
	case TYSTRING:
		textPtr = CharArray(argv[1]);
		break;
	case TYQUOTEDSYMBOL:
	case TYSYMBOL:
		textPtr = SymbolArray(argv[1]);
		break;
	default:
	InvalidSetCommandTextArgument :
		*ec = TERROR("!console.setCommandText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP, gTP);
		FrameExit(*ec);
	}

	// Set the entire contents of the console command line.
	textQString = textPtr;
	textQString = textQString.right(RADMAXCOMMANDSIZE);
	cpCommand->setEditText(textQString);

	RADGlue_ProcessEvents(gCP, gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr, "setConsoleMgr") == 0)
{
	/* Get the name of the console manager Lambda in the main context */
	if (argc!=2)
	{
		*ec = TERROR("!console.setConsoleMgr: 1st argument must be Lambda!");
		FrameExit(*ec);
	}

	// Set the name of the console manager in the main context.
	// Note: This name must be assigned to the new console manager Lambda
	//       in the global variables of the main context.

	cpConsoleMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
	cpClearEventMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpConsoleMgr,gpSym_clear);
	cpDisplayEventMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpConsoleMgr,gpSym_display);
	cpFKeyEventMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpConsoleMgr,gpSym_fkey);
	cpRunEventMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpConsoleMgr,gpSym_run);
	cpMouseSingleClickMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpConsoleMgr,gpSym_singleclick);
	cpMouseDoubleClickMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpConsoleMgr,gpSym_dblclick);

	RADGlue_ProcessEvents(gCP,gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr,"setDisplayText") == 0)
{
	/* Get the entire contents of the console display pane */
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
		*ec = TERROR("!console.setDisplayText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
    }

	// Set the entire contents of the console display pane.
	// Note: We don't allow HTML in the console like we do in the Demo Window.
	textQString = textPtr;
	textQString = textQString.right(RADMAXCONSOLESIZE);
	cpConsole->setPlainText(textQString);

	// Move the cursor to the end of the text in the console pane.
	cursor = cpConsole->textCursor();
	cursor.movePosition(QTextCursor::End, QTextCursor::MoveAnchor);
	cpConsole->setTextCursor(cursor);
	cpConsole->setFocus();

	RADGlue_ProcessEvents(gCP,gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr,"setDisplayHtml") == 0)
{
	/* Get the entire contents of the console display pane */
	if (argc!=2) goto InvalidSetDisplayHtmlArgument;
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
		InvalidSetDisplayHtmlArgument:
		*ec = TERROR("!console.setDisplayHtml: mandatory 2nd argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
    }

	// Set the entire contents of the console display pane.
	// Note: We allow HTML in the console like we do in the Demo Window.
	textQString = textPtr;
	textQString = textQString.right(RADMAXCONSOLESIZE);
	cpConsole->setHtml(textQString);

	// Move the cursor to the end of the text in the console pane.
	cursor = cpConsole->textCursor();
	cursor.movePosition(QTextCursor::End, QTextCursor::MoveAnchor);
	cpConsole->setTextCursor(cursor);
	cpConsole->setFocus();

	RADGlue_ProcessEvents(gCP,gTP);
	FrameExit(gCP->Tval_TRUE);
}
else if (strcmp(messagePtr,"setSelectedText") == 0)
{
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
				*ec = TERROR("!console.setSelectedText: mandatory 2nd argument must be all: block: line: or word:!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Translate the command into an integer for QTextCursor.
		if (strcmp(textPtr,"all") == 0) n = QTextCursor::Document;
		else if (strcmp(textPtr,"line") == 0) n = QTextCursor::LineUnderCursor;
		else if (strcmp(textPtr,"word") == 0) n = QTextCursor::WordUnderCursor;
		else goto InvalidSetSelectedTextArgument;

		// Set the current text selection as commanded.
		cursor = cpConsole->textCursor();
		cursor.select((QTextCursor::SelectionType)n);
		//cursor.setPosition(20,QTextCursor::MoveAnchor);
		cpConsole->setTextCursor(cursor);
		goto Last;
	} 
	else if (argc==3)
	{
		if ((argv[1].Tag != TYNUM)||(argv[2].Tag != TYNUM)) 
		{
			*ec = TERROR("!console.setSelectedText: 2nd and 3rd arguments must be Integers!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the current text selection as commanded.
		cursor = cpConsole->textCursor();
		cursor.setPosition(argv[1].u.Int,QTextCursor::MoveAnchor);
		cursor.setPosition(argv[2].u.Int,QTextCursor::KeepAnchor);
		cpConsole->setTextCursor(cursor);
		goto Last;
	}
	else
	{
		*ec = TERROR("!console.setSelectedText: invalid number of arguments!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
	}

}
else if (strcmp(messagePtr,"setTabIndex") == 0)
{
	/* Get the new tab index for the console display system */
	if (argc!=2) goto InvalidSetTabIndexArgument;
    switch (argv[1].Tag) 
	{
		case TYNUM: 
			if (argv[1].u.Int<0) goto InvalidSetTabIndexArgument; 
			cpConsoleMainTabIndex = argv[1].u.Int; 
			break;
		default:
		InvalidSetTabIndexArgument:
		*ec = TERROR("!console.setTabIndex: mandatory 2nd argument must be of type Integer only!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
    }
	goto Last;
} 
else if (strcmp(messagePtr,"setTitle") == 0)
{
	/* Get the new title for the demo window */
	if (argc!=2) goto InvalidSetTitleTextArgument;
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
		InvalidSetTitleTextArgument:
		*ec = TERROR("!console.setTitle: mandatory 2nd argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
    }

	/* Set the title of the demo window to the specified string. */
	textQString = textPtr;
	textQString = textQString.right(100);
	gpRadMainWindow->setWindowTitle(textQString);
	goto Last;
} 
else
{
	/* Invalid message */
	goto InvalidMessage;
}

Last:
RADGlue_ProcessEvents(gCP,gTP);
FrameExit(*result);
}


/********************************************************************************************
RadConsoleClearButton 

Constructor for the RADIDE Console Clear Button.

********************************************************************************************/
RadConsoleClearButton::RadConsoleClearButton(const QString& irTitle)
	: QPushButton(irTitle)
{

}

/********************************************************************************************
RadConsoleClearButton::event

Manager for the RADIDE Console Clear Button event.

********************************************************************************************/
bool RadConsoleClearButton::event(QEvent *e)
{
	bool result = QPushButton::event(e);

	if (e->type() == QEvent::MouseButtonRelease) 
	{
		// If there is no console manager, then clear the display pane.
		if (gRadConsole->cpClearEventMgr.Tag == TYVOID)
			gRadConsole->cpConsole->setText("");
		else
		{
			// If there is a console manager, then call (consoleMgr.clear).
			RADGlue_RunLambdaMember(gRadConsole->cpConsoleMgr,gRadConsole->cpClearEventMgr,0,NULL,FALSE);
		}
	}

	return(result);
}

/********************************************************************************************
RadConsoleRunButton 

Constructor for the RadIde Console Run Button.

********************************************************************************************/
RadConsoleRunButton::RadConsoleRunButton(BOLE busySW)
	: QPushButton()
{
	cpEngineBusySW = busySW;
	if (busySW == (BOLE)TRUE)
		setText("Stop");
	else
		setText("Run");
}

/********************************************************************************************
RadConsoleRunButton::SetConsoleBusy 

Set the RadIde Console Run Button to "Run" or "Stop" as necessary.

********************************************************************************************/
void RadConsoleRunButton::SetConsoleBusy(BOLE busySW)
{
	if (cpEngineBusySW != busySW)
	{
		cpEngineBusySW = busySW;
		if (busySW == (BOLE)TRUE)
			setText("Stop");
		else
			setText("Run");
	}
}


/********************************************************************************************
RadConsoleRunButton::event

Manager for the RadIde Console Run Button event.

********************************************************************************************/
bool RadConsoleRunButton::event(QEvent *e)
{
	bool result = QPushButton::event(e);
	TVAL ret;
	QString lispCommand;


	if (e->type() == QEvent::MouseButtonRelease) {
		if (text() == "Run")
		{
			// If there is no console manager, then run the command as is.
			if (gRadConsole->cpRunEventMgr.Tag == TYVOID)
			{
				lispCommand = gRadConsole->cpCommand->currentText();
				gRadConsole->cpConsole->setText((gRadConsole->cpConsole->toPlainText() + "\n" + lispCommand + "\n").right(RADMAXCONSOLESIZE));
				ret = RADGlue_Run(lispCommand,TRUE);
			}
			else
			{
				// If there is a console manager, then call (ConsoleMgr.run).
				RADGlue_RunLambdaMember(gRadConsole->cpConsoleMgr,gRadConsole->cpRunEventMgr,0,NULL,FALSE);

			}
		}
		else
		{
			RADGlue_SetEscapeRequest();
		}
	}

	return(result);
}

/********************************************************************************************
RadConsoleTextEdit 

Constructor for the RadIde Console Display pane.

********************************************************************************************/
RadConsoleTextEdit::RadConsoleTextEdit()
	: QTextEdit()
{
	QFont font("Courier",10,1,false);
	font.setBold(true);
	
	QTextEdit::QTextEdit();
	QTextEdit::setAcceptRichText(false); 
	QTextEdit::setFont(font);
	QTextEdit::setLineWrapMode(QTextEdit::NoWrap);
}

/********************************************************************************************
RadConsoleTextEdit::keyPressEvent

Manager for the RADIDE Console Console Display pane.

********************************************************************************************/
void RadConsoleTextEdit::keyPressEvent(QKeyEvent *e)
{
	int						key = e->key();
	Qt::KeyboardModifiers	keyState = e->modifiers();
	bool					altKey = (keyState == Qt::AltModifier);		
	bool					ctrlKey = (keyState == Qt::ControlModifier);
	bool					noModifiers = (keyState == Qt::NoModifier);
	TVAL					argv[1];

	// We do NOT allow Alt key presses to pass through (they are designated RadTextEdit Function keys).
	if (altKey == true) 
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
RadConsoleTextEdit::keyReleaseEvent

Manager for the RADIDE Console Console Display pane.

********************************************************************************************/
void RadConsoleTextEdit::keyReleaseEvent(QKeyEvent *e)
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
	if (gRadConsole->cpFKeyEventMgr.Tag != TYVOID)
	{
		fkey = RADGlue_FunctionKeyCode(e);
		if (fkey > 0)
		{
			argv[0].Tag = TYNUM;
			argv[0].u.Int = fkey;
			RADGlue_RunLambdaMember(gRadConsole->cpConsoleMgr,gRadConsole->cpFKeyEventMgr,1,argv,FALSE);
		}
	}
}

/********************************************************************************************
RadConsoleTextEdit::mousePressEvent

Manager for the RADIDE Console single click mouse event.

********************************************************************************************/
void RadConsoleTextEdit::mousePressEvent(QMouseEvent *e)
{
	QTextCursor	cursor;
	int			click = e->button();
	int			type = e->type();
	QPoint		pos = e->pos();
	int			x = pos.x();
	int			y = pos.y();
	TVAL		argv[1];

	// Perform the normal mouse press event processing
	QTextEdit::mousePressEvent(e);

	// Manage a single mouse click
	if ((type!=QEvent::MouseButtonDblClick)&&((click == Qt::LeftButton)||(click == Qt::RightButton)))
	{
		// If there is a console manager, then call (mgr.singleclick click).
		if (gRadConsole->cpMouseSingleClickMgr.Tag != TYVOID)
		{
			// If there is a text edit manager, then call (mgr.singleclick click).
			argv[0].Tag = TYNUM;argv[0].u.Int = click;
			RADGlue_RunLambdaMember(gRadConsole->cpConsoleMgr,gRadConsole->cpMouseSingleClickMgr,1,argv,FALSE);
		}
	}
}


/********************************************************************************************
RadConsoleTextEdit::mouseDoubleClickEvent

Manager for the RadIde Console mouseDoubleClickEvent.

********************************************************************************************/
void RadConsoleTextEdit::mouseDoubleClickEvent(QMouseEvent *e)
{
	QTextCursor	cursor;
	int			click = e->button();
	int			type = e->type();
	QPoint		pos = e->pos();
	int			x = pos.x();
	int			y = pos.y();
	TVAL		argv[1];

	// Perform the normal double click event processing
	QTextEdit::mouseDoubleClickEvent(e);

	// Manage a mouse double click
	if ((click == Qt::LeftButton)||(click == Qt::RightButton))
	{
		// If there is a console manager, then call (mgr.dblclick click).
		if (gRadConsole->cpMouseDoubleClickMgr.Tag != TYVOID)
		{
			// If there is a text edit manager, then call (mgr.dblclick click).
			argv[0].Tag = TYNUM;argv[0].u.Int = click;
			RADGlue_RunLambdaMember(gRadConsole->cpConsoleMgr,gRadConsole->cpMouseDoubleClickMgr,1,argv,FALSE);
		}
	}
}
