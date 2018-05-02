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

													Rad Main Window
									Rapid Analytic Demo Integrated Developer Environment

CHANGE HISTORY
Version	Date		Who		Change
1.0000	2/14/2013	mfk 	First experiments with rad main window object.
												 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtCore/QEvent>

#include <QtGui/QMenu>
#include <QtGui/QAction>
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
#include <QtGui/QFileDialog>
#include <QtGui/QClipboard>

#include "radmainwindow.h"
#include "radglue.h"
#include "radconsole.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------
RadMainWindow*	gpRadMainWindow;


//	------------------------------------------------------ METHODS -------------------------------------------------------------
/********************************************************************************************
RadMainWindow  

Constructor for the RadIde main application window. RadIde is an acronym for...
				Rapid Analytic Demo Integrated Development Environment.

This is an in process GUI console object meant to be manipulated by local AIS Lisp Lambdas
in the Main Smartbase context running on this local thread.

Notes:	
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
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
RadMainWindow::RadMainWindow(const QString& irTitle)
	: QMainWindow(NULL,Qt::Window)
{
	// Create - but do NOT make visible the major RadIde GUI systems
	gpRadMainWindow = this;
	gRadConsole = new RadConsole();
	cpMainTabWidget = new RadMainTabWidget(this);
	cpMainWorkSpace = new QWidget;
	QVBoxLayout *mainLayout = new QVBoxLayout;
	mainLayout->addWidget(cpMainTabWidget);
	cpMainWorkSpace->setLayout(mainLayout);
	setCentralWidget(cpMainWorkSpace);

	setWindowTitle("");
}

	
/********************************************************************************************
RadMainWindow::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the main window.

Examples:
(aboutQT:)							;; Show the standard about QT msg box (supplied by QT).
(setq tabIndex (addTab: radWidgetPtr title tabManager))	;; Add a new tab to the main window (appended to the right).
(setq aMainMenu (addMenu: Title))	;; Add a new menu title to the Main Window menu bar (appended to the right).
(setq index (count:))				;; Return the number of tabs currently in the main Window Tab Widget.
(getAis:)							;; refresh the _ais global Structure.
(setq text (getClipboardText:))		;; Get the main window clipboard text.
(setq index (getCurrentIndex:))		;; Return the main Window tab index which has the current focus.
(setq tabIndex (insertTab: tabIndex radWidgetPtr title tabManager))	;; Insert a new tab to the main window tab bar (inserted at the specified index).
(maximize: (Optional)title)			;; Show main window maximized and optionally set window title.
(minimize: (Optional)title)			;; Show main window minimized optionally set window title.
(removeTab: tabIndex)				;; Remove the specified tab from the main window.
(setCurrentIndex: TabIndex)			;; Set the main window TabWidget to focus on the specified tab.
(setClipboardText: text)			;; Set the main window clipboard to the specified text.
(setTitle: title)					;; Set the main window title on the console display system BUT do not display
(setq button (showMsgBox: style title message buttonName))	;; Show a standard msg box and return the button pressed.
(setq fileName (showOpenFileDialog: title pathName wildCards))	;; Show a standard file dialog and return the file selected.
(setq fileName (showSaveFileDialog: title pathName wildCards))	;; Show a standard file dialog and return the file selected.

Programmer Notes:
These Lisp messages were meant for mainWindow: (which is the gpMainRadWindow object).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Main Window features to Lisp by adding messages to this function.
Please examine the RadMainWindow and the QMainWindow classes before adding more features here.

********************************************************************************************/
TVAL RadMainWindow::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	struct					FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	NUM						n;
	NUM						ret;
	LpCHAR					messagePtr;
	LpCHAR					textPtr;
	LpCHAR					text2Ptr;
	LpCHAR					text3Ptr;
	LpCHAR					text4Ptr;
	QWidget*				qtWidgetPtr;
	QString					textQString;
	QString					script;
    QByteArray				textBArray;
	QTextCursor				cursor;
	QTextDocumentFragment	fragment;
	TVAL					prmv[3];
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
		*result = TERROR("!mainWindow: first argument must be a valid message symbol!");
		goto Last;
	}

	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"aboutQT") == 0)
	{
		// (aboutQT:)	;; Show the standard about QT msg box (supplied by QT).

		// Show the standard QT About Msg Box supplied by QT.
		QMessageBox::aboutQt(this,"About QT");

		// Return the Ok button pressed.
		result->Tag = TYNUM;
		result->u.Int = 0;
		goto Last;
	}
	else if (strcmp(messagePtr,"addTab") == 0) 
	{
		// (setq tabIndex (addTab: radWidgetPtr title tabManager))	;; Add a new tab to the main window (appended to the right).
		// Is an implied setTitle being requested?
		if ((argc>3)&&(argv[1].Tag!=TYPOINTER))
		{
			*ec = TERROR("!mainWindow.addTab: 1st argument must be RadWidget object Pointer!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		else if (argc>3)
		{
			// The optional second argument must be the new main window title.
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
				*ec = TERROR("!mainWindow.addTab: optional 2nd argument must be of type Text, String, or Symbol only!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the title of the console window to the specified string.
			textQString = textPtr;
		} else
		{
			*ec = TERROR("!mainWindow.addTab: expecting 3 arguments radWidgetPtr title tabManager!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Add a new tab to Main Window Tabs (appended on the right) 
		qtWidgetPtr = ((RadObject*)argv[1].u.Pointer)->myQWidget();
		result->Tag = TYNUM;
		result->u.Int = cpMainTabWidget->addRadTab(qtWidgetPtr,textQString,argv[3]);
		goto Last;
	}
	else if (strcmp(messagePtr,"addMenu") == 0)
	{
		// (setq aMainMenu (addMenu: Title))	;; Add a new menu title to the Main Window menu bar (appended to the right).

		// The optional second argument must be the new menu title title
		if (argc!=2)
		{
			*result = TERROR("!mainWindow.addMenu: optional 2nd argument must be of type Text, String, or Symbol only!");
			goto Last;
		}

		// The optional second argument must be the new menu title title.
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
			*result = TERROR("!mainWindow.addMenu: optional 2nd argument must be of type Text, String, or Symbol only!");
			goto Last;
		}

		//	Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
		//				causes the QT menu bar NOT to display any menus!!
		//				Therefore, since multiple inheritance fails, the RadMainMenu class and
		//				the RadMainMenuItem class will use the fallback wrapper strategy.
		// Add the new main menu to the Main Window Menu Bar.
		QMenuBar*		mainMenuBar = this->menuBar(); 
		RadMainMenu*	radMenu = new RadMainMenu(textPtr);
		QMenu*			newMenu = radMenu->myPureQMenu;		
		mainMenuBar->addMenu(newMenu);

		// Return the main menu object pointer to the Lisp caller.
		result->Tag = TYPOINTER;
		result->u.Pointer = (POINTER)radMenu;
		goto Last;
	}
	else if (strcmp(messagePtr,"count") == 0)
	{
		// (setq index (count:))				;; Return the number of tabs currently in the main Window Tab Widget.
		result->Tag = TYNUM;
		result->u.Int = cpMainTabWidget->count();
		goto Last;
	}
	else if (strcmp(messagePtr,"getCurrentIndex") == 0)
	{
		// (setq index (currentIndex:))	;; Return the main Window tab index which has the current focus.
		result->Tag = TYNUM;
		result->u.Int = cpMainTabWidget->currentIndex();
		goto Last;
	}
	else if (strcmp(messagePtr,"getAis") == 0)
	{
		// (getAis:)							;; refresh the _ais global Structure.

		// Set the _ais Structure in the Main Context
		script = "(setq _ais (new Structure: ContextName: {" + gpContextName + "}" +
					" Heap: " + gpContextHeap + 
					" Objects: " + gpContextObjHdrs + 
					" CStack: " + gpContextCStack + 
					" MaxRecursions: " + gpMaxRecursions + 
					" ExeFileName: {" + gpApp.applicationFilePath() + "}" +
					" ExePath: {" + gpApp.applicationDirPath() + "}" +
					" LogFileName: {" + gpMyLogFileName + "}" +
					" ScriptFileName: {" + gpScriptFileName + "}" +
					" ScriptPath: {" + gpScriptPath + "}" +
					" MySQLDataDir: {" + gpMySQLDataDir + "}" +
					" MachineName: {localhost} HostIP: {127.0.0.1}" +
					" AisHelpURL: {" + gpAisHelpURL + "}" +
					" QtHelpURL: {" + gpQtHelpURL + "}" +
					" MySQLHelpURL: {" + gpMySQLHelpURL + "}" +
					" TcpPort: " + gpMyTcpPort +
	#ifdef _WIN
					" OS: MSWindows:" +
	#else
					" OS: Linux:" +
	#endif
					" Argv: #(";
		for (n = 0; n < gpArgc; ++n) { script = script + "{" + gpArgv[n] + "} "; }
		script = script + ")))";
		*result = RADGlue_Run(script, FALSE);
		goto Last;
	}
	else if (strcmp(messagePtr,"getClipboardText") == 0)
	{
		// (setq text (getClipboardText:))		;; Get the main window clipboard text.

		// Get the clipboard text.
		QClipboard *clipboard = QApplication::clipboard();
		textQString = clipboard->text();
		textBArray = textQString.toLocal8Bit(); 
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);

		goto Last;
	}  // (setq text (getClipboardText:))		;; Get the main window clipboard text.
	else if (strcmp(messagePtr,"insertTab") == 0) 
	{
		// (setq tabIndex (insertTab: tabIndex radWidgetPtr title tabManager))	;; Insert a new tab to the main window tab bar (inserted at the specified index).
		// Is an implied setTitle being requested?
		if ((argc>4)&&(argv[1].Tag!=TYNUM)&&(argv[2].Tag!=TYPOINTER))
		{
			*ec = TERROR("!mainWindow.insertTab: 1st argument must be a tab index and 2nd argument must be a RadWidget object Pointer!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		else if (argc>4)
		{
			// The third argument must be the new main window title.
			switch (argv[3].Tag) 
			{
				case TYTEXT: 
					textPtr = argv[3].u.Text; 
					break;
				case TYSTRING:
					textPtr = CharArray(argv[3]); 
					break;
				case TYSYMBOL:
				case TYQUOTEDSYMBOL:
					textPtr = SymbolArray(argv[3]); 
					break;
				default:
				*ec = TERROR("!mainWindow.insertTab: 3rd argument must be of type Text, String, or Symbol only!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the title of the console window to the specified string.
			textQString = textPtr;
		} else
		{
			*ec = TERROR("!mainWindow.insertTab: expecting 4 arguments tabIndex radWidgetPtr title tabManager!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Insert a new tab to Main Window Tabs (appended on the right) 
		qtWidgetPtr = ((RadObject*)argv[2].u.Pointer)->myQWidget();
		result->Tag = TYNUM;
		result->u.Int = cpMainTabWidget->insertRadTab(argv[1].u.Int,qtWidgetPtr,textQString,argv[4]);
		goto Last;
	}
	else if (strcmp(messagePtr,"maximize") == 0)
	{
		// (maximize: (Optional)title)		;; Show main window maximized and optionally set window title.

		// Is an implied setTitle being requested?
		if (argc>=2)
		{
			// The optional second argument must be the new main window title.
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
				*ec = TERROR("!mainWindow.maximize: optional 2nd argument must be of type Text, String, or Symbol only!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the title of the main window to the specified string.
			textQString = textPtr;
			textQString = textQString.left(100);
			gpRadMainWindow->setWindowTitle(textQString);
		}

		// Show the entire main window window maximized
		gpRadMainWindow->showMaximized();
		goto Last;
	}
	else if (strcmp(messagePtr,"minimize") == 0)
	{
		// (minimize: (Optional)title)		;; Show main window minimized optionally set window title.

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
				*result = TERROR("!mainWindow.minimize: optional 2nd argument must be of type Text, String, or Symbol only!");
				goto Last;
			}

			// Set the title of the console window to the specified string.
			textQString = textPtr;
			textQString = textQString.left(100);
			gpRadMainWindow->setWindowTitle(textQString);
		}

		// Show the entire console window minimized
		gpRadMainWindow->showMinimized();
		goto Last;
	}
	else if (strcmp(messagePtr,"removeTab") == 0) 
	{
		// (removeTab: tabIndex))	;; Remove a new tab from the main window.
		// Is an implied setTitle being requested?
		if ((argc>1)&&(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!mainWindow.removeTab: 1st argument must be a tab index!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Remove a tab from Main Window Tabs. 
		result->Tag = TYNUM;
		result->u.Int = cpMainTabWidget->removeRadTab(argv[1].u.Int);
		goto Last;
	} 
	else if (strcmp(messagePtr,"setClipboardText") == 0)
	{
		// (setClipboardText: text)			;; Set the main window clipboard to the specified text.
		if (argc<2) goto InvalidSetClipboardTextArgument;
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
			InvalidSetClipboardTextArgument:
			*result = TERROR("!mainWindow.setClipboardText: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			goto Last;
		}

		/* Set the main window clipboard to the specified string. */
		textQString = textPtr;
		QClipboard *clipboard = QApplication::clipboard();
		clipboard->setText(textQString);
		goto Last;
	}
	else if (strcmp(messagePtr,"setCurrentIndex") == 0)
	{
		// (setCurrentIndex: TabIndex)								;; Set the main TabWidget to focus on the specified tab.
		if ((argc>1)&&(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!mainWindow.setCurrentIndex: 1st argument must be Tab Index!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the specified Main Window Tab.
		cpMainTabWidget->setCurrentIndex(argv[1].u.Int);

		// Return the selected main Window tab index. 
		*result = argv[1];
		goto Last;
	}
	else if (strcmp(messagePtr,"setTitle") == 0)
	{
		// (setTitle: title)				;; Set the main window title BUT do not display

		/* Get the new title for the main window */
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
			*result = TERROR("!mainWindow.setTitle: mandatory 2nd argument must be of type Text, String, or Symbol only!");
			goto Last;
		}

		/* Set the title of the main window to the specified string. */
		textQString = textPtr;
		textQString = textQString.left(100);
		gpRadMainWindow->setWindowTitle(textQString);
		goto Last;
	} 
	else if (strcmp(messagePtr,"showMsgBox") == 0)
	{
		// Displays a standard QT QMessageBox and waits for a user response.
		//
		// Example:
		//	(setq button (showMsgBox: style title message buttons))	;; Show a standard msg box and return the button pressed.
		//
		// Args: Style		= information: question: warning: critical:  - Any one of these determines the look and feel of the QMessageBox
		//       title		= the title of the QMessageBox
		//       message	= the main content of the QMessageBox - May be RichText with basic HTML tags, etc.
		//       buttons	= Ok: Yes: No: YesNo: - YesNo displays two buttons and returns which button the user pressed (0=No, 1=Yes)

		// WARNING!	The rest of this showMsgBox code is hopelessly cumbersome because an apparent bug in QT4.6 causes the QMessageBox 
		//          static functions do display improperly unless the macro button identifiers are passed as explicit constants. Putting
		//			them in int, long, or StandardButton variables causes improper display of multi-button message boxes. 

		/* Get the new style for the QMessageBox */
		if (argc!=5) goto InvalidShowMsgBoxStyleArgument;
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
			InvalidShowMsgBoxStyleArgument:
				*result = TERROR("!mainWindow.showMsgBox: mandatory style argument must be  information, question, warning, critical  only!");
			goto Last;
		}

		/* Get the new title for the QMessageBox */
		switch (argv[2].Tag) 
		{
			case TYTEXT: 
				text2Ptr = argv[2].u.Text; 
				break;
			case TYSTRING:
				text2Ptr = CharArray(argv[2]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text2Ptr = SymbolArray(argv[2]); 
				break;
			default:
				*result = TERROR("!mainWindow.showMsgBox: mandatory title argument must be of type  Text String or Symbol only!");
			goto Last;
		}

		/* Get the new message for the QMessageBox */
		switch (argv[3].Tag) 
		{
			case TYTEXT: 
				text3Ptr = argv[3].u.Text; 
				break;
			case TYSTRING:
				text3Ptr = CharArray(argv[3]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text3Ptr = SymbolArray(argv[3]); 
				break;
			default:
				*result = TERROR("!mainWindow.showMsgBox: mandatory message argument must be of type  Text String or Symbol only!");
			goto Last;
		}

		/* Get the button name code for the QMessageBox */
		switch (argv[4].Tag) 
		{
			case TYTEXT: 
				text4Ptr = argv[4].u.Text; 
				break;
			case TYSTRING:
				text4Ptr = CharArray(argv[4]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text4Ptr = SymbolArray(argv[4]); 
				break;
			default:
				InvalidShowMsgBoxButtonsArgument:
				*result = TERROR("!mainWindow.showMsgBox: mandatory buttons argument must be  Ok, Yes, No, or YesNo  only!");
				goto Last;
		}

		/* Let the style & buttons of the standard QMessageBox determined the new few lines of code. */
		result->Tag = TYNUM;
		result->u.Int = 0;
		if (strcmp(textPtr,"information") == 0)
		{
			if (strcmp(text4Ptr,"YesNo") == 0)
			{
				ret = QMessageBox::information(NULL,text2Ptr,text3Ptr,(QMessageBox::Yes|QMessageBox::No));
				if (ret == QMessageBox::Yes) result->u.Int = 1;
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Yes") == 0)
			{
				ret = QMessageBox::information(NULL,text2Ptr,text3Ptr,QMessageBox::Yes);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"No") == 0)
			{
				ret = QMessageBox::information(NULL,text2Ptr,text3Ptr,QMessageBox::No);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Ok") == 0)
			{
				ret = QMessageBox::information(NULL,text2Ptr,text3Ptr,QMessageBox::Ok);
				goto Last;
			}
			else
			goto InvalidShowMsgBoxButtonsArgument;
		}
		else
		if (strcmp(textPtr,"question") == 0)
		{
			if (strcmp(text4Ptr,"YesNo") == 0)
			{
				ret = QMessageBox::question(NULL,text2Ptr,text3Ptr,(QMessageBox::Yes|QMessageBox::No));
				if (ret == QMessageBox::Yes) result->u.Int = 1;
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Yes") == 0)
			{
				ret = QMessageBox::question(NULL,text2Ptr,text3Ptr,QMessageBox::Yes);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"No") == 0)
			{
				ret = QMessageBox::question(NULL,text2Ptr,text3Ptr,QMessageBox::No);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Ok") == 0)
			{
				ret = QMessageBox::question(NULL,text2Ptr,text3Ptr,QMessageBox::Ok);
				goto Last;
			}
			else
			goto InvalidShowMsgBoxButtonsArgument;
		}
		else
		if (strcmp(textPtr,"warning") == 0)
		{
			if (strcmp(text4Ptr,"YesNo") == 0)
			{
				ret = QMessageBox::warning(NULL,text2Ptr,text3Ptr,(QMessageBox::Yes|QMessageBox::No));
				if (ret == QMessageBox::Yes) result->u.Int = 1;
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Yes") == 0)
			{
				ret = QMessageBox::warning(NULL,text2Ptr,text3Ptr,QMessageBox::Yes);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"No") == 0)
			{
				ret = QMessageBox::warning(NULL,text2Ptr,text3Ptr,QMessageBox::No);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Ok") == 0)
			{
				ret = QMessageBox::warning(NULL,text2Ptr,text3Ptr,QMessageBox::Ok);
				goto Last;
			}
			else
			goto InvalidShowMsgBoxButtonsArgument;
		}
		else
		if (strcmp(textPtr,"critical") == 0)
		{
			if (strcmp(text4Ptr,"YesNo") == 0)
			{
				ret = QMessageBox::critical(NULL,text2Ptr,text3Ptr,(QMessageBox::Yes|QMessageBox::No));
				if (ret == QMessageBox::Yes) result->u.Int = 1;
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Yes") == 0)
			{
				ret = QMessageBox::critical(NULL,text2Ptr,text3Ptr,QMessageBox::Yes);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"No") == 0)
			{
				ret = QMessageBox::critical(NULL,text2Ptr,text3Ptr,QMessageBox::No);
				goto Last;
			}
			else
			if (strcmp(text4Ptr,"Ok") == 0)
			{
				ret = QMessageBox::critical(NULL,text2Ptr,text3Ptr,QMessageBox::Ok);
				goto Last;
			}
			else
			goto InvalidShowMsgBoxButtonsArgument;
		}
		else
		goto InvalidShowMsgBoxStyleArgument;
	} 
	else if (strcmp(messagePtr,"showOpenFileDialog") == 0)
	{
		// Displays a standard QT QFileDialog and waits for a user to select a file.
		//
		// Example:
		//	(setq fileName (showOpenFileDialog: title pathName wildCards))	;; Show a standard file dialog and return the file selected.
		//	(setq fileName (showOpenFileDialog: "Open Imabe" "/home/images" "Image Files (*.png *.jpg *.bmp)"))	;; Show a standard file dialog and return the file selected.
		//
		// Args: title		= the title of the QFileDialog
		//       pathName	= the path name of the directory in which to start the search.
		//       wildCards	= wild cards to restrict the suffixes shown in the initial selection drop down
		//
		// WARNING!	The rest of this showFileDialog code is hopelessly cumbersome because an apparent bug in QT4.6 causes the QMessageBox 
		//          static functions do display improperly unless the macro button identifiers are passed as explicit constants. Putting
		//			them in int, long, or StandardButton variables causes improper display of multi-button message boxes. 

		/* Get the new title for the QFileDialog */
		if (argc!=4) goto InvalidShowFileDialogTitleArgument;
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
			InvalidShowFileDialogTitleArgument:
				*result = TERROR("!mainWindow.showOpenFileDialog: mandatory title argument must be of type Text, String, or Symbol  only!");
			goto Last;
		}

		/* Get the new pathName for the QFileDialog */
		switch (argv[2].Tag) 
		{
			case TYTEXT: 
				text2Ptr = argv[2].u.Text; 
				break;
			case TYSTRING:
				text2Ptr = CharArray(argv[2]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text2Ptr = SymbolArray(argv[2]); 
				break;
			default:
				*result = TERROR("!mainWindow.showOpenFileDialog: mandatory pahtName argument must be of type  Text String or Symbol only!");
			goto Last;
		}

		/* Get the new wild card for the QFileDialog */
		switch (argv[3].Tag) 
		{
			case TYTEXT: 
				text3Ptr = argv[3].u.Text; 
				break;
			case TYSTRING:
				text3Ptr = CharArray(argv[3]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text3Ptr = SymbolArray(argv[3]); 
				break;
			default:
				*result = TERROR("!mainWindow.showOpenFileDialog: mandatory wildCard argument must be of type  Text String or Symbol only!");
			goto Last;
		}


		/* Show the standard QFileDialog and receive the file path name selected by the user. */
		textQString = QFileDialog::getOpenFileName(this,textPtr,text2Ptr,text3Ptr);
		textBArray = textQString.toLocal8Bit(); 
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
		goto Last;
	}
	else if (strcmp(messagePtr,"showSaveFileDialog") == 0)
	{
		// Displays a standard QT QFileDialog and waits for a user to select a file.
		//
		// Example:
		//	(setq fileName (showSaveFileDialog: title pathName wildCards))	;; Show a standard file dialog and return the file selected.
		//	(setq fileName (showSaveFileDialog: "Open Imabe" "/home/images" "Image Files (*.png *.jpg *.bmp)"))	;; Show a standard file dialog and return the file selected.
		//
		// Args: title		= the title of the QFileDialog
		//       pathName	= the path name of the directory in which to start the search.
		//       wildCards	= wild cards to restrict the suffixes shown in the initial selection drop down
		//
		// WARNING!	The rest of this showFileDialog code is hopelessly cumbersome because an apparent bug in QT4.6 causes the QMessageBox 
		//          static functions do display improperly unless the macro button identifiers are passed as explicit constants. Putting
		//			them in int, long, or StandardButton variables causes improper display of multi-button message boxes. 

		/* Get the new title for the QFileDialog */
		if (argc!=4) goto InvalidShowSaveFileDialogTitleArgument;
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
			InvalidShowSaveFileDialogTitleArgument:
				*result = TERROR("!mainWindow.showSaveFileDialog: mandatory title argument must be of type Text, String, or Symbol  only!");
			goto Last;
		}

		/* Get the new pathName for the QFileDialog */
		switch (argv[2].Tag) 
		{
			case TYTEXT: 
				text2Ptr = argv[2].u.Text; 
				break;
			case TYSTRING:
				text2Ptr = CharArray(argv[2]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text2Ptr = SymbolArray(argv[2]); 
				break;
			default:
				*result = TERROR("!mainWindow.showSaveFileDialog: mandatory pahtName argument must be of type  Text String or Symbol only!");
			goto Last;
		}

		/* Get the new wild card for the QFileDialog */
		switch (argv[3].Tag) 
		{
			case TYTEXT: 
				text3Ptr = argv[3].u.Text; 
				break;
			case TYSTRING:
				text3Ptr = CharArray(argv[3]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				text3Ptr = SymbolArray(argv[3]); 
				break;
			default:
				*result = TERROR("!mainWindow.showSaveFileDialog: mandatory wildCard argument must be of type  Text String or Symbol only!");
			goto Last;
		}


		/* Show the standard QFileDialog and receive the file path name selected by the user. */
		textQString = QFileDialog::getSaveFileName(this,textPtr,text2Ptr,text3Ptr);
		textBArray = textQString.toLocal8Bit(); 
		textPtr = textBArray.data();
		*result = TSTRING(textPtr);
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
RadMainTabWidget 

Constructor for the RadIde main window tab widget.

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole
			RadCabinet
			RadEditor
			RadDebugger
			RadApp

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Cabinet, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Cabinet, Editor, 
		and Debugger tabs, leaving only the single/multiple AppWindow tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
RadMainTabWidget::RadMainTabWidget(QWidget * parent)
{
	int			n,N;

	cpCurrentTabIndex = -1;
	cpHighTabIndex = -1;
	setTabPosition(QTabWidget::North);
	setTabShape(QTabWidget::Triangular);

	// Make sure all auxilliary storage is cleared.
	for (n = 0; n < RadMainTabWidget_MaxTabs; ++n) 
	{
		cpTabManager[n].Tag = TYVOID;
		cpTabSelectMgr[n].Tag = TYVOID;
		cpTabUnSelectMgr[n].Tag = TYVOID;
		cpTabDisplayPane[n] = NULL;
		cpTabTitle[n] = "";
	}

	cpTabResponsiveSW = true;

}

/********************************************************************************************
RadMainTabWidget::addRadTab 

Add a Rad Display System Tab to the RadIde Main Window Tab Widget.

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole
			RadCabinet
			RadEditor
			RadDebugger
			RadApp

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Cabinet, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Cabinet, Editor, 
		and Debugger tabs, leaving only the single/multiple AppWindow tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
int RadMainTabWidget::addRadTab(QWidget* displayPane,const QString& irTitle,TVAL tabManager)
{
	LpXCONTEXT	gCP = gpMainContext;
	LpTHREAD	gTP = gpMainThread;
	int			newTabIndex;
	TVAL		prmv[2];
	int			n,N;
	StartFrame
	DeclareTVAL(tabSelect);
	DeclareTVAL(tabUnSelect);
	DeclareTVAL(ec);
	EndFrame

	// Add the RAD display system to the RADMainTabWiget.
	cpTabResponsiveSW = false;
	newTabIndex = QTabWidget::addTab(displayPane,irTitle);
	if (newTabIndex >= RadMainTabWidget_MaxTabs)
	{	
		// No more room in Main Tabs therefore refuse this add tab.
		QTabWidget::removeTab(newTabIndex);
		cpTabResponsiveSW = true;
		FrameExit(-1);
	}

	// Update the RADMainTabWiget auxilliary storage.
	cpHighTabIndex = newTabIndex;
	cpTabManager[newTabIndex] = tabManager;
	cpTabSelectMgr[newTabIndex] = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,tabManager,gpSym_tabSelect);
	cpTabUnSelectMgr[newTabIndex] = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,tabManager,gpSym_tabUnSelect);
	cpTabDisplayPane[newTabIndex] = displayPane;
	cpTabTitle[newTabIndex] = irTitle;

	// Tell the previous tab manager that it has be unSelected.
	if ((cpCurrentTabIndex >= 0)&&(cpTabUnSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabUnSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}
	
	// Tell the previous tab manager that it has be unSelected.
	cpCurrentTabIndex = newTabIndex;
	if ((cpCurrentTabIndex >= 0)&&(cpTabSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}

	cpTabResponsiveSW = true;
	FrameExit(newTabIndex);
}

/********************************************************************************************
RadMainTabWidget::insertRadTab 

Insert a Rad Display System Tab to the RadIde Main Window Tab Widget.

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Library, Editor, Debugger, and other Application Windows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Library, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Library, Editor, 
		and Debugger tabs, leaving only the other Application Window tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
int RadMainTabWidget::insertRadTab(int tabIndex,QWidget* displayPane,const QString& irTitle,TVAL tabManager)
{
	LpXCONTEXT	gCP = gpMainContext;
	LpTHREAD	gTP = gpMainThread;
	int			newTabIndex;
	TVAL		prmv[2];
	int			n,nn,N;
	StartFrame
	DeclareTVAL(tabSelect);
	DeclareTVAL(tabUnSelect);
	DeclareTVAL(ec);
	EndFrame

	// Insert the RAD display system to the RADMainTabWiget.
	cpTabResponsiveSW = false;
	newTabIndex = QTabWidget::insertTab(tabIndex,displayPane,irTitle);
	if (newTabIndex >= RadMainTabWidget_MaxTabs)
	{	
		// No more room in Main Tabs therefore refuse this add tab.
		QTabWidget::removeTab(newTabIndex);
		cpTabResponsiveSW = true;
		FrameExit(-1);
	}

	// Adjust the RadMainTabWidget tab information array.
	for (n = tabIndex;n<=cpHighTabIndex;++n)
	{
		// Move all RadMainTabWidget storage right one slot to make room for the new tab.
		nn = n + 1;	
		cpTabManager[nn] = cpTabManager[n];
		cpTabSelectMgr[nn] = cpTabSelectMgr[n];
		cpTabUnSelectMgr[nn] = cpTabUnSelectMgr[n];
		cpTabDisplayPane[nn] = cpTabDisplayPane[n];
		cpTabTitle[nn] = cpTabTitle[n];
	}
	++cpHighTabIndex;
	if (cpCurrentTabIndex >= newTabIndex) ++cpCurrentTabIndex; 

	// Update the RADMainTabWiget auxilliary storage.
	cpTabManager[newTabIndex] = tabManager;
	cpTabSelectMgr[newTabIndex] = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,tabManager,gpSym_tabSelect);
	cpTabUnSelectMgr[newTabIndex] = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,tabManager,gpSym_tabUnSelect);
	cpTabDisplayPane[newTabIndex] = displayPane;
	cpTabTitle[newTabIndex] = irTitle;

	// Tell the previous tab manager that it has be unSelected.
	if ((cpCurrentTabIndex >= 0)&&(cpTabUnSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabUnSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}
	
	// Tell the previous tab manager that it has be unSelected.
	cpCurrentTabIndex = newTabIndex;
	if ((cpCurrentTabIndex >= 0)&&(cpTabSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}

	cpTabResponsiveSW = true;
	FrameExit(newTabIndex);
}

/********************************************************************************************
RadMainTabWidget::removeRadTab 

Remove a Rad Display System Tab from the RadIde Main Window Tab Widget.

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Library, Editor, Debugger, and other Application Windows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Library, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Library, Editor, 
		and Debugger tabs, leaving only the other Application Window tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
int RadMainTabWidget::removeRadTab(int tabIndex)
{
	LpXCONTEXT	gCP = gpMainContext;
	LpTHREAD	gTP = gpMainThread;
	int			newTabIndex;
	TVAL		prmv[2];
	int			n,nn,N;
	StartFrame
	DeclareTVAL(tabSelect);
	DeclareTVAL(tabUnSelect);
	DeclareTVAL(ec);
	EndFrame

	// Insert the RAD display system to the RADMainTabWiget.
	cpTabResponsiveSW = false;
	QTabWidget::removeTab(tabIndex);

	// Tell the previous tab manager that it has be unSelected.
	if ((cpCurrentTabIndex >= 0)&&(cpTabUnSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabUnSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}
	
	// Adjust the RadMainTabWidget tab information array.
	for (nn = tabIndex;nn<cpHighTabIndex;++nn)
	{
		// Move all RadMainTabWidget storage left one slot to make room for the new tab.
		n = nn + 1;	
		cpTabManager[nn] = cpTabManager[n];
		cpTabSelectMgr[nn] = cpTabSelectMgr[n];
		cpTabUnSelectMgr[nn] = cpTabUnSelectMgr[n];
		cpTabDisplayPane[nn] = cpTabDisplayPane[n];
		cpTabTitle[nn] = cpTabTitle[n];
	}

	// Update the RADTabWiget auxilliary storage.
	cpTabManager[cpHighTabIndex] = gCP->Tval_VOID;
	cpTabSelectMgr[cpHighTabIndex] = gCP->Tval_VOID;
	cpTabUnSelectMgr[cpHighTabIndex] = gCP->Tval_VOID;
	cpTabDisplayPane[cpHighTabIndex] = NULL;
	cpTabTitle[cpHighTabIndex] = "";
	--cpHighTabIndex;
	cpCurrentTabIndex = currentIndex(); 

	// Tell the current tab manager that it has be Selected.
	if ((cpCurrentTabIndex >= 0)&&(cpTabSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}

	cpTabResponsiveSW = true;
	FrameExit(cpCurrentTabIndex);
}

/********************************************************************************************
RadMainTabWidget::event

Manager for the RadIde MainTabWidget event.

When a Rad Main Tab is pressed, this event is called, and the RadIde Main Window Tab Widget
switches focus to the Rad Display System located under the selected Tab.

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole
			RadCabinet
			RadEditor
			RadDebugger
			RadApp

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Cabinet, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Cabinet, Editor, 
		and Debugger tabs, leaving only the single/multiple AppWindow tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
bool RadMainTabWidget::event(QEvent *e)
{
	bool		result = QTabWidget::event(e);
	int			newTabIndex;
	TVAL		ret;
	QString		lispCommand;
	NUM			eventCode;
	TVAL		prmv[2];

	// MFK ...needs to be completed...
	eventCode = e->type();
	if (eventCode == QEvent::WindowActivate) 
	{
		if (cpTabResponsiveSW == true)
		{
			// Get current tab
			newTabIndex = currentIndex();

			// Tell the previous tab manager that it has be unSelected.
			if ((cpCurrentTabIndex >= 0)&&(cpCurrentTabIndex != newTabIndex)&&(cpTabUnSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
			{
				prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
				RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabUnSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
			}
			
			// Tell the previous tab manager that it has be unSelected.
			cpCurrentTabIndex = newTabIndex;
			if ((cpCurrentTabIndex >= 0)&&(cpTabSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
			{
				prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
				RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
			}
		}
	}

	return(result);
}

/********************************************************************************************
RadMainTabWidget::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the main window.

Examples:
	...no messages implemented yet...

Programmer Notes:
These Lisp messages were meant for RadMainTabWidget objects (which is the gpMainRadWindow->cpMainTabWidget object).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Main Tab Widget features to Lisp by adding messages to this function.
Please examine the RadMainWindow and the QMainWindow classes before adding more features here.

********************************************************************************************/
TVAL RadMainTabWidget::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadMainTabWidget: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"") == 0)
	{
		// Invalid message
		goto InvalidMessage;
	}
	else
	{
		// Invalid message
		goto InvalidMessage;
	}

	Last:
	Funcs->_Host_Escape = RADGlue_Escape;
	RADGlue_ProcessEvents(gCP,gTP);
	FrameExit(*result);
}

/********************************************************************************************
RadMainMenu 

Constructor for the RadIde main menu widget, which represents and manages a menu on the RadIde
Main Window Main Menu Bar.

This RadObject is returned to the Lisp caller after the following call...
	
	(setq aMainMenu (qt mainWindow: addMenu: title))

When the Lisp programmer wishes to add another menu to RadIde Main Window main menu bar, the above message is sent to the mainWindow
requesting a new menu be added to the Rad Main Menu Bar. The value returned is a pointer to the newly create RadMainMenu object
(study the RadMainMenu:lisp function and the QT::QMenu HTML documentation for a better understanding of the features and services
available for RadMainMenu objects).

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole
			RadCabinet
			RadEditor
			RadDebugger
			RadApp

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Cabinet, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Cabinet, Editor, 
		and Debugger tabs, leaving only the single/multiple AppWindow tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
			causes the QT menu bar NOT to display any menus OR to make the menu items unresponsive!!
			Therefore, we use a pure member strategy for the RadMainMenu class and multiple inheritance
			for the RadMainMenuItem class. We also have a fallback wrapper strategy ready if necessary.
********************************************************************************************/
RadMainMenu::RadMainMenu(const QString& title)
    	: RadObject()
{
	myPureQMenu = new QMenu(title);
	//myWrpMainMenu = new WrpMainMenu(this);	// Wrapper strategy is ready if necessary later.
}


/********************************************************************************************
RadMainMenu::lisp

Receives messages from Lisp function sent to a RadMainMenu object, which represents and manages 
a menu on the RadIde Main Window Main Menu Bar.

A RadMainMenu object is returned to the Lisp caller after the following call...
	
	(setq aMainMenu (qt mainWindow: addMenu: title))

When the Lisp programmer wishes to add another menu to RadIde Main Window main menu bar, the above message is sent to the mainWindow
requesting a new menu be added to the Rad Main Menu Bar. The value returned is a pointer to the newly create RadMainMenu object
(study the RadMainMenu:lisp function and the QT::QMenu HTML documentation for a better understanding of the features and services
available for RadMainMenu objects).


Args:	message		Symbol indicating the action which the Rad Main Menu is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the main menu.

Examples:
(setq aMenuItem (addMenuItem: itemTitle lambdaName memberName callBackArgOff callBackArgOn checkable checkSW))	;; Add a new menu item to the this menu (appended to the bottom).
(addMenuSeparator:)	;; Add a new menu separator to the this menu (appended to the bottom).

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole
			RadCabinet
			RadEditor
			RadDebugger
			RadApp

		The basic idea is to develop Rapid Analytic Demos in Lisp with GUI presentations in
		the single/multiple AppWindows (which are tabs in this Rad Main Window) while using
		the Console, Cabinet, Editor, and Debugger tabs during demo development. When demo
		development is complete, then the idea is to remove the Console, Cabinet, Editor, 
		and Debugger tabs, leaving only the single/multiple AppWindow tabs showing.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for RadIde is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

********************************************************************************************/
TVAL RadMainMenu::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	struct					FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	NUM						n;
	LpCHAR					messagePtr;
	LpCHAR					textPtr;
	QString					textQString;
    QByteArray				textBArray;
	QTextCursor				cursor;
	QTextDocumentFragment	fragment;
	StartFrame
	DeclareTVAL(ec);
	DeclareTVALArray(prmv,2);
	DeclareTVAL(result);
	EndFrame
  	*result = gCP->Tval_VOID;  
			
// **********************************************
// The first argument must be a symbol message 
// **********************************************
if ((argc<1)||((argv[0].Tag!=TYSYMBOL)&&(argv[0].Tag!=TYQUOTEDSYMBOL)))
{
	InvalidMessage:
	*ec = TERROR("!RadMainMenu: first argument must be a valid message symbol!");
	FrameExit(*ec);
}


// **********************************************
// Manage each distinct message
// **********************************************
messagePtr = SymbolArray(argv[0]);
if (strcmp(messagePtr,"addMenuItem") == 0)
{
	//	(setq aMenuItem (addMenuItem: itemTitle lambdaName memberName callBackArgOff callBackArgOn checkable checkSW))	;; Add a new menu item to the this menu (appended to the bottom).

	// Is an implied setTitle being requested?
	if (argc!=8)
	{
		*ec = TERROR("!RadMainMenu.addMenuItem: there must be 7 arguments - itemTitle lambdaName memberName callBackArgOff callBackArgOn checkable checkSW!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
	}

	// The first argument must be the new menu title.
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
		*ec = TERROR("!RadMainMenu.addMenuItem: the 1st argument must be of type Text, String, or Symbol only!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
	}

	// The 6th and 7th arguments must be boolean.
	if ((argv[6].Tag!=TYBOLE)&&(argv[7].Tag!=TYBOLE)) 
	{
		*ec = TERROR("!RadMenu.addMenuItem: 6th and 7th arguments must be of type Boolean!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
	}

	// Add the new action item to this Rad Main Menu.
	RadMainMenuItem* aMenuItem = new RadMainMenuItem(textPtr,this,argv[2],argv[3],argv[4],argv[5],argv[6].u.Bool,argv[7].u.Bool);

	// Return the rad main menu item object pointer to the Lisp caller.
	result->Tag = TYPOINTER;
	result->u.Pointer = (POINTER)aMenuItem;
	goto Last;
}
else
if (strcmp(messagePtr,"addMenuSeparator") == 0)
{
	// (addMenuSeparator:)	;; Add a new menu separator to the this menu (appended to the bottom).

	// Add the new separator to this Rad Main Menu.
	this->myPureQMenu->addSeparator();

	// Return void because there is no rad main menu item object resulting from adding a separator.
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
RadMainMenuItem 

Constructor for the RadIde main menu item widget, which represents and manages a menu item 
on one of the RadMainMenus on the RadIde Main Window Main Menu Bar.

This RadMainMenuItem object pointer is returned to the Lisp caller after the following call...

	;; Add a new main menu to the RadIde Main Menu Bar.
	(setq aRadMainMenu (qt mainWindow: addMenu: title))
	...
	;; Add a new menu item to this main menu (appended to the bottom).
	(setq aRadMainMenuItem (qt aRadMainMenu addMenuItem:	itemTitle		;; Title of the main menu entry
															lambdaName      ;; Global symbol of the call back lambda (mandatory)
															memberName		;; Member symbol of the call back lambda member function (void if none)
															callBackArgOff  ;; Call back argument sent my menu when it is checked OFF.
															callBackArgOn   ;; Call back argument sent my menu when it is checked ON.
															checkable       ;; True IFF this menu item is to be checkable
															checkSW))       ;; True/False sets the initial check status of the menu item


When the Lisp programmer wishes to add another menu item to a RadIde Main Window Main Menu, the above message is sent to the mainWindow
requesting a new menu item be added to the Rad Main Menu. The value returned is a pointer to the newly create RadMainMenuItem object
(study the RadMainMenuItem:lisp function and the QT::QMenu and QT::QAction HTML documentation for a better understanding of the features 
and services available for RadMainMenuItem objects).

Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
			causes the QT menu bar NOT to display any menus OR to make the menu items unresponsive!!
			Therefore, we use a pure member strategy for the RadMainMenu class and multiple inheritance
			for the RadMainMenuItem class. We also have a fallback wrapper strategy ready if necessary.
********************************************************************************************/
RadMainMenuItem::RadMainMenuItem(const QString & title,RadMainMenu* mainMenu,TVAL menuActionLambda,TVAL menuActionMember,TVAL callBackArgOff,TVAL callBackArgOn,bool checkable,bool checkSW) 
	: QAction(title,(QWidget*)mainMenu->myPureQMenu)
{
	cpEventsOffSW = true;	// The menu item will NOT respond to events
	cpMenuActionLambda = menuActionLambda;
	cpMenuActionMember = menuActionMember;
	cpMenuCallBackArgOff = callBackArgOff;
	cpMenuCallBackArgOn = callBackArgOn;
	cpMenuItemTitle = title;
	cpCheckable = checkable;
	setCheckable(true);		// This MUST be set true OTHERWISE the RadMainMenuItem::event will be unresponsive!!	- its a bug in QTv4.6??
	setEnabled(true);		// This MUST be set true OTHERWISE the mRadMainMenuItem::event will be unresponsive!!	- its a bug in QTv4.6??
	mainMenu->myPureQMenu->addAction(this);	// This appends the new menu item to the bottom of the specified menu.
	if (cpCheckable==true) setChecked(checkSW); else setChecked(false);
	cpEventsOffSW = false;	// The menu item will NOW respond to events
}


/********************************************************************************************
RadMainMenuItem::event

Manager for the RadIde RadMainMenuItem event, which represents and manages a menu item 
on one of the RadMainMenus on the RadIde Main Window Main Menu Bar. Each time the menu item
is selected, this event is called.

This RadMainMenuItem object pointer is returned to the Lisp caller after the following call...

	;; Add a new main menu to the RadIde Main Menu Bar.
	(setq aRadMainMenu (qt mainWindow: addMenu: title))
	...
	;; Add a new menu item to the this main menu (appended to the bottom).
	(setq aRadMainMenuItem (qt aRadMainMenu addMenuItem:	itemTitle		;; Title of the main menu entry
															lambdaName      ;; Global symbol of the call back lambda (mandatory)
															memberName		;; Member symbol of the call back lambda member function (void if none)
															callBackArg     ;; Call back argument to help identify this menu choice
															checkable))     ;; True IFF this menu item is to be checkable


When the Lisp programmer wishes to add another menu item to a RadIde Main Window Main Menu, the above message is sent to the mainWindow
requesting a new menu item be added to the Rad Main Menu. The value returned is a pointer to the newly create RadMainMenuItem object
(study the RadMainMenuItem:lisp function and the QT::QMenu and QT::QAction HTML documentation for a better understanding of the features 
and services available for RadMainMenuItem objects).

Programmer Notes:
The Lisp programmer should take note that each invocation of this menu item selection event creates one of
the followoing two call backs into the Smartbase engine ...

	((ref lambdaName) checked callBackArg)			;; If lambdaName.memberName is void
	(lambdaName.memberName checked callBackArg)		;; If lambdaName.memberName is NOT void

The action resulting from a user selection of the specified Rad Main Menu item is entirely dependent upon
the specified Lisp call bacl lambda.

WARNING!!:	Rad derivative objects are NOT garbage protected!!
			Therefore pass only the global symbol of the call back Lambda. As a Smartbase global symbol,
			the call back Lambda will be protected from garbage collection.
			If the callback Lambda is a child Lambda, a member name Symbol can be supplied (otherwise void
			can be passed as the memberName).
			A call back argument, to help identify this menu choice, can be suppied when the menu item
			isd created.
			Finally the Boolean checked is sent indicating whether the menu item is checked or unchecked 
			as a result of this selection (uncheckable menu items are always checked=false).

********************************************************************************************/
bool RadMainMenuItem::event(QEvent *e)
{
	int		eventType;
	bool	checked;
	TVAL	prmv[2];
	bool	result = QAction::event(e);

	eventType = e->type();
	if (cpEventsOffSW == false)
	{
		if (eventType == QEvent::ActionChanged) 
		{
			// Is the menu item checked OFF or ON
			checked = this->isChecked();
			// Warning: An apparent Bug in QTv4.6 makes ALL event-responsive QActions checkable.
			//          Here we include logic to make uncheckable QAction items appear uncheckable.
			if ((this->cpCheckable==false)&&(checked==true))
				{
					cpEventsOffSW = true;
					setChecked(false);
					cpEventsOffSW = false;
					checked = this->isChecked();
				}
			// Load the menu event call back arguments
			if (checked==false) prmv[0] = cpMenuCallBackArgOff; else prmv[0] = cpMenuCallBackArgOn; 
			prmv[1].Tag = TYBOLE; prmv[1].u.Bool = checked;

			// This Rad Main Menu Item has been selected, perform the call back requested.
			RADGlue_RunLambdaMember(cpMenuActionLambda,cpMenuActionMember,2,prmv,FALSE);
		}
	}

	return(result);
}


/********************************************************************************************
RadMainMenuItem::lisp

Receives messages from Lisp Lambdas to RadManinMenuItem objects.

Args:	message		Symbol indicating the action which the RadMainMenuItem is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the main window.

Examples:
		...no messages implemented yet...

Programmer Notes:
These Lisp messages were meant for RadMainMenuItem objects (which is the gpMainRadWindow->cpMainTabWidget object).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more RadMainMenuItem features to Lisp by adding messages to this function.
Please examine the RadMainMenuItem and the QAction classes before adding more features here.

Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
			causes the QT menu bar NOT to display any menus!!
			Therefore, since multiple inheritance fails, the RadMainMenu class and
			the RadMainMenuItem class will use the fallback wrapper strategy.
********************************************************************************************/
TVAL RadMainMenuItem::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadMainMenuItem: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"") == 0)
	{
		/* Invalid message */
		goto InvalidMessage;
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

