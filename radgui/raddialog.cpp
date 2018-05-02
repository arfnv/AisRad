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
												RadDialog for GUI

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
#include <QtGui/QDialog>

#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "raddialog.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------


/********************************************************************************************
RadDialog 

Constructor for the RadIde QDialog.

Notes:	
		The RadIde RadDialog is designed to implement a dialog window following the guidelines
		of QDialog modified for Lisp direct access and control.

				
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for the RadTabWidget is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).

		Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
		Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
		use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
		classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadDialog::RadDialog()
	: QDialog(gpRadMainWindow,Qt::Dialog)
{
	// Create the wrapper object to Lisp can communicate
	cpWrpDialog = new WrpDialog(this,this,"WrpDialog");
	cpDialogManager.Tag = TYVOID;
	cpDialogClose.Tag = TYVOID;
	setModal(true); 
	setSizeGripEnabled(true);
}

/********************************************************************************************
RadDialog::closeEvent

Manager for the RadIde RadDialog close event.

When a RadDialog is closed, this event is called.

Notes:	
		The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and maybe a Main Menu displayed.

		RadIde display systems are added as new tabs to the Main Tab Widget including... 
		Console, Cabinet, Editor, Debugger, and single/multiple AppWindows.

		The display systems shown in this Rad Main Tab Widget include the following RadIde
		recognized display systems...
			RadConsole

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
void RadDialog::closeEvent(QCloseEvent *e)
{
	QDialog::closeEvent(e);
	TVAL	prmv[2];

	// Tell the dialog close manager than it has been closed.
	if (cpDialogClose.Tag!=TYVOID)
	{
		prmv[0].Tag = TYVOID;
		RADGlue_RunLambdaMember(cpDialogManager,cpDialogClose,0,prmv,FALSE);
	}
	else
	if (cpDialogManager.Tag != TYVOID)
	{
		RADGlue_RunLambda(cpDialogManager, 0, prmv, FALSE);
	}


}

/********************************************************************************************
RadDialog::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the RadDialog is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadDialog.

Examples:
	(setq result (accept:))	;; Hide the RadDialog window and set the result code to Accepted.
	(setq result (done: integer))	;; Close the RadDialog window and set the return value to integer.
	(setq result (exec:))	;; Show and execute modally the RadDialog window.
	(setq result (reject:))	;; Hide the RadDialog window and set the result code to Rejected.
	(resize: width height)	;; Resize the RadDialog window to the specified pixel width and height.
	(setLayout: radLayoutPtr)	;; Set the layout of the RadDialog window.
	(setTitle: title)		;; Set the title of the RadDialog window.
	(setManager: manager)	;; Set the RadDialog mananger.


Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadDialog::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	struct					FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	NUM						n;
	LpCHAR					messagePtr;
	LpCHAR					textPtr;
	QString					textQString;
    QByteArray				textBArray;
	QTextCursor				cursor;
	QTextDocumentFragment	fragment;
	RadWidget*				radWidgetPtr;
	TVAL					prmv[2];
	StartFrame
	DeclareTVAL(ec);
	DeclareTVAL(ret);
	EndFrame

	// **********************************************
	// We always return *result at the last section of this function
	// Note: The default return valid is void.
	// **********************************************
    *ret = gCP->Tval_VOID;
			
	// **********************************************
	// The first argument must be a symbol message 
	// **********************************************
	if ((argc<1)||((argv[0].Tag!=TYSYMBOL)&&(argv[0].Tag!=TYQUOTEDSYMBOL)))
	{
		InvalidMessage:
		*ret = TERROR("!RadDialog: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************	

	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"accept") == 0)
	{
		// (setq result (accept:))	;; Hide the RadDialog window and set the result code to Accepted.
		accept();
		goto Last;
	} 
	else if (strcmp(messagePtr,"done") == 0)
	{
		// (setq result (done: integer))	;; Close the RadDialog window and set the return value to integer.
		if ((argc<2)||(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadDialog.done: expecting 1 argument which must be an Integer return value!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		done(argv[1].u.Int);
		goto Last;
	} 
	else if (strcmp(messagePtr,"exec") == 0)
	{
		// (setq result (exec:))	;; Show and execute modally the RadDialog window.
		ret->Tag = TYNUM;
		ret->u.Int = exec();
		//ret->u.Int = result();
		goto Last;
	} 
	else if (strcmp(messagePtr,"reject") == 0)
	{
		// (setq result (reject:))	;; Hide the RadDialog window and set the result code to Recjected.
		reject();
		goto Last;
	} 	

	else if (strcmp(messagePtr,"resize") == 0)
	{
		// 	(resize: width height)	;; Resize the RadDialog window to the specified pixel width and height.
		if ((argc<3)||(argv[1].Tag!=TYNUM)||(argv[2].Tag!=TYNUM))
		{
			*ec = TERROR("!RadDialog.resize: expecting 2 arguments which must be Integers!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		//      pixel width , pixel height
		resize(argv[1].u.Int,argv[2].u.Int);
		goto Last;
	} 	
	else if (strcmp(messagePtr,"setLayout") == 0)
	{
		// (setLayout: radLayoutPtr)	;; Set the layout of the RadDialog window.
		if ((argc<2)||(argv[1].Tag!=TYPOINTER))
		{
			*ec = TERROR("!RadDialog.setLayout: expecting 1 argument which must be a RadLayout pointer!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the layout of the RadDialog window.
		// Load and Identify the layout argument
		RadObject* radObjectPtr = (RadObject*)argv[1].u.Pointer;
		if (radObjectPtr->cpWrpClassName == "WrpVBoxLayout")
		{
			RadVBoxLayout* aRadVBoxLayout = (RadVBoxLayout*)radObjectPtr->cpWrpQWidget;
			setLayout((QVBoxLayout*)aRadVBoxLayout);
		} else if (radObjectPtr->cpWrpClassName == "WrpHBoxLayout") 
		{
			RadHBoxLayout* aRadHBoxLayout = (RadHBoxLayout*)radObjectPtr->cpWrpQWidget;
			setLayout((QHBoxLayout*)aRadHBoxLayout);
		} else 
		{
			*ec = TERROR("!RadDialog.setLayout: 1st argument is not a valid RadLayout pointer!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		goto Last;
	}
	else if (strcmp(messagePtr,"setManager") == 0)
	{
		// (setManager: manager)	;; Set the RadDialog mananger.
		if (argc<1)
		{
			*ec = TERROR("!RadDialog.setManager: expecting 1 argument which must be a Lambda!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the manager of the RadDialog window. 
		cpDialogManager = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,argv[1],gCP->Tval_VOID);
		cpDialogClose = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,cpDialogManager,gpSym_close);
		goto Last;
	}
	else if (strcmp(messagePtr,"setTitle") == 0)
	{
		// (setTitle: title)	;; Set the title of the RadDialog window.
		if (argc>1)
		{
			// The second argument must be the new RadDialog title.
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
				*ec = TERROR("!RadDialog.setTitle: 1st argument must be of type Text, String, or Symbol only!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the title of the tab to the specified string.
			textQString = textPtr;
		} 
		else
		{
			*ec = TERROR("!RadDialog.setTitle: expecting 1 argument which must be a title!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the title of the RadDialog window. 
		setWindowTitle(textQString);
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
	FrameExit(*ret);
}

/********************************************************************************************
lispNew

Lisp object creation method for the RadDialog class.

Example:
	(setq radDialogPtr (qt new: RadDialog: title (Optional)manager))	;; Create a new RadDialog
					...
	(setq RadDialogPtr (qt delete: RadDialogPtr)) ;; Delete a RadDialog and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory tpo the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 untol 1000000 (setq p (qt new: RadWidget:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.


********************************************************************************************/
TVAL RadDialog::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	QString		textQString;
	char*		textPtr;
	TVAL		ec;
	TVAL		manager;
	TVAL		result;

	// (title manager)	;; Create a RadDialog window with title and manager.
	if (argc>0)
	{
		// The 1st argument must be the new RadDialog title.
		switch (argv[0].Tag) 
		{
			case TYTEXT: 
				textPtr = argv[0].u.Text; 
				break;
			case TYSTRING:
				textPtr = CharArray(argv[0]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				textPtr = SymbolArray(argv[0]); 
				break;
			default:
			ec = TERROR("!RadDialog.new: 1st argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			return(ec);
		}

		// Set the title of the tab to the specified string.
		textQString = textPtr;
	} 
	else
	{
		ec = TERROR("!RadDialog.new: expecting as many as 2 arguments which must be a title and an (Optional) manager!");
		RADGlue_ProcessEvents(gCP,gTP);
		return(ec);
	}

	// Retrieve the optional manager argument.
	if (argc>1) manager = argv[1]; else manager = gCP->Tval_VOID;

	// Create the new RadDialog as a model window with the main window as its parent.
	RadDialog*	radDialogPtr = new RadDialog();

	// Set the title of the RadDialog window. 
	radDialogPtr->setWindowTitle(textQString);

	// Set the manager of the RadDialog window. 
	radDialogPtr->cpDialogManager = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,manager,gCP->Tval_VOID);
	radDialogPtr->cpDialogClose = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,radDialogPtr->cpDialogManager,gpSym_close);

	// Retrun a pointer to the new RadDialog object.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radDialogPtr->cpWrpDialog;
	return(result);
}

