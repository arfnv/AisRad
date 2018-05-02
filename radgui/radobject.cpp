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

												QT/Lisp Interface Base Object 
									Rapid Analytic Demo Integrated Developer Environment

CHANGE HISTORY
Version	Date		Who		Change
1.0000	7/25/2014	mfk 	First experiments with remote console window object.
												 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>
#include <QtGui/QWidget>
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
#include "radobject.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadObject

Constructors for the RadIde QT/Lisp Interface Base Object.

This is the RadIde QT/Lisp Interface Base Object class from which all other RadIde interface objects inherit.
Its main function is to declare the virtual method - lisp - which allows direct
communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

Programmer Notes:
	This class is used, via C++ multiple inheritance, to create QT derivative classes which can 
	communicate directly with the Lisp code in the Main Smartbase Context. For instance, if we
	wish to create a QTextEdit class (to which Lisp functions can send direct messages), we need
	only create the following class...

		class RadTextEdit : public RadObject, QTextEdit

	This new RadTextEdit class will inherit all of the desired behavior of QTextEdit AND will
	contain a slot for the virtual - lisp - method which can receive direct messages from Lisp
	functions executing in the Smartbase Main Context. By adding code to the - lisp - virtual
	method for this class, the QT/C++ programmer can determine which features of QTextEdit are
	made available to Lisp programs.

	Study the RADGlue_QT function for a better understanding of how Lisp functions can send messages
	directly to RadObject pointers.

	WARNING:	All RadObject derived classes MUST inherit from RadObject FIRST i.e.

										YES
	class RadDerivedClass : public RadObject, public QSomeClass		// Correct multiple inheritance

										NO
	class RadDerivedClass : public QSomeClass, public RadObject		// Incorrect multiple inheritance


Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadObject::RadObject()
	: QObject()
{
	cpWrpRadParent = NULL;
	cpWrpQWidget = NULL;
	cpWrpClassName = "";
}

RadObject::RadObject(QObject* parent,QWidget* display,const QString& className)
	: QObject()
{
	cpWrpRadParent = parent;
	cpWrpQWidget = display;
	cpWrpClassName = className;
}

/********************************************************************************************
RadObject::myRadParent

Returns the wrapper parent QObject, if any, of the RadObject.
	
Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
QObject*	RadObject::myRadParent()
{
	return(cpWrpRadParent);
}

/********************************************************************************************
RadObject::myQWidget

Returns the wrapper GUI display QWidget, if any, of the RadObject.
	
Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
QWidget*	RadObject::myQWidget()
{
	return(cpWrpQWidget);
}

/********************************************************************************************
RadObject::myClassName

Returns the QString class name of the cpWrpQWidget object, if any.
	
Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
QString*	RadObject::myClassName()
{
	return(&cpWrpClassName);
}

/********************************************************************************************
RadObject::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the main window.

Examples:
		...no messages implemented yet...

Programmer Notes:
These Lisp messages were meant for RadObject objects returned from (setq aRadObject (qt new: RadObject:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Widget features to Lisp by adding messages to this function.
Please examine the RadObject and the QObject classes before adding more features here.

********************************************************************************************/
TVAL RadObject::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadObject: first argument must be a valid message symbol!");
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

/********************************************************************************************
lispNew

Lisp object creation method for the RadObject class.

Example:
	(setq radObjectPtr (qt new: RadObject:))	;; Create a new RadObject
					...
	(setq radObjectPtr (qt delete: radObjectPtr))	;; Delete a RadObject and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory tpo the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 untol 1000000 (setq p (qt new: RadObject:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

********************************************************************************************/
TVAL RadObject::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	TVAL	result;

	RadObject*	radObjectPtr = new RadObject();
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radObjectPtr;
	return(result);
}
