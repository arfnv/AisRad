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

#ifndef radlineedit_H
#define radlineedit_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
												RadLineEdit for GUI

CHANGE HISTORY
Version	Date		Who		Change
1.0000	8/4/2014	mfk 	First experiments with remote GUI objects.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

/**************************************************************************************************************************
Note:	There are macro naming conflicts between QT and FSmartbase.h. 
        QT derived class headers must come first! 
		Be careful about the macros you use in QT derived classes!  
		Known conflicts include: isNull(), and multiple inheritance with QMainWindow. 
**************************************************************************************************************************/

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QMenuBar>
#include <QtGui/QMenu>
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtGui/QIcon>
#include <QtGui/QMainWindow>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QMessageBox>
#include <QtGui/QTextEdit>
#include <QtGui/QPlainTextEdit>
#include <QtGui/QLineEdit>
#include <QtGui/QHBoxLayout>
#include <QtGui/QvBoxLayout>
#include <QtGui/QScrollArea>

#include "radobject.h"
#include "radwidget.h"
#include "radmainwindow.h"

extern "C" { // includes for modules written in C
#include "../smtbase/fsmtbase.h" // SmartBase engine declarations
}

class RadObject;

class RadWidget; class WrpWidget;
class RadTabWidget; class WrpTabWidget;
class RadTextEdit; class WrpTextEdit;
class RadLineEdit; class WrpLineEdit;
class RadLabel; class WrpLabel;
class RadPushButton; class WrpPushButton;
class RadVBoxLayout; class WrpVBoxLayout;
class RadHBoxLayout; class WrpHBoxLayout;

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------
#define	RADMAXLINEEDITSIZE	1000		


//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadLineEdit 

Implements the RadIde Line Edit pane. RadLineEdit panes come standard with support 
for Lisp call back managers for key strokes.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
//class RadLineEdit : public RadObject, public QLineEdit
class RadLineEdit : public QLineEdit
{
// 	Q_OBJECT

//  **********
//	Properties
//  **********
	WrpLineEdit*			cpWrpLineEdit;	
	bool					cpReadOnlySW;

//  *********************
//	Lisp Callback Lambdas
//  *********************
	//	Callback managers for Text Line Actions
	TVAL					cpLineEditLambda;
	TVAL					cpFKeyMember;

public:

//  *******
//	Methods
//  *******
//	constructor
	RadLineEdit();

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	static TVAL lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	
private:

protected:
    void keyReleaseEvent(QKeyEvent *e);

};

/********************************************************************************************
WrpLineEdit

Implements the RadIde QLineEdit wrapper class.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
class WrpLineEdit : public RadObject
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********

//  MainMenu Window Elements

//  *******
//	Methods
//  *******


//	WrpTextEdit constructor
	WrpLineEdit(RadLineEdit* parent,QWidget* display,const QString& className)
		: RadObject((QObject*)parent,display,className)
	{
	}

//	WrpTextEdit destructor
	WrpLineEdit::~WrpLineEdit()
	{
		RadLineEdit* aRadLineEdit = (RadLineEdit*)cpWrpRadParent;
		if (aRadLineEdit!=NULL) delete aRadLineEdit;
		cpWrpRadParent = NULL;
		cpWrpQWidget = NULL;
	}
	  

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
	{
		return(((RadLineEdit*)cpWrpRadParent)->lisp(gCP,gTP,argc,argv));
	}
	  

private:

protected:
};

#endif
