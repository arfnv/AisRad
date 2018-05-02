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

#ifndef radtextedit_H
#define radtextedit_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
												RadTextEdit for GUI

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
#include "radtabwidget.h"
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
#define	RADMAXTEXTEDITSIZE	10000000		

//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadTextEdit 

Implements the RadIde Text Edit pane. RadTextEdit panes come standard with horizontal word wrap
at the pane boundry WYSIWYG and vertical scroll bars, full rich text capabilities, and support 
for Lisp call back managers for both key strokes and mouse clicks.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
//class RadTextEdit : public RadObject, public QTextEdit
class RadTextEdit : public QTextEdit
{
// 	Q_OBJECT

public:

//  **********
//	Properties
//  **********
	WrpTextEdit*			cpWrpTextEdit;	
	bool					cpReadOnlySW;
	bool					cpTabsOffSW;

//  *********************
//	Lisp Callback Lambdas
//  *********************
	//	Callback managers for Text Edit Actions
	TVAL					cpTextEditLambda;
	TVAL					cpFKeyMember;
	TVAL					cpMouseSingleClickMember;
	TVAL					cpMouseDoubleClickMember;

//  *******
//	Methods
//  *******
//	constructor
	RadTextEdit();


//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	static TVAL lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	
private:

protected:
    void keyPressEvent(QKeyEvent *e);
    void keyReleaseEvent(QKeyEvent *e);
	void mousePressEvent(QMouseEvent *e);
	void mouseDoubleClickEvent(QMouseEvent *e); 

};

/********************************************************************************************
WrpTextEdit

Implements the RadIde QTextEdit wrapper class.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
class WrpTextEdit : public RadObject
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
	WrpTextEdit(RadTextEdit* parent,QWidget* display,const QString& className)
		: RadObject((QObject*)parent,display,className)
	{
	}

//	WrpTextEdit destructor
	WrpTextEdit::~WrpTextEdit()
	{
		RadTextEdit* aRadTextEdit = (RadTextEdit*)cpWrpRadParent;
		if (aRadTextEdit!=NULL) 
		{
			// Programmer Note: QT/C++ has difficulty cleaning up the
			//                  objects in this circular reference set.
			//                  If the clean up is not handled in this
			//                  exact manner and order, QT either crashes
			//                  or leaks memory.
			//                  --MFK Note:
			//					  Currently leaks memory anyway, even though
			//                    each object created is individually deleted
			//                    in an order that, empirically, does NOT
			//                    crash QT -- we still leak memory!
			//					  This needs to be tracked down and fixed.
			delete aRadTextEdit;
		}
		cpWrpRadParent = NULL;
		cpWrpQWidget = NULL;
	}
	  

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
	{
		return(((RadTextEdit*)cpWrpRadParent)->lisp(gCP,gTP,argc,argv));
	}
	  

private:

protected:
};

#endif
