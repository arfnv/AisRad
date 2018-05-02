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

#ifndef radcombobox_H
#define radcombobox_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
												RadComboBox for GUI

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
#include <QtGui/QComboBox>

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
class RadComboBox; class WrpComboBox;

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------

//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadComboBox 

Implements the RadIde ComboBox.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
//class RadComboBox : public RadObject, public QComboBox
class RadComboBox : public QComboBox
{
// 	Q_OBJECT

//  **********
//	Properties
//  **********
	WrpComboBox*			cpWrpComboBox;	
	bool					cpReadOnlySW;

//  *********************
//	Lisp Callback Lambdas
//  *********************
	//	Callback managers for ComboBox Actions
	TVAL					cpComboBoxLambda;
	TVAL					cpFKeyMember;

public:

//  *******
//	Methods
//  *******
//	constructor
	RadComboBox();

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	static TVAL lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	
private:

protected:
    void keyReleaseEvent(QKeyEvent *e);

};

/********************************************************************************************
WrpComboBox

Implements the RadIde QComboBox wrapper class.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
class WrpComboBox : public RadObject
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


//	WrpComboBox constructor
	WrpComboBox(RadComboBox* parent,QWidget* display,const QString& className)
		: RadObject((QObject*)parent,display,className)
	{
	}

//	WrpComboBox destructor
	WrpComboBox::~WrpComboBox()
	{
		RadComboBox* aRadComboBox = (RadComboBox*)cpWrpRadParent;
		if (aRadComboBox!=NULL) delete aRadComboBox;
		cpWrpRadParent = NULL;
		cpWrpQWidget = NULL;
	}
	  

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
	{
		return(((RadComboBox*)cpWrpRadParent)->lisp(gCP,gTP,argc,argv));
	}
	  

private:

protected:
};

#endif
