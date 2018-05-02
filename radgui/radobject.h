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

#ifndef radobject_H
#define radobject_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

												QT/Lisp Interface Base Object 
									Rapid Analytic Demo Integrated Developer Environment

CHANGE HISTORY
Version	Date		Who		Change
1.0000	7/25/2014	mfk 	First experiments with remote console window object.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

/**************************************************************************************************************************
Note:	There are macro naming conflicts between QT and FSmartbase.h. 
        QT derived class headers must come first! 
		Be careful about the macros you use in QT derived classes!  
		Known conflicts include: isNull(), and multiple inheritance with QMainWindow. 
**************************************************************************************************************************/

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>

extern "C" { // includes for modules written in C
#include "../smtbase/fsmtbase.h" // SmartBase engine declarations
}


//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadObject

Implements the RadIde QT/Lisp Interface Base Object.

This is the RadIde Base Object class from which all other RadIde interface objects inherit.
Its main function is to declare the virtual method - lisp - which allows direct
communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

WARNING:	All RadObject derived classes MUST inherit from RadObject FIRST i.e.

									YES
class RadDerivedClass : public RadObject, public QSomeClass		// Correct multiple inheritance

									NO
class RadDerivedClass : public QSomeClass, public RadObject		// Incorrect multiple inheritance


********************************************************************************************/
class RadObject : public QObject
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********

//  Main Elements
	QObject*			cpWrpRadParent;	
	QWidget*			cpWrpQWidget;	
	QString				cpWrpClassName;	

//  *******
//	Methods
//  *******
	//	 constructors
	RadObject();
	RadObject(QObject* parent,QWidget* display,const QString& className);

	//	Indirection and owner methods
	QObject* myRadParent();
	QWidget* myQWidget();
	QString* myClassName();

	//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	static TVAL lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	
private:
};


#endif
