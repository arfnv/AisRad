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

#ifndef radconsole_H
#define radconsole_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/radgui/radconsole.h
												Console Display System

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
#include <QtGui/QComboBox>
#include <QtGui/QLineEdit>
#include <QtGui/QHBoxLayout>
#include <QtGui/QvBoxLayout>
#include <QtGui/QScrollArea>

#include "radobject.h"
#include "radmainwindow.h"

extern "C" { // includes for modules written in C
#include "../smtbase/fsmtbase.h" // SmartBase engine declarations
}
class QMenuBar;
class QComboBox;
class QLineEdit;
class QPushButton;
class QTextEdit;
class RadConsoleClearButton;
class RadConsoleRunButton;
class RadConsoleTextEdit;
class RadMainWindow;
class RadConsole;
class RadObject;

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------
extern  RadConsole*			gRadConsole;

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------
#define	RADMAXCONSOLESIZE	500000		
#define	RADMAXCOMMANDSIZE	1000		


//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadConsole

Implements the RadIde console.

This is an in process GUI console object meant to be manipulated by local AIS Lisp Lambdas
in the Main Smartbase context running on this local thread.
********************************************************************************************/
class RadConsole : public RadObject, public QWidget
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********
	bool					cpConsoleInstallStatus;
	int						cpConsoleMainTabIndex;
	QString					cpConsoleTitle;

//  *********************
//	Lisp Callback Lambdas
//  *********************
	//	Callback managers for Console Actions
	TVAL					cpConsoleMgr;
	TVAL					cpClearEventMgr;
	TVAL					cpDisplayEventMgr;
	TVAL					cpFKeyEventMgr;
	TVAL					cpRunEventMgr;
	TVAL					cpMouseSingleClickMgr;
	TVAL					cpMouseDoubleClickMgr;

//  Console Window Elements
	QWidget*				cpConsoleWorkSpace;
	QString					cpConsoleLog;
	QComboBox*				cpCommand;
	RadConsoleClearButton*	cpClearButton;
	RadConsoleRunButton*	cpRunButton;
	RadConsoleTextEdit*		cpConsole;

//  *******
//	Methods
//  *******

//	Console constructor
	RadConsole();

//	Console event processing function
	NUM	processEvents(LpXCONTEXT gCP,LpTHREAD gTP);
	NUM	displayOnConsole(LpCHAR displayMe,NUM newline);

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

private:
};


/********************************************************************************************
RadConsoleClearButton 

Implements the RADIDE main console CLEAR button.

********************************************************************************************/
class RadConsoleClearButton : public QPushButton
{
// 	Q_OBJECT

public:

//	Methods
	RadConsoleClearButton(const QString& irTitle);
	
private:

protected:
    bool event(QEvent *);

};

/********************************************************************************************
RadConsoleRunButton 

Implements the RADIDE main console RUN button.

********************************************************************************************/
class RadConsoleRunButton : public QPushButton
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********
	BOLE					cpEngineBusySW;

//	Methods
	RadConsoleRunButton(BOLE busySW);
	void	SetConsoleBusy(BOLE busySW);
	
private:

protected:
    bool event(QEvent *);

};

/********************************************************************************************
RadConsoleTextEdit 

Implements the RADIDE main console display pane.

********************************************************************************************/
class RadConsoleTextEdit : public QTextEdit
{
// 	Q_OBJECT

public:

//	Methods
	RadConsoleTextEdit();
	
private:

protected:
    void keyPressEvent(QKeyEvent *e);
    void keyReleaseEvent(QKeyEvent *e);
	void mousePressEvent(QMouseEvent *e);
	void mouseDoubleClickEvent(QMouseEvent *e); 


};
#endif
