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

#ifndef radmainwindow_H
#define radmainwindow_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

													Rad Main Window
									Rapid Analytic Demo Integrated Developer Environment

CHANGE HISTORY
Version	Date		Who		Change
1.0000	2/14/2013	mfk 	First experiments with remote console window object.
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
#include <QtGui/QAction>
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
#include <QtGui/QTabWidget>
#include <QtGui/QTabBar>

#include "radobject.h"
#include "radconsole.h"

extern "C" { // includes for modules written in C
#include "../smtbase/fsmtbase.h" // SmartBase engine declarations
}

class QMenuBar;
class QMenu;
class QAction;
class QLineEdit;
class QPushButton;
class QTextEdit;
class RadObject;
class RadConsoleClearButton;
class RadConsoleRunButton;
class RadConsoleTextEdit;
class RadConsole;
class RadMainTabWidget;
class RadMainWindow;
class RadMainMenu; class WrpMainMenu;	// Warning!! multiple inheritance failed for QMenu, so we use the fallback wrapper strategy
class RadMainMenuItem; class WrpMainMenuItem; 	// Warning!! multiple inheritance failed for QAction, so we use the fallback wrapper strategy 


//	------------------------------------------------------- GLOBALS -----------------------------------------------------------
extern	RadMainWindow*		gpRadMainWindow;

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------

//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadMainWindow 

Implements the RADIDE main demo window.

This is an in process GUI console object meant to be manipulated by local AIS Lisp Lambdas
in the Main Smartbase context running on this local thread.

Notes:	The RadIde Main Window is designed to ALWAYS display a Tab Widget and a Main Menu bar.
		Even if only a Console is displayed, there is ALWAYS a Tab Widget with the Tab "Console"
		displayed, and a Main Menu displayed.
		RadIde display systems are added as new tabs to the Main Tab Widget including: Console,
		Cabinet, Editor, Debugger, and multiple AppWindows.
		
********************************************************************************************/
class RadMainWindow : public RadObject, public QMainWindow // Note: This fails on a conflict of the tr macro if we use QMenuBar!!
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********

//  Main Demo Window Elements
	QWidget*				cpMainWorkSpace;
	RadMainTabWidget*		cpMainTabWidget;

//  *******
//	Methods
//  *******

//	Main Demo Window Methods
	RadMainWindow(const QString& irTitle);

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	
private:
};

/********************************************************************************************
RadMainTabWidget

Implements the RadIde Main Window Tab widget.

This is an in process GUI console object meant to be manipulated by local AIS Lisp Lambdas
in the Main Smartbase context running on this local thread.
********************************************************************************************/
#define		RadMainTabWidget_MaxTabs	100		
class RadMainTabWidget : public RadObject, public QTabWidget
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********
	bool					cpTabResponsiveSW;
	int						cpHighTabIndex;
	int						cpCurrentTabIndex;
	TVAL					cpTabManager[RadMainTabWidget_MaxTabs];
	TVAL					cpTabSelectMgr[RadMainTabWidget_MaxTabs];
	TVAL					cpTabUnSelectMgr[RadMainTabWidget_MaxTabs];
	QWidget*				cpTabDisplayPane[RadMainTabWidget_MaxTabs];
	QString					cpTabTitle[RadMainTabWidget_MaxTabs];

//  MainTabWidget Window Elements

//  *******
//	Methods
//  *******
	int addRadTab(QWidget* displayPane,const QString& irTitle,TVAL tabManager);
	int insertRadTab(int tabIndex,QWidget* displayPane,const QString& irTitle,TVAL tabManager);
	int removeRadTab(int tabIndex);


//	MainTabWidget constructor
	RadMainTabWidget(QWidget * parent);

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

private:

protected:
	bool RadMainTabWidget::event(QEvent *e);
};

/********************************************************************************************
RadMainMenu

Implements the RadIde Main Menu widget.

This class manages the top level menus along the Main Window's menu bar.

This is an in process GUI menu system designed to be manipulated by Lisp Lambdas in the
Smartbase Main Context running on this local thread.

Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
			causes the QT menu bar NOT to display any menus OR to make the menu items unresponsive!!
			Therefore, we use a pure member strategy for the RadMainMenu class and multiple inheritance
			for the RadMainMenuItem class. We also have a fallback wrapper strategy ready if necessary.
********************************************************************************************/
//class RadMainMenu : public RadObject, public QMenu	// Multiple inheritance does not work - Menus fail to display!!
//class RadMainMenu : public QMenu						// Single inheritance does not work - Menus fail to display!!
class RadMainMenu : public RadObject					// Only the pure-member-with-a-wrapper approach works!!
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********
	QMenu*					myPureQMenu;
	//WrpMainMenu*			myWrpMainMenu;		// Wrapper strategy is ready if necessary later

//  MainMenu Window Elements

//  *******
//	Methods
//  *******


//	MainTabWidget constructor
	RadMainMenu(const QString& title);

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

private:

protected:
};

/********************************************************************************************
WrpMainMenu

Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
			causes the QT menu bar NOT to display any menus OR to make the menu items unresponsive!!
			Therefore, we use a pure member strategy for the RadMainMenu class and multiple inheritance
			for the RadMainMenuItem class. We also have a fallback wrapper strategy ready if necessary.
********************************************************************************************/
class WrpMainMenu : public RadObject
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********
	RadMainMenu*				myRadMainMenu;

//  MainMenu Window Elements

//  *******
//	Methods
//  *******


//	MainTabWidget constructor
	WrpMainMenu(RadMainMenu* parent)
		: RadObject()
	{
		myRadMainMenu = parent;
	}

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
	{
		return(myRadMainMenu->lisp(gCP,gTP,argc,argv));
	}

private:

protected:
};

/********************************************************************************************
RadMainMenuItem

Implements the RadIde Main Menu Item widget.

This is an in process GUI menu system designed to be manipulated by Lisp Lambdas in the
Smartbase Main Context running on this local thread.

Warning!!	Multiple inheritance for the RadMainMenu class and the RadMainMenuItem class 
			causes the QT menu bar NOT to display any menus OR to make the menu items unresponsive!!
			Therefore, we use a pure member strategy for the RadMainMenu class and multiple inheritance
			for the RadMainMenuItem class. We also have a fallback wrapper strategy ready if necessary.
********************************************************************************************/
class RadMainMenuItem : public RadObject, public QAction 
{
// 	Q_OBJECT

public:
//  **********
//	Properties
//  **********
	TVAL					cpMenuActionLambda;
	TVAL					cpMenuActionMember;
	TVAL					cpMenuCallBackArgOff;
	TVAL					cpMenuCallBackArgOn;
	QString					cpMenuItemTitle;
	bool					cpEventsOffSW;
	bool					cpCheckable;

//  MainMenuItem Window Elements

//  *******
//	Methods
//  *******


//	RadMainMenuItem constructor
	RadMainMenuItem(const QString & title,RadMainMenu* mainMenu,TVAL menuActionLambda,TVAL menuActionMember,TVAL callBackArgOff,TVAL callBackArgOn,bool checkable,bool checkSW);

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

private:

protected:
	bool RadMainMenuItem::event(QEvent *e);
};


#endif
