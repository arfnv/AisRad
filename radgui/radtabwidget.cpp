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
												RadTabWidget for GUI

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

#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "radtabwidget.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadTabWidget 

Constructor for the RadIde tab widget.

Notes:	
		The RadIde Tab Widget is designed to implement a tab widget following the guidelines
		of QTabWidget modified for Lisp direct access and control.

				
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for the RadTabWidget is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).

		Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
		Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
		use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
		classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadTabWidget::RadTabWidget(RadWidget * parent)
	: QTabWidget(parent)
{
	int			n,N;
	
	// Create the wrapper object to Lisp can communicate
	cpWrpTabWidget = new WrpTabWidget(this,this,"WrpTabWidget");

	// Make sure all auxilliary storage is cleared.
	cpCurrentTabIndex = -1;
	cpHighTabIndex = -1;
	for (n = 0; n < RadTabWidget_MaxTabs; ++n) 
	{
		cpTabDisplayPane[n] = NULL;
		cpTabTitle[n] = "";
		cpTabManager[n].Tag = TYVOID;
		cpTabSelectMgr[n].Tag = TYVOID;
		cpTabUnSelectMgr[n].Tag = TYVOID;
	}


}

/********************************************************************************************
RadTabWidget::addRadTab 

Add a Tab to the RadIde Tab Widget.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for the RadTabWidget is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).

********************************************************************************************/
TVAL RadTabWidget::addRadTab(RadWidget* displayPane,const QString& irTitle,TVAL tabManager)
{
	LpXCONTEXT	gCP = gpMainContext;
	LpTHREAD	gTP = gpMainThread;
	int			newTabIndex;
	TVAL		prmv[2];
	int			n,N;
	StartFrame
	DeclareTVAL(tabSelect);
	DeclareTVAL(tabUnSelect);
	DeclareTVAL(result);
	DeclareTVAL(ec);
	EndFrame

	// Add the RAD display system to the RadTabWiget.
	cpTabResponsiveSW = false;
	newTabIndex = QTabWidget::addTab(displayPane,irTitle);
	if (newTabIndex >= RadTabWidget_MaxTabs)
	{	
		// No more room in Main Tabs therefore refuse this add tab.
		QTabWidget::removeTab(newTabIndex);
		cpTabResponsiveSW = true;
		FrameExit(TINT(-1));
	}

	// Update the RADTabWiget auxilliary storage.
	cpTabManager[newTabIndex] = tabManager;
	cpTabSelectMgr[newTabIndex] = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,tabManager,gpSym_tabSelect);
	cpTabUnSelectMgr[newTabIndex] = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,tabManager,gpSym_tabUnSelect);
	cpTabDisplayPane[newTabIndex] = displayPane;
	cpTabTitle[newTabIndex] = irTitle;

	// Tel the previous tab manager that it has be unSelected.
	if ((cpCurrentTabIndex >= 0)&&(cpTabUnSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabUnSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}
	
	// Tell the previous tab manager that it has be unSelected.
	++cpHighTabIndex;
	if ((cpCurrentTabIndex >= 0)&&(cpTabSelectMgr[cpCurrentTabIndex].Tag!=TYVOID))
	{
		prmv[0].Tag = TYNUM;prmv[0].u.Int = cpCurrentTabIndex;
		RADGlue_RunLambdaMember(cpTabManager[cpCurrentTabIndex],cpTabSelectMgr[cpCurrentTabIndex],1,prmv,FALSE);
	}

	cpTabResponsiveSW = true;
	result->Tag = TYNUM;
	result->u.Int = newTabIndex;
	return(*result);
}

/********************************************************************************************
RadTabWidget::insertRadTab 

Insert a Tab into the RadIde Tab Widget.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for the RadTabWidget is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).

********************************************************************************************/
TVAL RadTabWidget::insertRadTab(int tabIndex,RadWidget* displayPane,const QString& irTitle,TVAL tabManager)
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

	// Insert the RAD display system to the RADTabWiget.
	cpTabResponsiveSW = false;
	newTabIndex = QTabWidget::insertTab(tabIndex,displayPane,irTitle);
	if (newTabIndex >= RadTabWidget_MaxTabs)
	{	
		// No more room in Main Tabs therefore refuse this add tab.
		QTabWidget::removeTab(newTabIndex);
		cpTabResponsiveSW = true;
		FrameExit(TINT(-1));
	}

	// Adjust the RadTabWidget tab information array.
	for (n = tabIndex;n<=cpHighTabIndex;++n)
	{
		// Move all RadTabWidget storage right one slot to make room for the new tab.
		nn = n + 1;	
		cpTabManager[nn] = cpTabManager[n];
		cpTabSelectMgr[nn] = cpTabSelectMgr[n];
		cpTabUnSelectMgr[nn] = cpTabUnSelectMgr[n];
		cpTabDisplayPane[nn] = cpTabDisplayPane[n];
		cpTabTitle[nn] = cpTabTitle[n];
	}
	++cpHighTabIndex;
	if (cpCurrentTabIndex >= newTabIndex) ++cpCurrentTabIndex; 

	// Update the RadTabWiget auxilliary storage.
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
	FrameExit(TINT(newTabIndex));
}

/********************************************************************************************
RadTabWidget::removeRadTab 

Remove a Tab from the RadIde Tab Widget.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for the RadTabWidget is found in the Lisp
		code in the Smartbase main context (see main.cpp where the Smartbase Main Context is opened).

********************************************************************************************/
TVAL RadTabWidget::removeRadTab(int tabIndex)
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
	
	// Adjust the RadTabWidget tab information array.
	for (nn = tabIndex;nn<cpHighTabIndex;++nn)
	{
		// Move all RadTabWidget storage left one slot to make room for the new tab.
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
	FrameExit(TINT(cpCurrentTabIndex));
}

/********************************************************************************************
RadTabWidget::event

Manager for the RadIde TabWidget event.

When a Tab is pressed, this event is called, and the RadIde Tab Widget
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
bool RadTabWidget::event(QEvent *e)
{
	bool	result = QTabWidget::event(e);
	TVAL	ret;
	int		eventCode;
	int		newTabIndex;
	TVAL	prmv[2];

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
RadTabWidget::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the main window.

Examples:
	(setq tabIndex (addTab: radWidgetPtr title tabManager))	;; Add a new tab to the RadTabWidget (appended to the right).
	(setq index (count:))									;; Return the number of tabs currently in the RadTabWidget.
	(setq index (currentIndex:))							;; Return the RadTabWidget tab index which has the current focus.
	(setq tabIndex (insertTab: tabIndex radWidgetPtr title tabManager))	;; Insert a new tab into the RadTabWidget (inserted at the specified tab index).
	(removeTab: tabIndex)									;; Remove a new tab from the RadTabWidget.
	(setCurrentIndex: TabIndex)								;; Set the RadTabWidget to focus on the specified tab.
	(setFocus:)												;; Set the RadTabWidget to be the GUI focus.

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadTabWidget::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
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
		*result = TERROR("!RadTabWidget: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"addTab") == 0)
	{
		// (setq tabIndex (addTab: radWidgetPtr title tabManager))	;; Add a new tab to the RadTabWidget (appended to the right).
		// Is an implied setTitle being requested?
		if ((argc>3)&&(argv[1].Tag!=TYPOINTER))
		{
			*ec = TERROR("!RadTabWidget.addTab: 1st argument must be RadWidget object Pointer!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		else if (argc>3)
		{
			// The optional second argument must be the new tab title.
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
				*ec = TERROR("!RadTabWidget.addTab: optional 2nd argument must be of type Text, String, or Symbol only!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the title of the tab to the specified string.
			textQString = textPtr;
		} else
		{
			*ec = TERROR("!RadTabWidget.addTab: expecting 3 arguments radWidgetPtr title tabManager!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Add a new tab to RadTabWidget (appended on the right) 
		radWidgetPtr = (RadWidget*)((WrpWidget*)argv[1].u.Pointer)->cpWrpQWidget;
		*result = addRadTab(radWidgetPtr,textQString,argv[3]);
		goto Last;
	}
	else if (strcmp(messagePtr,"count") == 0)
	{
		// (setq index (count:))				;; Return the number of tabs currently in the RadTabWidget.
		result->Tag = TYNUM;
		result->u.Int = count();
		goto Last;
	}
	else if (strcmp(messagePtr,"currentIndex") == 0)
	{
		// (setq index (currentIndex:))	;; Return the RadTabWidget tab index which has the current focus.
		result->Tag = TYNUM;
		result->u.Int = currentIndex();
		goto Last;
	}
	else if (strcmp(messagePtr,"insertTab") == 0)
	{
		// (setq tabIndex (insertTab: tabIndex radWidgetPtr title tabManager))	;; Insert a new tab into the RadTabWidget (inserted at the specified tab index).
		// Is an implied setTitle being requested?
		if ((argc>4)&&(argv[1].Tag!=TYNUM)&&(argv[2].Tag!=TYPOINTER))
		{
			*ec = TERROR("!RadTabWidget.insertTab: 1st argument must be a tab index and 2nd argument must be RadWidget object Pointer!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		else if (argc>4)
		{
			// The third argument must be the new tab title.
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
				*ec = TERROR("!RadTabWidget.insertTab: 3rd argument must be of type Text, String, or Symbol only!");
				RADGlue_ProcessEvents(gCP,gTP);
				FrameExit(*ec);
			}

			// Set the title of the tab to the specified string.
			textQString = textPtr;
		} else
		{
			*ec = TERROR("!RadTabWidget.insertTab: expecting 4 arguments tabIndex radWidgetPtr title tabManager!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Insert a new tab into RadTabWidget (inserted at the specified tab index) 
		radWidgetPtr = (RadWidget*)((WrpWidget*)argv[1].u.Pointer)->cpWrpQWidget;
		*result = insertRadTab(argv[1].u.Int,radWidgetPtr,textQString,argv[4]);
		goto Last;
	}
	else if (strcmp(messagePtr,"removeTab") == 0)
	{
		// (removeTab: tabIndex)	;; Remove a new tab from the RadTabWidget.
		// Is an implied setTitle being requested?
		if ((argc>1)&&(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadTabWidget.removeTab: 1st argument must be a tab index!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Remove a new tab from RadTabWidget 
		*result = removeRadTab(argv[1].u.Int);
		goto Last;
	}
	else if (strcmp(messagePtr,"setCurrentIndex") == 0)
	{
		// (setCurrentIndex: TabIndex)								;; Set the RadTabWidget to focus on the specified tab.
		if ((argc>1)&&(argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadTabWidget.setCurrentIndex: 1st argument must be Tab Index!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the specified RadTabWidget tab index.
		setCurrentIndex(argv[1].u.Int);

		// Return the selected RadTabWidget tab index. 
		*result = argv[1];
		goto Last;
	}
	else if (strcmp(messagePtr,"setFocus") == 0)
	{
		// (setFocus:)	;; Set the RadTabWidget to be the GUI focus.
		setFocus();

		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(gCP->Tval_TRUE);
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

Lisp object creation method for the RadTabWidget class.

Example:
	(setq radTabWidgetPtr (qt new: RadTabWidget: tabPosition tabShape))	;; Create a new RadTabWidget
					...
	(setq RadTabWidgetPtr (qt delete: RadTabWidgetPtr)) ;; Delete a RadTabWidget and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory tpo the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 untol 1000000 (setq p (qt new: RadWidget:))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.


********************************************************************************************/
TVAL RadTabWidget::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	TVAL	result;

	// Make sure the arguments are correct
	if ((argc<2)||(argv[0].Tag!=TYNUM)||(argv[1].Tag!=TYNUM))
	{
		return(TERROR("!RadTabWidget.new: expected two Integer arguments for tabPosition and tabShape!"));
	}

	// Make sure the tab position argument is correct
	if ((argv[0].u.Int<0)||(argv[0].u.Int>3))
	{
		return(TERROR("!RadTabWidget.new: tabPosition must be between 0 and 3!"));
	}

	// Make sure the tab shape argument is correct
	if ((argv[1].u.Int<0)||(argv[1].u.Int>1))
	{
		return(TERROR("!RadTabWidget.new: tabShape must be between 0 and 1!"));
	}


	RadTabWidget*	radTabWidgetPtr = new RadTabWidget(NULL);
	radTabWidgetPtr->setTabPosition((QTabWidget::TabPosition)argv[0].u.Int);
	radTabWidgetPtr->setTabShape((QTabWidget::TabShape)argv[1].u.Int);

	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radTabWidgetPtr->cpWrpTabWidget;
	return(result);
}


