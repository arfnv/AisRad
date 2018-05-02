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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

														Rad AIS
									Rapid Analytic Demo Analytic Information Server

The Rad AIS - Rapid Analytic Demo Analytic Information Server - is a minimalist C++ GUI where most of the logic is handled by
the invoked AIS Lisp code. Rad AIS contains minimalist GUI visual components: Command Console, Main Window, and 
single/multiple App Windows. Rad AIS will also be able to send and receive minimalist HTML, XML, and 
other TCP/IP commands.

CHANGE HISTORY
Version	Date		Who		Change
1.0000	 1/26/2013	mfk		Started this Widows Lisp RAD AIS interface (based on Qt so it might work on non Windows platforms also?).
												--------------- ---------------
NOTE
See asys.h for details on the system startup.
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
#include <QtGui/QApplication>
#include <QtGui/QMenuBar>
#include <QtGui/QMenu>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QMessageBox>
#include <QtGui/QTextEdit>
#include <QtGui/QLineEdit>
#include <QtGui/QHBoxLayout>
#include <QtGui/QvBoxLayout>
#include <QtGui/QScrollArea>

#include "../radgui/radglue.h"	
#include "../radgui/radconsole.h"	
#include "../radgui/radmainwindow.h"	

//	------------------------------------------------------ GLOBALS ------------------------------------------------------------

/********************************************************************************************
main  

The main C/C++ function for the Rad AIS system. Rad AIS is an acronym for...
				Rapid Analytic Demo Analytic Information Server Environment.

Rad AIS is designed to allow rapid development of research demos within Analytic Information Server.

Analytic Information Server is unbrella concept for research programming systems which integrate...
		The Smartbase Analytic Engine
		The SmartLisp Compiler
		The MySQL Database Engine (embedded)
		The QT Class Library

Notes:	
		The basic idea is to integrate Lisp, C, and C++ functions in one seemless main thread, with
		the Lisp functions driving most of the demo logic. 

		A shell GUI (Graphic User Interface) is provided in the form of the RadMainWindow class,
		but the GUI will not even display without explicit instructions from Lisp. 

		If the Lisp code does not disply the GUI, then Rad AIS will run as a background process without
		any visible indicators, relying only on file IO.

		Rad AIS cannot operate properly without a Lisp program to execute, and therefore the driving
		elements of any Rad AIS execution are the contents of the Lisp Startup Script which must be
		passed as a command line argument when ais.exe is executed (in MS Windows, clicking on a
		Lisp source code file will suffice to invoke ais.exe with the selected Lisp code file as
		the designated startup script). 

		If no Lisp start up script is provided (in MS Windows, if the user clicked on ais.exe directly)
		then ais.exe will look for a file named "ais.sl" in the executable folder. If no start up Lisp code
		can be found, then ais.exe will launch with default parameters.
		
Programmer Notes:
		The QT/C++ programmer must be reminded that most logic for Rad AIS is found in the Lisp
		code in the Smartbase main context (see RADGlue_OpenMainContext, where the Smartbase Main Context is opened).
		If there are no specific Lisp commands to this Rad Main Window object, then this
		Rad Main Window will NOT display and the process will run without GUI as a background process.

		Study RadMainWindow.cpp for a better understanding RadIde shell GUI provided.
		Study RadGlue.cpp for a better understanding of the tight QT/Lisp integration provided in RadIde.
		Study FSmartbase.c and the AIS HTML Help for a better understanding of the Smartbase Analytic Engine.
		Study the MySQL HTML Help for a better understanding of the MySQL Database Engine.
		Study the QT HTML Help for a better understanding of the QT Class Library.
		Study the Tutorial_Console, and the many other Lisp tutorials for a better understanding of RadIde possibilities.

********************************************************************************************/
int main(int argc, char *argv[])
{
	int n;

	// *********************************************************
	// Initialize a standard QT QApplication to drive Rad AIS
	// Note: Retrieve the all important Lisp start up script
	//       path and file name form the command line arguments.
	// *********************************************************
	gpArgc = argc; for (n = 0; n < argc; ++n) {gpArgv[n] = argv[n];}
	QString aStartupScriptFileName((argc > 1) ? argv[1] : "");
	QApplication aApp(argc, argv);

	// *********************************************************
	// Initialize - BUT do NOT display - the RadMainWindow shell
	//  GUI provided for Rad AIS developers. 
	// Note: The GUI will only display on explicit commandeds
	//       from Lisp. Until the GUI is explicitly displayed,
	//		 ais.exe will run as a background process.
	// *********************************************************
	RadMainWindow mainWindow("Rad AIS - Rapid Analytic Demo Analytic Information Server");

	// *********************************************************
	// Initialize the Smartbase Engine main context and the
	//  embedded MySQL Database. Load, compile, and run the
	//  Lisp start up script. 
	// *********************************************************
	RADGlue_OpenMainContext(aStartupScriptFileName);

	// *********************************************************
	// Run the standard QT QApplication event code until a quit
	//  command is received from Lisp or until the user requests
	//  a quit action from the QT Rad AIS GUI. 
	// *********************************************************
	return(aApp.exec());
}
