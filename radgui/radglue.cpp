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
														RadGlue

This source file contains the functions necessary to marry the SmartBase engine with Rad AIS. 


														Rad AIS
									Rapid Analytic Demo Analytic Information Server

The Rad AIS - Rapid Analytic Demo Analytic Information Server - is a minimalist C++ GUI where most of the logic is handled by
the invoked AIS Lisp code. Rad AIS contains minimalist GUI visual components: Command Console, Main Window, and 
single/multiple App Windows. Rad AIS will also be able to send and receive minimalist HTML, XML, and 
other TCP/IP commands.

RADGlue.cpp

CHANGE HISTORY
Version	Date		Who		Change
1.0000	12/16/2013	mfk		Convert to radglue.cpp.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#ifdef _LINUX
#include "sys/time.h"
#include <sys/mman.h>
#include <stdlib.h>
#include <unistd.h>
#endif
#ifdef _MSVC
#ifdef _M64
#include <windows.h>
#endif
#endif
#include <assert.h>
#include <time.h>
#include <stdlib.h>
#include <QtCore/QString>
#include <QtCore/QDir>
#include <QtCore/QFileInfo>
#include <QtCore/QVariant>
#include <QtCore/QLibrary>
#include <QtCore/QMutex>
#include <QtXml/QtXml>
#include <QtCore/QThread>
#include <QtGui/QClipBoard>
#include <QtGui/QTextCursor>
#include <QtGui/QTextDocumentFragment>
#include <QtGui/QDialog>

// Note: there are macro naming conflicts between Qt and FSmartbase.h. qt derived class headers must come first! Be careful
// about the macros you use in qt derived classes compiled in this file!  Known conflicts include: isNull(). 

#include "radglue.h"
#include "radwidget.h"
#include "radtextedit.h"
#include "radtabwidget.h"
#include "radmainwindow.h"
#include "radconsole.h"
#include "raddialog.h"
#include "radcheckbox.h"
#include "radpushbutton.h"
#include "radlineedit.h"
#include "radlabel.h"
#include "radvboxlayout.h"
#include "radhboxlayout.h"
#include "radcombobox.h"
#include "radtcpsocket.h"
#include "radtcpserver.h"

extern "C" { // includes for modules written in C
#include "../smtbase/fsmtbase.h" // SmartBase engine declarations
#include "mysql.h"
}

//	------------------------------------------------------ GLOBALS ------------------------------------------------------------
struct		FSmartbase_HostCallBackFunctions	gpFuncs;
LpXCONTEXT										gpMainContext = NULL;
LpTHREAD										gpMainThread = NULL;
QString											gpContextName;
QString											gpContextMemory;
QString											gpContextObjHdrs;
QString											gpContextHeap;
QString											gpContextCStack;
QString											gpMaxRecursions;
QString											gpScriptFileName;
QString											gpMySQLDataDir;
QString											gpScriptPath;
QString											gpAisHelpURL;
QString											gpQtHelpURL;
QString											gpMySQLHelpURL;
QString											gpMyLogFileName;
QString											gpMyTcpPort;

int												gpArgc;
QString											gpArgv[maxMainArguments];

TVAL											gpSym__ais;
TVAL											gpSym__log;
TVAL											gpSym__path;
TVAL											gpSym_clear;
TVAL											gpSym_close;
TVAL											gpSym_dblclick;
TVAL											gpSym_display;
TVAL											gpSym_fkey;
TVAL											gpSym_httpRequest;
TVAL											gpSym_press;
TVAL											gpSym_run;
TVAL											gpSym_singleclick;
TVAL											gpSym_tabSelect;
TVAL											gpSym_tabUnSelect;

/*-----------------------------------------------------------------
RADGlue_OpenMainContext

It is this RADGlue_OpenMainContext, where the Smartbase Main Context is opened.
All logic for managing Lisp start up and launch scripts is contained in this
function.

	Programmer Notes:
	As a practicle matter, this RAD version of AIS requires a minimum of the
	AIS executable: ais.exe AND, at least a launch script: ais.sl
	If the application is to be delivered without Lisp source code, then an
	additional binary file will be needed: ais.bin

	This RAD implementation of AIS supports the delivery of clickable ???.exe
	AIS RAD applications. This goal is accomplished by simply renaming the
	base ais.exe, ais.sl, and ais.bin files to a new deliverable name such as
	???.exe, ???.sl, ???.bin

	The AIS RAD application launch strategies, to accomplish clickable 
	delivered applications, is as follows.

	The name of this executable is determined ???.exe (remember the ais.exe
	executable may have been renamed to deliver a clickable executable),
	and the following application launch strategy is employed.

		Look for the launch script named ???.sl, in the executable folder,
		and for the (optional) binary file ???.bin, in the executable folder.
		If no launch script can be found, then use the default settings.

		Scan the launch script, and search for the following list of 
		parameters:

		;#ContextName=Default
		;#heap=400
		;#objects=10  
		;#cstack=1
		;#aishelp=C:\Ais\onlinedocs\_AAStartMeFirst.html
		;#qthelp=C:\Qt\4.6.2\doc\html\index.html
		;#mysqlhelp=www.mysql.com
		;#script=yes

		Note: Each of the above start up parameters must be on a line all by themselves somewhere in the launch script.
	- 
		If a start script has been specified - ;#script=yes, then look to see
		if there is a startup script argument passed with this executable (the
		user has clicked on a start script to launch this app). 

		If a startup script is present AND if the ;#script=yes parameter is set, 
		Scan the startup script, and search for the following list of 
		parameters:

		;#ContextName=Default
		;#heap=400
		;#objects=10  
		;#cstack=1
		;#aishelp=C:\Ais\onlinedocs\_AAStartMeFirst.html
		;#qthelp=C:\Qt\4.6.2\doc\html\index.html
		;#mysqlhelp=www.mysql.com

		Note: Each of the above start up parameters must be on a line all by themselves somewhere in the start up script.


		Open the main Smartbase context with final parameter settings.

		Build the _ais Structure assigning all of the relevant parameters
		and settings as shown in the code.
		
		Then run the launch script.
		
		If a startup script is present AND if the ;#script=yes parameter is set, 
		then run the startup script.

	Study Notes:
	The QT/C++ programmer must be reminded that most logic for Rad AIS is found in the Lisp
	code in the Smartbase main context. 

	Study RadGlue.cpp for a better understanding of the tight QT/Lisp integration provided in RadIde.
	Study RadMainWindow.cpp for a better understanding RadIde shell GUI provided.
	Study FSmartbase.c and the AIS HTML Help for a better understanding of the Smartbase Analytic Engine.
	Study the MySQL HTML Help for a better understanding of the MySQL Database Engine.
	Study the QT HTML Help for a better understanding of the QT Class Library.
	Study the Tutorial_Console, and the many other Lisp tutorials for a better understanding of RadIde possibilities.

------------------------------------------------------------------*/
long	RADGlue_OpenMainContext(QString startScriptFilePathName)
{
	LpXCONTEXT	gCP;				
	LpTHREAD	gTP;				
	NUM			m, n, N;
	char		contextName[256] = "Main";
	char		aisHelpURL[256] = "";
	char		qtHelpURL[256] = "";
	char		mySQLHelpURL[256] = "";
	char		myLogFileName[256] = "";
	char		mySQLDataDir[256] = "";
	char		myTcpPort[256] = "80";	// Note: "80" is the standard Internet default port for HTML TCP/IP communications
	char		errorMsg[256] = "";
	char		temp[256] = "";
    const char *apServerArgs[] = {"none",
                                  "--datadir=./data",
                                  "--language=./english",
                                  "--basedir=./"
                                  };
    int			aArgCount = sizeof(apServerArgs)/sizeof(char*);
	NUM 		minimumHeap = FSMARTBASE_MINBLOCKSIZE;
	NUM			contextHeap = minimumHeap;
	NUM 		minimumObjHdrs = FSMARTBASE_MINBLOCKSIZE/10;
	NUM 		contextObjHdrs = minimumObjHdrs;
	NUM 		contextCStack = FSMARTBASE_MAINCONTEXTSTACK;
	NUM 		contextMemory = contextHeap + contextObjHdrs + contextCStack;
	bool		startupScriptSW = true;
	QString		appName = "ais";
	QString		lispCommand;
	QString		script = "";
	QString		aScriptLine = "";
	QString		startScript = "";
	QString		launchScriptFilePathName = "";
	QString		launchCommand =	"";
	QStringList slist;
	QString		tempL;
	QString		tempR;
	QString		aDataDirStr;
    QString		aInstallDirStr;
    QString		aLanguageStr;

	gpMySQLDataDir = "";
	gpScriptFileName = "";
	gpScriptPath = "";
	gpMyLogFileName = "";
	gpContextHeap = "";
	gpContextMemory = "";
	gpContextObjHdrs = "";
	gpContextCStack = "";
	gpMaxRecursions = "";
	gpAisHelpURL = aisHelpURL;
	gpQtHelpURL = qtHelpURL;
	gpMySQLHelpURL = mySQLHelpURL;
	gpMyLogFileName = myLogFileName;
	gpMyTcpPort = "80";	// Note: "80" would be the standard Internet default port for HTML TCP/IP communications

	// Determine the application base name
	// Note: The ais.exe application may have been renamed to provide a clickable executable.
	N = gpApp.applicationFilePath().length();
	N = N - gpApp.applicationDirPath().length() - 1;
	appName = gpApp.applicationFilePath().right(N);
	if (appName.right(4) == ".exe")
	{
		N = appName.length() - 4;
		appName = appName.left(N);
	}

	// Look for the standard launch script
	// Note: If the launch script and startup script are named the same
	//       then we skip the launch script step.
	launchScriptFilePathName = gpApp.applicationDirPath() + "/" + appName + ".sl";
	if (launchScriptFilePathName != startScriptFilePathName)
	{
		QFile flaunch(launchScriptFilePathName);
		if (!flaunch.open(QFile::ReadOnly | QFile::Text))
		{
			launchScriptFilePathName = "";
			launchCommand = "";
		}
		else
		{
			// Read the launch script file one line at a time.
			// Note: Look for specific ";#name=arg" launch parameters
			launchCommand = "(runScript {"+launchScriptFilePathName+"})";

			QTextStream launchIn(&flaunch);
			while (!launchIn.atEnd())
			{	
				aScriptLine = launchIn.readLine(1024);
				if (aScriptLine.left(2)==";#")
				{
					aScriptLine = aScriptLine.mid(2);
					slist = aScriptLine.split("=");
					tempL = slist[0].toAscii().trimmed();
					tempR = slist[1].toAscii().trimmed();

					// Is this ;#memory launch parameter?
					if (tempL=="ContextName") 
					{
						strncpy(contextName,tempR.toAscii(),255);
						contextName[tempR.length()] = 0;
					} 
					else if (tempL=="heap") 
					{
						contextHeap = tempR.toDouble()*1000000.0;
					}
					else if (tempL=="objects") 
					{
						contextObjHdrs = tempR.toDouble()*1000000.0;
					}
					else if (tempL=="cstack") 
					{
						contextCStack = tempR.toDouble()*1000000.0;
					}
					else if (tempL=="aishelp") 
					{
						strncpy(aisHelpURL,tempR.toAscii(),255);
						aisHelpURL[tempR.length()] = 0;
					}
					else if (tempL=="qthelp") 
					{
						strncpy(qtHelpURL,tempR.toAscii(),255);
						qtHelpURL[tempR.length()] = 0;
					}
					else if (tempL=="mysqlhelp") 
					{
						strncpy(mySQLHelpURL,tempR.toAscii(),255);
						mySQLHelpURL[tempR.length()] = 0;
					}
					else if (tempL=="mysqldata") 
					{
						strncpy(mySQLDataDir,tempR.toAscii(),255);
						mySQLDataDir[tempR.length()] = 0;
					}
					else if (tempL=="logfile") 
					{
						strncpy(myLogFileName,tempR.toAscii(),255);
						myLogFileName[tempR.length()] = 0;
					}
					else if (tempL=="tcpport") 
					{
						strncpy(myTcpPort,tempR.toAscii(),255);
						myTcpPort[tempR.length()] = 0;
					}
					else if (tempL=="script") 
					{
						strncpy(temp,tempR.toAscii(),255);
						temp[tempR.length()] = 0;
						if (strcmp(temp,"yes") == 0) 
							startupScriptSW = true;
						else
							startupScriptSW = false;
					}
				}
			} // end while
		}
	}

	// Identify the startup script file and path names (if requested).
	if (startScriptFilePathName != "")
	{
		gpScriptFileName = startScriptFilePathName;
		lispCommand = "(runScript {"+gpScriptFileName+"})";
		gpScriptPath = gpScriptFileName;
		N = gpScriptFileName.length();
		for (n=N-1;n>=0;--n)
		{
			m = n;
			if ((gpScriptFileName.at(n)=='/')||(gpScriptFileName.at(n)=='\\')||(gpScriptFileName.at(n)==':'))
			{
				gpScriptPath = gpScriptPath.left(m);
				n = 0;
			}
		}
	}
	else
	{
		gpScriptFileName = "";
		lispCommand = "";
		gpScriptPath = "";
	}

	// Scan the startup script file looking for parameters (if requested).
	if ((startScriptFilePathName == "") || (startupScriptSW == false))
	{
		lispCommand = "";
	}
	else
	{
		// Load the context creation parameters from the specified start script file.
		QFile fscript(gpScriptFileName);
		if (!fscript.open(QFile::ReadOnly | QFile::Text))
		{
			RADGlue_DisplayOnConsole(NULL,NULL,"RADAIS encountered an error reading start up script",TRUE);
			lispCommand = "";
		}
		else
		{
			QTextStream in(&fscript);

			// Read the start up script file one line at a time.
			// Note: Look for specific ";#name=arg" launch parameters
			while (!in.atEnd())
			{	
				QString aScriptLine = in.readLine(1024);
				if (aScriptLine.left(2)==";#")
				{
					aScriptLine = aScriptLine.mid(2);
					QStringList slist = aScriptLine.split("=");
					QString tempL = slist[0].toAscii().trimmed();
					QString tempR = slist[1].toAscii().trimmed();

					// Is this ;#memory launch parameter?
					if (tempL=="ContextName") 
					{
						strncpy(contextName,tempR.toAscii(),63);
						contextName[tempR.length()] = 0;
					} 
					else if (tempL=="heap") 
					{
						contextHeap = tempR.toDouble()*1000000.0;
					}
					else if (tempL=="objects") 
					{
						contextObjHdrs = tempR.toDouble()*1000000.0;
					}
					else if (tempL=="cstack") 
					{
						contextCStack = tempR.toDouble()*1000000.0;
					}
					else if (tempL=="aishelp") 
					{
						strncpy(aisHelpURL,tempR.toAscii(),255);
						aisHelpURL[tempR.length()] = 0;
					}
					else if (tempL=="qthelp") 
					{
						strncpy(qtHelpURL,tempR.toAscii(),255);
						qtHelpURL[tempR.length()] = 0;
					}
					else if (tempL=="mysqlhelp") 
					{
						strncpy(mySQLHelpURL,tempR.toAscii(),255);
						mySQLHelpURL[tempR.length()] = 0;
					}
					else if (tempL=="mysqldata") 
					{
						strncpy(mySQLDataDir,tempR.toAscii(),255);
						mySQLDataDir[tempR.length()] = 0;
					}
					else if (tempL=="logfile") 
					{
						strncpy(myLogFileName,tempR.toAscii(),255);
						myLogFileName[tempR.length()] = 0;
					}
					else if (tempL=="tcpport") 
					{
						strncpy(myTcpPort,tempR.toAscii(),255);
						myTcpPort[tempR.length()] = 0;
					}
				}
			} // end while
		}
	}


	// For the Main Context, the context stack must be the exact size of the stack in the RadIde VisualStudio/Properties/Linker/System section.
	contextCStack = FSMARTBASE_MAINCONTEXTSTACK;

	// The total context memory requested is the sum of the requested object headers and the requested heap.
	contextMemory = contextHeap + contextObjHdrs + contextCStack;
	gpContextMemory = QString::number(contextMemory);
	gpContextHeap = QString::number(contextHeap);
	gpContextObjHdrs = QString::number(contextObjHdrs);
	gpContextCStack = QString::number(contextCStack);
	gpMaxRecursions = QString::number(FSMARTBASE_MAINCONTEXTMAXRECURSIONS);
	gpAisHelpURL = aisHelpURL;
	gpQtHelpURL = qtHelpURL;
	gpMySQLHelpURL = mySQLHelpURL;
	gpMyLogFileName = myLogFileName;
	gpMySQLDataDir = mySQLDataDir;
	gpMyTcpPort = myTcpPort;

	// Initialize the static FSmartbase_HostCallBackFunctions structure.
	strcpy(gpFuncs.ContextName,contextName);
	gpContextName = contextName;
	gpFuncs._Host_Escape = RADGlue_Escape;
	gpFuncs._Host_Display = RADGlue_DisplayOnConsoleFromLisp;
	gpFuncs._Host_Escape = NULL;		// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_Display = NULL;		// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_UpdateState = NULL;	// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_OpenF = NULL;			// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_ReadF = NULL;			// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_WriteF = NULL;		// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_SeekF = NULL;			// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_ResizeF = NULL;		// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_CloseF = NULL;		// Let the Smartbase engine default to its own internal function
	gpFuncs._Host_RegisterLispFunctions = RADGlue_RegisterLispFunctions;
	gpFuncs._EmbeddedMySQLEnabled = __EXMYSQL;
	if ((gpFuncs._EmbeddedMySQLEnabled == 1) && (gpMySQLDataDir != "")) 
		{
		gpFuncs._EmbeddedMySQLEnabled = TRUE;

		// Look for the embedded MySQL database directory and the standard MySQL language files.
		// Note: If the necessary embedded MySQL files and directories are not present,
		//       then MySQL cannot be properly initialized.
		gpMySQLDataDir = gpScriptPath + "/" + gpMySQLDataDir;
		aDataDirStr = "--datadir=" + gpMySQLDataDir;
		aLanguageStr = "--language=" + gpApp.applicationDirPath() + "/mysqlmsgs";
		aInstallDirStr = "--basedir=" + gpApp.applicationDirPath() + "/";

#if _MSVC
		apServerArgs[1] = _strdup(qPrintable(aDataDirStr));
		apServerArgs[2] = _strdup(qPrintable(aLanguageStr));
		apServerArgs[3] = _strdup(qPrintable(aInstallDirStr));
#else
		apServerArgs[1] = strdup(qPrintable(aDataDirStr));
		apServerArgs[2] = strdup(qPrintable(aLanguageStr));
		apServerArgs[3] = strdup(qPrintable(aInstallDirStr));
#endif

		// Implementation Note: If MySQL is included (FSmtbase.h), we must also
		// add the following include path to the properties for this source file:
		// "$MYSQLDIR\include"
		if (mysql_library_init(aArgCount, const_cast<char**>(apServerArgs), NULL) != 0)
			{
			// AIS: embedded MySQL library initialization failed - the SQL feature will be disabled.
			//      This can be caused by several reasons:
			//         Missing or no read/write permission to data directory
			//         Missing or no read/write permission to language file
			//         Version of errmsg.sys does not match version of MySQL library
			return(FALSE);
			}

		free((void*)apServerArgs[3]);
		free((void*)apServerArgs[2]);
		free((void*)apServerArgs[1]);
		}
	else
		{
		gpFuncs._EmbeddedMySQLEnabled = FALSE;
		}


	// If there is no start script, then default the main context load parameters.
	gCP = gpMainContext = FSmartbase_MainContextStart(contextName,contextHeap,contextCStack,contextObjHdrs,&gpFuncs,errorMsg);
	if (gpMainContext == NULL)
		{
		RADGlue_DisplayOnConsole(NULL,NULL,errorMsg,TRUE);
		return(FALSE);	
		}
	gTP = gpMainThread = gpMainContext->ThreadBlocks;

	// Supply a default log file name (if necessary)
	if (gpMyLogFileName == "")
		{
		gpMyLogFileName = gpScriptPath + "/ais.log";
		}

	// Set the _ais Structure in the Main Context
	// Note: We ALWAYS pass the startup script information in the _ais Structure
	//       because even if the startup script argument is not used as a startup
	//       script it may be an important argument to the clickable application.
	//		 For instance an editor app might use the startup argument as a text
	//       file to opened on launch.
	script = "(setq _ais (new Structure: ContextName: {" + gpContextName + "}" +
		" Heap: " + gpContextHeap +
		" Objects: " + gpContextObjHdrs +
		" CStack: " + gpContextCStack +
		" MaxRecursions: " + gpMaxRecursions +
		" ExeFileName: {" + gpApp.applicationFilePath() + "}" +
		" ExePath: {" + gpApp.applicationDirPath() + "}" +
		" LogFileName: {" + gpMyLogFileName + "}" +
		" ScriptFileName: {" + gpScriptFileName + "}" +
		" ScriptPath: {" + gpScriptPath + "}" +
		" MySQLDataDir: {" + gpMySQLDataDir + "}" +
		" MachineName: {localhost} HostIP: {127.0.0.1}" +
		" AisHelpURL: {" + gpAisHelpURL + "}" +
		" QtHelpURL: {" + gpQtHelpURL + "}" +
		" MySQLHelpURL: {" + gpMySQLHelpURL + "}" +
		" TcpPort: " + gpMyTcpPort +
#ifdef _WIN
		" OS: MSWindows:" +
#else
		" OS: Linux:" +
#endif
		" Argv: #(";
	for (n = 0; n < gpArgc; ++n) { script = script + "{" + gpArgv[n] + "} "; }
	script = script + ")))";
	RADGlue_Run(script, FALSE);

	// Set the _log variable to the name of the AIS log file.
	script = "(setq _log _ais.LogFileName)";
	RADGlue_Run(script,FALSE);

	// Set the _path variable to the name of the AIS Script path.
	script = "(setq _path _ais.ScriptPath)";
	RADGlue_Run(script,FALSE);

	// Lock all the globals in the Main Context
	// Note: This prevents the Lisp (clear) command from deleting them.
	script = "(lock _globals)";
	RADGlue_Run(script,FALSE);

	// Set the Console Window Title
	if (contextMemory < 1000000) gpContextMemory = QString::number(contextMemory/1000)+"K";
	else
	if (contextMemory < 1000000000) gpContextMemory = QString::number(contextMemory/1000000)+"M";
	else
	if (contextMemory < 1000000000000) gpContextMemory = QString::number(contextMemory/1000000000)+"G";
	gpRadMainWindow->setWindowTitle("AIS - Analytic Information Server - "+(QString)contextName+"("+gpContextMemory+")");

	// Define the global Lisp symbols needed for all functions in RadIde AND protect them from garbage collection!!
	gpSym__ais = TSYMBOL("_ais");gpSym__ais.u.Symbol->itsGlobalLock = 1;
	gpSym__log = TSYMBOL("_log");gpSym__log.u.Symbol->itsGlobalLock = 1;
	gpSym__path = TSYMBOL("_path");gpSym__path.u.Symbol->itsGlobalLock = 1;
	gpSym_clear = TSYMBOL("clear");gpSym_clear.u.Symbol->itsGlobalLock = 1;
	gpSym_close = TSYMBOL("close");gpSym_close.u.Symbol->itsGlobalLock = 1;
	gpSym_dblclick = TSYMBOL("dblclick");gpSym_dblclick.u.Symbol->itsGlobalLock = 1;
	gpSym_display = TSYMBOL("display");gpSym_display.u.Symbol->itsGlobalLock = 1;
	gpSym_fkey = TSYMBOL("fkey");gpSym_fkey.u.Symbol->itsGlobalLock = 1;
	gpSym_httpRequest = TSYMBOL("httpRequest");gpSym_httpRequest.u.Symbol->itsGlobalLock = 1;
	gpSym_press = TSYMBOL("press");gpSym_press.u.Symbol->itsGlobalLock = 1;
	gpSym_run = TSYMBOL("run");gpSym_run.u.Symbol->itsGlobalLock = 1;
	gpSym_singleclick = TSYMBOL("singleclick");gpSym_singleclick.u.Symbol->itsGlobalLock = 1;
	gpSym_tabSelect = TSYMBOL("tabSelect");gpSym_tabSelect.u.Symbol->itsGlobalLock = 1;
	gpSym_tabUnSelect = TSYMBOL("tabUnSelect");gpSym_tabUnSelect.u.Symbol->itsGlobalLock = 1;

	// Execute the launch script (if one is present)
	if (launchCommand != "") 
		RADGlue_Run(launchCommand,TRUE);

	// Execute the start up script (if one is present)
	if (lispCommand != "") 
		RADGlue_Run(lispCommand,TRUE);
	
	return(TRUE);	
}

/*-----------------------------------------------------------------
RADGlue_QT

Sends messages to the QT Rad AIS derived classes.

There are three static RAD objects: 

	console
	mainWindow
	new 

Only one copy of each may exist, and they must be referenced by name.
All other RAD objects are RadObject derived and must be referred to by C++ object pointer.

(qt receiver message ......)

Args:	receiver	Either a Symbol to a valid QT derived class name OR a Pointer to an object from a RadObject derived class.
		message		Symbol indicating the action which the receiver is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the receiver.

Examples:
(setq radObjectPtr (qt new: RadClassName: .....)) ;; Create an object of the specified QT Rad class type
(qt delete: radObjectPtr) ;; Delete an object of the specified QT Rad class type
(qt console: maximize: (Optional)title)		;; Show console display system in main window and show window maximized
(qt console: minimize: (Optional)title)		;; Show console display system in main window and show window minimized
(qt console: setTitle: title)				;; Set the console title on the console display system
(qt mainWindow: setTitle: title)			;; Set the main window title on the console display system


Programmer Note:
Please look at the code below to determine the RadObject derived classes surfaced to Lisp. Every class and message,
in the code below, must be documented in the AIS QT online documentation (see the AisRadDoc application). Also
Note that most messages to this qt function are passed on to their appropriate RadObject derived classes. So, 
for both a complete understanding of the message action AND to properly document these messages in AisRadDocs,
one must study the message path all the way through to the appropriate receiving class code. This is especially
true of the (qt new: RadClassName: ...) message which is always passed on to the static lispNew function for the 
appropriate RadObject derived class.

The (qt delete: RadObjectPtr) is a special case because of the nature of C++ memory management. Lisp programmers,
who are used to automatic memory management, must be especially diligent when deleting QT objects. C++ is rather
unforgiving about object deletion. If an object is not explicitly deleted, either directly or indirectly via the class 
destructor, then the memory will be forever unavailable to the C++ heap (known as a memory leak). On the other hand,
if the object is deleted twice, the entire application will crash. Therefore here are some rules-of-thumb for deleting
RadObject derived QT objects and for good coding practices in the code surfacing new RadObject derived classes to Lisp.

Rule #1: ALWAYS delete this way: (setq p (qt delete: p)). NEVER delete this way (qt delete: p)!!

Rule #2: When adding new QT RadObject derived classes, make sure that all memory in members and properties of the new
	     RadObject derived class are properly deleted by the class destructor. This simple test should always be performed
		 when testing the C++ code of a newly surfaced RadObject derived class. Launch the Windows Task Manger application,
		 then launch RadIde with a console and run the following code from the console...

			(loop for n from 0 until 1000000 do (setq p (qt new: RadNewClassName: ...)) (setq p (qt delete: p)))

		 If the application crashes OR if the Windows task manager shows that RadIde is increasing its memory usage 
		 everytime the above code is run - then the C++ destructor code, for the newly surfaced RadObject derived
		 class, needs additional work. 

------------------------------------------------------------------*/
PUBLIC	TVAL RADGlue_QT(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	struct		FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	LpCHAR		messagePtr;
	LpCHAR		receiverPtr;
	LpCHAR		textPtr;
	char		errorMsg[1000];
	QString		textQString;
	RadObject*	objectPtr;
	StartFrame
	DeclareTVAL(ec);
	DeclareTVAL(result);
	EndFrame

/* ***********************************************************/
/* The first argument must be a receiver Symbol or a Pointer */
/* ***********************************************************/
if ((argc<1)||((argv[0].Tag!=TYSYMBOL)&&(argv[0].Tag!=TYPOINTER)))
{
	InvalidReceiver:
	*ec = TERROR("!qt: first argument must be a valid receiver!");
	FrameExit(*ec);
}

// Do we have a receiver which is one of the three static RAD objects?
// Note: There are three static RAD objects: console, mainWindow, & new. 
//		 Only one copy of each may exist and they must be referenced by name.
//		 All other RAD objects are RadObject derived and must be referred 
//		 to by C++ object pointer.
if ((argv[0].Tag == TYSYMBOL)||(argv[0].Tag == TYQUOTEDSYMBOL))
{
	receiverPtr = SymbolArray(argv[0]);
	if (strcmp(receiverPtr,"console") == 0)
	{
		argv[0].Tag = TYPOINTER;
		argv[0].u.Pointer = (POINTER)gRadConsole;
		goto VirtualRadObjectFunctionCall;
	}
	else
	if (strcmp(receiverPtr,"mainWindow") == 0)
	{
		// Remove the receiver from the argument list, and 
		//  send the message to the specified receiver directly.
		// Note: Because the multiple inheritance fails on compile
		//       collisions between RadObject and QMainWindow,
		//       we must call this Rad object explictly.
		*result = gpRadMainWindow->lisp(gCP,gTP,argc-1,&argv[1]);
		ExitOnError(*result);
		goto Last;
	}
	else
	if (strcmp(receiverPtr,"new") == 0)
	{
		// **************************************************************
		// **************************************************************
		// Create new RadObjects and return their pointers to the caller.
		// **************************************************************
		// **************************************************************
		if ((argc>1)&&((argv[1].Tag == TYSYMBOL)||(argv[1].Tag == TYQUOTEDSYMBOL)))
		{
			receiverPtr = SymbolArray(argv[1]);
			if (strcmp(receiverPtr,"RadObject") == 0) {FrameExit(RadObject::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadWidget") == 0) {FrameExit(RadWidget::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadTabWidget") == 0) {FrameExit(RadTabWidget::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadTextEdit") == 0) {FrameExit(RadTextEdit::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadLineEdit") == 0) {FrameExit(RadLineEdit::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadLabel") == 0) {FrameExit(RadLabel::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadPushButton") == 0) {FrameExit(RadPushButton::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadVBoxLayout") == 0) {FrameExit(RadVBoxLayout::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadHBoxLayout") == 0) {FrameExit(RadHBoxLayout::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadDialog") == 0) {FrameExit(RadDialog::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadCheckBox") == 0) {FrameExit(RadCheckBox::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadComboBox") == 0) {FrameExit(RadComboBox::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadTcpSocket") == 0) {FrameExit(RadTcpSocket::lispNew(gCP,gTP,argc-2,&argv[2]));}
			if (strcmp(receiverPtr,"RadTcpServer") == 0) {FrameExit(RadTcpServer::lispNew(gCP,gTP,argc-2,&argv[2]));}
			else
			{
				strcpy(errorMsg,"!qt.new: invalid QT RadObject class name [");
				strcat(errorMsg,receiverPtr);
				strcat(errorMsg,"]!");
				*ec = TERROR(errorMsg);
				FrameExit(*ec);
			}
		}
		else
		{
			*ec = TERROR("!qt.new: invalid QT RadObject class name [not a Symbol]!");
			FrameExit(*ec);
		}
	}
	else
	if (strcmp(receiverPtr,"delete") == 0)
	{
		// **************************************************************
		// **************************************************************
		// Delete old RadObjects and return #void pointers to the caller.
		// **************************************************************
		// **************************************************************
		if ((argc>1)&&(argv[1].Tag == TYPOINTER)&&(argv[1].u.Pointer != NULL))
		{
			//if (argv[1].u.Pointer!=NULL) delete (RadObject*)argv[1].u.Pointer;
			objectPtr = (RadObject*)argv[1].u.Pointer;
		}
		else
		{
			*ec = TERROR("!qt.delete: 1st argument must be a valid QT RadObject Pointer!");
			FrameExit(*ec);
		}

		// Use the static destructors for each wrapper class
		// Note: After trying many virtual destructor approaches, we had to settle for this fallback approach.
		QString* className = objectPtr->myClassName();
		if (className==QString("RadObject")) {delete ((RadObject*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpWidget")) {delete ((WrpWidget*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpTabWidget")) {delete ((WrpTabWidget*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpTextEdit")) {delete ((WrpTextEdit*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpLineEdit")) {delete ((WrpLineEdit*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpLabel")) {delete ((WrpLabel*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpPushButton")) {delete ((WrpPushButton*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpVBoxLayout")) {delete ((WrpVBoxLayout*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpHBoxLayout")) {delete ((WrpVBoxLayout*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpDialog")) {delete ((WrpDialog*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpCheckBox")) {delete ((WrpCheckBox*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpComboBox")) {delete ((WrpComboBox*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpTcpSocket")) {delete ((WrpTcpSocket*)objectPtr); FrameExit(gCP->Tval_VOID);}
		if (className==QString("WrpTcpServer")) {delete ((WrpTcpServer*)objectPtr); FrameExit(gCP->Tval_VOID);}
		else
		{
			*ec = TERROR("!qt.delete: 1st argument must be a valid QT RadObject Pointer!");
			FrameExit(*ec);
		}
	}
	else
	{
		/* Invalid receiver */
		goto InvalidReceiver;
	}
}
// Do we have a receiver as a Pointer to a RadObject derived class?
else if (argv[0].Tag == TYPOINTER)
{
	//**************************************************************************
	// WARNING:	For this virtual function code to work properly,
	//			all RadObject derived classes MUST inherit from RadObject FIRST i.e.
	//
	//										YES
	//	class RadDerivedClass : public RadObject, public QSomeClass		// Correct multiple inheritance
	//
	//										NO
	//	class RadDerivedClass : public QSomeClass, public RadObject		// Incorrect multiple inheritance
	//
	//**************************************************************************
	// Cast the Lisp Pointer as an object from a RadObject derived class.
	VirtualRadObjectFunctionCall:
	objectPtr = (RadObject*)argv[0].u.Pointer;

	// Remove the receiver from the argument list, and 
	//  send the message to the specified receiver directly.
	*result = objectPtr->lisp(gCP,gTP,argc-1,&argv[1]);
	ExitOnError(*result);
	goto Last;
}
// Any other type of receiver is invalid.
else
{
	/* Invalid receiver */
	goto InvalidReceiver;
}


Last:
RADGlue_ProcessEvents(gCP,gTP);
FrameExit(*result);
}


/*-----------------------------------------------------------------
RADGlue_QuitAIS

Quit the AIS main application and shut down the main Smartbase context.

------------------------------------------------------------------*/
PUBLIC	long	RADGlue_QuitAIS()
{
	// We must shut down the main Smartbase context properly.
	// FSmartbase_MainContextStop(gpMainContext, gpMainThread); // Note: caused crash which hindered application exit.
	gpApp.quit();

	// For the moment do not result a String value.
	return(0);	
}

/*-----------------------------------------------------------------
RADGlue_SetEscapeRequest

Set an Escape Request in the Main Context Smarbase Engine Thread.

------------------------------------------------------------------*/
PUBLIC	long	RADGlue_SetEscapeRequest()
{

	gpMainThread->escapeSW = 1;

	return(0);	
}

/*-----------------------------------------------------------------
RADGlue_Run

Submit the Lisp command to the main Smartbase context for evaluation.

------------------------------------------------------------------*/
PUBLIC TVAL	RADGlue_Run(QString lispCommand, bool printSW)
{
	QByteArray	QCommand;
	char*		command;
	TVAL		ret;

	// Convert the Lisp source code into the proper C format.
	QCommand = lispCommand.toAscii();
	command = QCommand.data();

	// If the Smartbase engine is already busy, Do NOT reset any of the console or engine switches
	// Note: Engine already busy means that this is a recursive internal Smarbase engine call
	if (gpMainThread->busySW == TRUE)
	{
		ret = FSmartbase_Run(gpMainContext,gpMainThread,command,FALSE);
		RADGlue_ProcessEvents(gpMainContext,gpMainThread);
		return(ret);
	}

	// Submit the user entered Lisp command text to the Smartbase engine & print result on Console.
	gpMainThread->escapeSW = 0;
	ret = FSmartbase_Run(gpMainContext,gpMainThread,command,printSW);

	// Check for user requested escape or quit requests.
	if (gpMainThread->escapeSW == 2) RADGlue_QuitAIS();

	// Turn engine escape request off and get ready for next run.
	gpMainThread->escapeSW = 0;

	// For the moment do not return a result String value.
	RADGlue_ProcessEvents(gpMainContext,gpMainThread);
	return(ret);	
}

/*-----------------------------------------------------------------
RADGlue_RunLambda

Call the Lisp Lambda in the main Smartbase context passing the
specified arguments.

------------------------------------------------------------------*/
PUBLIC	TVAL	RADGlue_RunLambda(TVAL lambda,NUM argc,TVAL argv[],bool printSW)
{
	TVAL		ret;

	// If the Smartbase engine is already busy, Do NOT reset any of the console or engine switches
	// Note: Engine already busy means that this is a recursive internal Smarbase engine call
	if (gpMainThread->busySW == TRUE)
	{
		ret = FSmartbase_RunLambda(gpMainContext,gpMainThread,lambda,argc,argv,FALSE);
		RADGlue_ProcessEvents(gpMainContext,gpMainThread);
		return(ret);
	}

	// Submit the user entered Lisp command text to the Smartbase engine & print result on Console.
	gpMainThread->escapeSW = 0;
	ret = FSmartbase_RunLambda(gpMainContext,gpMainThread,lambda,argc,argv,printSW);

	// Check for user requested escape or quit requests.
	if (gpMainThread->escapeSW == 2) RADGlue_QuitAIS();

	// Turn engine busy switch off and get ready for next run.
	gpMainThread->escapeSW = 0;

	// For the moment do not return a result String value.
	RADGlue_ProcessEvents(gpMainContext,gpMainThread);
	return(ret);	
}

/*-----------------------------------------------------------------
RADGlue_RunLambdaMember

Call the Lisp Lambda member in the main Smartbase context passing the
specified arguments.

------------------------------------------------------------------*/
PUBLIC	TVAL	RADGlue_RunLambdaMember(TVAL lambda,TVAL member,NUM argc,TVAL argv[],bool printSW)
{
	TVAL		ret;

	// If the Smartbase engine is already busy, Do NOT reset any of the console or engine switches
	// Note: Engine already busy means that this is a recursive internal Smarbase engine call
	if (gpMainThread->busySW == TRUE)
	{
		ret = FSmartbase_RunLambdaMember(gpMainContext,gpMainThread,lambda,member,argc,argv,FALSE);
		RADGlue_ProcessEvents(gpMainContext,gpMainThread);
		return(ret);
	}

	// Submit the user entered Lisp command text to the Smartbase engine & print result on Console.
	gpMainThread->escapeSW = 0;
	ret = FSmartbase_RunLambdaMember(gpMainContext,gpMainThread,lambda,member,argc,argv,printSW);

	// Check for user requested escape or quit requests.
	if (gpMainThread->escapeSW == 2) RADGlue_QuitAIS();

	// Turn engine busy switch off and get ready for next run.
	gpMainThread->escapeSW = 0;

	// For the moment do not return a result String value.
	RADGlue_ProcessEvents(gpMainContext,gpMainThread);
	return(ret);	
}

// *********************************************************************
// Internal functions for use by RADGLUE Only and NOT registered to Lisp
// *********************************************************************

/*-----------------------------------------------------------------
RADGlue_DisplayOnColsole

Display the specified string in the RADIDE Console window.
------------------------------------------------------------------*/
PUBLIC NUM	 RADGlue_DisplayOnConsole(LpXCONTEXT gCP,LpTHREAD gTP, char* displayString, NUM  newline)
{

	gRadConsole->displayOnConsole(displayString,newline);

	return(0);	
}

/*-----------------------------------------------------------------
RADGlue_Escape

Check for an escape request and return 1 if an escape is requested.
------------------------------------------------------------------*/
PUBLIC NUM	 RADGlue_Escape(LpXCONTEXT gCP,LpTHREAD gTP)
{
	NUM		result = 0;

	RADGlue_ProcessEvents(gCP,gTP);
	if (gTP->escapeSW != 0) 
		result = 1;

	return(result);
}

/*-----------------------------------------------------------------
RADGlue_ProcessEvents

Call the several important even processing loops in the Main RadIde thread.
------------------------------------------------------------------*/
PUBLIC NUM	 RADGlue_ProcessEvents(LpXCONTEXT gCP,LpTHREAD gTP)
{

	gpApp.processEvents();
	if (gRadConsole != NULL) gRadConsole->processEvents(gCP,gTP);

	return(0);
}

/*-----------------------------------------------------------------
RADGlue_RegisterLispFunctions 

Registers all RADIDE C Procedures to the Smartbase engine.
------------------------------------------------------------------*/
PUBLIC TVAL RADGlue_RegisterLispFunctions(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
	StartFrame
	DeclareTVAL(ec);
	EndFrame

	*ec = FSmartbase_RegisterCProcedure(gCP,gTP,(LpCHAR)"qt",(LpFUNC)&RADGlue_QT);
	ExitOnError(*ec);

	FrameExit(gCP->TObject_TRUE);
}

/*-----------------------------------------------------------------
RADGlue_DisplayOnColsoleFromLisp

Display the specified string in the RADIDE Console window.
------------------------------------------------------------------*/
PUBLIC	NUM	 RADGlue_DisplayOnConsoleFromLisp(LpXCONTEXT gCP,LpTHREAD gTP, char* displayString, NUM  newline)
{
	TVAL	ret;
	NUM		displayLen = strlen(displayString);
	QString lispCommand;
	QString displayMe;
	QString	oldDisplayBfr;
	QString	newDisplayBfr;

	// Make sure that the String does NOT exceed the RAD Console display size.
	if (displayLen >= RADMAXCONSOLESIZE)
	{
		displayString = &displayString[displayLen-RADMAXCONSOLESIZE];
	}

	// If there is no console manager, then display the text in the console pane.
	if ((gRadConsole->cpDisplayEventMgr.Tag == TYVOID) || (gTP->escapeSW != 0))
	{
		gRadConsole->displayOnConsole(displayString,newline);
		RADGlue_ProcessEvents(gCP,gTP);
	}
	else
	{
		displayMe = displayString;

		// If there is a console manager, save the specified text in the console log.
		if (newline != 0)
			gRadConsole->cpConsoleLog = displayMe.right(RADMAXCONSOLESIZE);
		else
			gRadConsole->cpConsoleLog = (displayMe+"\n").right(RADMAXCONSOLESIZE);

		// Call (consoleMgr.display).
		ret = RADGlue_RunLambdaMember(gRadConsole->cpConsoleMgr,gRadConsole->cpDisplayEventMgr,0,NULL,FALSE);
	}

	return(0);	
}

/*-----------------------------------------------------------------
RADGlue_DoesLambdaOrMemberExist

Returns either void, or lambdaName or memberName.

If memberName is void and (ref lambdaName)!=void then return lambdaName.

If lambdaName.memberName!= void then return memberName.

Otherwise return void.

Note:	This function is used to quickly determine if a Lisp manager
		function exists.

------------------------------------------------------------------*/
PUBLIC	TVAL	RADGlue_DoesLambdaOrMemberExist(LpXCONTEXT gCP,LpTHREAD gTP,TVAL lambdaName,TVAL memberName)
{
	TVAL		ret;
	TVAL		prmv[2];

	// If the specified lambda is a Symbol, then use the symbol's global value.
	if (lambdaName.Tag == TYSYMBOL)
		prmv[0] = lambdaName.u.Symbol->itsGlobalValue;
	else
		prmv[0] = lambdaName;

	// If the specified member is void, then return lambdaName. 
	if (memberName.Tag == TYVOID) return(lambdaName);

	// Perform a (ref lambda member) and return the result.
	prmv[1] = memberName;
	ret = FSmartbase_Refv(gCP,gTP,2,prmv);		// Does the specified member exist?
	if ((ret.Tag == TYERROR)||(ret.Tag==TYVOID)) return(gCP->Tval_VOID);		// If not exists, then return false.

	// Return memberName or Lambda value.
	if (memberName.Tag == TYSYMBOL)
		return(memberName);
	else
		return(ret);
}

/*-----------------------------------------------------------------
RADGlue_FunctionKeyCode

Converts the QT key (from a QKeyEvent) into a RadIde Function Key
code.

If return value is 0, then the input key was NOT recognized as  RadIde
Function Key.

If return value is greater than 0, then the input key WAS recognized as  RadIde
Function Key.

Note:	This function is used to quickly determine if a key press or
		key release event involved a RadIde Function Key.

------------------------------------------------------------------*/
PUBLIC	int		RADGlue_FunctionKeyCode(QKeyEvent *e)
{
	int						key = e->key();
	int						retkey = 0;
	Qt::KeyboardModifiers	keyState = e->modifiers();
	bool					altKey = (keyState == Qt::AltModifier);		
	bool					ctrlKey = (keyState == Qt::ControlModifier);	
	bool					noModifiers = (keyState == Qt::NoModifier);

	// Only certain key combinations are treated as function keys.
	// Note: These special key combinations are given Function Key Codes as follows in this code.
	if (noModifiers == true)
	{	// Define the Naked Function key codes
		if ((key >= Qt::Key_F1)&&(key <= Qt::Key_F35)) retkey = (key-Qt::Key_F1)+1; // The Naked Function keys F1~F35 are codes 001~036.
		else if ((key >= Qt::Key_A)&&(key <= Qt::Key_Z)) retkey = (key-Qt::Key_A)+37; // The Naked Alphabet keys A~Z are codes 037~062.
		else if (key == Qt::Key_Tab) retkey = 63;
		else if (key == Qt::Key_Home) retkey = 64;
		else if (key == Qt::Key_End) retkey = 65;
		else if (key == Qt::Key_Left) retkey = 66;
		else if (key == Qt::Key_Up) retkey = 67;
		else if (key == Qt::Key_Right) retkey = 68;
		else if (key == Qt::Key_Down) retkey = 69;
		else if (key == Qt::Key_PageUp) retkey = 70;
		else if (key == Qt::Key_PageDown) retkey = 71;
		else if (key == Qt::Key_Backspace) retkey = 72;
		else if ((key == Qt::Key_Return)||(key == Qt::Key_Enter)) retkey = 73;
		else if (key == Qt::Key_Delete) retkey = 74;
		else if (key == Qt::Key_Insert) retkey = 75;
	}
	else if (altKey == true)
	{	// Define the Alt Function key codes
		if ((key >= Qt::Key_F1)&&(key <= Qt::Key_F35)) retkey = (key-Qt::Key_F1)+189; // The Alt Function keys F1~F35 are codes 189~224.
		else if ((key >= Qt::Key_A)&&(key <= Qt::Key_Z)) retkey = (key-Qt::Key_A)+225; // The Alt Alphabet keys A~Z are codes 225~250.
		else if (key == Qt::Key_Tab) retkey = 251;
		else if (key == Qt::Key_Home) retkey = 252;
		else if (key == Qt::Key_End) retkey = 253;
		else if (key == Qt::Key_Left) retkey = 254;
		else if (key == Qt::Key_Up) retkey = 255;
		else if (key == Qt::Key_Right) retkey = 256;
		else if (key == Qt::Key_Down) retkey = 257;
		else if (key == Qt::Key_PageUp) retkey = 258;
		else if (key == Qt::Key_PageDown) retkey = 259;
		else if (key == Qt::Key_Backspace) retkey = 260;
		else if ((key == Qt::Key_Return)||(key == Qt::Key_Enter)) retkey = 261;
		else if (key == Qt::Key_Delete) retkey = 262;
		else if (key == Qt::Key_Insert) retkey = 263;
	}
	else if (ctrlKey == true)
	{	// Define the Cntrl Function key codes
		if ((key >= Qt::Key_F1)&&(key <= Qt::Key_F35)) retkey = (key-Qt::Key_F1)+264; // The Cntrl Function keys F1~F35 are codes 264~298.
		else if ((key >= Qt::Key_A)&&(key <= Qt::Key_Z)) retkey = (key-Qt::Key_A)+299; // The Cntrl Alphabet keys A~Z are codes 299~324.
		else if (key == Qt::Key_Tab) retkey = 325;
		else if (key == Qt::Key_Home) retkey = 326;
		else if (key == Qt::Key_End) retkey = 327;
		else if (key == Qt::Key_Left) retkey = 328;
		else if (key == Qt::Key_Up) retkey = 329;
		else if (key == Qt::Key_Right) retkey = 330;
		else if (key == Qt::Key_Down) retkey = 331;
		else if (key == Qt::Key_PageUp) retkey = 332;
		else if (key == Qt::Key_PageDown) retkey = 333;
		else if (key == Qt::Key_Backspace) retkey = 334;
		else if ((key == Qt::Key_Return)||(key == Qt::Key_Enter)) retkey = 335;
		else if (key == Qt::Key_Delete) retkey = 336;
		else if (key == Qt::Key_Insert) retkey = 337;
		else if (key == Qt::Key_Space) retkey = 338;
	}

	return(retkey);
}

