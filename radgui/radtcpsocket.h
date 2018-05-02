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

	Notes: This class must have us install the QT MOC compiler (they will not work properly without it).

***********************************************************************************/

#ifndef radtcpsocket_H
#define radtcpsocket_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
												RadTcpSocket for Internet

CHANGE HISTORY
Version	Date		Who		Change
1.0000	8/4/2014	mfk 	First experiments with remote Internet support.
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
#include <QtGui/QIcon>
#include <QtGui/QMainWindow>
#include <QtNetWork/QTcpSocket>
#include <QtNetWork/QHostAddress>

#include "radobject.h"
#include "radwidget.h"
#include "radmainwindow.h"

extern "C" { // includes for modules written in C
#include "../smtbase/fsmtbase.h" // SmartBase engine declarations
}

class RadObject;

class RadWidget; class WrpWidget;
class RadTcpSocket; class WrpTcpSocket;

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	----------------------------------------------------- DEFINITIONS ---------------------------------------------------------
#define	RADTCPSOCKETTIMEOUT			500		
#define	RADTCPSOCKETTIMEOUTTICKS	500		

//	------------------------------------------------------- CLASSES -----------------------------------------------------------

/********************************************************************************************
RadTcpSocket 

Implements the RadIde RadTcpSocket. RadTcpSocket allows the support in the Internet 
via basic TCP/IP management.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
//class RadTcpSocket : public RadObject, public QTcpSocket
class RadTcpSocket : public QTcpSocket
{
// 	Q_OBJECT

//  **********
//	Properties
//  **********
	WrpTcpSocket*			cpWrpTcpSocket;	

//  *********************
//	Lisp Callback Lambdas
//  *********************
	//	Callback managers for Text Line Actions
	TVAL					cpTcpSocketLambda;

public:

//  *******
//	Methods
//  *******
//	constructor
	RadTcpSocket(QObject* parent);

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
	static TVAL lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

	virtual NUM writeAll(QByteArray data);

private:

protected:
};

/********************************************************************************************
WrpTcpSocket

Implements the RadIde QTcpSocket wrapper class.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
class WrpTcpSocket : public RadObject
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
	WrpTcpSocket(RadTcpSocket* parent,QWidget* display,const QString& className)
		: RadObject((QObject*)parent,display,className)
	{
	}

//	WrpTextEdit destructor
	WrpTcpSocket::~WrpTcpSocket()
	{
		RadTcpSocket* aRadTcpSocket = (RadTcpSocket*)cpWrpRadParent;
		if (aRadTcpSocket!=NULL) delete aRadTcpSocket;
		cpWrpRadParent = NULL;
		cpWrpQWidget = NULL;
	}
	  

//	Main Lisp Method
	virtual TVAL lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
	{
		return(((RadTcpSocket*)cpWrpRadParent)->lisp(gCP,gTP,argc,argv));
	}
	  

private:

protected:
};

#endif
