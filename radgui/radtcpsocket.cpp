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
												RadTcpSocket for Internet

CHANGE HISTORY
Version	Date		Who		Change
1.0000	8/4/2014	mfk 	First experiments with Internet support.
												 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtCore/QEvent>
#include <QtGui/QMainWindow>


#include "radmainwindow.h"
#include "radglue.h"
#include "radwidget.h"
#include "radtcpserver.h"
#include "radtcpsocket.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadTcpSocket 

Constructor for the RadIde RadTcpSocket. RadTcpSocket allows the support in the Internet 
via basic TCP/IP management.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadTcpSocket::RadTcpSocket(QObject* parent=0)
	: QTcpSocket(parent)
{
	// A RadTcpSocket object comes with a default font.

	cpWrpTcpSocket = new WrpTcpSocket(this,NULL,"WrpTcpSocket");
}


/********************************************************************************************
RadTcpSocket::writeAll

Called when the RadTcpSocket has encou ntered an error.

********************************************************************************************/
NUM RadTcpSocket::writeAll(QByteArray data)
{
	NUM		result;
	result = writeData(data,data.length());
	return(result);
}

/********************************************************************************************
RadTcpSocket::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadTcpSocket object.

Examples:
(setq len (bytesAvailable:))				;; Returns the number of bytes available to be read from the socket.
(connectToHost: hostName port)				;; Connect the TcpSocket to the specified host and port (i.e. "LocalHost" 80 OR "mytarget.com" 80 OR "43.195.83.32" 90).
(disconnectFromHost:)						;; Disconnect the TcpSocket from the previously specified host.
(setq myError (error:))						;; Return the last error of the RadTcpSocket (myError is always an integer defined by QAbstractSocket::SocketError).
(setq data (readAll:))						;; Return the data read from the RadTcpSocket object.
(setSocketDescriptor: socket)				;; Set the specified TcpSocket descriptor.
(setq myState (state:))						;; Return the state of the RadTcpSocket (myState is always an integer defined by QAbstractSocket::SocketState).
(setq code (waitForBytesWritten: seconds))	;; Return the true IFF the socket has written all the bytes, otherwise return false.
(setq code (waitForConnected: seconds))		;; Return the true IFF the socket has been connected, otherwise return an Integer state code.
(setq code (waitForDisconnected: seconds))	;; Return the true IFF the socket has been disconnected, otherwise return an Integer state code.
(setq code (waitForReadyRead: seconds))		;; Return the true IFF the socket has data to read, otherwise return an Integer error code.
(writeAll: data)							;; Write the specified data to the TcpSocket.

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadTcpSocket::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	struct					FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	NUM						n;
	NUM						N;
	NUM						timeOut = RADTCPSOCKETTIMEOUT;
	NUM						timeOutTicks = RADTCPSOCKETTIMEOUTTICKS;
	BOLE					retcode;
	LpCHAR					messagePtr;
	LpCHAR					textPtr;
	QString					textQString;
    QByteArray				textBArray;
	QTextCursor				cursor;
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
		*result = TERROR("!RadTcpSocket: first argument must be a valid message symbol!");
		goto Last;
	}

	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"bytesAvailable") == 0)
	{
		// (setq len (bytesAvailable:))				;; Returns the number of bytes available to be read from the socket.
		// Return the number of bytes available to be read from the socket.
		result->Tag = TYNUM;
		result->u.Int = bytesAvailable();
		goto Last;
	}
	else
	if (strcmp(messagePtr,"connectToHost") == 0)
	{
		//  (connectToHost: hostName port)		;; Connect the TcpSocket to the specified host and port (i.e. "LocalHost" 80 OR "mytarget.com" 80 OR "43.195.83.32" 90).

		/* Get the RadTcpSocket socket */
		if ((argc!=3) || (argv[2].Tag!=TYNUM)) goto InvalidConnectToHostArgument; 
		switch (argv[1].Tag) 
		{
			case TYTEXT: 
				textPtr = argv[1].u.Text; 
				break;
			case TYSTRING:
				textPtr = CharArray(argv[1]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				textPtr = SymbolArray(argv[1]); 
				break;
			default:
			InvalidConnectToHostArgument:
			*ec = TERROR("!RadTcpSocket.connectToHost: mandatory 1st argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the entire contents of the RadTextEdit display pane.
		// Note: We don't allow HTML in the RadTextEdit like we do in the Demo Window.
		textQString = textPtr;

		// Connect the socket to the specified host.
		connectToHost(textQString,argv[2].u.Int,QIODevice::ReadWrite);

		*result = gCP->Tval_TRUE;
		goto Last;
	}
	else
	if (strcmp(messagePtr,"disconnectFromHost") == 0)
	{
		// (disconnectFromHost:)				;; Disconnect the TcpSocket from the previously specified host.
		// Note: The tcpmgr.disconnect will be invoked when the connection has been closed.
	
		/* Disconnect */
		disconnectFromHost();

		*result = gCP->Tval_TRUE;
		goto Last;
	}
	else
	if (strcmp(messagePtr,"error") == 0)
	{
		// (setq myError (error:))				;; Return the last error of the RadTcpSocket (myError is always an integer defined by QAbstractSocket::SocketError).		// Note: The tcpmgr.disconnect will be invoked when the connection has been closed.
	
		/* return the current socket error */
		result->Tag = TYNUM;
		result->u.Int = error();
		goto Last;
	}
	else
	if (strcmp(messagePtr,"readAll") == 0)
	{
		// Set the entire contents of the RadTextEdit display pane.
		// (setq data (readAll:))				;; Return the data read from the RadTcpSocket object.
		//retcode = waitForReadyRead(timeOut);
		*result = gCP->Tval_VOID;
		if ((retcode = bytesAvailable()) > 0)
		{
			textBArray = readAll();
			textBArray = textQString.toLocal8Bit();
			textPtr = textBArray.data();
			*result = TSTRING(textPtr);
		}
		goto Last;
	}
	else
	if (strcmp(messagePtr,"setSocketDescriptor") == 0)
	{
		//  (setSocketDescriptor: socket)		;; Set the specified TcpSocket descriptor.

		/* Get the RadTcpSocket host name & port */
		if ((argc!=2) || (argv[1].Tag!=TYNUM))
		{
			*ec = TERROR("!RadTcpSocket.setSocketDescriptor: mandatory 1st argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Set the socket descriptor for this RadTcpSocket object.
		setSocketDescriptor(argv[1].u.Int);

		*result = gCP->Tval_TRUE;
		goto Last;
	}
	else
	if (strcmp(messagePtr,"state") == 0)
	{
		// (setq myState (state:))				;; Return the state of the RadTcpSocket (myState is always an integer defined by QAbstractSocket::SocketState).
		// Note: The tcpmgr.disconnect will be invoked when the connection has been closed.
	
		/* return the current socket state */
		result->Tag = TYNUM;
		result->u.Int = state();
		goto Last;
	}
	else
	if (strcmp(messagePtr,"waitForBytesWritten") == 0)
	{
		// (setq code (waitForBytesWritten: seconds))	;; Return the true IFF the socket has written all the bytes, otherwise return false.

		// Note: seconds is the number of seconds to wait before returning empty handed.
		if (argc!=2) goto InvalidWaitForBytesWrittenArgument; 
		switch (argv[1].Tag) 
		{
			case TYNUM: 
				N = argv[1].u.Int; 
				break;
			case TYREAL:
				N = argv[1].u.Real; 
				break;
			default:
			InvalidWaitForBytesWrittenArgument:
			*ec = TERROR("!RadTcpSocket.waitForBytesWritten: mandatory 1st argument must be of type Integer or Number only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		++N;
		retcode = 0;
		while ((retcode != 1) && ((--N) >= 0))
		{
			RADGlue_ProcessEvents(gCP,gTP);
			retcode = waitForBytesWritten(timeOut);
		}
		if (retcode == 1) *result = gCP->Tval_TRUE; else *result = gCP->Tval_FALSE;
		goto Last;
	}
	else
	if (strcmp(messagePtr,"waitForConnected") == 0)
	{
		// (setq code (waitForConnected: seconds))	;; Return the true IFF the socket has been connected, otherwise return an Integer state code.

		// Note: seconds is the number of seconds to wait before returning empty handed.
		if (argc!=2) goto InvalidWaitForConnectedArgument; 
		switch (argv[1].Tag) 
		{
			case TYNUM: 
				N = argv[1].u.Int; 
				break;
			case TYREAL:
				N = argv[1].u.Real; 
				break;
			default:
			InvalidWaitForConnectedArgument:
			*ec = TERROR("!RadTcpSocket.waitForConnected: mandatory 1st argument must be of type Integer or Number only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		timeOutTicks = RADTCPSOCKETTIMEOUTTICKS*N;
		while (((retcode = state()) != QAbstractSocket::ConnectedState) && ((--timeOutTicks) >= 0))
		{
			RADGlue_ProcessEvents(gCP,gTP);
		}
		if (retcode == QAbstractSocket::ConnectedState) *result = gCP->Tval_TRUE; else *result = TINT(retcode);
		goto Last;
	}
	else
	if (strcmp(messagePtr,"waitForDisconnected") == 0)
	{
		// (setq code (waitForConnected: seconds))	;; Return the true IFF the socket has been connected, otherwise return an Integer state code.

		// Note: seconds is the number of seconds to wait before returning empty handed.
		if (argc!=2) goto InvalidWaitForDisconnectedArgument; 
		switch (argv[1].Tag) 
		{
			case TYNUM: 
				N = argv[1].u.Int; 
				break;
			case TYREAL:
				N = argv[1].u.Real; 
				break;
			default:
			InvalidWaitForDisconnectedArgument:
			*ec = TERROR("!RadTcpSocket.waitForDisconnected: mandatory 1st argument must be of type Integer or Number only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		timeOutTicks = RADTCPSOCKETTIMEOUTTICKS*N;
		while (((retcode = state()) != QAbstractSocket::UnconnectedState) && ((--timeOutTicks) >= 0))
		{
			RADGlue_ProcessEvents(gCP,gTP);
		}
		if (retcode == QAbstractSocket::UnconnectedState) *result = gCP->Tval_TRUE; else *result = TINT(state());
		goto Last;
	}
	else
	if (strcmp(messagePtr,"waitForReadyRead") == 0)
	{
		// (setq code (waitForReadyRead: seconds))	;; Return the true IFF the socket has data to read, otherwise return an Integer error code.
		// Note: seconds is the number of seconds to wait before returning empty handed.
		if (argc!=2) goto InvalidWaitForReadyReadArgument; 
		switch (argv[1].Tag) 
		{
			case TYNUM: 
				N = argv[1].u.Int; 
				break;
			case TYREAL:
				N = argv[1].u.Real; 
				break;
			default:
			InvalidWaitForReadyReadArgument:
			*ec = TERROR("!RadTcpSocket.waitForReadyRead: mandatory 1st argument must be of type Integer or Number only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}
		timeOutTicks = RADTCPSOCKETTIMEOUTTICKS*N;
		while (((retcode = bytesAvailable()) <= 0) && ((--timeOutTicks) >= 0))
		{
			RADGlue_ProcessEvents(gCP,gTP);
		}
		if (retcode > 0) *result = gCP->Tval_TRUE; else *result = TINT(error());
		goto Last;
	}
	else
	if (strcmp(messagePtr,"writeAll") == 0)
	{
		//  (writeAll: data)					;; Write the specified data to the TcpSocket.

		/* Get the RadTcpSocket socket */
		if (argc!=2) goto InvalidWriteAllArgument; 
		switch (argv[1].Tag) 
		{
			case TYTEXT: 
				textPtr = argv[1].u.Text; 
				break;
			case TYSTRING:
				textPtr = CharArray(argv[1]); 
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				textPtr = SymbolArray(argv[1]); 
				break;
			default:
			InvalidWriteAllArgument:
			*ec = TERROR("!RadTcpSocket.writeAll: mandatory 1st argument must be of type Text, String, or Symbol only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		// Write the data to the specified RadTcpSocket object.
		textBArray = textPtr;
		writeAll(textBArray);

		*result = gCP->Tval_TRUE;
		goto Last;
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

Lisp object creation method for the RadTcpSocket class. Implements the RadIde Line Edit pane. 
RadTextEdit panes come standard with a default fixed font and support for Lisp call back managers 
for key strokes.


Example:
	(setq radTcpSocketPtr (qt new: RadTcpSocket: WrpTcpServerPtr mgrLambda))	;; Create a new RadTcpSocket object
					...
	(setq RadTcpSocketPtr (qt delete: radTcpSocketPtr)) ;; Delete a RadTcpSocket object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadTcpSocket: 0 (lambda() true)))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

	Note: The RadTcpSocket class was tested, using the above code, and NO memory leaks were evident.

********************************************************************************************/
TVAL RadTcpSocket::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	TVAL			mgrLambda = gCP->Tval_VOID;
	TVAL			result;
	WrpTcpServer*	wrpTcpServerPtr = NULL;
	RadTcpServer*	radTcpServerPtr = NULL;
	RadTcpSocket*	radTcpSocketPtr = NULL;

	// Was an optional RadTcpServer argument passed?
	if (argc>0) radTcpServerPtr = (RadTcpServer*)((WrpTcpServer*)argv[0].u.Pointer)->cpWrpRadParent;

	// Was an optional manager Lambda argument passed?
	if (argc>1) mgrLambda = argv[1];

	// Initialize the RadTcpSocket object and its Lisp call back Lambdas.
	radTcpSocketPtr = new RadTcpSocket(radTcpServerPtr);
	radTcpSocketPtr->cpTcpSocketLambda = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,mgrLambda,gCP->Tval_VOID);

	// Return the wrapper object pointer to the Lisp caller.
	result.Tag = TYPOINTER;
	result.u.Pointer = (POINTER)radTcpSocketPtr->cpWrpTcpSocket;
	return(result);
}

