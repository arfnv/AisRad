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
												RadTcpServer for Internet

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
#include "radtcpsocket.h"
#include "radtcpserver.h"

//	------------------------------------------------------- GLOBALS -----------------------------------------------------------

//	------------------------------------------------------ METHODS -------------------------------------------------------------

/********************************************************************************************
RadTcpServer 

Constructor for the RadIde RadTcpServer. RadTcpServer allows the support in the Internet 
via basic TCP/IP management.

Programmer Note:
Our first choice, of multiple inheritance, does NOT work for QWidiget derivative classes.
Multiple inheritance objects crash QT when added as QWidget GUI objects. Therefore, we
use our fallback choice, the wrapper approach, to allow Lisp responsive QT derivative
classes for communication between the Smartbase Lisp engine and all RadIde QT interface Objects.

********************************************************************************************/
RadTcpServer::RadTcpServer(QObject* parent=0)
	: QTcpServer(parent)
{
	// A RadTcpServer object comes with a default font.

	cpWrpTcpServer = new WrpTcpServer(this,NULL,"WrpTcpServer");
	cpTcpSocket = NULL;
}

//	********************************************************************************************
//	RadTcpServer::incomingConnection ==> (tcpServerMgr.httpRequest httpData)
//	 
//	This virtual function is called by QTcpServer when an HTTP GET request or 
//	other connectionrequest is made of the host. At this point the RadTcpServer object
//	has been listening to the specified "localhost" socket.
//	The socketDescriptor argument is the native socket descriptor for the accepted connection.
//	
//	This implementation invokes the (tcpServerMgr.httpRequest httpData) lambda. 
//	
//	The httpData is the HTTP Get request or other connection request sent 
//	by MS Internet Explorer (IE) to this RadTcpServer object.
//	
//	A typial IE httpData request String might look as follows...
//	
//	httpData =  GET /home.htm HTTP/1.1
//				Accept: text/html, application/xhtml+xml, */*
//				Referer: http://localhost/EarnieStartup.html
//				Accept-Language: en-US
//				User-Agent: Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0; MDDCJS)
//				Accept-Encoding: gzip, deflate
//				Host: localhost
//				DNT: 1
//				Connection: Keep-Alive 
//	
//			OR
//	
//	httpData =  GET /minibot2.gif HTTP/1.1
//				Accept: image/png, image/svg+xml, image/*;q=0.8, */*;q=0.5
//				Referer: http://localhost/home.htm
//				Accept-Language: en-US
//				User-Agent: Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0; MDDCJS)
//				Accept-Encoding: gzip, deflate
//				Host: localhost
//				DNT: 1
//				Connection: Keep-Alive 
//	
//	
//			OR
//	
//	httpData =  POST /amp.dll HTTP/1.1
//				Accept: */*
//				Referer: http://localhost/WeeklyTrainingWindowViewer.htm
//				Accept-Language: en-US
//				Content-Type: text/plain;charset=UTF-8
//				Accept-Encoding: gzip, deflate
//				User-Agent: Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; WOW64; Trident/6.0; MDDCJS)
//				Host: localhost
//				Content-Length: 137
//				DNT: 1
//				Connection: Keep-Alive
//				Cache-Control: no-cache
//				xml=<amp target='deepGreen' act='xmlSocketEvals'><cmd>(libraryMgr.delimitedString (stockWeekly.getSummaryDates all:) #\^ #\^)</cmd></amp>
//
//		Note: Enter "http://localhost/EarnieStartup.html" into IE to trigger this example
//	
//	********************************************************************************************/
void  RadTcpServer::incomingConnection(int socketDescriptor)
{
	LpXCONTEXT		gCP = gpMainContext;
	LpTHREAD		gTP = gpMainThread;
	NUM				timeOut = 3000;
	NUM				timeOutTicks = 3000;
	NUM				len;
	NUM				n;
	NUM				N;
	NUM				retcode;
	LpCHAR			textPtr;
	TVAL			argv[1];
	TVAL			result;
	RadTcpSocket*	radTcpSocket;
	QByteArray		textBArray;

	// Initialize a socket object to handle communication with IE.
	radTcpSocket = new RadTcpSocket(this);						
	radTcpSocket->setSocketDescriptor(socketDescriptor);

	// Establish the connection with IE.
	// Note: First connect then waitForConnected.
	radTcpSocket->connectToHost("localhost",cpTcpServerPort,QIODevice::ReadWrite);
	timeOutTicks = RADTCPSOCKETTIMEOUTTICKS;
	while (((retcode = radTcpSocket->state()) != QAbstractSocket::ConnectedState) && ((--timeOutTicks) >= 0))
	{
		RADGlue_ProcessEvents(gCP,gTP);
	}
	if (retcode != QAbstractSocket::ConnectedState) 
		goto DeleteSocket;

	// Read the data sent from IE: which will be an HTTP "GET" request.
	// Note: First, waitForReadyRead then read all data
	timeOutTicks = RADTCPSOCKETTIMEOUTTICKS;
	while (((len = radTcpSocket->bytesAvailable()) <= 0) && ((--timeOutTicks) >= 0))
	{
		RADGlue_ProcessEvents(gCP,gTP);
	}
	if (len <= 0) 
		textBArray = "";
	else
		textBArray = radTcpSocket->readAll();

	// Write the static HTML page back to IE.
	if (textBArray.length() > 0)
	{
		// Invoke the httpRequest Lambda in the TCP Server Manager.
		textPtr = textBArray.data();
		argv[0] = TSTRING(textPtr);
		result = RADGlue_RunLambdaMember(cpTcpServerMgr,cpHttpRequest,1,argv,FALSE);
		switch (result.Tag) 
		{
			case TYTEXT: 
				// Note: The result is probably an HTML or other text file.
				textPtr = result.u.Text;
				goto ReturnHTML;
				break;
			case TYSTRING:
				// Note: The result is probably an HTML or other text file.
				textPtr = CharArray(result); 
				goto ReturnHTML;
				break;
			case TYBYTEVECTOR:
				// Note: The result is probably an image or other binary file.
				textBArray = QByteArray((char*)ByteArray(result),result.u.ByteVector->itsMaxItemIndex);
				radTcpSocket->writeAll(textBArray);
				break;
			case TYSYMBOL:
			case TYQUOTEDSYMBOL:
				// Note: The result is probably an HTML or other text file.
				textPtr = SymbolArray(result); 
				ReturnHTML:
				if (strlen(textPtr) >= 0)
					textBArray = textPtr;
				else
					textBArray = "<!DOCTYPE HTML><html><h1>Sorry, an error Occurred in our server</h1></html>";
				radTcpSocket->writeAll(textBArray);
				break;
			default:
				// Note: The result is returned as an HTML file.
				textBArray = "<!DOCTYPE HTML><html><h1>Sorry, an error Occurred in our server</h1></html>";
				radTcpSocket->writeAll(textBArray);
				break;
		}
	}

	// Close the connection with IE.
	// Note: First disconnect, then waitForDisconnected.
	radTcpSocket->disconnectFromHost();
	timeOutTicks = RADTCPSOCKETTIMEOUTTICKS;
	while (((retcode = radTcpSocket->state()) != QAbstractSocket::UnconnectedState) && ((--timeOutTicks) >= 0))
	{
		RADGlue_ProcessEvents(gCP,gTP);
	}

	DeleteSocket:
	radTcpSocket->close();
	delete radTcpSocket;

}


/********************************************************************************************
RadTcpServer::lisp

Receives messages from Lisp Lambdas in the Smartbase engine.

Args:	message		Symbol indicating the action which the main window is to perform.
        ....		(Optional)Additional arguments depending upon the message.

Return:	result		A Lisp Word containing the result of the action taken by the RadTcpServer object.

Examples:
(setq result (islistening:))			;; Return true iff the RadTcpServer is listening for messages from the specified port.
(setq result (listen: port))			;; Instruct the RadTcpServer to listen for messages on the specified port.
(setq result (port:))					;; Return the port which the RadTcpServer is listening to.

Programmer Notes:
These Lisp messages were meant for RadTabWidget objects returned from (setq aRadTabWidget (qt new: RadRabWidget:)).
Lisp messages received here were sent by the RadGlue_QT function.
The Rad C++ programmer may surface more Rad Tab Widget features to Lisp by adding messages to this function.
Please examine the RadTabWidget and the QTabWidget classes before adding more features here.

********************************************************************************************/
TVAL RadTcpServer::lisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	struct					FSmartbase_HostCallBackFunctions* Funcs = &gpFuncs;
	NUM						n;
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
		*result = TERROR("!RadTcpServer: first argument must be a valid message symbol!");
		goto Last;
	}


	// **********************************************
	// Manage each distinct message
	// **********************************************
	messagePtr = SymbolArray(argv[0]);
	if (strcmp(messagePtr,"isListening") == 0)
	{
		// (setq result (islistening:))		;; Return true iff the RadTcpServer is listening for messages from the specified port.	
		result->Tag = TYBOLE;
		result->u.Bool = isListening();
		goto Last;
	}
	else
	if (strcmp(messagePtr,"listen") == 0)
	{
		// (setq result (listen: port))		;; Instruct the RadTcpServer to listen for messages on the specified port.

		/* Get the RadTcpServer host name & port */
		if ((argc!=2) || (argv[1].Tag!=TYNUM)) 
		{
			*ec = TERROR("!RadTcpServer.listen: mandatory 1st argument must be of type Integer only!");
			RADGlue_ProcessEvents(gCP,gTP);
			FrameExit(*ec);
		}

		/* begin listening to the specified port */
		cpTcpServerPort = argv[1].u.Int;
		result->Tag = TYBOLE;
		result->u.Bool = listen(QHostAddress::Any,cpTcpServerPort); 
		goto Last;
	}

	else
	if (strcmp(messagePtr,"port") == 0)
	{
		// (setq result (port:))				;; Return the port which the RadTcpServer is listening to.	
		result->Tag = TYNUM;
		result->u.Int = cpTcpServerPort;
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

Lisp object creation method for the RadTcpServer class. Implements the RadIde Line Edit pane. 
RadTextEdit panes come standard with a default fixed font and support for Lisp call back managers 
for key strokes.


Example:
	(setq radTcpServerPtr (qt new: RadTcpServer: port mgrLambda))	;; Create a new RadTcpServer object
					...
	(setq RadTcpServerPtr (qt delete: radTcpServerPtr)) ;; Delete a RadTcpServer object and release all of its memory.

Programmer Note:

	QT programmer must specifically delete each object created. If not deleted, the
	object will persist in the QT heap. When surfacing a derivative RadIde QT object
	to Lisp, the object must be constructed such that a delete returns all memory to the
	QT heap. To test that this is so, run the following lisp code...

	(loop for n from 0 until 1000000 (setq p (qt new: RadTcpServer: 80 (lambda() true)))(qt delete: p))

	If the code fails or crashes, then the new Rad class has not been properly constructed.

	Note: The RadTcpServer class was tested, using the above code, and NO memory leaks were evident.

********************************************************************************************/
TVAL RadTcpServer::lispNew(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
	NUM				port;
	RadTcpServer*	radTcpServerPtr = new RadTcpServer(NULL);
	StartFrame
	DeclareTVAL(ec);
	DeclareTVAL(result);
	DeclareTVAL(mgrLambda);
	EndFrame

	/* Get the RadTcpServer port and mgrLambda */
	if ((argc!=2) || (argv[0].Tag!=TYNUM)) 
	{
		*ec = TERROR("!RadTcpServer.new: mandatory 1st argument must be of type Integer and 2nd argument must be a Lambda!");
		RADGlue_ProcessEvents(gCP,gTP);
		FrameExit(*ec);
	}

	// Retrieve the port and the manager Lambda arguments.
	port = argv[0].u.Int;
	*mgrLambda = argv[1];

	// Initialize the RadTcpServer Lisp call back Lambdas.
	radTcpServerPtr->cpTcpServerMgr = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,*mgrLambda,gCP->Tval_VOID);
	radTcpServerPtr->cpHttpRequest = RADGlue_DoesLambdaOrMemberExist(gCP,gTP,*mgrLambda,gpSym_httpRequest);
	radTcpServerPtr->cpTcpServerPort = port;
	radTcpServerPtr->listen(QHostAddress::Any, port);

	// Return the wrapper object pointer to the Lisp caller.
	result->Tag = TYPOINTER;
	result->u.Pointer = (POINTER)radTcpServerPtr->cpWrpTcpServer;
	FrameExit(*result);
}

