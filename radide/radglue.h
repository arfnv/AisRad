/**********************************************************************************
    Copyright (C) 2013 AIS Foundation.

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

/*
radglue.h

CHANGE HISTORY
Version	Date		Who		Change
1.0000	12/14/2013	mfk		Initial experimentation.
*/

/*  Make sure that we skip this include file if it has already been seen.   */
#ifndef _H_RADGLUE
#define _H_RADGLUE

#define RADGLUE_URL_BUFFER_SIZE			4096

#define RADGLUE_RETURN_CHUNKSIZE		4096

#define RADBGLUE_MAX_CONTEXTS			128

#define RADGLUE_SUCCESS					0
#define RADGLUE_EVAL_SUCCESS			0
#define RADGLUE_EVAL_SUSPEND			1
#define RADGLUE_EVAL_FAILURE			-100
#define RADGLUE_ERR_FILE_READ			-101
#define RADGLUE_ERR_FILE_WRITE			-102
#define RADGLUE_ERR_OUT_OF_MEMORY		-103
#define RADGLUE_ERR_FRAME_ERROR			-104
#define RADGLUE_ERR_INVALID				-105
#define RADGLUE_ERR_STACK				-106
#define RADGLUE_ERR_ESCAPE				-107
#define RADGLUE_ERR_PCODE				-108
#define RADGLUE_ERR_BAD_DATATYPE		-109
#define RADGLUE_ERR_RECURSION			-110
#define RADGLUE_ERR_FRAME_RELEASE		-111
#define RADGLUE_ERR_STACK_RELEASE		-112
#define RADGLUE_ERR_RECURSION_RELEASE	-113
#define RADGLUE_ERR_QUIT				-114
#define RADGLUE_ERR_WRONG_VERSION		-115
#define RADGLUE_ERR_ENGINE_BUSY			-116
#define RADGLUE_ERR_REPOSITORY_GC		-117
#define SGBLUE_UNEXPECTED_ERROR			-118
#define RADGLUE_ERR_NOT_IMPLEMENTED		-119
#define RADGLUE_ERR_SYSTEM_ERROR		-120
#define RADGLUE_ERR_EVAL_SECURITY		-121
#define RADGLUE_ERR_EVAL_BAD_CHECKIN	-122
#define RADGLUE_ERR_EVAL_BAD_FILTER		-123
#define RADGLUE_ERR_EVAL_BAD_SCORE		-124
#define RADGLUE_SENDTOCLIENT_FAILURE	-125
#define RADGLUE_ERR_BADBUFFERSIZE		-126

// iEvalTypes
#define RADGLUE_CMDSTRING				0		// simple cmd expression request
#define RADGLUE_MSGBLOCK				1		// amp message request
#define RADGLUE_DEBUGCMD				2
#define RADGLUE_CHECKIN					3
#define RADGLUE_DIRINFO					4
#define RADGLUE_FILEOPENRESPONSE		5
#define RADGLUE_EVENTMSG				6		// amp message event 
#define RADGLUE_EVENTCMD				7		// simple cmd expression event
#define RADGLUE_CMDSTRING_BINTRANSFER	8		// execute returning binary result

#define RADGLUE_NO_DEBUG				1
#define RADGLUE_WITH_DEBUG				0

/* AIS/SMTBASE control flags */
// Note that some of these defines are also defined in
// appclient.cpp. Change then there if you change them here!
#define RADGLUE_ERROR_TRACE				0x01	//1st bit 
#define RADGLUE_INSTRUCTION_TRACE		0x02	//2nd bit
#define RADGLUE_SYSCHECKON				0x04	//3rd bit
#define RADGLUE_JITON					0x10	//4th bit
#define RADGLUE_ESCAPE					0x100	//7th bit
#define RADGLUE_MODIFIED				0x200	//8th bit

/*  Host system external symbol definition (How API symbols are exported). */
#ifndef PUBLIC
#ifdef _MSVC
#define PUBLIC              extern _declspec(dllexport)
#else
#define PUBLIC              extern
#endif
#endif

// External functions for use by SessionManager:
long RADGLUE_CalculateStackSpace(long iMemorySize);

#endif
