/**********************************************************************************
    Copyright (C) 2013 AIS Foundation

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

#if 0
FConio.h

PARENT:             None 


AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FConio
#define _H_FConio

#ifdef  _C_FCONIO
#include "tobject.h"
#endif

#include "fsmtbase.h"

/*  Macro definitions */

/*  Function declarations */
extern  TVAL    FConio_Init			(LpXCONTEXT gCP, LpTHREAD gTP);
extern  TVAL    FConio_print		(LpXCONTEXT gCP, LpTHREAD gTP, TVAL aThing);
extern  TVAL    FConio_printf		(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR fmt, ...);
extern  TVAL    FConio_sprint		(LpXCONTEXT gCP, LpTHREAD gTP, NUM maxLen, LpCHAR buf, TVAL aThing);
extern  TVAL    FConio_pathName		(LpXCONTEXT gCP, LpTHREAD gTP, NUM maxLen, LpCHAR buf, TVAL pathName, TVAL fileName);
extern  BOLE    FConio_sprintf		(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR dest,LpCHAR fmt, ...);
extern  TVAL    FConio_sprintn		(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR buf,LpNUM size,TVAL aThing);
extern  BOLE    FConio_strblk		(LpXCONTEXT gCP, LpTHREAD gTP, register LpPOINTER p, register LpCHAR     s);
extern  BOLE    FConio_vsprintf		(LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR dest,LpCHAR fmt, LpPOINTER argument);
extern  TVAL    FConio_LexBuffer	(LpXCONTEXT gCP, LpTHREAD gTP, HMChar hData, NUM start, NUM end);
extern  TVAL    FTemp_Unimplemented	(void);
		TVAL	FConio_Fill32KBuffer(LpXCONTEXT gCP, LpTHREAD gTP,LpNUM fileSize, LpNUM readSize, NUM theFile, HMChar hData);
		void	FConio_GetRecord	(HMChar hData, register LpNUM recordSize, register LpNUM curCharIndex,LpNUM recordType, LpNUM recordCount, BOLE isSBFfile);
		void	FConio_GetColumnTAB (HMChar hData, register LpNUM recordSize, register LpNUM curCharIndex);
		void	FConio_GetColumnCSV (HMChar hData, register LpNUM recordSize, register LpNUM curCharIndex);

extern  TVAL    FConio_Parse		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_Display		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_Writeln		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_Writelg		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_AppendWriteln(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_Errorcr		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_Newline		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_RunScript	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_Silent		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_GetTickCount	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_GetRecursionCount(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_loadObject	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_saveObject	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_ImportTAB	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_ImportCSV	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_ExportTAB	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_ExportCSV	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_CheckEscape	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_System		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_FDisplay		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_FWriteln		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_fopen		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fclose		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fdir			(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_ferase		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_feraseDir	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fexists		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_FileReadRecord	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fmkDir		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fresize		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fread		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fseek		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_ftell		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_fwrite		(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_logConsole	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  TVAL    FConio_CreateBuffer	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
extern  TVAL    FConio_DeleteBuffer (LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);

extern  NUM		FConio_Escape		(LpXCONTEXT gCP, LpTHREAD gTP);
extern	NUM		FConio_IO_printf	(LpXCONTEXT gCP, LpTHREAD gTP,char*  string,NUM  newline);
extern  NUM	    FConio_IO_fopen		(LpXCONTEXT gCP,LpTHREAD gTP,char* name, NUM mode, NUM type);
extern  NUM	    FConio_IO_fread		(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM length, char* bufptr);
extern  NUM	    FConio_IO_fwrite	(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM length, const char* bufptr);
extern  REAL    FConio_IO_fseek		(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, REAL adjustment, NUM code);
extern  REAL    FConio_IO_fresize	(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, REAL newsize);
extern  NUM	    FConio_IO_fclose	(LpXCONTEXT gCP,LpTHREAD gTP,NUM fileID, NUM opcode);
extern  NUM	    FConio_IO_fremove	(LpXCONTEXT gCP,LpTHREAD gTP,char* name);
extern  NUM	    FConio_IO_writeLog	(LpXCONTEXT gCP, LpTHREAD gTP,char*  string,NUM  newline);


/* Define the record constants for .SBF files */

#define COLUMNREC	1
#define TYPEREC		2
#define KEYDEFREC	3
#define DATAREC		4

#endif

