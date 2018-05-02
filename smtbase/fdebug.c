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

#define _C_FDEBUG
#define _SMARTBASE

#if 0
FDebug.c

AUTHORS:            Michael F. Korns

MODIFICATIONS:  

#endif

#include    "fdebug.h"
#include    "futil3.h"
#include    "fpred2.h"
#include    "fvmscpt.h"
#include    "tdirect.h"
#include    "tstruct.h"
#include    "tbytevec.h"
#include    "tvector.h"
#include    "tobject.h"

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_Init

Initialize the console io and string formatting portion of the AIS Lisp function 
library.  

#endif

TVAL FDebug_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ec);
EndFrame
 
if(gCP->FDebug_Initialized) 
    FrameExit(gCP->TObject_OK);

gCP->FDebug_Initialized = TRUE;
    
/* Register the AIS Lisp cProcedures contained in this package */

*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"debug",(LpFUNC)&FDebug_Debug);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"debugEval",(LpFUNC)&FDebug_DebugEval);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"disassemble",(LpFUNC)&FDebug_SourceDisassemble);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"debugBrowsableProcs",(LpFUNC)&FDebug_browsableProcs);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"getSymbolTable",(LpFUNC)&FDebug_GetSymbolTable);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&gCP->FDebug_getGlobalValue,(LpCHAR)"getGlobalValue",(LpFUNC)&FDebug_GetGlobalValue);
ExitOnError(*ec);
*ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"globalBinding",(LpFUNC)&FDebug_GetGlobalBind);
ExitOnError(*ec);

gCP->FDebug_exit     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"exit");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_exit,TRUE);
gCP->FDebug_short    = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"short");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_short,TRUE);
gCP->FDebug_bp       = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bp");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_bp,TRUE);
gCP->FDebug_bl       = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bl");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_bl,TRUE);
gCP->FDebug_bc       = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"bc");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_bc,TRUE);
gCP->FDebug_src      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"src");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_src,TRUE);
gCP->FDebug_asm      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"asm");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_asm,TRUE);
gCP->FDebug_envs     = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"envs");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_envs,TRUE);
gCP->FDebug_srcOnly  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"srcOnly");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_srcOnly,TRUE);
gCP->FDebug_srcCnt   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"srcCnt");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_srcCnt,TRUE);
gCP->FDebug_traceon  = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"traceon");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_traceon,TRUE);
gCP->FDebug_traceoff = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"traceoff");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_traceoff,TRUE);
gCP->FDebug_on       = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"on");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_on,TRUE);
gCP->FDebug_off      = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"off");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_off,TRUE);
gCP->FDebug_jiton    = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"jiton");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_jiton,TRUE);
gCP->FDebug_jitoff   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"jitoff");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_jitoff,TRUE);
gCP->FDebug_until    = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"until");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_until,TRUE);
gCP->FDebug_go       = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"go");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_go,TRUE);
gCP->FDebug_dbgVec   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"dbgListLines");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_dbgVec,TRUE);
gCP->FDebug_currentResult   = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_currentResult");
FObject_Perm(gCP,gTP,(TObject*)gCP->FDebug_currentResult,TRUE);

gCP->FDebug_Space = TObjVector_New(gCP,gTP);
FObject_Perm(gCP,gTP,(TObject*)(TObject*)gCP->FDebug_Space, TRUE);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FDebug_Debug

Commands are provided for setting the Smartbase engine switches and debugger trace settings.

The Smartbase engine has two major execution settings: JitON and JitOFF. In JitON mode, the
Just-In-Time compiler generates actual machine register code for extremely fast execution.
In JitOFF mode, the Virtual Machine Emulator is turned on allowing debugger tracing of instructions.

In JitOFF mode, the Smartbase engine allows tracing of VM Emulated instructions IF AND ONLY IF the
instructions have been executed as a result of the debugEval function. For example, the following code

				(debugEval "(foo testing:)")

will evaluate the phrase "(foo testing:)" with VM instruction tracing on under the following rules and
guidelines.

JitON Rule:		If the Smartbase engine JIT is ON, then debugEval will evaluate the specified expression WITHOUT	
				instruction tracing. Instruction tracing is ONLY available with JitOFF.

Trace Rule:		Once inside the debugEval function, with JitOFF, instruction execution will proceed normally until
				the current debug trace settings requests a trace break. A trace break is always requested under
				the following trace rules:

					Trace-Lambda-Setting:
					gTP->DebugTraceLambda = #<current Lambda>			A trace break is requested IFF the Trace-CodeL-Setting requests.	
					gTP->DebugTraceLambda = -1							A trace break is requested IFF the Trace-CodeL-Setting requests.	
					gTP->DebugTraceLambda = -2							A trace break is NEVER requested.	

					Trace-CodeL-Setting:
					gTP->DebugTraceCodeL = current IP					A trace break is requested IFF the Trace-Recursion-Setting requests.	
					gTP->DebugTraceCodeL = -1							A trace break is requested IFF the Trace-Recursion-Setting requests.	
					gTP->DebugTraceCodeL = -2							A trace break is NEVER requested.	

					Trace-Recursion-Setting:
					gTP->RecursionCount <= gTP->DebugTraceRecursion		A trace break is requested.	
					gTP->DebugTraceRecursion = -1						A trace break is requested.	
					gTP->DebugTraceRecursion = -2						A trace break is NEVER requested.	

Break Rule:		Once inside the debugEval function, with JitOFF, if a trace break is requested, by the above trace
				rules, then the VM Emulator calls the "debugDetective" function, which has been supplied by the user.
				As soon as the debugDetective function returns control (or if debugDetective is VOID), the VM Emulator
				continues instruction evaluation normally, subject to the above trace rules.

debugDetective:	The user supplied debugDetective, in combination with the debugEval function, provide an enhancement to
				the standard Lisp debugging method, which is to insert writeln instructions at strategic locations in the code.
				Before the debugDectective function is called, the VM Emulator sets the gTP->DebugSuspended switch to ON
				so that instuction tracing is inactive until the debugDetective function returns control to the VM Emulator.

				The activities of the debugDetective can include the traditional Lisp writeln instructions or they can be as 
				complex and creative as the user supplied debugDetective function can manage.

				Once the debugDetective function returns control to the VM Emulator, instruction tracing continues in the
				normal manner until the debugEval function retiurns control. Instruction tracing is ONLY active while
				inside the debugEval function when JitOFF is true.

				The debugDetective function is called by two callers as follows:

				;; Called by the debug & debugEval functions when the engine state is changed.
				(debugDetective)	

				;; Called by the VM Emulator when a trace break has been requested.
				(debugDetective Lambda CodeL Recursion Rv Av Tv Pv Cv Sv Iv)	


Commands and arguments to the "debug" function:

(debug compileoff:)		Sets the Smartbase "compile" function NOT to generate debug information.
(debug compileon:)		Sets the Smartbase "compile" function to generate debug information.
(debug erroroff:)		Turns off the gTP->DebugErrorON switch.
(debug erroron:)		Turns on the gTP->DebugErrorON switch.
(debug get:)			Returns a Structure with the following attributes:
							JitON:			gTP->DebugJitON	
							TraceON:		gTP->DebugJitON	
							TraceSuspended:	gTP->DebugSuspended	
							ErrorON:		gTP->DebugErrorOn
							Lambda:			gTP->DebugTraceLambda
							CodeL:			gTP->DebugTraceCodeL
							Recursion:		gTP->DebugTraceRecursion
							Compile:		gTP->FCompile_GenerateDebugInfo
							Changed:		gTP->EngineStateChanged
(debug jitoff:)			Turns off the gTP->DebugJitON switch.
(debug jiton:)			Turns on the gTP->DebugJitON switch.
(debug command)			Sets the current instruction trace rules using the command string which is parsed
						as follows:

							"Lambda:CodeL:Recursion"

						Some examples follow:
							"!"				gTP->DebugTraceLambda=-2,gTP->DebugTraceCodeL=-2,gTP->DebugTraceRecursion=-2		
							"*"				gTP->DebugTraceLambda=-1,gTP->DebugTraceCodeL=-1,gTP->DebugTraceRecursion=-1		
							"name:*:*"		gTP->DebugTraceLambda=(eval name),gTP->DebugTraceCodeL=-1,gTP->DebugTraceRecursion=-1		
							"*:45:*"		gTP->DebugTraceLambda=-1,gTP->DebugTraceCodeL=45,gTP->DebugTraceRecursion=-1		
							"*:*:3"			gTP->DebugTraceLambda=-1,gTP->DebugTraceCodeL=45,gTP->DebugTraceRecursion=3		
							"name:52:5"		gTP->DebugTraceLambda=(eval name),gTP->DebugTraceCodeL=52,gTP->DebugTraceRecursion=5

						The grammar rules are as follows:

							"!"		
							"*"
							"name|*:number|*:number|*"

*/			
#endif

TVAL    FDebug_Debug(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM					ai;			/* Argument index */
LpCHAR				stringPtr;
NUM					stringLen;

StartFrame
DeclareTVAL(debugDetective);
DeclareTVAL(ret);
DeclareTVAL(vec);
DeclareTVAL(traceLambda);
DeclareTVAL(traceCodeL);
DeclareTVAL(traceRecursion);
EndFrame

/*  Make sure there is at least one argument. */
if (argc < 1) goto BadCleanUp;
    
ai = 0;
while (ai < argc) {
	// (debug compileoff:)		Sets the Smartbase "compile" function NOT to generate debug information.
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"compileoff") ==0))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE))
		{
			IgnoreRequest:
			FrameExit(TERROR("!debug: cannot accept this command while instruction trace is on!"));
		}
		gTP->FCompile_GenerateDebugInfo = FALSE;
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
	// (debug compileon:)		Sets the Smartbase "compile" function to generate debug information.
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"compileon") ==0))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE)) goto IgnoreRequest;
		gTP->FCompile_GenerateDebugInfo = TRUE;
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
	// (debug erroroff:)		Turns off the gTP->DebugErrorON switch.
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"erroroff") ==0))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE)) goto IgnoreRequest;
		gTP->DebugErrorON = FALSE;
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
	// (debug erroron:)		Turns on the gTP->DebugErrorON switch.
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"erroron") ==0))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE)) goto IgnoreRequest;
		gTP->DebugErrorON = TRUE;
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
	// (debug jitoff:)			Turns off the gTP->DebugJitON switch.
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"jitoff") ==0))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE)) goto IgnoreRequest;
		gTP->DebugJitON = FALSE;
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
	// (debug jiton:)			Turns on the gTP->DebugJitON switch.
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"jiton") ==0))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE)) goto IgnoreRequest;
		gTP->DebugJitON = TRUE;
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
	// (debug get:)			Returns a Structure with the following attributes:
	//							JitON:			gTP->DebugJitON	
	//							TraceON:		gTP->DebugTraceON	
	//							TraceSuspended:	gTP->DebugSuspended	
	//							ErrorON:		gTP->DebugErrorOn
	//							CompileDebugON:	gTP->FCompile_GenerateDebugInfo
	//							Changed:		gTP->EngineStateChanged
	//							Lambda:			gTP->DebugTraceLambda
	//							CodeL:			gTP->DebugTraceCodeL
	//							Recursion:		gTP->DebugTraceRecursion
	if ((argv[ai].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[ai]),"get") ==0))
		{
			ret->u.Structure = TStructure_New(gCP,gTP);ret->Tag = TYSTRUCTURE;
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("JitON"),TBOOL(gTP->DebugJitON));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("TraceON"),TBOOL(gTP->DebugTraceON));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("TraceSuspended"),TBOOL(gTP->DebugSuspended));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("ErrorON"),TBOOL(gTP->DebugErrorON));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("CompileDebugON"),TBOOL(gTP->FCompile_GenerateDebugInfo));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("Changed"),TBOOL(gTP->EngineStateChanged));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("Lambda"),gTP->DebugTraceLambda);
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("CodeL"),TINT(gTP->DebugTraceCodeL));
			TStructure_AddNewValue(gCP,gTP,*ret,TSYMBOL("Recursion"),TINT(gTP->DebugTraceRecursion));
			FrameExit(*ret);	/* ignore any extra arguments */
		}
	else
	// (debug command)			Sets the current instruction trace rules using the command string which is parsed
	//							as follows:
	//
	//							"Lambda:CodeL:Recursion"
	//
	//							Some examples follow:
	//								"!"				gTP->DebugTraceLambda=-2,gTP->DebugTraceCodeL=-2,gTP->DebugTraceRecursion=-2		
	//								"*"				gTP->DebugTraceLambda=-1,gTP->DebugTraceCodeL=-1,gTP->DebugTraceRecursion=-1		
	//								"name:*:*"		gTP->DebugTraceLambda=(eval name),gTP->DebugTraceCodeL=-1,gTP->DebugTraceRecursion=-1		
	//								"*:45:*"		gTP->DebugTraceLambda=-1,gTP->DebugTraceCodeL=45,gTP->DebugTraceRecursion=-1		
	//								"*:*:3"			gTP->DebugTraceLambda=-1,gTP->DebugTraceCodeL=45,gTP->DebugTraceRecursion=3		
	//								"name:52:5"		gTP->DebugTraceLambda=(eval name),gTP->DebugTraceCodeL=52,gTP->DebugTraceRecursion=5		
	if ((argv[ai].Tag == TYTEXT) || (argv[ai].Tag == TYSTRING))
		{
		if ((gTP->DebugJitON == FALSE)&&(gTP->DebugTraceON == TRUE)) goto IgnoreRequest;

		/* Get a pointer to the String argument */
		FSmartbase_StringPtr(gCP,gTP,&argv[0],&stringPtr,&stringLen);

		if ((stringLen == 1) && (stringPtr[0] == '!'))
		{
			/* Turn trace break OFF for EVERY instruction */
			gTP->DebugTraceLambda = TINT(-2);
			gTP->DebugTraceCodeL = -2;
			gTP->DebugTraceRecursion = -2;
			goto SetTraceLast;
		}
		else
		if ((stringLen == 1) && (stringPtr[0] == '*'))
		{
			/* Turn trace break ON for EVERY instruction */
			gTP->DebugTraceLambda = TINT(-1);
			gTP->DebugTraceCodeL = -1;
			gTP->DebugTraceRecursion = -1;
			goto SetTraceLast;
		}
		else
		{
			/* Attempt to separate the setting into its mandatory three ":" parts */
			*vec = FSmartbase_Eval(gCP,gTP,TGVALUE("stringToVector"),2,argv[0],TSTRING(":"));
			if ((vec->Tag != TYVECTOR) || (vec->u.Vector->itsMaxItemIndex != 3)) goto BadCleanUp;
			*traceLambda = TvalArray(*vec)[0];
			*traceCodeL = TvalArray(*vec)[1];
			*traceRecursion = TvalArray(*vec)[2];

			/* Parse the traceLambda portion of the command */
			*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("="),2,*traceLambda,TSTRING("*"));
			if ((ret->Tag == TYBOLE) && (ret->u.Bool == (BOLE)TRUE))
				gTP->DebugTraceLambda = TINT(-1);
			else
			{
				*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("eval"),1,*traceLambda);
				if (ret->Tag != TYLAMBDA) goto BadCleanUp;
				gTP->DebugTraceLambda = *ret;
			}

			/* Parse the traceCodeL portion of the command */
			*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("="),2,*traceCodeL,TSTRING("*"));
			if ((ret->Tag == TYBOLE) && (ret->u.Bool == (BOLE)TRUE))
				gTP->DebugTraceCodeL = -1;
			else
			{
				*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("integer"),1,*traceCodeL);
				if (ret->Tag != TYNUM) goto BadCleanUp;
				gTP->DebugTraceCodeL = ret->u.Int;
				if (gTP->DebugTraceCodeL < 0) goto BadCleanUp; 
			}

			/* Parse the traceRecursion portion of the command */
			*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("="),2,*traceRecursion,TSTRING("*"));
			if ((ret->Tag == TYBOLE) && (ret->u.Bool == (BOLE)TRUE))
				gTP->DebugTraceRecursion = -1;
			else
			{
				*ret = FSmartbase_Eval(gCP,gTP,TGVALUE("integer"),1,*traceRecursion);
				if (ret->Tag != TYNUM) goto BadCleanUp;
				gTP->DebugTraceRecursion = ret->u.Int;
				if (gTP->DebugTraceRecursion < 0) goto BadCleanUp; 
			}
		}

		SetTraceLast:
		gTP->EngineStateChanged = TRUE;
		++ai; /* accept any extra arguments */
		}
	else
		goto BadCleanUp; /* Unrecognized argument */

	++ai;
	}/* end while on ai < argc */

Exit:
/* The debugDetective function must be notified of any change in the engine debug flags. */
if (gTP->EngineStateChanged == TRUE)
	{
		*debugDetective = gCP->FVmscript_Symbol_debugDetective->itsGlobalValue;
		if (debugDetective->Tag != TYVOID)
			FSmartbase_Eval(gCP,gTP,*debugDetective,0,gCP->Tval_VOID);
		gTP->EngineStateChanged = FALSE;
	}
FrameExit(gCP->TObject_TRUE);

BadCleanUp:
FrameExit(TERROR("!debug: invalid argument!"));
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FDebug_DebugEval

The debugEval function evaluates the specified Lisp String argument with VM Emulator
instruction tracing on.

The rule plus the conditions and restrictions governinh instruction tracing are explained
as follows.

The Smartbase engine has two major execution settings: JitON and JitOFF. In JitON mode, the
Just-In-Time compiler generates actual machine register code for extremely fast execution.
In JitOFF mode, the Virtual Machine Emulator is turned on allowing debugger tracing of instructions.

In JitOFF mode, the Smartbase engine allows tracing of VM Emulated instructions IF AND ONLY IF the
instructions have been executed as a result of the debugEval function. For example, the following code

				(debugEval "(foo testing:)")

will evaluate the phrase "(foo testing:)" with VM instruction tracing on under the following rules and
guidelines.

JitON Rule:		If the Smartbase engine JIT is ON, then debugEval will evaluate the specified expression WITHOUT	
				instruction tracing. Instruction tracing is ONLY available with JitOFF.

Trace Rule:		Once inside the debugEval function, with JitOFF, instruction execution will proceed normally until
				the current debug trace settings requests a trace break. A trace break is always requested under
				the following trace rules:

					Trace-Lambda-Setting:
					gTP->DebugTraceLambda = #<current Lambda>			A trace break is requested IFF the Trace-CodeL-Setting requests.	
					gTP->DebugTraceLambda = -1							A trace break is requested IFF the Trace-CodeL-Setting requests.	
					gTP->DebugTraceLambda = -2							A trace break is NEVER requested.	

					Trace-CodeL-Setting:
					gTP->DebugTraceCodeL = current IP					A trace break is requested IFF the Trace-Recursion-Setting requests.	
					gTP->DebugTraceCodeL = -1							A trace break is requested IFF the Trace-Recursion-Setting requests.	
					gTP->DebugTraceCodeL = -2							A trace break is NEVER requested.	

					Trace-Recursion-Setting:
					gTP->RecursionCount <= gTP->DebugTraceRecursion		A trace break is requested.	
					gTP->DebugTraceRecursion = -1						A trace break is requested.	
					gTP->DebugTraceRecursion = -2						A trace break is NEVER requested.	

Break Rule:		Once inside the debugEval function, with JitOFF, if a trace break is requested, by the above trace
				rules, then the VM Emulator calls the "debugDetective" function, which has been supplied by the user.
				As soon as the debugDetective function returns control (or if debugDetective is VOID), the VM Emulator
				continues instruction evaluation normally, subject to the above trace rules.

debugDetective:	The user supplied debugDetective, in combination with the debugEval function, provide an enhancement to
				the standard Lisp debugging method, which is to insert writeln instructions at strategic locations in the code.
				Before the debugDectective function is called, the VM Emulator sets the gTP->DebugSuspended switch to ON
				so that instuction tracing is inactive until the debugDetective function returns control to the VM Emulator.

				The activities of the debugDetective can include the traditional Lisp writeln instructions or they can be as 
				complex and creative as the user supplied debugDetective function can manage.

				Once the debugDetective function returns control to the VM Emulator, instruction tracing continues in the
				normal manner until the debugEval function retiurns control. Instruction tracing is ONLY active while
				inside the debugEval function when JitOFF is true.

				The debugDetective function is called by two callers as follows:

				;; Called by the debug & debugEval functions when the engine state is changed.
				(debugDetective)	

				;; Called by the VM Emulator when a trace break has been requested.
				(debugDetective Lambda CodeL Recursion Rv Av Tv Pv Cv Sv Iv)	

*/			
#endif

TVAL    FDebug_DebugEval(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
LpCHAR			stringPtr;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Make sure there is only one valid argument. */
if ((argc == 1) && ((argv[0].Tag == TYTEXT) || (argv[0].Tag == TYSTRING) || (argv[0].Tag == TYBYTEVECTOR)))
{
	/*  Make sure that instruction tracing is NOT already on. */
	if ((gTP->DebugTraceON == TRUE) || (gTP->DebugSuspended == TRUE))
	{
		gTP->DebugTraceON = FALSE;
		gTP->DebugSuspended = FALSE;
		FrameExit(TERROR("!debugEval: cannot evaluate when instruction tracing is on!"));
	}

	// Convert the single input argument to a text pointer
	// Note: The text must be valid Lisp source statements
	switch(argv[0].Tag)
	{
	case TYTEXT:
		stringPtr = &argv[0].u.Text[0];
		break;
		
	case TYSTRING:
		stringPtr = CharArray(argv[0]);
		break;
		
	case TYBYTEVECTOR:
		stringPtr = ByteArray(argv[0]);
		break;
	}


	/*  Do NOT trace if the JIT is ON or if the debugDetective is void. */
	if ((gTP->DebugJitON == TRUE) || (gCP->FVmscript_Symbol_debugDetective->itsGlobalValue.Tag == TYVOID))
	{
		/* Evaluate the Lisp String argument with instruction tracing OFF. */
		gTP->DebugTraceON = FALSE;
		gTP->DebugSuspended = FALSE;
		*ret = FSmartbase_Evals(gCP,gTP,stringPtr,FALSE);
		gTP->DebugTraceON = FALSE;
		gTP->DebugSuspended = FALSE;
	}
	else
	{
		/* Evaluate the Lisp String argument with instruction tracing ON. */
		gTP->DebugTraceON = TRUE;
		gTP->DebugSuspended = FALSE;
		//FSmartbase_Eval(gCP,gTP,gCP->FVmscript_Symbol_debugDetective->itsGlobalValue,0,NULL);
		*ret = FSmartbase_Evals(gCP,gTP,stringPtr,FALSE);
		gTP->DebugTraceON = FALSE;
		gTP->DebugSuspended = FALSE;
		//FSmartbase_Eval(gCP,gTP,gCP->FVmscript_Symbol_debugDetective->itsGlobalValue,0,NULL);
	}

	FrameExit(*ret);
}
    

BadCleanUp:
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
FrameExit(TERROR("!debugEval: invalid argument!"));
}

/*--------------------------------------------------------------------------------------- */
/*
FDebug_SourceDisassemble

This is the interface procedure which supports the disassembly of Lambda objects as follows:

The first form shown is used to dump the pcode vector in a symbolic format, start and end are
optional instruction offsets into the pcode vector itself and indicate the limits of the assembler
instructions to be displayed:

    (disassemble Lambda [start [end])
    (disassemble Lambda short:)
	(disassemble Lambda src: short:)

The second form allows the display of the entire Lambda including all of
the Lambda's supporting variable and environment structures. 

    (disassemble Lambda all:)

This form allows the display of only the Lambda's supporting variable 
and environment structures. 

    (disassemble Lambda envs:)

This form allows the display of only the Lambda's original source 
lines prefixed by their related assembler instruction displacements. 

    (disassemble Lambda source:)

The third form allows the display of only the Lambda's disassembled 
DRM VM pcode instruction lines prefixed by their related displacements. 

    (disassemble Lambda asm:)

Note: In Smartbase v4.0, we only disassemble in Lisp format.
*/

TVAL    FDebug_SourceDisassemble(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 cn;
NUM                 cn1;
NUM                 nn;
NUM                 brkNum = 0;
NUM                 line = 0;
NUM                 minLimit = 0;
NUM                 maxLimit = 1000000;
BOLE                sourceCnt = TRUE;
BOLE                showSource;
BOLE                showAsm;
BOLE                showEnvs;
BOLE				showAll = FALSE;
BOLE				hasBrk = FALSE;
CHAR				hdrBuf[32];
TVAL                append;
StartFrame
DeclareOBJ(TLambda,proc);
DeclareOBJ(TVector,Sc);
DeclareOBJ(TStructure,anEnv);
DeclareTVAL(tmp);
DeclareTVAL(tmp2);
DeclareTVAL(result);
DeclareTVAL(header);
DeclareTVAL(eName);
DeclareTVAL(dbgSourceLinesInstr);
DeclareTVAL(dbgSourceLines);

EndFrame

/*  Argument validation and setup */

append = TGVALUE("append");
*result = TSTRING("");
if (((argv[0].Tag == TYLAMBDA) || (argv[0].Tag == TYMACRO)) &&
    (argv[0].u.Lambda->VirtualMachine == gCP->TLambda_LispVirtualMachine) &&
    (argv[0].u.Lambda->PcodeVector != NIL))
    {
    /*  Syntax looks ok so we will init control variables for this procedure */
    
    proc = (TLambda*)asObject(&argv[0]);
    hasBrk = FALSE;
    line = 0;
    brkNum = 0;
    minLimit = 0;
    maxLimit = 100000;
    showAsm = TRUE;
    showSource = FALSE;
    showEnvs = FALSE;
    sourceCnt = FALSE;
    strcpy((char*)hdrBuf, (const char*)"                     ");

    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda all:)                                 */
    /*  Note: In Smartbase v4.0, we only disassemble in short Lisp format. */
    /* ******************************************************************* */
    if ((argc >= 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"all") == 0))
        {
        showAll = TRUE;

		/* Display the Lambda object. */
		*result = FSmartbase_Eval(gCP,gTP,append,3,*result,argv[0],TCHAR(LINEBREAK));

        /* ************************************* */
        /* Display the Lambda variable structures */
        /* ************************************* */
        
        for(cn1 = 0; cn1 < 7; cn1++)
            {
            
            switch(cn1)
                {
                case 0: 
                    *tmp = TOBJ(proc->Interfaces);
                    *eName = TSTRING("In.");
                    break;

                case 1: 
                    *tmp = TOBJ(proc->ArgumentVariables);
                    *eName = TSTRING("Av.");
                    break;

                case 2: 
                    *tmp = TOBJ(proc->RegisterVariables);
                    *eName = TSTRING("Rv.");
                    break;

                case 3: 
                    *tmp = TOBJ(proc->TemporaryVariables);
                    *eName = TSTRING("Tv.");
                    break;

                case 4: 
                    *tmp = TOBJ(proc->ClassVariables);
                    *eName = TSTRING("Sv.");
                    break;

                case 5: 
                    *tmp = TOBJ(proc->PersistantVariables);
                    *eName = TSTRING("Pv.");
                    break;

                case 6: 
                    *tmp = TOBJ(proc->ConstantVariables);
                    *eName = TSTRING("Cv.");
                    break;
                }
                
            if(asTag(tmp) == TYSTRUCTURE)
                {
                anEnv = (TStructure*)asObject(tmp);
                for(cn = 0; cn < anEnv->itsMaxItemIndex; cn++)
                    {
                    asObject(tmp) = (TObject*)atHMBind(anEnv->itsDictionaryArray,cn).Key;
                    asTag(tmp) = asObject(tmp)->itsObjectType;
                    *result = FSmartbase_Eval(gCP,gTP,append,6,
                                                    *result,
                                                    *eName,
                                                    *tmp,
                                                    TSTRING(" = "),
                                                    atHMBind(anEnv->itsDictionaryArray,cn).Value,
                                                    TCHAR(LINEBREAK));
                    }
                }
            }

        /*  We show either source or vm instructions (if they are available). */
		*header = *result;

		goto DisplayShortSourceInfo;
        }
	else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda envs:)                                 */
    /* ******************************************************************* */
    if ((argc >= 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"envs") == 0))
        {
        showEnvs = TRUE;
        
        /*  Display the Structures ONLY */
        
        for(cn1 = 0; cn1 < 7; cn1++)
            {
            
            switch(cn1)
                {
                case 0: 
                    *tmp = TOBJ(proc->Interfaces);
                    *eName = TSTRING("In.");
                    break;

                case 1: 
                    *tmp = TOBJ(proc->ArgumentVariables);
                    *eName = TSTRING("Av.");
                    break;

                case 2: 
                    *tmp = TOBJ(proc->RegisterVariables);
                    *eName = TSTRING("Rv.");
                    break;

                case 3: 
                    *tmp = TOBJ(proc->TemporaryVariables);
                    *eName = TSTRING("Tv.");
                    break;

                case 4: 
                    *tmp = TOBJ(proc->ClassVariables);
                    *eName = TSTRING("Sv.");
                    break;

                case 5: 
                    *tmp = TOBJ(proc->PersistantVariables);
                    *eName = TSTRING("Pv.");
                    break;

                case 6: 
                    *tmp = TOBJ(proc->ConstantVariables);
                    *eName = TSTRING("Cv.");
                    break;
                }
                
            if (tmp->Tag == TYSTRUCTURE)
                {
                anEnv = (TStructure*)asObject(tmp);
                for(cn = 0; cn < anEnv->itsMaxItemIndex; cn++)
                    {
                    asObject(tmp) = (TObject*)atHMBind(anEnv->itsDictionaryArray,cn).Key;
                    asTag(tmp) = asObject(tmp)->itsObjectType;
                    *result = FSmartbase_Eval(gCP,gTP,append,6,
                                                    *result,
                                                    *eName,
                                                    *tmp,
                                                    TSTRING(" = "),
                                                    atHMBind(anEnv->itsDictionaryArray,cn).Value,
                                                    TCHAR(LINEBREAK));
                    }
                }
            }
        
        goto CleanUp;
        }
	else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda start end)                            */
    /*  Note: In Smartbase v4.0, we only disassemble in short Lisp format. */
    /* ******************************************************************* */
    if ((argc >= 3) && (argv[1].Tag == TYNUM) && (argv[2].Tag == TYNUM))
        {
        minLimit = argv[1].u.Int;
        maxLimit = argv[2].u.Int;

        /*  We show vm instructions.			                                */
		/*  Note1: In Smartbase v4.0, we only disassemble in short Lisp format. */
		/*  Note2: Place the results in the DebuggerSource field of the Lambda.  */
		gTP->FDebug_ShowShort = TRUE;
       
		*header = *result;
        *result = FDebug_VMDisassemble(gCP,gTP,argc,argv);
		proc->DebuggerSource = (_TObject_TypeFlag(result->Tag) == _TObject_TfTOBJECT) ? result->u.Object : NIL;
		

		goto CleanUp;
        }
    else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda source:)                              */
    /* ******************************************************************* */
    if ((argc >= 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"source") == 0))
        {
		*header = TSTRING("");

		DisplayShortSourceInfo:
		/* Try to locate the head of this list in the DebugListLines Directory of the Lambda */
		if (proc->Interfaces != NIL)
			{
			/* Get Vector of the original source lines.  */
			*dbgSourceLines = FSmartbase_Ref(gCP,gTP,2,TOBJ(proc->Interfaces),TOBJ(gCP->FCompile_dbgSourceLinesSYM));
			ExitOnError(*dbgSourceLines);
			if (dbgSourceLines->Tag != TYVECTOR) goto NoSourceAvailable;

			/* Get source lines ==> pcode instruction displacement vector.  */
			*dbgSourceLinesInstr = FSmartbase_Ref(gCP,gTP,2,TOBJ(proc->Interfaces),TOBJ(gCP->FCompile_dbgSourceLinesInstrSYM));
			ExitOnError(*dbgSourceLinesInstr);
			if (dbgSourceLinesInstr->Tag != TYDIRECTORY) goto NoSourceAvailable;
			
			/* Create a display of the original source lines and the associated debugger information. */
			line = 100000000;
			maxLimit = dbgSourceLinesInstr->u.Directory->itsMaxItemIndex;
			for (cn = 0; cn < maxLimit; ++cn)
				{
				/* Covert assembler instruction displacement as a string in the following format -  "000000:" */  
				tmp->Tag = TYNUM;
				cn1 = PBindArray(*dbgSourceLinesInstr)[cn].Key.u.Int;
				tmp->u.Int = PBindArray(*dbgSourceLinesInstr)[cn].Value.u.Int + 1000000;
				*tmp = FSmartbase_Eval(gCP,gTP,TGVALUE("string"),1,*tmp);
				if (tmp->Tag != TYTEXT) goto BadCleanUp;
				tmp->u.Text[0] = tmp->u.Text[1];
				tmp->u.Text[1] = tmp->u.Text[2];
				tmp->u.Text[2] = tmp->u.Text[3];
				tmp->u.Text[3] = tmp->u.Text[4];
				tmp->u.Text[4] = tmp->u.Text[5];
				tmp->u.Text[5] = tmp->u.Text[6];
				tmp->u.Text[6] = ':';
				tmp->u.Text[7] = 0;

				/* Fill in any gaps in the source lines (if necessary). */
				if (line < (cn1 + 1))
					{
					for (nn = line + 1; nn < cn1; ++nn)
						{
						/* Get the missing original source string from the source lines Vector. */ 
						tmp2->Tag = TYNUM;
						tmp2->u.Int = nn;
						*result = FSmartbase_Ref(gCP,gTP,2,*dbgSourceLines,*tmp2);
						ExitOnError(*result);
						
						/* Append this source string to the other source strings for this Lambda. */ 
						tmp2->u.Text[0] = ' ';
						tmp2->u.Text[1] = 0;
						tmp2->Tag = TYTEXT;
						*header = FSmartbase_Eval(gCP,gTP,append,4,*header,*tmp,*tmp2,*result);
						ExitOnError(*header);
						}
					}
				line = cn1;

				/* Get the original source string from the source lines Vector. */ 
				tmp2->Tag = TYNUM;
				tmp2->u.Int = PBindArray(*dbgSourceLinesInstr)[cn].Key.u.Int;
				*result = FSmartbase_Ref(gCP,gTP,2,*dbgSourceLines,*tmp2);
				ExitOnError(*result);
				
				/* Append this source string to the other source strings for this Lambda. */ 
				tmp2->u.Text[0] = ' ';
				tmp2->u.Text[1] = 0;
				tmp2->Tag = TYTEXT;
				*header = FSmartbase_Eval(gCP,gTP,append,4,*header,*tmp,*tmp2,*result);
				ExitOnError(*header);
				}
			
			*result = *header;
			}
		else
			{
			/*  We show vm instructions.			                                */
			/*  Note1: In Smartbase v4.0, we only disassemble in short Lisp format. */
			NoSourceAvailable:
			gTP->FDebug_ShowShort = TRUE;
			*result = FDebug_VMDisassemble(gCP,gTP,1,argv);
            *result = FSmartbase_Eval(gCP,gTP,append,2,*header,*result);
			}
			
		/*  Place the results in the DebuggerSource field of the Lambda.  */
		proc->DebuggerSource = (_TObject_TypeFlag(result->Tag) == _TObject_TfTOBJECT) ? result->u.Object : NIL;
		
		goto CleanUp;
        }
    else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda asm:)                                 */
    /* ******************************************************************* */
    if ((argc >= 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"asm") == 0))
        {
		*header = TSTRING("");

		/*  We show vm instructions.			                           */
		/*  In Smartbase v4.0, we only disassemble in short Lisp format.   */
		gTP->FDebug_ShowShort = TRUE;
		*result = FDebug_VMDisassemble(gCP,gTP,1,argv);
			
		/*  Place the results in the DebuggerSource field of the Lambda.    */
		proc->DebuggerSource = (_TObject_TypeFlag(result->Tag) == _TObject_TfTOBJECT) ? result->u.Object : NIL;
		
		goto CleanUp;
        }
	else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda short:)                               */
    /* ******************************************************************* */
    if ((argc >= 2) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"short") == 0))
        {
		/* If there is already source for the Lambda, then we will return that source. */
		if (proc->DebuggerSource != NIL)
			{
			FrameExit(TOBJ(proc->DebuggerSource));
			}
			
		goto DisplayShortSourceInfo;
        }
	else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda src: short:)                          */
    /* ******************************************************************* */
    if ((argc >= 3) && (argv[1].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[1]),"src") == 0) && (argv[2].Tag == TYSYMBOL) && (strcmp(SymbolArray(argv[2]),"short") == 0))
        {
		/* If there is already source for the Lambda, then we will return that source. */
		if (proc->DebuggerSource != NIL)
			{
			FrameExit(TOBJ(proc->DebuggerSource));
			}
			
		goto DisplayShortSourceInfo;
        }
	else
    /* ******************************************************************* */
    /*  Check for the optional format:                                     */
    /*            (disassemble Lambda)                                      */
    /* ******************************************************************* */
    if (argc == 1)
        {
		/* If there is already source for the Lambda, then we will return that source. */
		if (proc->DebuggerSource != NIL)
			{
			FrameExit(TOBJ(proc->DebuggerSource));
			}
			
		goto DisplayShortSourceInfo;
        }
    else
    /*  Incorrect arguments? */
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    }
else
    {
    *result = TSTRING("0000: ...drmVirtualMachine disassembly unavailable...");
    }

CleanUp:
gTP->FDebug_ShowShort = FALSE;
if (sourceCnt)
    {
    asTag(result) = TYNUM;
    asInt(result) = line;
    }
FrameExit(*result);

BadCleanUp:
gTP->FDebug_ShowShort = FALSE;
*result = TERROR("!disassemble: error generating display information!");
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_VMDisassemble

This is the interface procedure which supports the disassembly of procedure objects as follows:

The form shown is used to dump the pcode vector in a symbolic format, start and end are
instruction offsets into the pcode vector itself:

    (disassemble proc [start [end]])

#endif

TVAL    FDebug_VMDisassemble(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 vecIndex;
NUM                 startIns;
NUM                 endIns;
StartFrame
DeclareOBJ(TString,aString);
DeclareOBJ(TLambda,Proc);
DeclareOBJ(TPcodeVector,Pc);
DeclareTVAL(result);
DeclareTVAL(ret);
EndFrame

/*  argument validation and setup */

if ((asTag(&argv[0]) == TYLAMBDA || asTag(&argv[0]) == TYMACRO) &&
    ((asProcedure(&argv[0]))->PcodeVector != NIL))
    {
    /*  Syntax appears OK, we will init some control variables */
    
    Proc = (TLambda*)asObject(&argv[0]);
    Pc = Proc->PcodeVector;
    *result = gCP->Tval_VOID;
    startIns = -1;
    endIns = 1000000;

    if(argc >= 2 )
        {
        if(asTag(&argv[1]) == TYNUM)
            {
            /*  We expect the 2nd [1] arg to be an integer. */
            
            startIns = asInt(&argv[1]);
            }
        else
            FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
    if(argc == 3 && asTag(&argv[2]) == TYNUM)
        {
        endIns = asInt(&argv[2]);
        }

    if(argc >3)
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
/*  Examine all pcodes in the Pcode Vector  */

vecIndex = 0;
while (vecIndex < Pc->itsMaxItemIndex)
    {
    /*  We loop through the entire object until we get to the last requested offset. */

    if (startIns >= 0)
        {
        /*  Disassemble the specified range. */
        
        *ret = FDebug_VMDisassembleInstruction(gCP, gTP, Proc, &vecIndex, result, 0);
        ExitOnError(*ret);
        
        if (vecIndex > startIns )
            {
            if( vecIndex >= endIns) 
                break;
            }
        else
            *result = gCP->Tval_VOID;
        }
    else
        {
        /*  Disassemble the entire program */
        *ret = FDebug_VMDisassembleInstruction(gCP, gTP, Proc, &vecIndex, result, 0);
        ExitOnError(*ret);
        }
    }
    
FrameExit(*result);

*result = TERROR("!Disassemble!");
FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_VMDisassembleInstruction

This is the procedure which formats text to represent a single instruction.

#endif

TVAL    FDebug_VMDisassembleInstruction(LpXCONTEXT gCP,LpTHREAD gTP, TLambda* self, LpNUM vecIndex, LpTVAL inputTval, NUM toplineDisplay)
{
NUM					charIndex;
NUM                 errorSuspected;
NUM                 symNdx;
OPCODE              ExtendedOpcode;     
OPCODE              BreakListOpcode;     
UNUM                modifier[3];
NUM                 modIndex;
char                bufIns[128];
char                glabel[128];
char                bufMod[3][128];
char                loc[3][40];
char                buf[512];
char                tmp[512];
LpTVAL              dumpTval;
TVAL                eol = TGVALUE("_eol");
LpCHAR              eolPtr;
NUM                 eolLen;
CHAR				cbuf[2];
NUM					offset;
StartFrame
DeclareOBJ(TObject,anObject);
DeclareOBJ(TPcodeVector,Pc);
DeclareOBJ(TStructure,Sv);
DeclareOBJ(TStructure,Av);
DeclareOBJ(TStructure,Tv);
DeclareOBJ(TStructure,Pv);
DeclareOBJ(TStructure,Cv);
DeclareOBJ(TStructure,Rv);
DeclareOBJ(TStructure,tmpEnv);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(oneTval);
DeclareTVAL(wrdValue);
DeclareTVAL(ret);
DeclareTVAL(breakList);
EndFrame


Pc = self->PcodeVector;
errorSuspected = FALSE;

/*  Get length of and pointer to end of line string data. */
if (eol.Tag == TYTEXT)
    {
    eolPtr = &eol.u.Text[0];
    eolLen = strlen(eolPtr);
    }
else
if (eol.Tag == TYCHAR)
    {
    cbuf[0] = eol.u.Char;
    cbuf[1] = 0;
    eolPtr = &cbuf[0];
    eolLen = 1;
    }
else
    {
    eolPtr = FSmartbase_ObjectPtr(gCP,gTP,&eol);
    eolLen = FSmartbase_StringLen(gCP,gTP,&eol);
    }


if ((*vecIndex) < Pc->itsMaxItemIndex)
    {
    if(inputTval == NULL)
        {
        /*  Allow one line debugging output */
        
        dumpTval = oneTval;
        }
    else
        dumpTval = inputTval;

    if(self->ArgumentVariables != NIL) 
		Av = self->ArgumentVariables;
    if(self->ClassVariables != NIL)
        Sv = self->ClassVariables;
    if(self->TemporaryVariables != NIL)
        Tv = self->TemporaryVariables;
    if(self->PersistantVariables != NIL)
        Pv = self->PersistantVariables;
    if(self->ConstantVariables != NIL)
        Cv = self->ConstantVariables;
    if(self->RegisterVariables != NIL)
        Rv = self->RegisterVariables;

    /*  Separate the opcode into the six modifiers. */
    
    ExtendedOpcode.Opcode = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
    
    ResumeAfterBreakPoint:
    modifier[0] = ExtendedOpcode.u.Am1;
    modifier[1] = ExtendedOpcode.u.Am2;
    modifier[2] = ExtendedOpcode.u.Am3;
    
    /*  Get the string representation for the instruction opcode. */
    
    FDebug_INSToString(gCP, gTP, ExtendedOpcode.u.Pcode, (LpCHAR)bufIns);
    
	if (ExtendedOpcode.u.Pcode < VMSTARTREGISTERINS)
		{
		/* Manager the vmdebugger instruction (if necessary). */
		if (ExtendedOpcode.u.Pcode == VMDEBUGGER)
			{
			*breakList = FSmartbase_Ref(gCP,gTP,2,self->Interfaces,TOBJ(gCP->TLambda_BreakList));
			ExitOnError(*breakList);
			if (breakList->Tag != TYDIRECTORY) goto BadCleanUp;
			*wrdValue = TINT((*vecIndex)-1);
			*ret = FSmartbase_Ref(gCP,gTP,2,*breakList,*wrdValue);
			ExitOnError(*ret);
			if (ret->Tag != TYNUM) goto BadCleanUp;
			BreakListOpcode.Opcode = ret->u.Int;
			ExtendedOpcode.Opcode = BreakListOpcode.Opcode;
			goto ResumeAfterBreakPoint;
			}
			
		/* Convert the location code to the appropirate register: Pv, Tv, Av, etc */
		FDebug_AMToLocString(gCP, gTP, modifier[0], (LpCHAR)loc[0]);
		FDebug_AMToLocString(gCP, gTP, modifier[1], (LpCHAR)loc[1]);
		FDebug_AMToLocString(gCP, gTP, modifier[2], (LpCHAR)loc[2]);

		/* Start each line with the instruction location followed by the symbolic instruction name. */
		sprintf((char*)buf, "%6.6ld: %s",(*vecIndex)-1, bufIns);
		*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
		if(isERROR(ret)) goto BadCleanUp;

		/*  Loop through the modifier patterns */
    
		for (modIndex = 0; modIndex < 3; ++modIndex)
			{
			switch (modifier[modIndex])
				{
				case AMVOID:
					break;
                
				case AMAVOFFSET:
					tmpEnv = Av;
					symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					symNdx = symNdx/TVALARRAYITEMSIZE;
					goto DisplayBoundSymbol;
				case AMSVOFFSET:
					tmpEnv = Sv;
					symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					symNdx = symNdx/TVALARRAYITEMSIZE;
					goto DisplayBoundSymbol;
				case AMTVOFFSET:
					tmpEnv = Tv;
					symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					symNdx = symNdx/TVALARRAYITEMSIZE;
					goto DisplayBoundSymbol;
				case AMPVOFFSET:
					tmpEnv = Pv;
					symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					symNdx = symNdx/BINDARRAYITEMSIZE;
					goto DisplayBoundSymbol;
				case AMCVOFFSET:
					tmpEnv = Cv;
					symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					symNdx = symNdx/BINDARRAYITEMSIZE;
            
				DisplayBoundSymbol:
            
					if(tmpEnv == NULL)
						goto SymbolError;
					if(symNdx < tmpEnv->itsMaxItemIndex)
						{
						if (((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itNeedsBars)
							sprintf((char*)buf, " |%s|[%-s] ", loc[modIndex], &atHMChar(((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						else
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " %s[%-s] ", loc[modIndex], &atHMChar(((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(tmpEnv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars:
							sprintf((char*)buf, " %s[%-s:\"%s\"] ", loc[modIndex], &atHMChar(((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						}
					else
						{
						/*  Dump an error diagnostic in the stream */
                    
						SymbolError:
                    
						sprintf((char*)buf, "!%s["INTFORMAT"]! ",bufMod[modifier[modIndex]], symNdx  );
						*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR) buf);
						if(isERROR(ret)) goto BadCleanUp;
						errorSuspected = TRUE;
						}
					break;
                
				case AMREGISTER:
					tmpEnv = Rv;
					symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					symNdx = symNdx/TVALARRAYITEMSIZE;
					if(!tmpEnv) goto SymbolError;
					if(symNdx < tmpEnv->itsMaxItemIndex)
						{
						if (((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itNeedsBars)
							sprintf((char*)buf, " |%s| ", &atHMChar(((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						else
							{
							sprintf((char*)buf, " %s ", &atHMChar(((TSymbol*)(atHMBind(tmpEnv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}

						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						}
					else
						{
						goto SymbolError;
						}
					break;


				case AMINTEGER:
					if ((*vecIndex) < Pc->itsMaxItemIndex)
						{
						if(atHMInt(Pc->itsInstructionArray,(*vecIndex)) == -1)
							{
							if  ( (modIndex == 1 &&
								(
								ExtendedOpcode.u.Pcode == vmnatJmpLTInteger ||
								ExtendedOpcode.u.Pcode == vmnatJmpLEInteger ||
								ExtendedOpcode.u.Pcode == vmnatJmpEQInteger ||
								ExtendedOpcode.u.Pcode == vmnatJmpNEInteger ||
								ExtendedOpcode.u.Pcode == vmnatJmpGEInteger ||
								ExtendedOpcode.u.Pcode == vmnatJmpGTInteger ||
								ExtendedOpcode.u.Pcode == vmnatJmpLTNumber ||
								ExtendedOpcode.u.Pcode == vmnatJmpLENumber ||
								ExtendedOpcode.u.Pcode == vmnatJmpEQNumber ||
								ExtendedOpcode.u.Pcode == vmnatJmpNENumber ||
								ExtendedOpcode.u.Pcode == vmnatJmpGENumber ||
								ExtendedOpcode.u.Pcode == vmnatJmpGTNumber ||
								ExtendedOpcode.u.Pcode == VMJMPLT ||
								ExtendedOpcode.u.Pcode == VMJMPLE ||
								ExtendedOpcode.u.Pcode == VMJMPEQ ||
								ExtendedOpcode.u.Pcode == VMJMPNE ||
								ExtendedOpcode.u.Pcode == VMJMPGE ||
								ExtendedOpcode.u.Pcode == VMJMPGT
								)
								) ||
								(modIndex == 2 && ExtendedOpcode.u.Pcode == VMJUMP))
								{
								/*  Insert indicator in disassembly that jump was never resolved */
                            
								sprintf((char*)buf, "!"INTFORMAT"! ", atHMInt(Pc->itsInstructionArray,(*vecIndex)));
								errorSuspected = TRUE;
								}
							else
								{
								sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)));
								}
							}
						else
							{
							sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)));
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR) buf);
						if(isERROR(ret)) goto BadCleanUp;
						(*vecIndex)++;
						}
					break;
                
				case AMGVOFFSET:
					if ((*vecIndex) < Pc->itsMaxItemIndex)
						{
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
                    
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
                    
						/* Place the global variable in the buffer */
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						if (wrdValue->Tag == TYSYMBOL)
							{
							aSymbol = asSymbol(wrdValue);
							if (aSymbol->itNeedsBars == TRUE)
								sprintf((char*)buf, " Gv[|%s|]", glabel);
							else
								sprintf((char*)buf, " Gv[%s]", glabel);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						}
					break;
                                
				case AMR07OFFST:
				case AMR08OFFST:
				case AMR09OFFST:
				case AMR10OFFST:
				case AMR11OFFST:
				case AMR12OFFST:
				case AMR13OFFST:
				case AMR14OFFST:
				case AMR15OFFST:
				case AMR16OFFST:
				case AMR17OFFST:
				case AMR18OFFST:
				case AMR19OFFST:
				case AMR20OFFST:
				case AMR21OFFST:
				case AMR22OFFST:
				case AMR23OFFST:
				case AMR24OFFST:
				case AMR25OFFST:
				case AMR26OFFST:
				case AMR27OFFST:
				case AMR28OFFST:
				case AMR29OFFST:
				case AMR30OFFST:
				case AMR31OFFST:
				case AMR32OFFST:
				case AMR33OFFST:
				case AMR34OFFST:
				case AMR35OFFST:
				case AMR36OFFST:
				case AMR37OFFST:
				case AMR38OFFST:
				case AMR39OFFST:
				case AMR40OFFST:
				case AMR41OFFST:
				case AMR42OFFST:
				case AMR43OFFST:
				case AMR44OFFST:
				case AMR45OFFST:
				case AMR46OFFST:
				case AMR47OFFST:
				case AMR48OFFST:
				case AMR49OFFST:
					symNdx = modifier[modIndex];
					offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
					sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
					*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
					if(isERROR(ret)) goto BadCleanUp;
					break;
                                

				default:
					sprintf((char*)buf, "!["INTFORMAT"]: "INTFORMAT"!", (NUM)modIndex, (NUM)modifier[modIndex]);
					*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);  
					errorSuspected = TRUE;
					if(asTag(ret) == TYERROR) 
						{
						goto BadCleanUp;
						}
					break;
				}       
			}

		/* Add the trailing right paren to the instruction. */
		sprintf((char*)buf, ")");
		*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);  

		if(toplineDisplay != 0 && asTag(dumpTval) == TYSTRING)
			{
			/*  Show only one line at a time, at the top of the console window */
        
			if( errorSuspected == TRUE && inputTval != NULL)
				{
				goto BadCleanUp;
				}
			}
		else
			{
			*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)eolPtr);
			if(asTag(ret) == TYERROR) 
				{
				goto BadCleanUp;
				}
			}
		}
	else
	if (ExtendedOpcode.u.Pcode >= VMSTARTREGISTERINS)
		{
		/*  We display the modifiers if we are not in short format. */
		sprintf((char*)buf, "%6.6ld: %s",(*vecIndex)-1, bufIns);
    
		*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
		if(isERROR(ret)) goto BadCleanUp;

		switch (ExtendedOpcode.u.Pcode)
			{
			/* Register opcodes with no arguments. */
			case vmvecLoop:
				break;

			/* Register opcodes with one register argument. */
			case vmvecPopNumber:
			case vmvecPushNumber:
			case vmregJump:
				/*  Convert the register modifier to a register displacement. */
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with two register arguments. */
			case vmregAddInteger:
			case vmregAddNumber:
			case vmregAndInteger:
 			case vmregDivInteger:
			case vmregDivNumber:
			case vmregDivrInteger:
			case vmregDivrNumber:
			case vmregMoveInteger:
			case vmregMoveNumber:
			case vmregMulInteger:
			case vmregMulNumber:
			case vmregOrInteger:
			case vmregRefCharacter:
			case vmregRefFloat:
			case vmregRefInteger:
			case vmregRefNumber:
			case vmregRefShort:
			case vmregRefLong:
			case vmregSetCharacter:
			case vmregSetFloat:
			case vmregSetInteger:
			case vmregSetNumber:
			case vmregSetShort:
			case vmregSetLong:
			case vmregSubInteger:
			case vmregSubNumber:
			case vmregXorInteger:
			case vmregAbsNumber:
			case vmregNumber:
			case vmregInteger:
			case vmregCosNumber:
			case vmregSinNumber:
			case vmregSqrtNumber:
			case vmregTanNumber:
				/*  Convert the two register modifiers to a register displacements. */
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with three register arguments. */
			case vmregLogNumber:
			case vmregPwrNumber:
			case vmregRefXCharacter:
			case vmregRefXFloat:
			case vmregRefXInteger:
			case vmregRefXLong:
			case vmregRefXNumber:
			case vmregRefXShort:
			case vmregSetXCharacter:
			case vmregSetXFloat:
			case vmregSetXLong:
			case vmregSetXInteger:
			case vmregSetXNumber:
			case vmregSetXShort:
			case vmregShlInteger:
			case vmregShrInteger:
			case vmvecSetIncrements:
			case vmvecSetPointers:
				/*  Convert the three register modifiers to a register displacements. */
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[2]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register jmpcc opcodes with two register arguments and a label displacement. */
			case vmregJmpEQInteger:
			case vmregJmpLTInteger:
			case vmregJmpGTInteger:
			case vmregJmpNEInteger:
			case vmregJmpGEInteger:
			case vmregJmpLEInteger:
			case vmregJmpEQNumber:
			case vmregJmpLTNumber:
			case vmregJmpGTNumber:
			case vmregJmpNENumber:
			case vmregJmpGENumber:
			case vmregJmpLENumber:
				/*  Convert the two register modifiers to register displacements. */
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register jmpcc opcodes with one register argument one immediate argument and a label displacement. */
			case vmregJmpEQImmediate:
			case vmregJmpLTImmediate:
			case vmregJmpGTImmediate:
			case vmregJmpNEImmediate:
			case vmregJmpGEImmediate:
			case vmregJmpLEImmediate:
				/*  Convert the two register modifiers to register displacements. */
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with one immediate and one register argument. */
			case vmregAddImmediate:
			case vmregAndImmediate:
			case vmregDivImmediate:
			case vmregLoadJmpPointer:
			case vmregMoveImmediate:
			case vmregDivrImmediate:
			case vmregMulImmediate:
			case vmregOrImmediate:
			case vmregSetCharImmediate:
			case vmregSetIntImmediate:
			case vmregSetLongImmediate:
			case vmregSetShortImmediate:
			case vmregShlImmediate:
			case vmregShrImmediate:
			case vmregSubImmediate:
			case vmregXorImmediate:
			case vmvecNumVector:
			case vmvecInitialize:
				/*  Convert the register modifier to a register displacement. */
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with two immediate and one register argument. */
			case vmregIncPointer:
				/*  Convert the register modifier to a register displacement. */
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[2]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with one immediate and two register arguments. */
			case vmregAddPointer:
			case vmregSubPointer:
			case vmregSetXCharImmediate:
			case vmregSetXIntImmediate:
			case vmregSetXLongImmediate:
			case vmregSetXShortImmediate:
			case vmvecNumScalar:
				/*  Convert the register modifier to a register displacement. */
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[2]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with two immediate arguments. */
			case vmvecPop:
			case vmvecPush:
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with one immediate argument. */
			case vmregRunInHardware:
			case vmvecBinary:
			case vmvecSwapCC:
			case vmvecUnary:
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break;

			/* Register opcodes with one memory and one register argument. */
			case vmregLoadAddress:
			case vmregLoadInteger:
			case vmregLoadTail:
			case vmregLoadDeclType:
			case vmregLoadType:
			case vmregLoadNumber:
			case vmregObjPointer:
			case vmregObjLength:
			case vmregSetWord:
				/*  Convert the register modifier to a register displacement. */
				switch (modifier[0])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars3;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars3:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[0];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
				sprintf((char*)buf," %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break; 

			/* Register opcodes with one immediate and one memory argument. */
			case vmregSaveTailImmediate:
			case vmregSaveDeclTypeImmediate:
				sprintf((char*)buf, " "INTFORMAT" ", atHMInt(Pc->itsInstructionArray,(*vecIndex)++));
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				switch (modifier[1])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars4;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars4:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[1];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
				break;

			/* Register opcodes with one register and one memory argument. */
			case vmregRefWord:
			case vmregSaveInteger:
			case vmregSaveTail:
			case vmregSaveDeclType:
			case vmregSaveNumber:
				sprintf((char*)buf, " %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				switch (modifier[1])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars5;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars5:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[1];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
				break;

			/* Register opcodes with two register and one memory argument. */
			case vmregRefXWord:
				sprintf((char*)buf," %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[0]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf," %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				/*  Convert the regoffset modifier to a register displacement. */
				switch (modifier[2])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars8;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars8:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[0];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
			break;
			/* Register opcodes with one memory and two register arguments. */
			case vmregSetXWord:
				/*  Convert the regoffset modifier to a register displacement. */
				switch (modifier[0])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars9;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars9:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[0];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
				sprintf((char*)buf," %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[1]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				sprintf((char*)buf," %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[2]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
			break;

			/* Register opcodes with two memory and one register argument. */
			case vmregStringCompare:
			case vmregStringiCompare:
				/*  Convert the regoffset modifier to a register displacement. */
				switch (modifier[0])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars6;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars6:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[0];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
				/*  Convert the regoffset modifier to a register displacement. */
				switch (modifier[1])
					{
					case AMSVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Sv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMAVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Av[%s] ", &atHMChar(((TSymbol*)(atHMBind(Av->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMTVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/TVALARRAYITEMSIZE;
						sprintf((char*)buf, " Tv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Tv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMPVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						sprintf((char*)buf, " Pv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Pv->itsDictionaryArray,symNdx).Key))->itsCString,0));
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;
					case AMCVOFFSET:
						symNdx = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						symNdx = symNdx/BINDARRAYITEMSIZE;
						if (strncmp(&atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),"__C",3) != 0)
							{
							/* Display a normal bound variable as location & variable name i.e. vars:(x) */
							sprintf((char*)buf, " Cv[%s] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0));
							}
						else
							{
							/* Display a constant variable as location, variable name & constant i.e. cvars:(__C90:"Hello") */
							*wrdValue = atHMBind(Cv->itsDictionaryArray,symNdx).Value;
							TObject_CnvToText(gCP,gTP,(LpCHAR)tmp, 12, *wrdValue);
							for (charIndex = 0; charIndex < 12; ++charIndex)
								{
								if (tmp[charIndex] == 0) goto NoMoreChars7;
								if (tmp[charIndex] == '"') tmp[charIndex] = '.';
								if (tmp[charIndex] == '\\') tmp[charIndex] = '.';
								if (tmp[charIndex] < ' ') tmp[charIndex] = '.';
								}
							NoMoreChars7:
							sprintf((char*)buf, " Cv[%-s:\"%s\"] ", &atHMChar(((TSymbol*)(atHMBind(Cv->itsDictionaryArray,symNdx).Key))->itsCString,0),tmp);
							}
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;                
					case AMGVOFFSET:
						anObject = *(TObject**)&atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						asTag(wrdValue) = anObject->itsObjectType;
						asObject(wrdValue) = anObject;
						TObject_CnvToText(gCP,gTP,(LpCHAR)glabel, sizeof(buf) - 1, *wrdValue);
						sprintf((char*)buf, " Gv[%s] ", (LpCHAR)glabel);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);
						if(isERROR(ret)) goto BadCleanUp;
						break;

					case AMR07OFFST:
					case AMR08OFFST:
					case AMR09OFFST:
					case AMR10OFFST:
					case AMR11OFFST:
					case AMR12OFFST:
					case AMR13OFFST:
					case AMR14OFFST:
					case AMR15OFFST:
					case AMR16OFFST:
					case AMR17OFFST:
					case AMR18OFFST:
					case AMR19OFFST:
					case AMR20OFFST:
					case AMR21OFFST:
					case AMR22OFFST:
					case AMR23OFFST:
					case AMR24OFFST:
					case AMR25OFFST:
					case AMR26OFFST:
					case AMR27OFFST:
					case AMR28OFFST:
					case AMR29OFFST:
					case AMR30OFFST:
					case AMR31OFFST:
					case AMR32OFFST:
					case AMR33OFFST:
					case AMR34OFFST:
					case AMR35OFFST:
					case AMR36OFFST:
					case AMR37OFFST:
					case AMR38OFFST:
					case AMR39OFFST:
					case AMR40OFFST:
					case AMR41OFFST:
					case AMR42OFFST:
					case AMR43OFFST:
					case AMR44OFFST:
					case AMR45OFFST:
					case AMR46OFFST:
					case AMR47OFFST:
					case AMR48OFFST:
					case AMR49OFFST:
						symNdx = modifier[1];
						offset = atHMInt(Pc->itsInstructionArray,(*vecIndex)++);
						sprintf((char*)buf, " %s["INTFORMAT"] ", &atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,symNdx).Key))->itsCString,0),offset);
						*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);

						if(isERROR(ret)) goto BadCleanUp;
						break;
					}       
				sprintf((char*)buf," %s ",&atHMChar(((TSymbol*)(atHMBind(Rv->itsDictionaryArray,modifier[2]).Key))->itsCString,0));   
				*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)buf);
				if(isERROR(ret)) goto BadCleanUp;
				break; 

			default:
				goto BadCleanUp;
				break;
			}

		/* Add the trailing right paren to the instruction. */
		sprintf((char*)buf, ")");
		*ret = FDebug_DumpAdd(gCP,gTP,1,dumpTval,(LpCHAR)buf);  

		if(toplineDisplay && asTag(dumpTval) == TYSTRING)
			{
			/*  Show only one line at a time, at the top of the console window */
        
			if( errorSuspected == TRUE && inputTval != NULL)
				{
				goto BadCleanUp;
				}
			}
		else
			{
			*ret = FDebug_DumpAdd(gCP,gTP,0,dumpTval,(LpCHAR)eolPtr);
			if(asTag(ret) == TYERROR) 
				{
				goto BadCleanUp;
				}
			}

		}
	}
else
	goto BadCleanUp;
    
if(errorSuspected)
    {
    if(inputTval != NULL)
        {
        /*  If we suspect an error and we were not called from the virtual machine then we */
        /*  we return an error at this point. */
        
        goto BadCleanUp;
        }
    }

FrameExit(*dumpTval);

BadCleanUp:
*ret = TERROR("!Disassemble instruction!");
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_AMToString

Convert a modifier to text string

Note:

#endif

TVAL FDebug_AMToString(LpXCONTEXT gCP,LpTHREAD gTP, NUM mod,  LpCHAR result)
{

switch (mod)
    {
    case AMVOID:        sprintf((char*)result, gTP->FDebug_fmtStr77, "amvoid");break;
    case AMSVOFFSET:    sprintf((char*)result, gTP->FDebug_fmtStr77, "amsfoffset");break;
    case AMAVOFFSET:    sprintf((char*)result, gTP->FDebug_fmtStr77, "amsboffset");break;
    case AMTVOFFSET:    sprintf((char*)result, gTP->FDebug_fmtStr77, "amfboffset");break;
    case AMPVOFFSET:    sprintf((char*)result, gTP->FDebug_fmtStr77, "ampvoffset");break;
    case AMCVOFFSET:    sprintf((char*)result, gTP->FDebug_fmtStr77, "amcvoffset");break;
    case AMREGISTER:    sprintf((char*)result, gTP->FDebug_fmtStr77, "amregister");break;
    case AMINTEGER:     sprintf((char*)result, gTP->FDebug_fmtStr77, "aminteger");break;
    case AMGVOFFSET:    sprintf((char*)result, gTP->FDebug_fmtStr77, "amgvoffset");break;
    default:            sprintf((char*)result, "AM : %-8.8ld", mod);break; /* gpc */
    }
return gCP->TObject_TRUE;
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_AMToLocString

Convert a modifier to text string describing the location of the variable


#endif

TVAL FDebug_AMToLocString(LpXCONTEXT gCP,LpTHREAD gTP, NUM mod,  LpCHAR result)
{

gTP = gTP; // NOOP to hide unused parameter warning message
switch (mod)
    {
    case AMAVOFFSET:    sprintf((char*)result, "%s", "Av");break;
    case AMSVOFFSET:    sprintf((char*)result, "%s", "Sv");break;
    case AMTVOFFSET:    sprintf((char*)result, "%s", "Tv");break;
    case AMPVOFFSET:    sprintf((char*)result, "%s", "Pv");break;
    case AMREGISTER:    sprintf((char*)result, "%s", "Rv");break;
    case AMCVOFFSET:    sprintf((char*)result, "%s", "Cv");break;
    case AMGVOFFSET:    sprintf((char*)result, "%s", "");break;
    default:            sprintf((char*)result, "R%-2.2ld", mod);break;
    }
return gCP->TObject_TRUE;
}
/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_INSToString

Convert an instruction to text string

Note:

#endif

TVAL FDebug_INSToString(LpXCONTEXT gCP,LpTHREAD gTP, NUM ins,  LpCHAR result)
{
gTP = gTP; // NOOP to hide unused parameter warning message
switch (ins)
    {
    case VMRETURN:						sprintf((char*)result, "(%s  ", "vmreturn");break;
    case VMADD:							sprintf((char*)result, "(%s  ", "vmadd");break;
    case VMADDI:						sprintf((char*)result, "(%s  ", "vmaddi");break;
    case VMAND:							sprintf((char*)result, "(%s  ", "vmand");break;
    case VMARGCOUNT:					sprintf((char*)result, "(%s  ", "vmargcount");break;
    case VMARGFETCH:					sprintf((char*)result, "(%s  ", "vmargfetch");break;
    case VMAPPLY:						sprintf((char*)result, "(%s  ", "vmapply");break;
    case VMCALL:						sprintf((char*)result, "(%s  ", "vmcall");break;
    case VMDIV:							sprintf((char*)result, "(%s  ", "vmdiv");break;
    case VMDIVI:						sprintf((char*)result, "(%s  ", "vmdivi");break;
    case VMDIVR:						sprintf((char*)result, "(%s  ", "vmdivr");break;
    case VMDIVRI:						sprintf((char*)result, "(%s  ", "vmdivri");break;
    case VMIADD:						sprintf((char*)result, "(%s  ", "vmiadd");break;
    case VMIDIV:						sprintf((char*)result, "(%s  ", "vmidiv");break;
    case VMIDIVR:						sprintf((char*)result, "(%s  ", "vmidivr");break;
    case VMIMUL:						sprintf((char*)result, "(%s  ", "vmimul");break;
    case VMISUB:						sprintf((char*)result, "(%s  ", "vmisub");break;
    case VMJMPLT:						sprintf((char*)result, "(%s  ", "vmjmplt");break;
    case VMJMPLE:						sprintf((char*)result, "(%s  ", "vmjmple");break;
    case VMJMPEQ:						sprintf((char*)result, "(%s  ", "vmjmpeq");break;
    case VMJMPNE:						sprintf((char*)result, "(%s  ", "vmjmpne");break;
    case VMJMPGE:						sprintf((char*)result, "(%s  ", "vmjmpge");break;
    case VMJMPGT:						sprintf((char*)result, "(%s  ", "vmjmpgt");break;
    case VMJUMP:						sprintf((char*)result, "(%s  ", "vmjump");break;
    case VMMOVE:						sprintf((char*)result, "(%s  ", "vmmove");break;
    case VMMUL:							sprintf((char*)result, "(%s  ", "vmmul");break;
    case VMMULI:						sprintf((char*)result, "(%s  ", "vmmuli");break;
    case VMNADD:						sprintf((char*)result, "(%s  ", "vmnadd");break;
    case VMIAND:						sprintf((char*)result, "(%s  ", "vmiand");break;
    case VMIANDB:						sprintf((char*)result, "(%s  ", "vmiandb");break;
    case VMNDIV:						sprintf((char*)result, "(%s  ", "vmndiv");break;
    case VMNDIVR:						sprintf((char*)result, "(%s  ", "vmndivr");break;
    case VMDEBUGGER:					sprintf((char*)result, "(%s  ", "vmdebugger");break;
    case VMNMUL:						sprintf((char*)result, "(%s  ", "vmnmul");break;
    case VMONERROR:						sprintf((char*)result, "(%s  ", "vmonerror");break;
    case VMIOR:							sprintf((char*)result, "(%s  ", "vmior");break;
    case VMIORB:						sprintf((char*)result, "(%s  ", "vmiorb");break;
    case VMNSUB:						sprintf((char*)result, "(%s  ", "vmnsub");break;
    case VMIXOR:						sprintf((char*)result, "(%s  ", "vmixor");break;
    case VMIXORB:						sprintf((char*)result, "(%s  ", "vmixorb");break;
    case VMOR:							sprintf((char*)result, "(%s  ", "vmor");break;
    case VMPOP:							sprintf((char*)result, "(%s  ", "vmpop");break;
    case VMPUSH:						sprintf((char*)result, "(%s  ", "vmpush");break;
    case VMREF:							sprintf((char*)result, "(%s  ", "vmref");break;
    case VMSELF:						sprintf((char*)result, "(%s  ", "vmself");break;
    case VMSEND:						sprintf((char*)result, "(%s  ", "vmsend");break;
    case VMSET:							sprintf((char*)result, "(%s  ", "vmset");break;
    case VMSHL:							sprintf((char*)result, "(%s  ", "vmshl");break;
    case VMSHR:							sprintf((char*)result, "(%s  ", "vmshr");break;
    case VMSUB:							sprintf((char*)result, "(%s  ", "vmsub");break;
    case VMSUBI:						sprintf((char*)result, "(%s  ", "vmsubi");break;
    case VMXOR:							sprintf((char*)result, "(%s  ", "vmxor");break;

    case VMADDN:						sprintf((char*)result, "(%s  ", "vmaddn");break;
    case VMDIVN:						sprintf((char*)result, "(%s  ", "vmdivn");break;
    case VMMOVEI:						sprintf((char*)result, "(%s  ", "vmmovei");break;
    case VMMULN:						sprintf((char*)result, "(%s  ", "vmmuln");break;
    case VMSUBN:						sprintf((char*)result, "(%s  ", "vmsubn");break;

    case VMCADD:						sprintf((char*)result, "(%s  ", "vmcadd");break;
    case VMCDIV:						sprintf((char*)result, "(%s  ", "vmcdiv");break;
    case VMMOVEN:						sprintf((char*)result, "(%s  ", "vmmoven");break;
    case VMCMUL:						sprintf((char*)result, "(%s  ", "vmcmul");break;
    case VMCSUB:						sprintf((char*)result, "(%s  ", "vmcsub");break;

	case VMREFTEXT:						sprintf((char*)result, "(%s  ", "vmreftext");break;
    case VMREFSTRING:					sprintf((char*)result, "(%s  ", "vmrefstring");break;
    case VMSETSTRING:					sprintf((char*)result, "(%s  ", "vmsetstring");break;
    case VMREFSYMBOL:					sprintf((char*)result, "(%s  ", "vmrefsymbol");break;
    case VMREFVECTOR:					sprintf((char*)result, "(%s  ", "vmrefvector");break;
    case VMSETVECTOR:					sprintf((char*)result, "(%s  ", "vmsetvector");break;
    case VMREFSTRVALUE:					sprintf((char*)result, "(%s  ", "vmrefstrvalue");break;
    case VMSETSTRVALUE:					sprintf((char*)result, "(%s  ", "vmsetstrvalue");break;
    case VMREFSTRKEY:					sprintf((char*)result, "(%s  ", "vmrefstrkey");break;
    case VMSETSTRKEY:					sprintf((char*)result, "(%s  ", "vmsetstrkey");break;
    case VMREFDICVALUE:					sprintf((char*)result, "(%s  ", "vmrefdicvalue");break;
    case VMSETDICVALUE:					sprintf((char*)result, "(%s  ", "vmsetdicvalue");break;
    case VMREFDICKEY:					sprintf((char*)result, "(%s  ", "vmrefdickey");break;
    case VMSETDICKEY:					sprintf((char*)result, "(%s  ", "vmsetdickey");break;
    case VMREFDIRVALUE:					sprintf((char*)result, "(%s  ", "vmrefdirvalue");break;
    case VMSETDIRVALUE:					sprintf((char*)result, "(%s  ", "vmsetdirvalue");break;
    case VMREFDIRKEY:					sprintf((char*)result, "(%s  ", "vmrefdirkey");break;
    case VMSETDIRKEY:					sprintf((char*)result, "(%s  ", "vmsetdirkey");break;
    case VMREFBITVECTOR:				sprintf((char*)result, "(%s  ", "vmrefbitvector");break;
    case VMSETBITVECTOR:				sprintf((char*)result, "(%s  ", "vmsetbitvector");break;
    case VMREFBYTVECTOR:				sprintf((char*)result, "(%s  ", "vmrefbytevector");break;
    case VMSETBYTVECTOR:				sprintf((char*)result, "(%s  ", "vmsetbytevector");break;
    case VMREFPCDVECTOR:				sprintf((char*)result, "(%s  ", "vmrefpcdvector");break;
    case VMSETPCDVECTOR:				sprintf((char*)result, "(%s  ", "vmsetpcdvector");break;
    case VMREFOBJVECTOR:				sprintf((char*)result, "(%s  ", "vmrefobjvector");break;
    case VMSETOBJVECTOR:				sprintf((char*)result, "(%s  ", "vmsetobjvector");break;
    case VMREFINTVECTOR:				sprintf((char*)result, "(%s  ", "vmrefintvector");break;
    case VMSETINTVECTOR:				sprintf((char*)result, "(%s  ", "vmsetintvector");break;
    case VMREFNUMVECTOR:				sprintf((char*)result, "(%s  ", "vmrefnumvector");break;
    case VMSETNUMVECTOR:				sprintf((char*)result, "(%s  ", "vmsetnumvector");break;
    case VMREFFLTVECTOR:				sprintf((char*)result, "(%s  ", "vmreffltvector");break;
    case VMSETFLTVECTOR:				sprintf((char*)result, "(%s  ", "vmsetfltvector");break;
    case VMREFMATRIX:					sprintf((char*)result, "(%s  ", "vmrefmatrix");break;
    case VMSETMATRIX:					sprintf((char*)result, "(%s  ", "vmsetmatrix");break;
    case VMREFNUMMATRIX:				sprintf((char*)result, "(%s  ", "vmrefnummatrix");break;
    case VMSETNUMMATRIX:				sprintf((char*)result, "(%s  ", "vmsetnummatrix");break;
    case VMTESTESCAPE:					sprintf((char*)result, "(%s  ", "vmtestescape");break;

    case vmnatJmpLTInteger:				sprintf((char*)result, "(%s  ", "vmnatJmpLTInteger");break;
    case vmnatJmpLEInteger:				sprintf((char*)result, "(%s  ", "vmnatJmpLEInteger");break;
    case vmnatJmpEQInteger:				sprintf((char*)result, "(%s  ", "vmnatJmpEQInteger");break;
    case vmnatJmpNEInteger:				sprintf((char*)result, "(%s  ", "vmnatJmpNEInteger");break;
    case vmnatJmpGEInteger:				sprintf((char*)result, "(%s  ", "vmnatJmpGEInteger");break;
    case vmnatJmpGTInteger:				sprintf((char*)result, "(%s  ", "vmnatJmpGTInteger");break;
    case vmnatJmpLTNumber:				sprintf((char*)result, "(%s  ", "vmnatJmpLTNumber");break;
    case vmnatJmpLENumber:				sprintf((char*)result, "(%s  ", "vmnatJmpLENumber");break;
    case vmnatJmpEQNumber:				sprintf((char*)result, "(%s  ", "vmnatJmpEQNumber");break;
    case vmnatJmpNENumber:				sprintf((char*)result, "(%s  ", "vmnatJmpNENumber");break;
    case vmnatJmpGENumber:				sprintf((char*)result, "(%s  ", "vmnatJmpGENumber");break;
    case vmnatJmpGTNumber:				sprintf((char*)result, "(%s  ", "vmnatJmpGTNumber");break;
    case vmnatAddInteger:				sprintf((char*)result, "(%s  ", "vmnatAddInteger");break;
    case vmnatAddNumber:				sprintf((char*)result, "(%s  ", "vmnatAddNumber");break;
    case vmnatAndInteger:				sprintf((char*)result, "(%s  ", "vmnatAndInteger");break;
	case vmnatDivInteger:				sprintf((char*)result, "(%s  ", "vmnatDivInteger");break;
    case vmnatDivNumber:				sprintf((char*)result, "(%s  ", "vmnatDivNumber");break;
    case vmnatDivrInteger:				sprintf((char*)result, "(%s  ", "vmnatDivrInteger");break;
    case vmnatDivrNumber:				sprintf((char*)result, "(%s  ", "vmnatDivrNumber");break;
    case vmnatMulInteger:				sprintf((char*)result, "(%s  ", "vmnatMulInteger");break;
    case vmnatMulNumber:				sprintf((char*)result, "(%s  ", "vmnatMulNumber");break;
    case vmnatOrInteger:				sprintf((char*)result, "(%s  ", "vmnatOrInteger");break;
    case vmnatShlInteger:				sprintf((char*)result, "(%s  ", "vmnatShlInteger");break;
    case vmnatShrInteger:				sprintf((char*)result, "(%s  ", "vmnatShrInteger");break;
    case vmnatSubInteger:				sprintf((char*)result, "(%s  ", "vmnatSubInteger");break;
    case vmnatSubNumber:				sprintf((char*)result, "(%s  ", "vmnatSubNumber");break;
    case vmnatXorInteger:				sprintf((char*)result, "(%s  ", "vmnatXorInteger");break;

    case vmnatLoadCharacter:			sprintf((char*)result, "(%s  ", "vmnatLoadCharacter");break;
    case vmnatLoadFloat:				sprintf((char*)result, "(%s  ", "vmnatLoadFloat");break;
    case vmnatLoadInteger:				sprintf((char*)result, "(%s  ", "vmnatLoadInteger");break;
    case vmnatLoadLong:					sprintf((char*)result, "(%s  ", "vmnatLoadLong");break;
    case vmnatLoadNumber:				sprintf((char*)result, "(%s  ", "vmnatLoadNumber");break;
    case vmnatLoadObject:				sprintf((char*)result, "(%s  ", "vmnatLoadObject");break;
    case vmnatLoadShort:				sprintf((char*)result, "(%s  ", "vmnatLoadShort");break;
    case vmnatSaveCharacter:			sprintf((char*)result, "(%s  ", "vmnatSaveCharacter");break;
    case vmnatSaveFloat:				sprintf((char*)result, "(%s  ", "vmnatSaveFloat");break;
    case vmnatSaveInteger:				sprintf((char*)result, "(%s  ", "vmnatSaveInteger");break;
    case vmnatSaveLong:					sprintf((char*)result, "(%s  ", "vmnatSaveLong");break;
    case vmnatSaveNumber:				sprintf((char*)result, "(%s  ", "vmnatSaveNumber");break;
    case vmnatSaveObject:				sprintf((char*)result, "(%s  ", "vmnatSaveObject");break;
    case vmnatSaveShort:				sprintf((char*)result, "(%s  ", "vmnatSaveShort");break;

	case vmregAbsNumber:				sprintf((char*)result, "(%s  ", "vmregAbsNumber");break;
	case vmregInteger:					sprintf((char*)result, "(%s  ", "vmregInteger");break;
	case vmregNumber:					sprintf((char*)result, "(%s  ", "vmregNumber");break;
	case vmregAddImmediate:				sprintf((char*)result, "(%s  ", "vmregAddImmediate");break;
	case vmregAddInteger:				sprintf((char*)result, "(%s  ", "vmregAddInteger");break;
	case vmregAddNumber:				sprintf((char*)result, "(%s  ", "vmregAddNumber");break;
	case vmregAddPointer:				sprintf((char*)result, "(%s  ", "vmregAddPointer");break;
	case vmregAndImmediate:				sprintf((char*)result, "(%s  ", "vmregAndImmediate");break;  
	case vmregAndInteger:				sprintf((char*)result, "(%s  ", "vmregAndInteger");break;  
	case vmregCosNumber:				sprintf((char*)result, "(%s  ", "vmregCosNumber");break;  
	case vmregDivImmediate:				sprintf((char*)result, "(%s  ", "vmregDivImmediate");break;  
	case vmregDivInteger:				sprintf((char*)result, "(%s  ", "vmregDivInteger");break;  
	case vmregDivNumber:				sprintf((char*)result, "(%s  ", "vmregDivNumber");break;  
	case vmregDivrImmediate:			sprintf((char*)result, "(%s  ", "vmregDivrImmediate");break;
	case vmregDivrInteger:				sprintf((char*)result, "(%s  ", "vmregDivrInteger");break;
	case vmregDivrNumber:				sprintf((char*)result, "(%s  ", "vmregDivrNumber");break;  
	case vmregIncPointer:				sprintf((char*)result, "(%s  ", "vmregIncPointer");break;  

	case vmregJmpEQImmediate:			sprintf((char*)result, "(%s  ", "vmregJmpEQImmediate");break;		
	case vmregJmpLTImmediate:			sprintf((char*)result, "(%s  ", "vmregJmpLTImmediate");break;
	case vmregJmpGTImmediate:			sprintf((char*)result, "(%s  ", "vmregJmpGTImmediate");break;
	case vmregJmpNEImmediate:			sprintf((char*)result, "(%s  ", "vmregJmpNEImmediate");break;
	case vmregJmpGEImmediate:			sprintf((char*)result, "(%s  ", "vmregJmpGEImmediate");break;
	case vmregJmpLEImmediate:			sprintf((char*)result, "(%s  ", "vmregJmpLEImmediate");break;
 
	case vmregJmpEQInteger:				sprintf((char*)result, "(%s  ", "vmregJmpEQInteger");break;		
	case vmregJmpLTInteger:				sprintf((char*)result, "(%s  ", "vmregJmpLTInteger");break;
	case vmregJmpGTInteger:				sprintf((char*)result, "(%s  ", "vmregJmpGTInteger");break;
	case vmregJmpNEInteger:				sprintf((char*)result, "(%s  ", "vmregJmpNEInteger");break;
	case vmregJmpGEInteger:				sprintf((char*)result, "(%s  ", "vmregJmpGEInteger");break;
	case vmregJmpLEInteger:				sprintf((char*)result, "(%s  ", "vmregJmpLEInteger");break;
 
	case vmregJmpEQNumber:				sprintf((char*)result, "(%s  ", "vmregJmpEQNumber");break;		
	case vmregJmpLTNumber:				sprintf((char*)result, "(%s  ", "vmregJmpLTNumber");break;
	case vmregJmpGTNumber:				sprintf((char*)result, "(%s  ", "vmregJmpGTNumber");break;
	case vmregJmpNENumber:				sprintf((char*)result, "(%s  ", "vmregJmpNENumber");break;
	case vmregJmpGENumber:				sprintf((char*)result, "(%s  ", "vmregJmpGENumber");break;
	case vmregJmpLENumber:				sprintf((char*)result, "(%s  ", "vmregJmpLENumber");break;
 
	case vmregJump:						sprintf((char*)result, "(%s  ", "vmregJump");break;  
	case vmregLoadAddress:	    		sprintf((char*)result, "(%s  ", "vmregLoadAddress");break;  
	case vmregLoadInteger:				sprintf((char*)result, "(%s  ", "vmregLoadInteger");break;  
	case vmregLoadTail:					sprintf((char*)result, "(%s  ", "vmregLoadTail");break;  
	case vmregLoadDeclType:				sprintf((char*)result, "(%s  ", "vmregLoadDeclType");break;  
	case vmregLoadType:					sprintf((char*)result, "(%s  ", "vmregLoadType");break;  
	case vmregLoadJmpPointer:			sprintf((char*)result, "(%s  ", "vmregLoadJmpPointer");break;
	case vmregLoadNumber:				sprintf((char*)result, "(%s  ", "vmregLoadNumber");break;
	case vmregLogNumber:				sprintf((char*)result, "(%s  ", "vmregLogNumber");break;
	case vmregMoveImmediate:			sprintf((char*)result, "(%s  ", "vmregMoveImmediate");break;  
	case vmregMoveInteger:				sprintf((char*)result, "(%s  ", "vmregMoveInteger");break;  
	case vmregMoveNumber:				sprintf((char*)result, "(%s  ", "vmregMoveNumber");break;  
	case vmregMulImmediate:				sprintf((char*)result, "(%s  ", "vmregMulImmediate");break;  
	case vmregMulInteger:				sprintf((char*)result, "(%s  ", "vmregMulInteger");break;  
	case vmregMulNumber:				sprintf((char*)result, "(%s  ", "vmregMulNumber");break;  
	case vmregObjLength:				sprintf((char*)result, "(%s  ", "vmregObjLength");break;
	case vmregObjPointer:				sprintf((char*)result, "(%s  ", "vmregObjPointer");break;
	case vmregOrImmediate:				sprintf((char*)result, "(%s  ", "vmregOrImmediate");break;
	case vmregOrInteger:				sprintf((char*)result, "(%s  ", "vmregOrInteger");break;
	case vmregPwrNumber:				sprintf((char*)result, "(%s  ", "vmregPwrNumber");break;
	case vmregRefCharacter:				sprintf((char*)result, "(%s  ", "vmregRefCharacter");break;
	case vmregRefFloat:					sprintf((char*)result, "(%s  ", "vmregRefFloat");break;  
	case vmregRefInteger:				sprintf((char*)result, "(%s  ", "vmregRefInteger");break;  
	case vmregRefNumber:				sprintf((char*)result, "(%s  ", "vmregRefNumber");break;  
	case vmregRefShort:					sprintf((char*)result, "(%s  ", "vmregRefShort");break;  
	case vmregRefLong:					sprintf((char*)result, "(%s  ", "vmregRefLong");break;  
	case vmregRefWord:					sprintf((char*)result, "(%s  ", "vmregRefWord");break;
	case vmregRefXCharacter:			sprintf((char*)result, "(%s  ", "vmregRefXCharacter");break;  
	case vmregRefXFloat:				sprintf((char*)result, "(%s  ", "vmregRefXFloat");break;  
	case vmregRefXInteger:				sprintf((char*)result, "(%s  ", "vmregRefXInteger");break;
	case vmregRefXNumber:				sprintf((char*)result, "(%s  ", "vmregRefXNumber");break;
	case vmregRefXShort:				sprintf((char*)result, "(%s  ", "vmregRefXShort");break;
	case vmregRefXWord:					sprintf((char*)result, "(%s  ", "vmregRefXWord");break;
	case vmregRunInHardware:			sprintf((char*)result, "(%s  ", "vmregRunInHardware");break;
	case vmregSaveInteger:				sprintf((char*)result, "(%s  ", "vmregSaveInteger");break;
	case vmregSaveTailImmediate:		sprintf((char*)result, "(%s  ", "vmregSaveTailImmediate");break;
	case vmregSaveDeclTypeImmediate:	sprintf((char*)result, "(%s  ", "vmregSaveDeclTypeImmediate");break;
	case vmregSaveTail:					sprintf((char*)result, "(%s  ", "vmregSaveTail");break;
	case vmregSaveDeclType:				sprintf((char*)result, "(%s  ", "vmregSaveDeclType");break;
	case vmregSaveNumber:				sprintf((char*)result, "(%s  ", "vmregSaveNumber");break;  
	case vmregSetCharacter:				sprintf((char*)result, "(%s  ", "vmregSetCharacter");break;  
	case vmregSetCharImmediate:			sprintf((char*)result, "(%s  ", "vmregSetCharImmediate");break;  
	case vmregSetFloat:					sprintf((char*)result, "(%s  ", "vmregSetFloat");break;  
	case vmregSetInteger:				sprintf((char*)result, "(%s  ", "vmregSetInteger");break;  
	case vmregSetIntImmediate:			sprintf((char*)result, "(%s  ", "vmregSetIntImmediate");break;  
	case vmregSetLongImmediate:			sprintf((char*)result, "(%s  ", "vmregSetLongImmediate");break;  
	case vmregSetNumber:				sprintf((char*)result, "(%s  ", "vmregSetNumber");break;  
	case vmregSetShort:					sprintf((char*)result, "(%s  ", "vmregSetShort");break;  
	case vmregSetLong:					sprintf((char*)result, "(%s  ", "vmregSetLong");break;  
	case vmregSetWord:					sprintf((char*)result, "(%s  ", "vmregSetWord");break;
	case vmregSetShortImmediate: 		sprintf((char*)result, "(%s  ", "vmregSetShortImmediate");break;  
	case vmregSetXCharImmediate: 		sprintf((char*)result, "(%s  ", "vmregSetXCharImmediate");break;
	case vmregSetXIntImmediate: 		sprintf((char*)result, "(%s  ", "vmregSetXIntImmediate");break;
	case vmregSetXLongImmediate: 		sprintf((char*)result, "(%s  ", "vmregSetXLongImmediate");break;
	case vmregSetXShortImmediate: 		sprintf((char*)result, "(%s  ", "vmregSetXShortImmediate");break;
	case vmregSetXCharacter:			sprintf((char*)result, "(%s  ", "vmregSetXCharacter");break;
	case vmregSetXFloat:				sprintf((char*)result, "(%s  ", "vmregSetXFloat");break;
	case vmregSetXInteger:				sprintf((char*)result, "(%s  ", "vmregSetXInteger");break;
	case vmregSetXNumber:				sprintf((char*)result, "(%s  ", "vmregSetXNumber");break;
	case vmregSetXShort:				sprintf((char*)result, "(%s  ", "vmregSetXShort");break;  
	case vmregSetXWord:					sprintf((char*)result, "(%s  ", "vmregSetXWord");break;
	case vmregShlImmediate:				sprintf((char*)result, "(%s  ", "vmregShlImmediate");break;  
	case vmregShlInteger:				sprintf((char*)result, "(%s  ", "vmregShlInteger");break;  
	case vmregShrImmediate:				sprintf((char*)result, "(%s  ", "vmregShrImmediate");break;  
	case vmregShrInteger:				sprintf((char*)result, "(%s  ", "vmregShrInteger");break;  
	case vmregSinNumber:				sprintf((char*)result, "(%s  ", "vmregSinNumber");break;  
	case vmregSqrtNumber:				sprintf((char*)result, "(%s  ", "vmregSqrtNumber");break;  
	case vmregStringCompare:			sprintf((char*)result, "(%s  ", "vmregStringCompare");break;  
	case vmregStringiCompare:			sprintf((char*)result, "(%s  ", "vmregStringiCompare");break;  
	case vmregSubImmediate:				sprintf((char*)result, "(%s  ", "vmregSubImmediate");break;  
	case vmregSubInteger:				sprintf((char*)result, "(%s  ", "vmregSubInteger");break;
	case vmregSubNumber:				sprintf((char*)result, "(%s  ", "vmregSubNumber");break;
	case vmregSubPointer:				sprintf((char*)result, "(%s  ", "vmregSubPointer");break;
	case vmregTanNumber:				sprintf((char*)result, "(%s  ", "vmregTanNumber");break;  
	case vmregXorImmediate:				sprintf((char*)result, "(%s  ", "vmregXorImmediate");break;  
	case vmregXorInteger:				sprintf((char*)result, "(%s  ", "vmregXorInteger");break;  

    case vmvecBinary:					sprintf((char*)result, "(%s  ", "vmvecBinary");break;
    case vmvecInitialize:				sprintf((char*)result, "(%s  ", "vmvecInitialize");break;
    case vmvecLoop:						sprintf((char*)result, "(%s  ", "vmvecLoop");break;
    case vmvecNumScalar:				sprintf((char*)result, "(%s  ", "vmvecNumScalar");break;
    case vmvecPop:						sprintf((char*)result, "(%s  ", "vmvecPop");break;
    case vmvecPopNumber:				sprintf((char*)result, "(%s  ", "vmvecPopNumber");break;
    case vmvecPush:						sprintf((char*)result, "(%s  ", "vmvecPush");break;
    case vmvecPushNumber:				sprintf((char*)result, "(%s  ", "vmvecPushNumber");break;
    case vmvecSetIncrements:			sprintf((char*)result, "(%s  ", "vmvecSetIncrements");break;
    case vmvecSetPointers:				sprintf((char*)result, "(%s  ", "vmvecSetPointers");break;
    case vmvecSwapCC:					sprintf((char*)result, "(%s  ", "vmvecSwapCC");break;
    case vmvecUnary:					sprintf((char*)result, "(%s  ", "vmvecUnary");break;

	default:							sprintf((char*)result, "INS : %-8.8ld", ins);break;
    }

return gCP->TObject_TRUE;
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_DumpAdd

We do some formatting depending on the input and then concatenate the given text to the 
given TString.

#endif

TVAL FDebug_DumpAdd(LpXCONTEXT gCP,LpTHREAD gTP, NUM format, LpTVAL input, LpCHAR addition)
{
CHAR                buf[1024];
NUM                 len;
StartFrame
DeclareOBJ(TString,aString);
DeclareTVAL(tmp);
EndFrame

len = strlen((char*)addition);
if (len > (NUM)(sizeof(buf) - 16))
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    
if (len > 1 || ISLINEBREAK(addition[0]))
    {
    if (format) sprintf((char*)buf, "%s", addition);
    else
	if (ISLINEBREAK(addition[0])) sprintf((char*)buf, "%s", addition);
    else sprintf((char*)buf, "%s ", addition);
    }
else
    strcpy((char*)buf, (const char*)addition);
    
if (input->Tag == TYVOID)
    *input = TSTRING(buf);
else
	{
    *tmp = TSTRING(buf);
    *input = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),2,*input,*tmp);
	}
    
FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_TStringCat

Concatenate the given text to the given TString.

#endif

TVAL FDebug_TStringCat(LpXCONTEXT gCP,LpTHREAD gTP, LpTVAL input, LpCHAR buf)
{
StartFrame
DeclareTVAL(tmp);
EndFrame

if (input->Tag == TYVOID)
    *input = TSTRING(buf);
else
	{
    *tmp = TSTRING(buf);
    *input = FSmartbase_Eval(gCP,gTP,TGVALUE("append"),2,*input,*tmp);
	}

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
/*
FDebug_ClearBreakPoints

Clears the specified breakpoint, or all the breakpoints in the specified Lambda,
or all breakpoints in all Lambdas. 

*/			
#endif

TVAL FDebug_ClearBreakpoints(LpXCONTEXT gCP,LpTHREAD gTP, TLambda* self,  NUM instruction)
{
NUM                 m;
NUM                 M;
NUM                 n;
NUM                 N;

StartFrame
DeclareOBJ(TLambda,proc);
DeclareTVAL(breakList);
DeclareTVAL(brkinstr);
DeclareTVAL(err);
DeclareTVAL(tmp);
DeclareTVAL(oldOpcode);
EndFrame

if ((self != NIL) && (instruction >= 0))
	{
	/* ********************************************************* */
	/* clears a breakpoint in the specified Lambda at the         */
	/* specified instruction displacement in the Pcode Vector.   */
	/* ********************************************************* */
	proc = self;
	*brkinstr = TINT(instruction);
	if (proc->Interfaces != NIL) 
		{
		*breakList = FSmartbase_Ref(gCP,gTP,2,TOBJ(proc->Interfaces),TOBJ(gCP->TLambda_BreakList));
		if (breakList->Tag == TYDIRECTORY)
			{
			*oldOpcode = FSmartbase_Ref(gCP,gTP,2,*breakList,*brkinstr);
			if (oldOpcode->Tag == TYNUM)
				{
				*err = FSmartbase_Set(gCP,gTP,3,*breakList,TINT(instruction),gCP->Tval_VOID);
				*tmp = FSmartbase_Set(gCP,gTP,3,TOBJ(proc->PcodeVector),*brkinstr,*oldOpcode);
				} 
			}
		}
	}
else
if ((self != NIL) && (instruction < 0))
	{
	/* ********************************************************* */
	/* clears all breakpoints in the specified Lambda.            */
	/* ********************************************************* */
	proc = self;
	if (proc->Interfaces != NIL) 
		{
		*breakList = FSmartbase_Ref(gCP,gTP,2,TOBJ(proc->Interfaces),TOBJ(gCP->TLambda_BreakList));
		if (breakList->Tag == TYDIRECTORY)
			{
			N = breakList->u.Directory->itsMaxItemIndex;
			for (n = 0; n < N; ++n)
				{
				*brkinstr = FSmartbase_Ref(gCP,gTP,3,*breakList,TINT(n),TINT(0));
				if (brkinstr->Tag == TYNUM)
					{
					*oldOpcode = FSmartbase_Ref(gCP,gTP,2,*breakList,*brkinstr);
					if (oldOpcode->Tag == TYNUM)
						{
						*tmp = FSmartbase_Set(gCP,gTP,3,proc->PcodeVector,*brkinstr,*oldOpcode);
						}
					}
				} 
			*breakList = FSmartbase_Set(gCP,gTP,3,proc->Interfaces,TOBJ(gCP->TLambda_BreakList),gCP->Tval_VOID);
			}
		}
	}
else
if (self == NULL)
	{
	/* ********************************************************* */
	/* clears all breakpoints in all Lambdas active in the system */
	/* ********************************************************* */

    M = gCP->TObject_MaxObjectCount;
	for (m = 0; m < M; ++m)
		{
		if ((_TObject_ObjectFlag(m) != _TObject_OfVOID) && ((_TObject_ObjectByIndex(m))->itsObjectType == TYLAMBDA))
			{
			proc = (TLambda*)_TObject_ObjectByIndex(m);
			if (proc->Interfaces != NIL) 
				{
				*breakList = FSmartbase_Ref(gCP,gTP,2,TOBJ(proc->Interfaces),TOBJ(gCP->TLambda_BreakList));
				if (breakList->Tag == TYDIRECTORY)
					{
					N = breakList->u.Directory->itsMaxItemIndex;
					for (n = 0; n < N; ++n)
						{
						*brkinstr = FSmartbase_Ref(gCP,gTP,3,*breakList,TINT(n),TINT(0));
						if (brkinstr->Tag == TYNUM)
							{
							*oldOpcode = FSmartbase_Ref(gCP,gTP,2,*breakList,*brkinstr);
							if (oldOpcode->Tag == TYNUM)
								{
								*tmp = FSmartbase_Set(gCP,gTP,3,TOBJ(proc->PcodeVector),*brkinstr,*oldOpcode);
								}
							}
						} 
					*breakList = FSmartbase_Set(gCP,gTP,3,proc->Interfaces,TOBJ(gCP->TLambda_BreakList),gCP->Tval_VOID);
					}
				}
			}
		}
	}

FrameExit(gCP->TObject_TRUE);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_FormatText

Strips excess trailing white space (incl tab and extra return).

#endif

TVAL    FDebug_FormatText(LpXCONTEXT gCP,LpTHREAD gTP, CHAR* theText)
{
NUM                 cn;
BOLE                needsReturn;
TVAL                eol = TGVALUE("_eol");
LpCHAR              eolPtr;
CHAR				cbuf[2];
NUM                 eolLen;

/*  Get length of and pointer to end of line string data. */
if (eol.Tag == TYTEXT)
    {
    eolPtr = &eol.u.Text[0];
    eolLen = strlen(eolPtr);
    }
else
if (eol.Tag == TYCHAR)
    {
    cbuf[0] = eol.u.Char;
    cbuf[1] = 0;
    eolPtr = &cbuf[0];
    eolLen = 1;
    }
else
    {
    eolPtr = FSmartbase_ObjectPtr(gCP,gTP,&eol);
    eolLen = FSmartbase_StringLen(gCP,gTP,&eol);
    }

needsReturn = FALSE;

for(cn = strlen((char*)theText)-1; cn >= 0; cn--)
    {
    /*  Strip excess trailing white space (incl tab and return) */
    
    if(theText[cn] < 32)
        {
        if (ISLINEBREAK(theText[cn]))
            {
            needsReturn = TRUE;
            }
            
        theText[cn] = 0;
        }
    }

if(needsReturn)
    strcat((char*)theText, (char*)eolPtr);

return(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_browsableProcs

Formats a TVAL containing a TString with a text listing of all symbols for TYLAMBDAS 
whose itsGlobalLock == FALSE. And also include all Scripts registered to the _currentViews.

#endif

TVAL    FDebug_browsableProcs(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 cn;
CHAR                buf[512];
TVAL				eol = TGVALUE("_eol");
LpCHAR				eolPtr;
NUM					eolLen;
CHAR				cbuf[2];
BOLE				showLocked = FALSE;
StartFrame
DeclareOBJ(TWorkspace,workspace);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TObjVector,anObjVec);
DeclareTVAL(result);
EndFrame

/*  Show locked as well as unlocked globally bound Lambdas? */
if ((argc >= 1) && (argv[0].Tag == TYBOLE) && (argv[0].u.Bool == TRUE)) showLocked = TRUE;

/*  Get length of and pointer to end of line string data. */
if (eol.Tag == TYTEXT)
    {
    eolPtr = &eol.u.Text[0];
    eolLen = strlen(eolPtr);
    }
else
if (eol.Tag == TYCHAR)
    {
    cbuf[0] = eol.u.Char;
    cbuf[1] = 0;
    eolPtr = &cbuf[0];
    eolLen = 1;
    }
else
    {
    eolPtr = FSmartbase_ObjectPtr(gCP,gTP,&eol);
    eolLen = FSmartbase_StringLen(gCP,gTP,&eol);
    }

anObjVec = TObjVector_New(gCP,gTP);

for (cn = 0;cn < gCP->TObject_MaxObjectCount; ++cn)
    {
    if ((_TObject_ObjectFlag(cn) != _TObject_OfVOID) &&
        ((_TObject_ObjectByIndex(cn))->itsObjectType == TYSYMBOL))
        {
        aSymbol = (TSymbol*)_TObject_ObjectByIndex(cn);
        if (showLocked)
			{
			if (aSymbol->itsGlobalValue.Tag == TYLAMBDA)
				{
				TSymbol_AddSortedValue(gCP,gTP,anObjVec,aSymbol);
				}
            }
        else
			{
			if ((aSymbol->itsGlobalLock == FALSE) && (aSymbol->itsGlobalValue.Tag == TYLAMBDA))
				{
				TSymbol_AddSortedValue(gCP,gTP,anObjVec,aSymbol);
				}
			}
        }
    }

*result = gCP->Tval_VOID;
for (cn = 0;cn < anObjVec->itsMaxItemIndex; ++cn)
    {
    aSymbol = (TSymbol*)atHMObject(anObjVec->itsObjectArray,cn);
    FConio_sprintf(gCP, gTP,(LpCHAR)buf,(LpCHAR)"%y = %a",(LpCHAR)&aSymbol,&aSymbol->itsGlobalValue);
    FDebug_TStringCat(gCP, gTP, result, buf);
    if(cn < anObjVec->itsMaxItemIndex - 1)
        FDebug_TStringCat(gCP, gTP, result, (LpCHAR)eolPtr);
    }

FrameExit(*result);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_GetSymbolTable

Constructs a TObjVector containing the current bindings for global symbols. 
Takes three flags which indicate whether to sort, include locked symbols and/or cprocs.

Note:   Symbols, whose values are #void, are never included.

(getSymbolTable sort [showCProcs showLocked] )

#endif

TVAL    FDebug_GetSymbolTable(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 cn;
NUM                 doSort;
NUM                 showCProcs;
NUM                 showLocked;
StartFrame
DeclareOBJ(TObjVector,resultSpace);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
DeclareTVAL(tmp);
EndFrame

if(argc > 3)
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }
else
for(cn = 0; cn < argc; cn++)
	{
    if (argv[cn].Tag == TYBOLE)
        {
        argv[cn].Tag = TYNUM;
		argv[cn].u.Int = (argv[cn].u.Bool == TRUE) ? 1 : 0;
        }
	else
    if (argv[cn].Tag == TYVOID)
        {
        argv[cn].Tag = TYNUM;
		argv[cn].u.Int = 0;
        }
	else
    if (argv[cn].Tag != TYNUM)
        {
        FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
        }
	}
doSort = (argc >= 1) && asInt(&argv[0]) != 0;
showCProcs = (argc >= 2) && asInt(&argv[1]) != 0;
showLocked = (argc == 3) && asInt(&argv[2]) != 0;

if(gCP->TSymbol_SymbolTable) 
    { 
    resultSpace = TObjVector_New(gCP,gTP);
    asObject(ret) = (TObject*)resultSpace;
    asTag(ret) = TYOBJVECTOR;
    
    asTag(tmp) = TYSYMBOL;
    
	/*  Create a vector of all symbols whose global value is not #void.                */
	/*  Note:	To increase the speed of Symbol lookup, we use the symbol table vector */
	/*			which is sorted alphabetically on the text contents of each symbol.    */
    for(cn = 0; cn < gCP->TSymbol_SymbolTable_ActiveCount; cn++)
        {
        /*  Examine each sorted vector in the hashtable */
        
		/*  Examine each symbol within each vector. */
    
		aSymbol = (TSymbol*)(atHMObject(gCP->TSymbol_SymbolTable->itsObjectArray,cn));

		if (TESTON)
			{
			if (!_VALIDOBJ(aSymbol) || 
				((aSymbol->itsImmediatePtr == NULL) && ((aSymbol->itsCString == NULL) || (((LpMHANDLEINFO)aSymbol->itsCString)->Free == TRUE)))
				)
				{
				/* WARNING: We have detected a corrupted symbol table. */
				TSymbol_CheckSymbolTable(gCP,gTP);
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
			}
    
		asObject(tmp) = (TObject*)aSymbol;
    
		if (asTag(&aSymbol->itsGlobalValue) == TYVOID);
		else
		if (aSymbol->itsGlobalLock == TRUE && !showLocked);
		else
		if (SymbolArray(*tmp)[0] == ':' && !showCProcs);
		else
		if (!showCProcs && (asTag(&aSymbol->itsGlobalValue) == TYCPROCEDURE || 
			asTag(&aSymbol->itsGlobalValue) == TYCFUNCTION || 
			asTag(&aSymbol->itsGlobalValue) == TYSPECIALFORM || 
			asTag(&aSymbol->itsGlobalValue) == TYPCODE || 
			asTag(&aSymbol->itsGlobalValue) == TYCMACRO));
		else
			{
			if(doSort)
				TSymbol_AddSortedValue(gCP,gTP,resultSpace,aSymbol);
			else
				TObjVector_AddNewValue(gCP,gTP,*ret,*tmp);
			}
        }
    }
else
    *ret = gCP->Tval_VOID;
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_GetGlobalBind

Returns the first global symbol which is bound to this value. If there is no global 
symbol bound to this value, the value itself is returned.

Note:   Symbols, whose values are #void, are never included.

(get-globalbind value)

#endif

TVAL    FDebug_GetGlobalBind(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
NUM                 cn;
CHAR				buf[1000];
StartFrame
DeclareOBJ(TSymbol,parentSymbol);
DeclareOBJ(TLambda,parentLambda);
DeclareOBJ(TSymbol,aSymbol);
DeclareOBJ(TObjVector,resultSpace);
DeclareTVAL(pvars);
DeclareTVAL(ret);
EndFrame

/*  Make sure we have the proper arguments. */

if (argc != 1)
    {
    FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
    }

/*  Search the current symbol table for the first symbol whose */
/*  global value matches the value we are searching for. */
/*  Note:	To increase the speed of Symbol lookup, we use a symbol hash table */
/*			which is an object vector (accessed as a hash table) of alphabetically */
/*			sorted symbol vectors (accessed by binary search). The size of the */
/*			first hash table object vector is defined in TSymbol.h. */

if (gCP->TSymbol_SymbolTable) 
    { 
    for (cn = 0; cn < gCP->TSymbol_SymbolTable_ActiveCount; cn++)
        {
		/*  Examine each symbol within the symbol table. */

        aSymbol = (TSymbol*)(atHMObject(gCP->TSymbol_SymbolTable->itsObjectArray,cn));
        
		if (TESTON)
			{
			if (!_VALIDOBJ(aSymbol) || 
				(aSymbol->itsObjectType != TYSYMBOL) ||
				((aSymbol->itsImmediatePtr == NULL) && ((aSymbol->itsCString == NULL) || (((LpMHANDLEINFO)aSymbol->itsCString)->Free == TRUE)))
				)
				{
				/* WARNING: We have detected a corrupted symbol table. */
				TSymbol_CheckSymbolTable(gCP,gTP);
				FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
				}
			}

		/* Search for a symbol whose global value equals the search argument. */

        if ((aSymbol != gCP->FDebug_currentResult) && (asTag(&aSymbol->itsGlobalValue) != TYVOID))
            {
            *ret = FPredicate2_FullIdentical(gCP, gTP,aSymbol->itsGlobalValue,argv[0]);
            if ((ret->Tag == TYBOLE) && (ret->u.Bool == TRUE))
                {
                ret->Tag = TYSYMBOL;
                ret->u.Object = (TObject*)aSymbol;
                goto Last;
                }
			else
            if ((argv[0].Tag == TYLAMBDA) && (aSymbol->itsGlobalValue.Tag == TYLAMBDA))
				{
				*ret = FPredicate2_FullIdentical(gCP, gTP,TOBJ(aSymbol->itsGlobalValue.u.Lambda->PersistantVariables),TOBJ(argv[0].u.Lambda->PersistantVariables));
				if ((ret->Tag == TYBOLE) && (ret->u.Bool == TRUE))
					{
					/* This may be the parent Lambda of the Lambda we're looking for. */
					parentSymbol = aSymbol;
					parentLambda = aSymbol->itsGlobalValue.u.Lambda;
					}
				}
            }   

        } 

	/* Search for a child Lambda whose persistent value equals the search argument. */

    if ((parentLambda != NULL) && (parentLambda->PersistantVariables != NIL))
        {
		*pvars = TOBJ(parentLambda->PersistantVariables);
		for (cn = 0; cn < Structure(*pvars)->itsMaxItemIndex; cn++)
			{
			/*  Examine each symbol within the symbol table. */
                
			*ret = FPredicate2_FullIdentical(gCP, gTP,BindArray(*pvars)[cn].Value,argv[0]);
			if ((ret->Tag == TYBOLE) && (ret->u.Bool == TRUE))
				{
				/* Create the string parentLambda:childLambda. */

				sprintf(buf,"%s:%s",(char*)*parentSymbol->itsCString,(char*)*((TSymbol*)BindArray(*pvars)[cn].Key)->itsCString);
				*ret = FSmartbase_CnvFromText(gCP,gTP,buf);
				goto Last;
				}
			}
        }

	*ret = gCP->TObject_VOID;
    }
else
    *ret = gCP->TObject_VOID;
    
Last:
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_GetLambdaBind

#endif

TVAL    FDebug_GetLambdaBind(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TVAL	binding = gCP->Tval_VOID;

if(argc != 1 || argv[0].Tag != TYLAMBDA)
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
    
binding = TOBJ(Lambda(argv[0])->Interfaces);
if (binding.Tag != TYVOID)
	{
	binding = FSmartbase_Ref(gCP,gTP,2,binding,gCP->TLambda_TvalBinding);
	}

return(binding);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FDebug_GetGlobalValue

#endif

TVAL    FDebug_GetGlobalValue(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[])
{
TVAL	result;

gTP = gTP; // NOOP to hide unused parameter warning message
if(argc != 1 || asTag(&argv[0]) != TYSYMBOL)
	{
    return(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/* Return the global value of the symbol */
result = asSymbol(argv)->itsGlobalValue;
if (asSymbol(argv)->itsGlobalValue.Tag == TYERROR)
	{
	strcpy(result.u.Text,"!error!");
	result.Tag = TYTEXT;		
	}

return(result);
}


