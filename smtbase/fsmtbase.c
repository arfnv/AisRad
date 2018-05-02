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

#define _C_FSMARTBASE
#define _SMARTBASE

#if 0
FSmtBase.c
    
Declarations of the top level API procedures required for the Smartbase
High Speed Engine.

AUTHORS:            Michael F. Korns

#endif

/*  SmartBase Standard Configuration include files. */

#include    "fsmtbase.h"
#include    "tobject.h"
#include    "fmemory.h"
#include    "fobject.h"
#include    "tlambda.h"
#include    "twkspace.h"
#include    "fproc.h"
#include    "fdefine.h"
#include    "futil1.h"
#include    "futil2.h"
#include    "futil3.h"
#include    "fdebug.h"
#include    "fdatabas.h"
#include    "fconio.h"
#include    "fmake.h"
#include    "fcompile.h"
#include    "fpred.h"
#include    "fpred2.h"
#include    "flisp.h"
#include    "tbytevec.h"
#include    "tbitvec.h"
#include    "tnumvec.h"
#include    "tfltvec.h"
#include    "tdiction.h"
#include    "fpropty.h"
#include    "fconvert.h"
#include    "fdatefnc.h"
#include    "tdirect.h"
#include    "tdatabas.h"
#include    "fmath2.h"
#include    "terror.h"
#include    "fvmscpt.h"
#include    "tneural.h"
#include    "ffinance.h"
#include    "tmatrix.h"
#include    "tnummat.h"
#include    "tcpxvec.h"
#include    "tpcodvec.h"
#include    "tshortvec.h"
#include    "tlongvec.h"
#include    "tbrick.h"
#include    "ffinance.h"

#ifdef _LINUX
#include	"sys/time.h"
#include	<sys/mman.h>
#include	<stdlib.h>
#include	<unistd.h>
#endif
#if defined(_MSVC) && defined(_M64)
#include	<windows.h>
#endif
#include	<assert.h>
#include	<time.h>
#include	<stdlib.h>

void FSmartbase_Log(LpXCONTEXT gCP,LpTHREAD gTP,char* ipMsg); /* TLW Writes to log file in install directory for debugging only */

/*  Include SmartBase extended configurations here. */
#if __EXMYSQL
#include    "fmysql1.h"
#endif

extern  void    TObject_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TString_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TSymbol_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TContinuation_Init(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TStructure_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TObjVector_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TPair_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TVector_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TLambda_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TLongVector_Init(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TShtVector_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TWorkspace_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern	void	TCpx_Init		(LpXCONTEXT gCP,LpTHREAD gTP);

extern  TVAL    FConvert_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FPredicate_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FList_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FString_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FMake_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FProperty_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FControl_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FDefine_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FLisp_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FUtil1_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FUtil2_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FUtil3_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FConio_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FMath1_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FMath2_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FMath3_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FVmScript_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FVmCode_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FMacro_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FCompile_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FDebug_Init		(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FStatFnc_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FTextFnc_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FDateFnc_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FWorkspace_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    FDatabas_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  TVAL    TDirect_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TDatabase_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern  void    TBitVector_Functions_Init	(LpXCONTEXT gCP,LpTHREAD gTP);
extern	NUM		FMemory_SizeOf	(HMemory hMemory);
extern  TVAL    FFinance_Init   (LpXCONTEXT gCP, LpTHREAD gTP);

/*  Initialize SmartBase extended configurations here. */

#if __EXMYSQL
extern  TVAL    FMySQL1_Init	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);
#endif

/*--------------------------------------------------------------------------------------- */
/* Array of Context Handles for use with multi-context-multi-tasking functions			  */
/*--------------------------------------------------------------------------------------- */
#define		MAXCONTEXTHANDLES		100
#define		MAXCONTEXTNAMESIZE		62
#define		MINFIRSTCONTEXTBLOCK	100000

CHAR 		gContextNames[MAXCONTEXTHANDLES+1][MAXCONTEXTNAMESIZE+1] = {0,0,0};
LpXCONTEXT	gContextMap[MAXCONTEXTHANDLES+1] = {NULL,NULL,NULL};

/* ************************************************************************************************************/
/* *** API Function declarations ******************************************************************************/
/* ************************************************************************************************************/

/* *** Basic (most often used) Caller API Function Declarations ***********************************************/
/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_MainContextStart

Initialize the Smartbase High Speed Engine.

Note:   Must be called once BEFORE using any other Smartbase API functions.  

#endif

LpXCONTEXT FSmartbase_MainContextStart(LpCHAR name,NUM heapSize,NUM stackSize,NUM objHdrSize,struct FSmartbase_HostCallBackFunctions* Funcs,LpCHAR errorMsg)
{
NUM				i, j, n, lowSlot;
LpXCONTEXT		gCP = NULL;
LpTHREAD        gTP = NULL;
NUM				memorySize = heapSize + objHdrSize;
NUM				minimumHeap = FSMARTBASE_MINBLOCKSIZE;
NUM				minimumObjHdrs = FSMARTBASE_MINBLOCKSIZE/10;
NUM				minimumStack = FSMARTBASE_MAINCONTEXTSTACK;
NUM				contextIndex;
NUM				aMaxRecursions = FSMARTBASE_MAINCONTEXTMAXRECURSIONS;
NUM				aMinBlockSize;
NUM				aRequiredHostStackSpace;
POINTER			apAllocBlocks[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
NUM				aAllocBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
NUM				aTotalAcquired = 0;
NUM				aRequest;
NUM				aBlockSize;
NUM				aPageSize;
POINTER			apBlock;
POINTER			firstBlock = NULL;
TVAL			err;
NUM 			aCatchCode;

/* Make sure the heap memory and the object header memory meet at least the minimum required.  */
if (heapSize < minimumHeap) heapSize = minimumHeap; 
if (objHdrSize <= minimumObjHdrs) objHdrSize = minimumObjHdrs;
if (stackSize < minimumStack) stackSize = minimumStack;
memorySize = heapSize + objHdrSize;

/* Make sure the page size meets at least the minimum system requirements.  */
#if defined(_LINUX)
aPageSize = sysconf(_SC_PAGESIZE);		/* Virtual memory page size (must be a power of 2) */
#else
aPageSize = 0;							/* Virtual memory page size (must be a power of 0) */
#endif

/*
Add the requested Context name to the list of open contexts
Note1: The requested context name must NOT exist in the table of open contexts.
*/
if (strlen(name) >= MAXCONTEXTNAMESIZE) {strcpy(errorMsg,"FSmartbase_MainContextStart: context name too long"); return(NULL);}
strcpy(Funcs->ContextName, name);
for (contextIndex = 0; contextIndex <= MAXCONTEXTHANDLES; ++contextIndex)
	{
	if ((gContextMap[contextIndex] != NULL) && (strcmp(name,gContextNames[contextIndex]) == 0))
		{
		strcpy(errorMsg,"FSmartbase_MainContextStart: context name already exists"); 
		return(NULL);
		}
	}

 for (contextIndex = 0; contextIndex <= MAXCONTEXTHANDLES; ++contextIndex)
	{
	if (gContextMap[contextIndex] == NULL) goto ContextSlotFound;
	}


/* 
Initialize Host call back functions and other important context initial settings.
Note1: Many of the call back functions will have already been initialized by the Host caller.
Note2: The Smartbase engine prefers to use its own file IO functions unless the Host caller specifically overrides.
*/
ContextSlotFound:
if (Funcs->_Host_Escape == NULL) Funcs->_Host_Escape = FConio_Escape;
if (Funcs->_Host_Display == NULL) Funcs->_Host_Display = FConio_IO_writeLog;
if (Funcs->_Host_OpenF == NULL) Funcs->_Host_OpenF = FConio_IO_fopen;
if (Funcs->_Host_ReadF == NULL) Funcs->_Host_ReadF	= FConio_IO_fread;
if (Funcs->_Host_WriteF == NULL) Funcs->_Host_WriteF = FConio_IO_fwrite;
if (Funcs->_Host_SeekF == NULL) Funcs->_Host_SeekF	= FConio_IO_fseek;
if (Funcs->_Host_ResizeF == NULL) Funcs->_Host_ResizeF = FConio_IO_fresize;
if (Funcs->_Host_CloseF == NULL) Funcs->_Host_CloseF = FConio_IO_fclose;
Funcs->_Host_memorySize = memorySize;
Funcs->_Host_memoryObjHdrSize = objHdrSize;
Funcs->_Host_MaxThreadCount = 1;
Funcs->_Host_RequiredStackSpace = stackSize;
Funcs->_MaxRecursions = aMaxRecursions;
Funcs->_Host_OperationsStackWords = Funcs->_Host_GarbageStackWords = (aMaxRecursions*100);

for (i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; Funcs->_Host_memBlockPtr[i++] = NULL) ;
for (i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; apAllocBlocks[i++] = NULL) ;
for (i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; aAllocBlockSize[i++] = 0) ;

/* Calculate the minimum size of the first block */
aMinBlockSize = (XCONTEXTBLOCKLENGTH
			     + (Funcs->_Host_MaxThreadCount * THREADBLOCKLENGTH) 
			     + ((Funcs->_Host_GarbageStackWords + 100) * sizeof(OBJ**))
			     + ((Funcs->_Host_OperationsStackWords  + 100) * sizeof(TVAL)));
if (aMinBlockSize >= Funcs->_Host_memorySize) {strcpy(errorMsg,"FSmartbase_MainContextStart: context memory request must be large enough to hold minimum block size"); return(NULL);}
if (aMinBlockSize < FSMARTBASE_MINBLOCKSIZE) aMinBlockSize = FSMARTBASE_MINBLOCKSIZE;

//***************************
// BEG Allocate Memory Blocks
//***************************
/*
Allocate the memory for the Context Heap from the Host operating system.
Note: The memory for the Context Heap need not be allocated in one monolithic block.
      Many smaller memory blocks may be combined for the Context Heap
	  This is important as it allows a larger Heap to be requested especially
	  in a fragmented Host operating system which often occurs when many Host OS
	  programs have been executing for long periods of time.
*/
aTotalAcquired = 0;
aRequest = Funcs->_Host_memorySize;
i = 0;
#if defined(_LINUX)
aPageSize = sysconf(_SC_PAGESIZE);		/* Virtual memory page size (must be a power of 2) */
#else
aPageSize = 0;
#endif
while ((aRequest >= aMinBlockSize) && (aTotalAcquired < Funcs->_Host_memorySize) && (i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS)) 
	{
	/*
	Attempt to allocate the requested memory block
	Note: Each Host OS environment has its own methods for memory allocation
	*/
	aBlockSize = aRequest;
	if  (firstBlock != NIL)
		{
		if (aBlockSize < MINFIRSTCONTEXTBLOCK)
			{
			apBlock = (POINTER)realloc(firstBlock, aBlockSize);
			firstBlock = NIL;
			}
		else
			{
			apBlock = firstBlock;
			aBlockSize = MINFIRSTCONTEXTBLOCK;
			firstBlock = NIL;
			}
		}
	else
		{
#if defined(_MSVC) && defined(_M64)
		apBlock = (POINTER)VirtualAlloc(NULL, aBlockSize, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
		VirtualAlloc(apBlock, aBlockSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
#else
#if defined(_GCC) && defined(_LINUX)
		posix_memalign((void**)&apBlock, aPageSize, aBlockSize); // Allocates memory on page boundary
#else
		apBlock = (POINTER)malloc(aBlockSize);
#endif
#endif
		}

	/*
	Protect and save successfully allocated memory blocks.
	Note: Memory must be allocated AND locked/protected/fixed depending upon the Host OS.
	*/
	if (apBlock != NULL)
		{
		/* Attempt to lock/protect/fix the requested memory block */ 
#ifdef _LINUX
		if (mprotect(apBlock, aRequest, PROT_EXEC|PROT_WRITE|PROT_READ) < 0) goto CleanUpUnwantedMallocs;
#endif
#if defined(_MSVC) && defined(_M64)
		if (VirtualLock(apBlock, aBlockSize)) goto CleanUpUnwantedMallocs;
#endif

		/*
		Save the successfully allocated memory blocks and sizes in the apAllocBlocks array.
		Note: Each Host OS environment has its own methods for memory allocation
		*/
		aTotalAcquired += aBlockSize;
		apAllocBlocks[i] = apBlock;
		aAllocBlockSize[i++] = aBlockSize;

		/* Try to get remainder as another single block (repeat via the while loop) */
		if (Funcs->_Host_memorySize > aTotalAcquired) aRequest = Funcs->_Host_memorySize - aTotalAcquired;
		}
	else
		{
		AllocationTooLargeTryForSmallerAllocation:

		/* Lets try getting a smaller block (repeat via the while loop) */
		aRequest = aBlockSize - FSMARTBASE_BLOCKDECREMENT;
		}
	} /* end while */
//***************************
// END Allocate Memory Blocks
//***************************


//***********************
// BEG Free Memory Blocks
//***********************
/*
If we did not get enough memory then we return a failure state
*/
if (aTotalAcquired < Funcs->_Host_memorySize) 
	{
	CleanUpUnwantedMallocs:
	/*
	Free any partially allocated memory blocks
	IF we FAILED to allocate enough memory to satisfy the request, then we must quit
	Note: The memory allocated for the Context Heap must be freed block by block
		  so that there are no memory leaks resulting from the failed create Context.
	*/
	if (apBlock != NULL)
		{	
#if defined(_MSVC) && defined(_M64)
		VirtualFree(apBlock, aBlockSize, MEM_DECOMMIT);
		VirtualFree(apBlock, aBlockSize, MEM_RELEASE);
#else
		free(apBlock);
#endif
		apBlock = NULL;
		aBlockSize = 0;
		}
	else
		{	
		apBlock = NULL;
		aBlockSize = 0;
		}

	/* Clean up all fully registered memory allocations if we failed to open the context properly */
	for (i = 0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i) 
		{	
		if (Funcs->_Host_memBlockPtr[i] != NULL)
			{	
#if defined(_MSVC) && defined(_M64)
			VirtualFree(Funcs->_Host_memBlockPtr[i], Funcs->_Host_memBlockSize[i], MEM_DECOMMIT);
			VirtualFree(Funcs->_Host_memBlockPtr[i], Funcs->_Host_memBlockSize[i], MEM_RELEASE);
#else
			free(Funcs->_Host_memBlockPtr[i]);
#endif
			Funcs->_Host_memBlockPtr[i] = NULL;
			Funcs->_Host_memBlockSize[i] = 0;
			}
		else
			{	
			Funcs->_Host_memBlockPtr[i] = NULL;
			Funcs->_Host_memBlockSize[i] = 0;
			}

		/* Clean up all unregistered memory allocations if we failed to open the context properly */
		if (apAllocBlocks[i] != NULL)
			{	
#if defined(_MSVC) && defined(_M64)
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_DECOMMIT);
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_RELEASE);
#else
			free(apAllocBlocks[i]);
#endif
			apAllocBlocks[i] = NULL;
			aAllocBlockSize[i] = 0;
			}
		else
			{	
			apAllocBlocks[i] = NULL;
			aAllocBlockSize[i] = 0;
			}
		} /* end for loop */

	gCP = NULL;
	strcpy(errorMsg,"FSmartbase_MainContextStart: context memory request too large for available Host memory");
	return(NULL);
	} /* end clean up if */
//***********************
// END Free Memory Blocks
//***********************

//***********************
// BEG Sort Memory Blocks
//***********************
/*
IF we arrive here, the we SUCCEEDED in allocating enough memory to satisfy the request.
Note1: The memory allocated for the Context Heap must be registered block by block
       so that all memory blocks allocated can be permamently recorded.
Note2: Due to the decremental size approach to memory block allocation, all memory
       blocks for the Context Heap are recorded in decending order of block-size.
Note3: Due to the requirements of the Smartbase engine Context management, 
       all memory blocks allocated for the Context Heap must be registered in 
	   ascending order by block-memory-address (lower memory addresses first).
Note4: This makes the multiple allocated memory blocks look like one large
	   memory allocation with missing gaps. The Smartbase engine context
	   memory manager will fill in the gaps as if they had been assigned to
	   permanent heap objects. 
*/
for (i = 0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i) // For each slot in _Host_memBlockPtr
	{	
	/* Find next lowest pointer in apAllocBlocks for assignment into _Host_memBlockPtr */
	lowSlot = 0;
	for (j = 1; j < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++j)
		{
		if ((apAllocBlocks[lowSlot] == NULL) || (apAllocBlocks[j] != NULL && (apAllocBlocks[j] < apAllocBlocks[lowSlot])))
			lowSlot = j;
		}
	if (apAllocBlocks[lowSlot] != NULL)
		{	
		Funcs->_Host_memBlockPtr[i] = apAllocBlocks[lowSlot];
		Funcs->_Host_memBlockSize[i] = aAllocBlockSize[lowSlot];
		memset(Funcs->_Host_memBlockPtr[i], 0, Funcs->_Host_memBlockSize[i]);
		apAllocBlocks[lowSlot] = NULL;
		aAllocBlockSize[lowSlot] = 0;
		}
	else
		{	
		Funcs->_Host_memBlockPtr[i] = NULL;
		Funcs->_Host_memBlockSize[i] = 0;
		}
	} /* end registration for loop */	
//***********************
// END Sort Memory Blocks
//***********************

/* 
Request the Smartbase Engine initialize the newly allocated context memory blocks.
*/
gCP = FSmartbase_ContextInit(Funcs);
if (gCP == NULL)
	{
	strcpy(errorMsg,"FSmartbase_MainContextStart: context Smartbase Engine initialization failed");
	goto CleanUpUnwantedMallocs;
	}
gCP->hostRegisterLispFunctions = Funcs->_Host_RegisterLispFunctions;

/* 
Return initialized XContext Structure to caller.
Note: The new context name and structure is added to the table of open contexts.
*/
Last:
gTP = (LpTHREAD)&gCP->ThreadBlocks[0];
gTP->SessionID = -1; 
gTP->CStackSpace = stackSize;
gTP->MaxRecursions = aMaxRecursions;
gCP->SessionMgrContextThread = NULL;
gCP->ContextIndex = contextIndex = 0;
strcpy(gContextNames[contextIndex],name);
gContextMap[contextIndex] = gCP;

// Setup the setjmp since smartbase uses longjump to throw an error.
_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);

/* 
Register the RadIde function library.
*/
err = (*Funcs->_Host_RegisterLispFunctions)(gCP,gTP,0,NULL);
if (err.Tag == TYERROR) 
	{
	strcpy(errorMsg,ErrorArray(err));
	goto CleanUpUnwantedMallocs;
	}

LBLSystemThrowError:
gTP->busySW = FALSE;
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
return(gCP);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_MainContextStop

Shut down the Smartbase Engine Main Context.

Note:   Must be called once BEFORE shutting down the calling application.  

#endif

NUM FSmartbase_MainContextStop(LpXCONTEXT gCP, LpTHREAD gTP)
{
	NUM			successCode = 0;
	NUM			failedCode = 1;
	NUM			i;
	NUM			aCatchCode;
	POINTER		apAllocBlocks[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
	NUM			aAllocBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];

	// Do not flush the main context if it is already closed.
	if (gCP == NULL)
		return(successCode);

	// Setup the setjmp since smartbase uses longjump to throw an error.
	_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);

	// Close all files opened by context
	FSmartbase_Evals(gCP,gTP,(LpCHAR)"(clear)",FALSE);

	// Force garbage collection for the context.
 	FSmartbase_MarkAndSweep(gCP,gTP);

	// Close MySQL connections
	FSmartbase_Evals(gCP,gTP,(LpCHAR)"(sqlend)",FALSE);

	// Free the memory for the context
	// Copy the apAllocBlocks for the context so we can free them
	for(i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
	{
		apAllocBlocks[i] = gCP->ContextBlocks[i];
		aAllocBlockSize[i] = gCP->ContextBlockSize[i];
	}

	for(i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
	{
		if (apAllocBlocks[i] != NULL)
		{
#if defined(_MSVC) && defined(_M64)
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_DECOMMIT);
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_RELEASE);
#else
			free(apAllocBlocks[i]);
#endif
		}
		// FCC: gCP points to the first block. After deallocation, gCP will be garbage
		//gCP->ContextBlocks[i] = NULL;
		//gCP->ContextBlockSize[i] = 0;
	}

	return(successCode);

LBLSystemThrowError:
	// Write error to an errorFile so it can be reported during next launch of context
	// We can not report this error in the normal logs by calling back into AIS
	// as AIS may be shutting down when SBGlue_FlushLispCodeInContext was called.

	///// Deferred until we get the new OS file layer in place
	///// It is difficult to use asysglue functions here because the context
	///// is partially toasted.

	// Free the memory for the context
	// Copy the apAllocBlocks for the context so we can free them
	for(i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
		apAllocBlocks[i] = gCP->ContextBlocks[i];

	for(i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; ++i)
		if (apAllocBlocks[i] != NULL)
		{
#if defined(_MSVC) && defined(_M64)
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_DECOMMIT);
			VirtualFree(apAllocBlocks[i], aAllocBlockSize[i], MEM_RELEASE);
#else
			free(apAllocBlocks[i]);
#endif
		}
	return(failedCode);
}


/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Run

Compile and evaluate a SmartLisp formula. Return the result to the caller.

Arguments:

theScript           A C string containing the Lisp source code to be compiled and evaluated.
                    The string will be copied to an internal handle so that even
                    temporary strings may be evaluated.

print               Determines whether or not the result value is to be displayed
                    on the SmartBase Console (TRUE = display, FALSE = no display).

Return Argument:

                    The tagged value resulting from execution of the compiled Lisp code.


#endif

TVAL FSmartbase_Run(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theScript,const BOLE print)
{
BOLE 		oldBusySW = gTP->busySW;
NUM			oldEscapeSW = gTP->escapeSW;
NUM 		aCatchCode;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame
*ret	=	gCP->Tval_VOID;

/*  If the engine is already busy then call Evals (which does not ECatch/Throw) */
/*	Note: Run is designed to be called from outside the Smartbase engine. */
/*        Run must set the busy switch and perform the initial ECath/Throw. */
/*        Evals is designed to be called from inside the engine. */
/*        If the engine busySW is TRUE, then we should reroute this request to Evals. */
if (gTP->busySW==TRUE) 
{
	*ret = FSmartbase_Evals(gCP,gTP,theScript,print);
	FrameExit(*ret);
}

/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_CheckFrame */
/*          macro at the end of this function. */
_TObject_RecordFrame
gTP->busySW = TRUE;

/*  Turn off the error switch so the system will know to replace */
/*  the _error call tree in the unlikely event of an error. */

gTP->TObject_ErrorSwt = FALSE;
if (oldBusySW == TRUE) goto DoNotCatchIfEngineBusy;
gTP->escapeSW = 0;
_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);
DoNotCatchIfEngineBusy:
    
/*  Interpret all command expressions with a full compilation. */

CheckAndThrowOnEscape
*ret = TSTRING(theScript);
*ret =  FLisp_Lisp(gCP,gTP,1,ret);
if (isERROR(ret)) goto LBLSystemThrowError;
*ret =  FCompile_Compile(gCP,gTP,1,ret);
if (isERROR(ret)) goto LBLSystemThrowError;
*ret =  FSmartbase_Eval(gCP,gTP,*ret,0);

/* Make sure the temporary result stays around until the next evaluation. */
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_currentResult");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, *ret);
oldEscapeSW = gTP->escapeSW;
gTP->escapeSW = 0;
if (print) FConio_print(gCP, gTP, *ret);
gTP->escapeSW = oldEscapeSW;
    
/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_RecordFrame */
/*          macro at the start of this function. */
LBLSystemThrowError:

_TObject_CheckFrame
gTP->busySW = FALSE;
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
CheckEscapeOnly
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_RunLambda

Call the specified Lisp Lambda and return the result to the caller.

Arguments:

lambda				The Lambda object to be called (a Symbol will be evaluated as its global value).
argc				The number of arguments passed by the caller.
argv				The array of arguments passed by the caller.

print               Determines whether or not the result value is to be displayed
                    on the SmartBase Console (TRUE = display, FALSE = no display).

Return Argument:

                    The tagged value returned from the called Lambda.


#endif

TVAL FSmartbase_RunLambda(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL lambda, const NUM argc, const TVAL argv[],const BOLE print)
{
BOLE 		oldBusySW = gTP->busySW;
NUM			oldEscapeSW = gTP->escapeSW;
TVAL		executable;
NUM 		aCatchCode;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame
*ret	=	gCP->Tval_VOID;

/*  If the engine is already busy then call Evalv (which does not ECatch/Throw) */
/*	Note: RunLambda is designed to be called from outside the Smartbase engine. */
/*        RunLambda must set the busy switch and perform the initial ECatch/Throw. */
/*        Evalv is designed to be called from inside the engine. */
/*        If the engine busySW is TRUE, then we should reroute this request to Evalv. */
if (gTP->busySW==TRUE) 
{
	/* Symbols: the global value of symbols are evaluated */
	/* Note: this is an important special indirection which allows external */
	/*       FSmartbase callers to hold symbol values (which do not often change) */
	/*       as opposed to the value in the symbol which may change often */
	executable = lambda;
	if (executable.Tag == TYSYMBOL) executable = executable.u.Symbol->itsGlobalValue;
	if (executable.Tag != TYLAMBDA) FrameExit(gCP->Tval_VOID);
	*ret =  FSmartbase_Evalv(gCP,gTP,executable,argc,argv);
	FrameExit(*ret);
}

/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_CheckFrame */
/*          macro at the end of this function. */
_TObject_RecordFrame
gTP->busySW = TRUE;

/* Symbols: the global value of symbols are evaluated */
/* Note: this is an important special indirection which allows external */
/*       FSmartbase callers to hold symbol values (which do not often change) */
/*       as opposed to the value in the symbol which may change often */
executable = lambda;
if (executable.Tag == TYSYMBOL) executable = executable.u.Symbol->itsGlobalValue;
if (executable.Tag != TYLAMBDA) goto LBLSystemThrowError;

/*  Turn off the error switch so the system will know to replace */
/*  the _error call tree in the unlikely event of an error. */
gTP->TObject_ErrorSwt = FALSE;
if (oldBusySW == TRUE) goto DoNotCatchIfEngineBusy;
gTP->escapeSW = 0;
_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);
DoNotCatchIfEngineBusy:
    
/*  Call the Lisp executable directly. */
CheckAndThrowOnEscape
*ret =  FSmartbase_Evalv(gCP,gTP,executable,argc,argv);

/* Make sure the temporary result stays around until the next evaluation. */
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_currentResult");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, *ret);
oldEscapeSW = gTP->escapeSW;
gTP->escapeSW = 0;
if (print) FConio_print(gCP, gTP, *ret);
gTP->escapeSW = oldEscapeSW;
    
/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_RecordFrame */
/*          macro at the start of this function. */
LBLSystemThrowError:

_TObject_CheckFrame
gTP->busySW = FALSE;
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
CheckEscapeOnly
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_RunLambdaMember

Call the specified Lisp Lambda member function and return the result to the caller.

Arguments:

lambda				The Lambda object owning the member Lambda which is to be called.
member				The member Lambda object to be called.
argc				The number of arguments passed by the caller.
argv				The array of arguments passed by the caller.

print               Determines whether or not the result value is to be displayed
                    on the SmartBase Console (TRUE = display, FALSE = no display).

Return Argument:

                    The tagged value returned from the called member Lambda.


#endif

TVAL FSmartbase_RunLambdaMember(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL lambda, const TVAL member, const NUM argc, const TVAL argv[],const BOLE print)
{
BOLE 		oldBusySW = gTP->busySW;
NUM			oldEscapeSW = gTP->escapeSW;
TVAL		executable;
TVAL		memberLambda;
TVAL		prmv[2];
NUM 		aCatchCode;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame
*ret	=	gCP->Tval_VOID;

/*  If the engine is already busy then call Evalv (which does not ECatch/Throw) */
/*	Note: RunLambda is designed to be called from outside the Smartbase engine. */
/*        RunLambda must set the busy switch and perform the initial ECatch/Throw. */
/*        Evalv is designed to be called from inside the engine. */
/*        If the engine busySW is TRUE, then we should reroute this request to Evalv. */
if (gTP->busySW==TRUE) 
{
	/* Symbols: the global value of symbols are evaluated */
	/* Note: this is an important special indirection which allows external */
	/*       FSmartbase callers to hold symbol values (which do not often change) */
	/*       as opposed to the value in the symbol which may change often */
	executable = lambda;
	if (executable.Tag == TYSYMBOL) executable = executable.u.Symbol->itsGlobalValue;
	if (executable.Tag != TYLAMBDA) goto LBLSystemThrowError; 

	/* Perform a (ref executable member) to get the executable's member to be executed. */
	prmv[0] = executable;
	prmv[1] = member;
	memberLambda = FSmartbase_Refv(gCP,gTP,2,prmv);

	if (memberLambda.Tag != TYLAMBDA)
		*ret =  FSmartbase_Evalv(gCP,gTP,executable,argc,argv);
	else
		*ret =  FSmartbase_Evalv(gCP,gTP,memberLambda,argc,argv);
	FrameExit(*ret);
}

/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_CheckFrame */
/*          macro at the end of this function. */
_TObject_RecordFrame
gTP->busySW = TRUE;

/* Symbols: the global value of symbols are evaluated */
/* Note: this is an important special indirection which allows external */
/*       FSmartbase callers to hold symbol values (which do not often change) */
/*       as opposed to the value in the symbol which may change often */
executable = lambda;
if (executable.Tag == TYSYMBOL) executable = executable.u.Symbol->itsGlobalValue;
if (executable.Tag != TYLAMBDA)  goto LBLSystemThrowError;

/* Perform a (ref executable member) to get the executable's member to be executed. */
prmv[0] = executable;
prmv[1] = member;
memberLambda = FSmartbase_Refv(gCP,gTP,2,prmv);

/*  Turn off the error switch so the system will know to replace */
/*  the _error call tree in the unlikely event of an error. */
gTP->TObject_ErrorSwt = FALSE;
if (oldBusySW == TRUE) goto DoNotCatchIfEngineBusy;
gTP->escapeSW = 0;
_FSmartbase_ECatch(gCP,/*out*/aCatchCode, LBLSystemThrowError);
DoNotCatchIfEngineBusy:


/*  Call the specified Lisp executable directly. */
CheckAndThrowOnEscape
if (memberLambda.Tag != TYLAMBDA)
	*ret =  FSmartbase_Evalv(gCP,gTP,executable,argc,argv);
else
	*ret =  FSmartbase_Evalv(gCP,gTP,memberLambda,argc,argv);


/* Make sure the temporary result stays around until the next evaluation. */
aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_currentResult");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, *ret);
oldEscapeSW = gTP->escapeSW;
gTP->escapeSW = 0;
if (print) FConio_print(gCP, gTP, *ret);
gTP->escapeSW = oldEscapeSW;
    
/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_RecordFrame */
/*          macro at the start of this function. */
LBLSystemThrowError:

_TObject_CheckFrame
gTP->busySW = FALSE;
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
CheckEscapeOnly
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_RegisterCProcedure

The FSmartbase_RegisterCProcedure function converts the specified text into a callable symbol. 

Note:   TYCPROCEDURE functions must be of type LpFUNC, and must have the following
        standard argument list:
        
            LpFUNC(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

TVAL FSmartbase_RegisterCProcedure(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpFUNC lpFunc)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(tmpTval);
EndFrame

aSymbol = TSymbol_MakeUnique(gCP,gTP,funcSymbolName);
aSymbol->itsCProcedure =  lpFunc;

tmpTval->Tag = TYCPROCEDURE;
tmpTval->u.Symbol = aSymbol;

/*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */

TSymbol_SetGlobalValue(gCP,gTP,aSymbol, *tmpTval);

FrameExit(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_RegisterVMEvaluator

The FSmartbase_RegisterVMEvaluator function converts the specified text into a callable 
Virtual Machine Evaluator symbol. 

Note:   TYVMEVALUATOR functions must be of type LpVMEVALUATOR, and must have the following
        standard argument list:
        
            LpVMEVALUATOR(TLambda* proc,NUM argc,TVAL argv[]);

#endif

TVAL FSmartbase_RegisterVMEvaluator(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpVMEVALUATOR lpFunc)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(tmpTval);
EndFrame

aSymbol = TSymbol_MakeUnique(gCP,gTP,funcSymbolName);
aSymbol->itsCProcedure =  (LpFUNC)lpFunc;

tmpTval->Tag = TYVMEVALUATOR;
tmpTval->u.Symbol = aSymbol;

/*  Calling SetGlobalValue with a non-void value will automatically PERM the symbol */

TSymbol_SetGlobalValue(gCP,gTP,aSymbol, *tmpTval);

FrameExit(gCP->TObject_OK);
}

/* ************************************************************************************************************/
/* *** Inter Context Function Declarations ********************************************************************/
/* ************************************************************************************************************/


/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_ContextInit

Initialize the Smartbase High Speed Engine.

Note:   Must be called once BEFORE using any other Smartbase API functions.  

#endif

LpXCONTEXT FSmartbase_ContextInit(struct FSmartbase_HostCallBackFunctions* Funcs)
{
TVAL			newValue;
TVAL			ec;
NUM				i;
struct TSymbol* aSymbol;
LpXCONTEXT		gCP;
LpTHREAD        gTP;
char			script[1000];

i = sizeof(TLambda);

/*	Make sure we are properly ported to the current host machine. */
if (sizeof(TVAL) != MAXREGISTERLEN) return FALSE;

/*	Save the new context pointer supplied by the caller. */
gCP = (LpXCONTEXT)Funcs->_Host_memBlockPtr[0];

/*	Save the new thread block pointer supplied by the caller. */
gTP = (LpTHREAD)&gCP->ThreadBlocks[0];
gCP->maxNumberOfThreads = 1;

/*	Do not initialize, if already initialized */
if (gCP->FSmartbase_Initialized) return gCP;

/*  Set the init flag so that we may create Procedure objects. */
/*  At this point we are ready to run the SmartLisp Compiler. */

gCP->FSmartbase_Initialized = TRUE;
strcpy(gCP->ContextName,Funcs->ContextName);
gCP->ContextName[63] = 0; 

/* Copy the memory block allocations */
for (i=0; i < FSMARTBASE_MAXCONTEXTMEMORYBLOCKS; i++)
{	gCP->ContextBlocks[i] = Funcs->_Host_memBlockPtr[i];
	gCP->ContextBlockSize[i] = Funcs->_Host_memBlockSize[i];
}

gCP->Tval_VOID.Tag = TYVOID;
gCP->Tval_VOID.u.Void = 0;
gCP->Tval_VOID.Offset = 0;
gCP->Tval_VOID.Modifier = AMVOID;
gCP->Tval_VOID.DeclaredType = 0;
gCP->JIT_codeBloatFactor = 5;

gCP->FSmartbase_RegisterCProcedure = (LpFUNTVAL)&FSmartbase_RegisterCProcedure;
gCP->FSmartbase_ObjectPtr = (LpFUNPOINTER)&FSmartbase_ObjectPtr;
gCP->FSmartbase_GetSymbolValuePtr = (LpFUNTVALPTR)&FSmartbase_GetSymbolValuePtr;
gCP->FSmartbase_Eval = (LpFUNTVAL)&FSmartbase_Eval;
gCP->FSmartbase_Evals = (LpFUNTVAL)&FSmartbase_Evals;
gCP->FSmartbase_Evalv = (LpFUNTVAL)&FSmartbase_Evalv;
gCP->FSmartbase_Ref = (LpFUNTVAL)&FSmartbase_Ref;
gCP->FSmartbase_Refv = (LpFUNTVAL)&FSmartbase_Refv;
gCP->FSmartbase_Set = (LpFUNTVAL)&FSmartbase_Set;
gCP->FSmartbase_Setv = (LpFUNTVAL)&FSmartbase_Setv;
gCP->FSmartbase_CnvFromChar = (LpFUNTVAL)&FSmartbase_CnvFromChar;
gCP->FSmartbase_CnvFromBool = (LpFUNTVAL)&FSmartbase_CnvFromBool;
gCP->FSmartbase_Error = (LpFUNTVAL)&FSmartbase_Error;
gCP->FSmartbase_Perror = (LpFUNTVAL)&FSmartbase_Perror;
gCP->FSmartbase_MakeCFunction = (LpFUNTVAL)&FSmartbase_MakeCFunction;
gCP->FSmartbase_CnvFromInt = (LpFUNTVAL)&FSmartbase_CnvFromInt;
gCP->FSmartbase_CnvFromPtr = (LpFUNTVAL)&FSmartbase_CnvFromPtr;
gCP->FSmartbase_CnvFromObj = (LpFUNTVAL)&FSmartbase_CnvFromObj;
gCP->FSmartbase_CnvFromReal = (LpFUNTVAL)&FSmartbase_CnvFromReal;
gCP->FSmartbase_CnvToFrame = (LpFUNTVAL)&FSmartbase_CnvToFrame;
gCP->FSmartbase_CnvFromText = (LpFUNTVAL)&FSmartbase_CnvFromText;
gCP->FSmartbase_CnvToSymbol = (LpFUNTVAL)&FSmartbase_CnvToSymbol;
gCP->FSmartbase_CnvToQSymbol = (LpFUNTVAL)&FSmartbase_CnvToQSymbol;
gCP->FSmartbase_GetSymbolValue = (LpFUNTVAL)&FSmartbase_GetSymbolValue;
#if defined(_MSVC) && defined(_M64)
gCP->atoi = (LpATOI)&_atoi64;
#else
gCP->atoi = (LpATOI)&atol;
#endif
gCP->atol = (LpATOL)&atol;
gCP->atof = (LpATOF)&atof;
gCP->modf = (LpMODF)&modf;
gCP->logFileID = -1;
gCP->logTime = TRUE;
gCP->logMode = FALSE;
time(&gCP->logStartTime);
gCP->logNewLineFlag = TRUE;

gTP->FConio_LogConsoleSW = FALSE;
gTP->FCompile_DebugSourceVector = NULL;
gTP->FCompile_DebugListLines = NULL;
gTP->FCompile_DebugInterfaces = gCP->Tval_VOID;
gTP->DebugTraceLambda.Tag = TYVOID;
gTP->DebugTraceCodeL = 0;
gTP->DebugTraceRecursion = -1;
gTP->MaxTvalStackSize = 0;
gTP->TvalStack = 0;
gTP->TvalStackIdx = 0;
gTP->RecursionCount = 0;
gTP->ObjStackIdx = 0;
gTP->MaxObjStackSize = 0;
gTP->ObjStack = 0;
gTP->SessionID = -1;
gCP->FSmartbase_SilentMode = FALSE;
gCP->TObject_MaxTypes = 0;

/*  Initialize all global variables. */

gTP->DebugTraceLambda.Tag = TYVOID;
gTP->DebugTraceCodeL = -1;
gTP->DebugTraceRecursion = -1;
gTP->TvalStackIdx = 0;
gTP->RecursionCount = 0;
_TvalStackReset()

#if 0
#ifdef MEMTEST
gCP->FMemory_SelfTest = TRUE; /* Force early memory selftesting */
#endif
#endif

gTP->FDebug_fmtStr77 = "%-20.20s";
gCP->FMath2_mask[0] =  0x00000000;
gCP->FMath2_mask[1] =  0x00000001;
gCP->FMath2_mask[2] =  0x00000003;
gCP->FMath2_mask[3] =  0x00000007;
gCP->FMath2_mask[4] =  0x0000000F;
gCP->FMath2_mask[5] =  0x0000001F;
gCP->FMath2_mask[6] =  0x0000003F;
gCP->FMath2_mask[7] =  0x0000007F;
gCP->FMath2_mask[8] =  0x000000FF;
gCP->FMath2_mask[9] =  0x000001FF;
gCP->FMath2_mask[10] = 0x000003FF;
gCP->FMath2_mask[11] = 0x000007FF;
gCP->FMath2_mask[12] = 0x00000FFF;
gCP->FMath2_mask[13] = 0x00001FFF;
gCP->FMath2_mask[14] = 0x00003FFF;
gCP->FMath2_mask[15] = 0x00007FFF;
gCP->FMath2_mask[16] = 0x0000FFFF;
gCP->FMath2_mask[17] = 0x0001FFFF;
gCP->FMath2_mask[18] = 0x0003FFFF;
gCP->FMath2_mask[19] = 0x0007FFFF;
gCP->FMath2_mask[20] = 0x000FFFFF;
gCP->FMath2_mask[21] = 0x001FFFFF;
gCP->FMath2_mask[22] = 0x003FFFFF;
gCP->FMath2_mask[23] = 0x007FFFFF;
gCP->FMath2_mask[24] = 0x00FFFFFF;
gCP->FMath2_mask[25] = 0x01FFFFFF;
gCP->FMath2_mask[26] = 0x03FFFFFF;
gCP->FMath2_mask[27] = 0x07FFFFFF;
gCP->FMath2_mask[28] = 0x0FFFFFFF;
gCP->FMath2_mask[29] = 0x1FFFFFFF;
gCP->FMath2_mask[30] = 0x3FFFFFFF;
gCP->FMath2_mask[31] = 0x7FFFFFFF;
gTP->FOpt2_labelCounter = 100000;
gCP->TBitVector_OrMasks[0] = 0x80;
gCP->TBitVector_OrMasks[1] = 0x40;
gCP->TBitVector_OrMasks[2] = 0x20;
gCP->TBitVector_OrMasks[3] = 0x10;
gCP->TBitVector_OrMasks[4] = 0x08;
gCP->TBitVector_OrMasks[5] = 0x04;
gCP->TBitVector_OrMasks[6] = 0x02;
gCP->TBitVector_OrMasks[7] = 0x01;
gCP->TBitVector_AndMasks[0] = 0x7F;
gCP->TBitVector_AndMasks[1] = 0xBF;
gCP->TBitVector_AndMasks[2] = 0xDF;
gCP->TBitVector_AndMasks[3] = 0xEF;
gCP->TBitVector_AndMasks[4] = 0xF7;
gCP->TBitVector_AndMasks[5] = 0xFB;
gCP->TBitVector_AndMasks[6] = 0xFD;
gCP->TBitVector_AndMasks[7] = 0xFE;

/* Disassembly style selector */
gTP->FVmscript2_DisassemblyStyle = 0;
gTP->FVmscript2_VariablesToShow = 5;

/* Initialize the context structure  */
/* Note: We ignore what the caller requested, */
/*       and we always allocate enough garbage */
/*       stack and operations stack words to */
/*       make the context function correctly. */
gTP->CStackSpace = Funcs->_Host_RequiredStackSpace;
gTP->MaxRecursions = Funcs->_MaxRecursions;
gCP->TObject_initMaxObjects  = (Funcs->_Host_memoryObjHdrSize/_FSmartbase_ObjectHeaderMaxSize);

/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Calling FMemory_Init\r\n");	*/
FMemory_Init(gCP, gTP, Funcs);
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Set pointers to host-supplied functions.\r\n");	*/

/*  Setup all of the pointers to host supplied functions. */

gCP->_Host_Display       = (LpHOST_DISPLAY)(*Funcs->_Host_Display);
gCP->_Host_Escape        = (LpHOST_ESCAPE)(*Funcs->_Host_Escape);
gCP->_Host_UpdateState   = (LpHOST_UPDATESTATE)(*Funcs->_Host_UpdateState);
gCP->_Host_Openf         = (LpHOST_OPENF)(*Funcs->_Host_OpenF);
gCP->_Host_Readf         = (LpHOST_READF)(*Funcs->_Host_ReadF);
gCP->_Host_Writef        = (LpHOST_WRITEF)(*Funcs->_Host_WriteF);
gCP->_Host_Seekf         = (LpHOST_SEEKF)(*Funcs->_Host_SeekF);
gCP->_Host_Resizef       = (LpHOST_RESIZEF)(*Funcs->_Host_ResizeF);
gCP->_Host_Closef        = (LpHOST_CLOSEF)(*Funcs->_Host_CloseF);

/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize new types\r\n");			*/
/*  Initialize the new types for this class. */
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYVOID,
                    (LpCHAR)"Void",
                    _TObject_TfNATIVE,
                    NIL,
                    (LpFNEW)&FMake_List,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_VoidAnyCnv,
                    &TObject_VoidAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize Word\r\n");	*/
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYTVAL,
                    (LpCHAR)"Word",
                    _TObject_TfNATIVE,
                    sizeof(TVAL),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkTval,
                    &TObject_GlobalMarkNever,
                    &FObject_TvalAnyCnv,
                    &TObject_TvalAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYBOLE,
                    (LpCHAR)"Boolean",
                    _TObject_TfNATIVE,
                    sizeof(BOLE),
                    (LpFNEW)&FConvert_ToBoolean,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_BoolAnyCnv,
                    &TObject_BoolAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYCHAR,
                    (LpCHAR)"Character",
                    _TObject_TfNATIVE,
                    sizeof(CHAR),
                    (LpFNEW)&FConvert_ToCharacter,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_CharAnyCnv,
                    &TObject_CharAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_CharPrint,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYCOMPARE,
                    (LpCHAR)"Compare",
                    _TObject_TfNATIVE,
                    sizeof(COMPARE),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_ShortAnyCnv,
                    &TObject_ShortAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize NewType for Object\r\n");			*/

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYOBJ,
                    (LpCHAR)"Object",
                    _TObject_TfTOBJECT,
                    sizeof(OBJ),
                    (LpFNEW)&TObject_New,
                    &TObject_MarkTval,
                    &TObject_GlobalMark,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYPOINTER,
                    (LpCHAR)"Pointer",
                    _TObject_TfNATIVE,
                    sizeof(POINTER),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					TYNUM,
                    (LpCHAR)"Integer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

FSmartbase_NewType	(gCP,
                     gTP,
                    TYUNUM,
                    (LpCHAR)"UnsignedInteger",
                    _TObject_TfNATIVE,
                    sizeof(UNUM),
                    (LpFNEW)&FConvert_ToUnsignedInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
                    &FObject_SaveNever,
                    &FObject_ComputeSizeNever,
                    &FObject_CopyNever,
                    &FObject_DoomNever);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYREAL,
                    (LpCHAR)"Number",
                    _TObject_TfNATIVE,
                    sizeof(REAL),
                    (LpFNEW)&FConvert_ToNumber,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_RealAnyCnv,
                    &TObject_RealAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYDATE,
                    (LpCHAR)"Date",
                    _TObject_TfNATIVE,
                    sizeof(REAL),
                    (LpFNEW)&FDateFnc_date,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_DateAnyCnv,
                    &TObject_DateAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYFLOAT,
                    (LpCHAR)"Float",
                    _TObject_TfNATIVE,
                    sizeof(FLOAT),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_RealAnyCnv,
                    &TObject_FloatAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYMONEY,
                    (LpCHAR)"Money",
                    _TObject_TfNATIVE,
                    sizeof(REAL),
                    (LpFNEW)&FConvert_ToMoney,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &TObject_FloatAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYSHORT,
                    (LpCHAR)"Short",
                    _TObject_TfNATIVE,
                    sizeof(SHORT),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_ShortAnyCnv,
                    &TObject_ShortAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYWORDPOINTER,
                    (LpCHAR)"WordPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYSHORTPOINTER,
                    (LpCHAR)"ShortPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYREALPOINTER,
                    (LpCHAR)"NumPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYJUMPPOINTER,
                    (LpCHAR)"JumpPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYINTPOINTER,
                    (LpCHAR)"IntPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYFLOATPOINTER,
                    (LpCHAR)"FloatPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYCHARPOINTER,
                    (LpCHAR)"CharPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYLONGPOINTER,
                    (LpCHAR)"LongPointer",
                    _TObject_TfNATIVE,
                    sizeof(NUM),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					TYLONG,
                    (LpCHAR)"Long",
                    _TObject_TfNATIVE,
                    sizeof(NUM32),
                    (LpFNEW)&FConvert_ToInteger,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Bits,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Bits,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYTYPE,
                    (LpCHAR)"Type",
                    _TObject_TfNATIVE,
                    sizeof(TYPE),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_ShortAnyCnv,
                    &TObject_ShortAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYCFUNCTION,
                    (LpCHAR)"internalFunction",
                    _TObject_TfNATIVE,
                    sizeof(LpFUNC),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &FObject_IntAnyCnv,
                    &TObject_IntAnyCmp,
                    &FObject_SetIV1Never,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &FObject_GetIV1Never,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapObject,
                    &TObject_MapcObject,
                    &TObject_PrintDefault,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);
FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYTEXT,
                    (LpCHAR)"Text",
                    _TObject_TfNATIVE,
                    sizeof(CHAR),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkNever,
                    &TObject_GlobalMarkNever,
                    &TObject_TextAnyCnv,
                    &TObject_TextAnyCmp,
                    &TObject_SetIVText,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TObject_GetIVText,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapText,
                    &TObject_MapcText,
                    &TObject_TextPrint,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

                                                                                                
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize TObject_Init\r\n");	*/

TObject_Init		(gCP,gTP);
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize TString_Init\r\n");	*/
TString_Init		(gCP,gTP);
TError_Init			(gCP,gTP);
TSymbol_Init		(gCP,gTP);
TContinuation_Init	(gCP,gTP);
TStructure_Init		(gCP,gTP);
TObjVector_Init		(gCP,gTP);
TPair_Init			(gCP,gTP);
TVector_Init		(gCP,gTP);
TLambda_Init		(gCP,gTP);
TShtVector_Init		(gCP,gTP);
TLongVector_Init	(gCP,gTP);
TWorkspace_Init		(gCP,gTP);
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize FConvert_Init\r\n"); */

FConvert_Init		(gCP,gTP);
FPredicate_Init		(gCP,gTP);
FPredicate2_Init	(gCP,gTP);
FList_Init			(gCP,gTP);
FString_Init		(gCP,gTP);
FMake_Init			(gCP,gTP);
FProperty_Init		(gCP,gTP);
FControl_Init		(gCP,gTP);
FDefine_Init		(gCP,gTP);
FLisp_Init			(gCP,gTP);
FUtil1_Init			(gCP,gTP);
FUtil2_Init			(gCP,gTP);
FUtil3_Init			(gCP,gTP);
FConio_Init			(gCP,gTP);
FMath1_Init			(gCP,gTP);
FMath2_Init			(gCP,gTP);
FMath3_Init			(gCP,gTP);
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize FVmScript_Init\r\n");			*/
FVmScript_Init		(gCP,gTP);
FVmCode_Init		(gCP,gTP);
FMacro_Init			(gCP,gTP);
FCompile_Init		(gCP,gTP);
FDebug_Init			(gCP,gTP);
FTextFnc_Init		(gCP,gTP);
FDateFnc_Init		(gCP,gTP);
FWorkspace_Init		(gCP,gTP);
FDatabas_Init		(gCP,gTP);
FStatFnc_Init		(gCP,gTP);
TDictionary_Init	(gCP,gTP);
TDirectory_Init		(gCP,gTP);
TDatabase_Init		(gCP,gTP);
TBitVector_Functions_Init(gCP,gTP);
TMatrix_Init		(gCP,gTP);
TNumMatrix_Init		(gCP,gTP);
TCpx_Init			(gCP,gTP);
TBrick_Init			(gCP,gTP);


/*  Initialize the error reporting variable. */
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_ContextInit(), Initialize error reporting\r\n");			*/
gCP->FSmartbase_errorMsg = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_errorMsg");
FObject_Perm(gCP,gTP,(TObject*)gCP->FSmartbase_errorMsg,TRUE);
gCP->FSmartbase_errorSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_error");
FObject_Perm(gCP,gTP,(TObject*)gCP->FSmartbase_errorSym,TRUE);
gCP->FSmartbase_sysErrorSym = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_sysError");
FObject_Perm(gCP,gTP,(TObject*)gCP->FSmartbase_sysErrorSym,TRUE);
gCP->FSmartbase_sysError = TERROR("!System Error: .....................................................................................................................!");
FObject_Perm(gCP,gTP,(TObject*)gCP->FSmartbase_sysError.u.Object,TRUE);

/*  Initialize the current file path */

gCP->TSymbol_Path = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_path");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_Path,TRUE);
newValue.Tag = TYTEXT;
newValue.u.Text[0] = 0;
TSymbol_SetGlobalValue(gCP,gTP,gCP->TSymbol_Path,newValue);

/*  Initialize the current symbol type setting */

gCP->TSymbol_SaveTypes = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_saveTypes");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_SaveTypes,TRUE);
newValue.Tag = TYBOLE;
newValue.u.Bool = FALSE;
TSymbol_SetGlobalValue(gCP,gTP,gCP->TSymbol_SaveTypes, newValue);

/*  Initialize the current Workspace */

gCP->TSymbol_Currentworkspace = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_currentViews");
FObject_Perm(gCP,gTP,(TObject*)gCP->TSymbol_Currentworkspace,TRUE);
newValue = gCP->Tval_VOID;
newValue.u.Object = (TObject*)TWorkspace_New(gCP,gTP);
newValue.Tag = TYWORKSPACE;
TSymbol_SetGlobalValue(gCP,gTP,gCP->TSymbol_Currentworkspace, newValue);

/* Register the SmartBase cProcedures contained in this package */
FSmartbase_RegisterCProcedure(gCP, gTP, (LpCHAR)"globalReferences",(LpFUNC)&FSmartbase_GlobalReferences);

/* Register the SmartBase permanent Symbols contained in this package */
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_add = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"+")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_addi = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"addi")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compare = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compare")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compareLT = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareLT")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compareLE = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareLE")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compareEQ = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareEQ")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compareNE = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareNE")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compareGE = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareGE")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_compareGT = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"compareGT")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_div = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"/")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_divi = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"divi")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_divr = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"divr")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_divri = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"divri")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_mul = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"*")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_muli = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"muli")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_new = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"new")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_ref = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"ref")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_set = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"set")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_sub = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"-")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_subi = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"subi")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol___send = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"__send")),TRUE);
FObject_Perm(gCP,gTP,(TObject*)(gCP->TSymbol_eol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_eol")),TRUE);

/*  Initialize Smartbase_ API variables within this program. */

gCP->Tval_FALSE.Tag = TYBOLE;
gCP->Tval_FALSE.u.Bool = FALSE;
gCP->Tval_TRUE.Tag = TYBOLE;
gCP->Tval_TRUE.u.Bool = TRUE;
gCP->Tval_VOID.Tag = TYVOID;
gCP->Tval_VOID.u.Void = 0;
if (gCP->TObject_MaxTypes != (TYMAXVALIDTYPE+1))
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

/*  We set the value of _eol depending upon the host operating system. */

#ifdef _MACOS
ec = FSmartbase_Evals(gCP,gTP,"(define _eol #\\return )",FALSE);
#endif
#ifdef _SUN
ec = FSmartbase_Evals(gCP,gTP,"(define _eol #\\newline)",FALSE);
#endif
#ifdef _LINUX
ec = FSmartbase_Evals(gCP,gTP,"(define _eol #\\newline)",FALSE);
#endif
#ifdef _WIN
ec = FSmartbase_Evals(gCP,gTP,"(define _eol #\\newline)",FALSE);
#endif
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

// *****************************************************
// *****************************************************
//  Register extended function libararies at this point.
//  Note: These must not be registered sooner.
// *****************************************************
// *****************************************************
FFinance_Init(gCP, gTP);

gCP->FMySQL1_Enabled = Funcs->_EmbeddedMySQLEnabled;
if (gCP->FMySQL1_Enabled == TRUE)
	{
	FMySQL1_Init(gCP,gTP,0,NULL);
	}

// *********************************************************************************
// *********************************************************************************
//  Register any remaining C builtin functions required by the Smartbase Engine.
// *********************************************************************************
// *********************************************************************************

ec = FProcedure_NewCProcedure(gCP,gTP,&aSymbol,(LpCHAR)"preAllocateFixedMemoryBlocks",(LpFUNC)&FMemory_PreAllocateFixedBlock);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);


// *********************************************************************************
// *********************************************************************************
//  Register any remaining Lisp builtin functions required by the Smartbase Engine.
// *********************************************************************************
// *********************************************************************************


// *********************************************************************************
// "loadWorkspace" Loads a previously saved workspace (see AIS online documentation)
// *********************************************************************************
strcpy(script,	"(defun loadWorkspace (filename)\n"); 
strcat(script,	"   vars:(fileID)\n");  
strcat(script,	"  (if (isNumber filename)\n");  
strcat(script,	"      (setq fileID filename)\n");  
strcat(script,	"      (setq fileID (fileOpen filename 0 3))\n"); 
strcat(script,	"      ) ; end if\n");  
strcat(script,	"  (if (<> 0 fileID)\n");  
strcat(script,	"      (begin\n");  
strcat(script,	"        (setCdr _currentViews true)\n");  
strcat(script,	"        (setq _saveTypes true)\n");  
strcat(script,	"        (setq _currentViews (loadObject fileID))\n");  
strcat(script,	"        (setq _saveTypes false)\n");  
strcat(script,	"        (setCdr _currentViews #void)\n");  
strcat(script,	"        (fileClose fileID 1))\n"); 
strcat(script,	"      ) ; end if\n"); 
strcat(script,	"  ) ; end loadWorkspace");
ec = FSmartbase_Evals(gCP,gTP,script,FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

// **************************************************************************
// "saveWorkspace" Saves the current workspace (see AIS online documentation)
// **************************************************************************
strcpy(script,	"(defun saveWorkspace(filename)\n");
strcat(script,	"   vars:(fileID)\n"); 
strcat(script,	"  (if (isNumber filename)\n");  
strcat(script,	"      (setq fileID filename)\n");  
strcat(script,	"      (setq fileID (fileOpen filename 1 3))\n"); 
strcat(script,	"      ) ; end if\n");  
strcat(script,	"  (if (<> 0 fileID)\n");  
strcat(script,	"      (begin\n");  
strcat(script,	"        (setCdr _currentViews (getSymbolTable 0 1 1))\n");  
strcat(script,	"        (setq _saveTypes true)\n");  
strcat(script,	"        (saveObject fileID _currentViews)\n");  
strcat(script,	"        (setq _saveTypes false)\n");  
strcat(script,	"        (setCdr _currentViews #void)\n");  
strcat(script,	"        (fileClose fileID 1))\n"); 
strcat(script,	"      ) ; end if\n"); 
strcat(script,	"  ) ; end saveWorkspace");
ec = FSmartbase_Evals(gCP,gTP,script,FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);


// *************************************************************************
// "srandom" Generates a pseudo random number (see AIS online documentation)
// *************************************************************************
strcpy(script,	"(defun srandom(n)\n"); 
strcat(script,	"  pvars:((seed 0.0) (prime 2796203.0) (primeNew 2147483647.0) (adjust 0.610699392797))\n"); 
strcat(script,	"  (defun test(m)\n"); 
strcat(script,	"    vars:(rtop rbottom stop sbottom i rmean smean rsq ssq rn sn)\n"); 
strcat(script,	"    (loop for i from 0 until m do \n");
strcat(script,	"	  (setq sn (srandom 1))\n"); 
strcat(script,	"	  (setq rn (random 1))\n"); 
strcat(script,	"	  (+= rmean rn)\n"); 
strcat(script,	"	  (+= smean sn)\n"); 
strcat(script,	"	  (+= rsq (* rn rn))\n"); 
strcat(script,	"	  (+= ssq (* sn sn))\n"); 
strcat(script,	"	  (if (< rn .5) (++ rbottom) (++ rtop))\n"); 
strcat(script,	"	  (if (< sn .5) (++ sbottom) (++ stop))\n"); 
strcat(script,	"	  ) ; end loop\n");
strcat(script,	"	(writeln _eol {Avg = } (/ rmean m) {[} (/ rmean m) {]}\n"); 
strcat(script,	"	              {,Std = } (sqrt (- (/ rsq m) (* (/ rmean m) (/ rmean m)))) {[} (sqrt (- (/ ssq m) (* (/ smean m) (/ smean m)))) {]}\n"); 
strcat(script,	"	              {,Ratio = } (/ rtop rbottom) {[} (/ stop sbottom) {]} ))\n");
strcat(script,	"  ;; Main logic\n"); 
strcat(script,	"  (setq seed (abs (fraction (* (- seed (/ (+ adjust seed) primeNew)) prime))))\n"); 
strcat(script,	"  (* (fraction seed) n)) ;; end of srandom\n"); 
ec = FSmartbase_Evals(gCP,gTP,script,FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

// *******************************************************************************
// "fileReadAll" Generates the fileReadAll function (see AIS online documentation)
// *******************************************************************************
strcpy(script,	"(defun fileReadAll(fileName)\n"); 
strcat(script,	"  vars:(fileID data (type 0))\n"); 
strcat(script,	"  (setq fileID (fileOpen fileName 0 type))\n"); 
strcat(script,	"  (setq data (fileRead fileID))\n"); 
strcat(script,	"  (fileClose fileID 1)\n");
strcat(script,	"  data)\n"); 
ec = FSmartbase_Evals(gCP,gTP,script,FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

// *********************************************************************************
// "fileWriteAll" Generates the fileWriteAll function (see AIS online documentation)
// *********************************************************************************
strcpy(script,	"(defun fileWriteAll(fileName data)\n"); 
strcat(script,	"  vars:(fileID self (type 0))\n"); 
strcat(script,	"  (setq fileID (fileOpen fileName 1 type))\n"); 
strcat(script,	"  (setq self (fileWrite fileID data))\n"); 
strcat(script,	"  (fileClose fileID 1)\n");
strcat(script,	"  true)\n"); 
ec = FSmartbase_Evals(gCP,gTP,script,FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

// *******************************************************************
// "sleep" Generates the sleep function (see AIS online documentation)
// *******************************************************************
strcpy(script, "(defun sleep(milleseconds)\n");
strcat(script, "  regs:(Number:start Number:end Number:elapsed Number:ticks)\n");
strcat(script, "  (setq ticks (number (/ milleseconds 1000.0)))\n");
strcat(script, "  (setq start (getTickCount 0))\n");
strcat(script, "  (while (< (setq elapsed (getTickCount start)) ticks) do \n");
strcat(script, "     (checkEscape)\n");
strcat(script, "     )\n");
strcat(script, "  elapsed)\n");
ec = FSmartbase_Evals(gCP, gTP, script, FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP, gTP, FSMARTBASE_ERR_INVALID);

// ****************************************************************************************
// "super" Sends a message to the parent class of the object (see AIS online documentation)
// ****************************************************************************************
strcpy(script, "(defun super(msg obj ...)\n");
strcat(script, "  regs:(n N)\n");
strcat(script, "  vars:(argList fun)\n");
strcat(script, "  (setq argList(new Vector: 1 obj))\n");
strcat(script, "  (setq N (argCount))\n");
strcat(script, "  (loop for n from 2 until N do (setq argList[(- n 1)] (argFetch n)))\n");
strcat(script, "  (setq fun (cdr (methodsOf obj))[In:].parent.In.methods[msg])\n");
strcat(script, "  (apply fun argList))\n");
ec = FSmartbase_Evals(gCP, gTP, script, FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP, gTP, FSMARTBASE_ERR_INVALID);

// **********************************************************************************************
// "class" Returns the class name of specified Lambda or Structure (see AIS online documentation)
// **********************************************************************************************
strcpy(script, "(defun class(arg)\n");
strcat(script, "  (if (isSymbol arg) then (setq arg (getGlobalValue arg)))\n");
strcat(script, "  (if (isLambda arg) then (return arg.In.class))\n");
strcat(script, "  (if (isStructure arg) then (return (cdr (methodsOf arg))[In:].class))\n");
strcat(script, "  #void)\n");
ec = FSmartbase_Evals(gCP, gTP, script, FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP, gTP, FSMARTBASE_ERR_INVALID);

// *******************************************************************************************************************
// "parent" Returns the class name of parent class of the specified Lambda or Structure (see AIS online documentation)
// *******************************************************************************************************************
strcpy(script, "(defun parent(arg)\n");
strcat(script, "  (if (isSymbol arg) then (setq arg (getGlobalValue arg)))\n");
strcat(script, "  (if (isLambda arg) then (return arg.In.parent.In.class))\n");
strcat(script, "  (if (isStructure arg) then (return (cdr (methodsOf arg))[In:].parent.In.class))\n");
strcat(script, "  #void)\n");
ec = FSmartbase_Evals(gCP, gTP, script, FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP, gTP, FSMARTBASE_ERR_INVALID);

// ***************************************************************************************************************************************
// "isClass" Returns true iff the specified class name is equal to one of the classes in the ancestor chain (see AIS online documentation)
// ***************************************************************************************************************************************isClass
strcpy(script, "(defun isClass(arg ...)\n");
strcat(script, "  vars:(name cname)\n");
strcat(script, "  (if (> (argCount) 1) then (setq name (symbol (argFetch 1))))\n");
strcat(script, "  (if (isSymbol arg) then (setq arg (getGlobalValue arg)))\n");
strcat(script, "  (if (isLambda arg) then (setq cname arg.In.class))\n");
strcat(script, "  (if (isStructure arg) then (setq cname (cdr (methodsOf arg))[In:].class))\n");
strcat(script, "  (if (= name #void) then (return (<> cname #void)))\n");
strcat(script, "  (while (<> cname #void) do\n");
strcat(script, "    (if (= cname name) (return true))\n");
strcat(script, "    (setq cname (parent cname)))\n");
strcat(script, "  false)\n");
ec = FSmartbase_Evals(gCP, gTP, script, FALSE);
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP, gTP, FSMARTBASE_ERR_INVALID);



/*  We must lock globals to preserve all builtin functions created during initialization. */

ec = FSmartbase_Eval(gCP,gTP,TGVALUE("lock"),1,TGVALUE("_globals"));
if (ec.Tag == TYERROR) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_INVALID);

FMemory_SystemDiagnostic(gCP,gTP,1,&gCP->Tval_FALSE);

gTP->SessionID = 0;
gTP->DebugJitON = TRUE;
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
gTP->busySW = FALSE;

return(gCP);   /* Return the pointer to the initialized context structure */
}

/* TLW Writes to the SysLog.txt file in the install directory for debugging only */
void FSmartbase_Log(LpXCONTEXT gCP, LpTHREAD gTP, char* ipMsg)
{
	/* Append this message to the log file */
	FILE *apFile;
	if ((apFile  = fopen("SysLog.txt", "a")) != NULL)
	{	fwrite(ipMsg, 1, strlen(ipMsg), apFile);
		fclose(apFile);
	}
	/* Throw an exception if the linked list of memory headers has been trampled. */
	FMemory_CheckMemoryBlocks(gCP, gTP);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_CalculateStackSpace
Calculate the required stack size based on the amount of context memory requested by user.

Note:   This function is called by the host application prior to creating the thread the
		context will run on. It is also called by FSmartbase_ContextInit
#endif

NUM FSmartbase_CalculateStackSpace(NUM memorySize) {
	NUM stackSize;
	stackSize = min(20000000,memorySize);
	stackSize = max(stackSize,memorySize/10);
	return stackSize;
	}



/* ************************************************************************************************************/
/* *** Intra Context Function Declarations ********************************************************************/
/* ************************************************************************************************************/


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_InitTVALArray

Return a pointer to a contiguous array of garbage-collection-protected TVALs on the
thread TvalStack.

#endif

TVAL* FSmartbase_InitTVALArray(LpXCONTEXT gCP,LpTHREAD gTP,NUM* stackIndexPtr,const NUM arraySize) 
{
TVAL* stackArrayPtr;
NUM	  stackIndex;

gCP = gCP; // NOOP to hide unused parameter warning message
stackIndex = *stackIndexPtr;
stackArrayPtr = (TVAL*)&gTP->TvalStack[stackIndex];
stackIndex += arraySize;
*stackIndexPtr = stackIndex;
return(stackArrayPtr);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_InitOBJArray

Return a pointer to a contiguous array of garbage-collection-protected TVALs on the
thread TvalStack.

#endif

OBJ** FSmartbase_InitOBJArray(LpXCONTEXT gCP,LpTHREAD gTP,NUM* stackIndexPtr,OBJ* variables[],const NUM arraySize) 
{
NUM			arrayIndex;
NUM			stackIndex;

gCP = gCP; // NOOP to hide unused parameter warning message
stackIndex = *stackIndexPtr;
for (arrayIndex = 0; arrayIndex < arraySize; ++arrayIndex)                 
    {                                                           
    variables[arrayIndex] =  NIL;                                      
    gTP->ObjStack[stackIndex] =	(OBJ*)&variables[arrayIndex];					
    stackIndex++;                          
    }                                                           
*stackIndexPtr = stackIndex;
return((OBJ**)&variables[0]);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_Convert

Convert the old value into a value of the target type.

#endif

TVAL FSmartbase_Convert(LpXCONTEXT gCP,LpTHREAD gTP,const TYPE targetType, const TVAL oldValue)
{
LpFCONVERT      aFunction;

aFunction = (LpFCONVERT)_TObject_TypeConvert(targetType);

return((*aFunction)(gCP,gTP,targetType,oldValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Evals

Compile and evaluate a SmartLisp formula. Return the result to the caller.

Arguments:

theScript           A C string containing the AISLisp formula to be interpreted.
                    The string will be copied to an internal handle so that even
                    temporary strings may be evaluated.

print               Determines whether or not the result value is to be displayed
                    on the SmartBase Console (TRUE = display, FALSE = no display).

Return Argument:

                    The tagged value resulting from interpreting the AISLisp
                    formula.


#endif

TVAL FSmartbase_Evals(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theScript,const BOLE print)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame

/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_CheckFrame */
/*          macro at the end of this function. */
_TObject_RecordFrame

/*  Turn off the error switch so the system will know to replace */
/*  the _error call tree in the unlikely event of an error. */

gTP->TObject_ErrorSwt = FALSE;
    
/*  Interpret all command expressions with a full compilation. */

*ret = TSTRING(theScript);
*ret =  FLisp_Lisp(gCP,gTP,1,ret);
if (isERROR(ret)) goto LongReturn;
*ret =  FCompile_Compile(gCP,gTP,1,ret);
if (isERROR(ret)) goto LongReturn;
*ret =  FSmartbase_Eval(gCP,gTP,*ret,0);

/* Make sure the temporary result stays around until the next evaluation. */

LongReturn:

aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)"_currentResult");
TSymbol_SetGlobalValue(gCP,gTP,aSymbol, *ret);
if (print) FConio_print(gCP, gTP, *ret);
    
/*  Make sure we leave the stack and frame in the same condition */
/*  as when we entered this function. */
/*  Note:   This macro is paired with the _TObject_RecordFrame */
/*          macro at the start of this function. */
_TObject_CheckFrame

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Writeln

Display the argument list on the Console window. Return the result to the caller.

Arguments:

argc                The number of arguments in the argument list.

argv                The optional argument list.

Return Argument:

                    The value of the displaying the argument list.


Note:   The result is not protected from garbage collection as in FSmartbase_Evals!!

#endif

TVAL FSmartbase_Writeln(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,...)
{
va_list         args;
NUM             i;
StartFrame
DeclareTVALArray(argv,_FSmartbase_MAXARGUMENTS);
DeclareTVAL(ret);
EndFrame

/* Gather the variable arguments into an array. */

if (argc > __MAXPARMS) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
va_start(args,argc);
for (i = 0; i < argc; i++)
    {
    argv[i] = va_arg(args,TVAL);
    }
va_end(args);
    

/*  Display the specified argument list. */
    
*ret = FConio_Writeln(gCP,gTP,argc,&argv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Eval

Evaluate a procedure against and argument list. Return the result to the caller.

Arguments:

argc                The procedure to evaluate against the argument list.

argc                The number of arguments in the argument list.

argv                The optional argument list.

Return Argument:

                    The value of the evaluating the procedure against the argument list.


Note:   For speed of execution, this code has been duplicated in several
        locations. Any change to this code should also be accompanied by
        similar changes in the all locations, which currently are: 

            FSmartbase_Eval
            FSmartbase_Evalv
            FVmScript_Eval

Note:   The result is not protected from garbage collection as in FSmartbase_Evals!!

#endif

TVAL FSmartbase_Eval(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL proc,const NUM argc,...)
{
#define         MaxParms    50
LpTVAL          argv;
NUM             i;
va_list         args;
NUM             __tvalf__;
NUM             __objsf__;
NUM             __recur__;
StartFrame
DeclareTVALArray(argp,MaxParms+1);
DeclareTVAL(ret);
EndFrame

/* Gather the variable arguments into an array.  On risc */
/* machines, because they are register machines, we must. */
/* use the ansii standard arg_list approach. On cisc stack */
/* machines we just pass the variable arguments address. */

if (argc > __MAXPARMS) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
va_start(args,argc);
for (i = 0; i < argc; i++)
    {
    argp[i] = va_arg(args,TVAL);
    }
va_end(args);
argv = &argp[0];   

/*  Evaluate the specified procedure against the argument list. */
    
__tvalf__ = gTP->TvalStackIdx;
__objsf__ = gTP->ObjStackIdx;
__recur__ = gTP->RecursionCount;
Evaluate:
switch (proc.Tag)
    {
    case TYCPROCEDURE:
        *ret = (*(asSymbol(&proc)->itsCProcedure))(gCP,gTP,argc,argv);
        break;

    case TYCFUNCTION:
        *ret = (*asFunction(&proc))(gCP,gTP,argc,argv);     
        break;

    case TYLAMBDA:
        *ret = _VmEvaluate(asProcedure(&proc),argc,argv);
        break;

    case TYCONTINUATION:
        *ret = TContinuation_Evaluate(gCP,gTP,proc,argc,argv);
        break;

    case TYMACRO:
        *ret = _VmEvaluate(asProcedure(&proc),argc,argv);
        break;

    case TYERROR:
        *ret = proc;
        break;

    default:
        *ret = TERROR("!eval: Missing or invalid function!");
        break;
    }

gTP->TvalStackIdx = __tvalf__;
gTP->ObjStackIdx = __objsf__;
gTP->RecursionCount = __recur__;
FrameExit(*ret);
#undef          MaxParms
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_Evalv

Evaluate a procedure against and argument list. Return the result to the caller.

Arguments:

argc                The procedure to evaluate against the argument list.

argc                The number of arguments in the argument list.

argv                The vectored argument list.

Return Argument:

                    The value of the evaluating the procedure against the argument list.


Note:   For speed of execution, this code has been duplicated in several
        locations. Any change to this code should also be accompanied by
        similar changes in the all locations, which currently are: 

            FSmartbase_Eval
            FSmartbase_Evalv
            FVmScript_Eval

Note:   The result is not protected from garbage collection as in FSmartbase_Evals!!

#endif

TVAL    FSmartbase_Evalv(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL proc, const NUM argc, const TVAL argv[])
{
#define         MaxParms    50
NUM             __tvalf__;
NUM             __objsf__;
NUM             __recur__;
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Evaluate the specified procedure against the argument list. */
    
__tvalf__ = gTP->TvalStackIdx;
__objsf__ = gTP->ObjStackIdx;
__recur__ = gTP->RecursionCount;
switch (proc.Tag)
    {
    case TYCPROCEDURE:
        *ret = (*(asSymbol(&proc)->itsCProcedure))(gCP,gTP,argc,(LpTVAL)argv);
        break;

    case TYCFUNCTION:
        *ret = (*asFunction(&proc))(gCP,gTP,argc,(LpTVAL)argv);     
        break;

    case TYLAMBDA:
        *ret = _VmEvaluate(asProcedure(&proc),argc,(LpTVAL)argv);
        break;

    case TYCONTINUATION:
        *ret = TContinuation_Evaluate(gCP,gTP,proc,argc,(LpTVAL)argv);
        break;

    case TYMACRO:
        *ret = _VmEvaluate(asProcedure(&proc),argc,(LpTVAL)argv);
        break;

    case TYERROR:
        *ret = proc;
        break;

    default:
        *ret = TERROR("!eval: Missing or invalid function!");
        break;
    }

gTP->TvalStackIdx = __tvalf__;
gTP->ObjStackIdx = __objsf__;
gTP->RecursionCount = __recur__;
FrameExit(*ret);
#undef          MaxParms
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Ref

Return the value of the argument list. Invoking FSmartbase_Ref is the equivalent of
invoking the (ref ...) procedure within a SmartLisp expression.

Arguments:

argc                The number of arguments in the argument list.

arg ...             The optional direct argument list.

Return Argument:

                    The value of the argument list.


#endif

TVAL FSmartbase_Ref(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... )
{
va_list         args;
NUM             i;
StartFrame
DeclareTVALArray(argv,_FSmartbase_MAXARGUMENTS);
DeclareTVAL(ret);
EndFrame

/* Gather the variable arguments into an array. */

if (argc > __MAXPARMS) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
va_start(args,argc);
for (i = 0; i < argc; i++)
    {
    argv[i] = va_arg(args,TVAL);
    }
va_end(args);
    

/*  Reference the specified object using the argument list. */
    
*ret = FUtil2_Ref(gCP,gTP,argc,&argv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Refv

Return the value of the argument list. Invoking FSmartbase_Refv is the equivalent of
invoking the (ref ...) procedure within a SmartLisp expression.

Arguments:

argc                The number of arguments in the argument list.

argv                The argument list.

Return Argument:

                    The value of the argument list.


#endif

TVAL FSmartbase_Refv(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,const TVAL argv[])
{
return(FUtil2_Ref(gCP,gTP,argc,(LpTVAL)argv));
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Set

Set a value using the argument list. Invoking FSmartbase_Set is the equivalent of
invoking the (set ...) procedure within a SmartLisp expression.

Arguments:

argc                The number of arguments in the argument list.

arg ...             The optional direct argument list.

Return Argument:

                    The value after setting using the argument list.


#endif

TVAL FSmartbase_Set(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... )
{
va_list         args;
NUM             i;
StartFrame
DeclareTVALArray(argv,_FSmartbase_MAXARGUMENTS);
DeclareTVAL(ret);
EndFrame

/* Gather the variable arguments into an array. */

if (argc > __MAXPARMS) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
va_start(args,argc);
for (i = 0; i < argc; i++)
    {
    argv[i] = va_arg(args,TVAL);
    }
va_end(args);
    

/*  Set the specified object using the argument list. */
    
*ret = FDefine_Set(gCP,gTP,argc,&argv[0]);
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_Setv

Set a value using the argument list. Invoking FSmartbase_Setv is the equivalent of
invoking the (set ...) procedure within a SmartLisp expression.

Arguments:

argc                The number of arguments in the argument list.

argv                The argument list.

Return Argument:

                    The value after setting using the argument list.


#endif

TVAL FSmartbase_Setv(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,const TVAL argv[])
{
return(FDefine_Set(gCP,gTP,argc,(LpTVAL)argv));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_MakeCFunction

The FSmartbase_MakeCFunction function converts the specified C function into an eval
callable tval

Note:   TYCFUNCTION functions must be of type LpFUNC, and must have the following
        standard argument list:
        
            LpFUNC(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

#endif

TVAL FSmartbase_MakeCFunction(LpXCONTEXT gCP,LpTHREAD gTP,const LpFUNC lpFunc)
{
StartFrame
DeclareTVAL(retValue);
EndFrame

retValue->Tag = TYCFUNCTION;
retValue->u.Pointer = (POINTER)lpFunc;

FrameExit(*retValue);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_MarkAndSweep

#endif

void FSmartbase_MarkAndSweep(LpXCONTEXT gCP,LpTHREAD gTP)
{
TObject_MarkAndSweep(gCP,gTP);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_Throw

Allow us to more easily trace a throw.

#endif

TVAL FSmartbase_Throw(LpXCONTEXT gCP,LpTHREAD gTP,const NUM code)
{
#define			errCantLaunch       20000
#define			errFileRead         20001
#define			errFileWrite        20002
#define			errTooManyFiles     20003
#define			errCantLockFile     20004
#define			errCantUnlockFile   20005
NUM				indexOf;
TLambda*		Lambda;
NUM				oldEscapeSW;

/*  Make sure the context is reset NOT to show a garbage collect in progress. */
gCP->TObject_GarbageCollectInProgress = FALSE;

/*  Make sure the debugger compilation variables are reset to reflect the throw. */
gTP->FCompile_DebugSourceVector = NIL;
gTP->FCompile_DebugListLines = NIL;
gTP->FCompile_DebugInterfaces = gCP->Tval_VOID;
gTP->FCompile_DebugCurrentSourceLine = gCP->Tval_VOID;
gTP->FCompile_DebugCurrentSourceLineIndex = gCP->Tval_VOID;

/*  If there is a system error handler, clear the error call tree. */
/*  Record this system error. */
gCP->FSmartbase_errorSym->itsGlobalValue = gCP->FSmartbase_sysError;

/*  Set all of the reachable Lambda objects to inactive. */
for (indexOf = 1; indexOf < gCP->TObject_MaxObjectCount; ++indexOf)
    {
    if ((_TObject_ObjectFlag(indexOf) != _TObject_OfVOID) && (_TObject_ObjectByIndex(indexOf)->itsObjectType == TYLAMBDA))
        {
        Lambda = (TLambda*)_TObject_ObjectByIndex(indexOf);
        Lambda->InUse = 0;
        }
    }

/*  Manage the system error based upon the error code argument. */
switch (code)
    {
    case errCantLaunch:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Can not launch this object!");
    break;

    case errFileRead:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: File read I/O error!");
    break;

    case errFileWrite:
    strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: File write I/O error!");
    break;

    case errTooManyFiles:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Too Many Files Open error!");
    break;

    case errCantLockFile:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Can't Lock File error!");
    break;

    case errCantUnlockFile:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Can't Unlock File error!");
    break;

    case FSMARTBASE_ERR_OUT_OF_MEMORY:
    strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Out of memory condition!");
    break;

    case FSMARTBASE_ERR_OUT_OF_MEMORY_FRAGMENTED:
    strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Fragmented or low memory condition!");
    break;

    case FSMARTBASE_ERR_TOO_MANY_OBJECTS:
    strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Too many objects!");
    break;

    case FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE:
    strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Memory request exceeded maximum block size!");
    break;

    case FSMARTBASE_ERR_FRAME_ERROR:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Garbage frame overflow!");
    break;

    case FSMARTBASE_ERR_INVALID:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Invalid memory handle!");
    break;

    case FSMARTBASE_ERR_STACK:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Lisp stack overflow!");
    break;

    case FSMARTBASE_ERR_ESCAPE:
	case FSMARTBASE_ERR_QUIT:	
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: User requested escape condition!");
    break;

    case FSMARTBASE_ERR_PCODE:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Invalid pcode append attempted!");
    break;

    case FSMARTBASE_ERR_BAD_DATATYPE:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Bad type found in system!");
    break;

    case FSMARTBASE_ERR_RECURSION:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Exceeded Lisp recursion limit!");
    break;

    case FSMARTBASE_ERR_FRAME_RELEASE:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Garbage frame release failure!");
    break;

    case FSMARTBASE_ERR_STACK_RELEASE:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Lisp stack release failure!");
    break;

    case FSMARTBASE_ERR_RECURSION_RELEASE:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Lisp recursion imbalance!");
    break;

    case FSMARTBASE_ERR_WRONG_VERSION:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Lambda repository found wrong disk file version!");
    break;

    case FSMARTBASE_ERR_ENGINE_BUSY:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: SmartBase Engine Busy!");
    break;

    case FSMARTBASE_ERR_REPOSITORY_GC:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Lambda repository found garbage collection error!");
    break;

    default:
	strcpy(ErrorArray(gCP->FSmartbase_sysError),"!System Error: Unknown catastrophic error condition!");
    break;
    }

/*  Turn all debugger settings off. */
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = FALSE;
oldEscapeSW = gTP->escapeSW;

// Display the error message.
// Note: Without using extra stack recursions in case this was a recursion limit error!
(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)"\n", 0);
(*gCP->_Host_Display)((POINTER)gCP,gTP,(char *)(LpCHAR)ErrorArray(gCP->FSmartbase_sysError), 0);

FProcedure_FailureReset(gCP,gTP);   /*  Reset the Procedure switches. */
//_TvalStackReset()					/*  Reset the Thread Tval Stack.  */
//ResetRecursion;					/*  Reset the Recursion Counter.  */

gTP->escapeSW = oldEscapeSW;
_FSmartbase_Throw(code);            /*  Reset the Host Machine Stack. */
/* return(gCP->Tval_VOID); This blows up VC7? */
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromPointer

Convert from a Pointer into a TVAL.

#endif

TVAL FSmartbase_CnvFromPtr(LpXCONTEXT gCP,LpTHREAD gTP,const POINTER inputPtr)
{
TVAL        ret;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
ret.Tag = TYPOINTER;
ret.u.Pointer = inputPtr;

return(ret);
}



/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromBool

Convert from a boolean into a TVAL.

#endif

TVAL FSmartbase_CnvFromBool(LpXCONTEXT gCP,LpTHREAD gTP,const BOLE inputBool)
{
TVAL        ret;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
ret.Tag = TYBOLE;
ret.u.Bool = inputBool;

return(ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromChar

Convert from a character into a TVAL.

#endif

TVAL FSmartbase_CnvFromChar(LpXCONTEXT gCP,LpTHREAD gTP,const CHAR inputChar)
{
TVAL        ret;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
ret.Tag = TYCHAR;
ret.u.Char = inputChar;

return(ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromInt

Convert from a integer into a TVAL.

#endif

TVAL FSmartbase_CnvFromInt(LpXCONTEXT gCP,LpTHREAD gTP,const NUM inputInt)
{
TVAL        ret;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
ret.Tag = TYNUM;
ret.u.Int = inputInt;

return(ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromObj

Convert from a TObject* into a TVAL.

#endif

TVAL FSmartbase_CnvFromObj(LpXCONTEXT gCP,LpTHREAD gTP,const OBJ obj)
{
StartFrame
DeclareTVAL(ret);
EndFrame

if (obj == NIL)
    {
    *ret = gCP->Tval_VOID;
    ret->u.Object = NIL;
    }
else
    {
    ret->Tag = ((TObject*)obj)->itsObjectType;
    ret->u.Object = (TObject*)obj;
    }

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromReal

Convert from a real into a TVAL.

#endif

TVAL FSmartbase_CnvFromReal(LpXCONTEXT gCP,LpTHREAD gTP,const REAL inputReal)
{
TVAL        ret;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
ret.Tag = TYREAL;
ret.u.Real = inputReal;

return(ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvToFrame

Convert from a real into a TVAL.

#endif

TVAL FSmartbase_CnvToFrame(LpXCONTEXT gCP,LpTHREAD gTP,const REAL inputReal)
{
TVAL        ret;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
ret.Tag = TYFRAME;
ret.u.Real = inputReal;

return(ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvToSymbol

Convert from a C string into a symbol TVAL.

#endif

TVAL FSmartbase_CnvToSymbol(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputSymbol)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame

/*  Locate an old symbol or create a new symbol. */

*ret = gCP->Tval_VOID;
aSymbol = TSymbol_MakeUnique(gCP,gTP,inputSymbol);
ret->Tag = TYSYMBOL;
ret->u.Object = (TObject*)aSymbol;

/*  Make sure any new symbols stay around. */

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvToQSymbol

Convert from a C string into a quoted symbol TVAL.

#endif

TVAL FSmartbase_CnvToQSymbol(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputSymbol)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame

/*  Locate an old symbol or create a new symbol. */
*ret = gCP->Tval_VOID;
aSymbol = TSymbol_MakeUnique(gCP,gTP,inputSymbol);
ret->Tag = TYQUOTEDSYMBOL;
ret->u.Object = (TObject*)aSymbol;
asQuoteCnt(ret) = 1;

/*  Make sure any new quoted symbols stay around. */

if (aSymbol->itsGlobalValue.Tag == TYVOID)
    aSymbol->itsGlobalValue = *ret;

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromText

Convert from a c string into a TVAL.

#endif

TVAL FSmartbase_CnvFromText(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputText)
{
TVAL		empty;

if (inputText == NULL) 
	{
	empty.Tag = TYTEXT;
	empty.u.Text[0] = 0;
	return(empty);
	}

return(TObject_CnvFromText(gCP, gTP, inputText));
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvFromSubstring

Convert from a c substring into a TVAL.

#endif

TVAL FSmartbase_CnvFromSubstring(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputText,const NUM start,const NUM length)
{
StartFrame
DeclareTVAL(string);
EndFrame

/* Is this small enough to be a text type? */

if (length < (MAXTVALTEXTLEN-1))
	{
	string->Tag = TYTEXT;
	strncpy(&string->u.Text[0],&inputText[start],length);
	string->u.Text[length] = 0;
	}
else
	{
	string->u.Object = (TObject*)TString_SubString_MakeUnique(gCP,gTP,inputText,start,length);
	string->Tag = TYSTRING;
	}

FrameExit(*string);
}

/*--------------------------------------------------------------------------------------- */

#if 0
FSmartbase_CnvToText

Convert from a TVAL to a c string.

#endif

TVAL FSmartbase_CnvToText(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR buf, const NUM maxLen, const TVAL source)
{
return(TObject_CnvToText(gCP, gTP, buf,  maxLen, source));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_DebugOn

The FSmartbase_DebugOn function returns non-zero if we are in debug mode.

#endif

NUM     FSmartbase_DebugOn(LpXCONTEXT gCP,LpTHREAD gTP)
{
gCP = gCP; // NOOP to hide unused parameter warning message
return((NUM)gTP->DebugTraceON);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_GetObjectID

Return the object ID as an integer for the given object. A negative number indicates
an error.

#endif

NUM FSmartbase_GetObjectID(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL objectTval)
{
gTP = gTP; // NOOP to hide unused parameter warning message
if (_TObject_TypeFlag(asTag(&objectTval)) == _TObject_TfTOBJECT)
    {
    return asObject(&objectTval)->itsObjectIndex;
    }
else
    return(0);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_GetSymbolValue

Returns the globalvalue if not void, else returns the symbol itsself. Handles single 
quotes in either syntax `foo foo:.

#endif

TVAL    FSmartbase_GetSymbolValue(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR cStrP)
{
NUM                 len;
NUM                 nameLen = 0;
NUM					indexOf;
CHAR                saveChar;
LpCHAR              cTmpP;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(ret);
EndFrame


/* Check for an object reference #<Typename 28564> */

len = strlen(cStrP) - 1;
indexOf = 0;
if ((len >= 5) &&
	(cStrP[len] == '>') && 
	(cStrP[indexOf] == '#') && 
	(cStrP[indexOf+1] == '<'))
	{
	indexOf += 2;
	/* Try to recognize an object reference. */
	/* Check for the object type name. */
	if (ISSYMBOL((NUM)cStrP[indexOf]))
		{
		nameLen = 0;
		for (; ISSYMBOL((NUM)cStrP[indexOf]); ++indexOf)
			{
			nameLen++;
			}
		}

	/* Must be a type name ending in whitespace. */
	if ((nameLen == 0) || (cStrP[indexOf] != ' '))
		goto CheckForQuotedSymbol;

	/* Ignore any blanks between stype and index. */
	indexOf += 1;
	while (cStrP[indexOf] == 32) indexOf++;

	/* Check for an object index number. */
	*ret = FSmartbase_recNumber(gCP,gTP,cStrP,&indexOf);
	if (ret->Tag != TYBOLE)
		{
		/* An object reference must end with > */
		if (cStrP[indexOf] != '>')
			goto CheckForQuotedSymbol;
		
		indexOf++;
		if ((cStrP[indexOf] > 32) && (cStrP[indexOf] != ')'))
			goto CheckForQuotedSymbol;

		/* Return the converted object reference. */
		*ret = FUtil2_Inspect(gCP,gTP,1,ret);
		FrameExit(*ret);
		}
	} 


/* Look for the two forms of quoted symbols. */
CheckForQuotedSymbol:

saveChar = 0;
if(*cStrP == '\'')
    {
    saveChar = '\'';
    cTmpP = cStrP + 1;
    }
else
    {
    cTmpP = cStrP;
    len = strlen((char*)cTmpP);
    if(cTmpP[len - 1] == ':')
        {
        saveChar = ':';
        cTmpP[len - 1] = 0;
        }
    }

/* Return the global value of all non-quoted symbol whose */
/* global value is not #void. */

if ((aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)cTmpP)))
    {
    if(!saveChar && !isNullTval(&aSymbol->itsGlobalValue))
        *ret = aSymbol->itsGlobalValue;
    else
        {
        asObject(ret) = (TObject*)aSymbol;
        asTag(ret) = aSymbol->itsObjectType;
        }
    }
else
    *ret = gCP->TObject_ERROR_INVALID;


if(saveChar == ':')
    cTmpP[len - 1] = saveChar;
    
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_GetSymbolValuePtr

Returns a pointer to the global value (always a TVAL) of the specified symbol. If the
specified symbol is a constrained symbol, a NIL pointer is returned. This function will
also mark the symbol as permanent (TRUE) or not permanent (FALSE). If permanenet, the
symbol will never be garbage collected.

#endif

LpTVAL    FSmartbase_GetSymbolValuePtr(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR cStrP,const BOLE perm)
{
LpTVAL              pValue = NIL;
StartFrame
DeclareOBJ(TSymbol,aSymbol);
EndFrame

/* Return the symbol object for the specified string. */
/* Set the symbol object as permanent or not permanent. */

aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)cStrP);
FObject_Perm(gCP,gTP,(TObject*)aSymbol,perm);

/* Return a NIL pointer for any linked symbols. */


/* Return a pointer to the global value of the symbol. */

pValue = &aSymbol->itsGlobalValue;

FrameReset;
FrameExit(pValue);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_NilPtr

Returns a Pointer to a static NIL area, so that NIL pointers will never cause machine
address expections. The predefined dummy area is reset to all zero each time in case
any previous user has incorrectly placed data in the dummy area.

#endif

POINTER FSmartbase_NilPtr(void)
{
	static char		dummy[64] = {0};
	
	NUM	i;

	for (i = 0; i < 64; ++i) {dummy[i] = 0;}

	return((POINTER)&dummy[0]);
}




/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_NilHandle

Either returns the specified handle, or returns a Handle to a static NIL area, 
so that NIL handles will never cause machine address expections. The predefined 
dummy area is reset to all zero each time in case any previous user has 
incorrectly placed data in the dummy area.

#endif

HMemory FSmartbase_NilHandle(HMemory handle)
{
	static POINTER	dummyPtr;

	if (handle != NIL)
		{
		return((HMemory)handle);
		}
	else
		{
		dummyPtr = FSmartbase_NilPtr();
		return((HMemory)&dummyPtr);
		}
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_ObjectLen

Returns the Length of the variable data portion of a collection object, Vector, string, etc.

#endif

NUM FSmartbase_ObjectLen(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL anObject)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return((NUM)((anObject->Tag < TYTEXT) ? 0 : ((anObject->Tag == TYTEXT) ? (NUM)strlen(&anObject->u.Text[0]) : (NUM)anObject->u.Lambda->itsMaxItemIndex)));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_ObjectPtr

Returns a Pointer to the variable data portion of a collection object, Vector, String, etc.

#endif

POINTER FSmartbase_ObjectPtr(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL anObject)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return((POINTER)((anObject->Tag < TYTEXT) ? NIL : ((anObject->Tag == TYTEXT) ? (NUM)&anObject->u.Text[0] : (NUM)*anObject->u.Lambda->itsNilArray)));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_VectorLen

Returns the Length of the variable data portion of a Vector, text, string, etc.

#endif

NUM FSmartbase_VectorLen(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL aVector)
{
	return(FSmartbase_ObjectLen(gCP,gTP,(LpTVAL)&aVector));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_VectorPtr

Returns a Pointer to the variable data portion of a Vector, text, string, etc.

#endif

POINTER FSmartbase_VectorPtr(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL aVector)
{
	return(FSmartbase_ObjectPtr(gCP,gTP,(LpTVAL)&aVector));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_StringPtr

Returns the length and a Pointer to the character data portion of a ByteVector, Text, 
String, etc. If the tval is a String, then the function returns true; otherwise, this
function returns false.

#endif

BOLE FSmartbase_StringPtr(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL aString,LpPOINTER sptr,LpNUM slen)
{
static char c[2] = "";

/*  Return a Pointer to the character data for each of the following types. */

switch (aString->Tag)
    {
    case TYERROR:
        *sptr = (POINTER)ErrorArray(*aString);
        *slen = strlen(ErrorArray(*aString));
        return(TRUE);
        break;
    
    case TYSTRING:
        *sptr = (POINTER)CharArray(*aString);
        *slen = strlen(CharArray(*aString));
        return(TRUE);
        break;
    
    case TYSYMBOL:
    case TYQUOTEDSYMBOL:
        *sptr = (POINTER)SymbolArray(*aString);
        *slen = strlen(SymbolArray(*aString));
        return(TRUE);
        break;
    
    case TYBYTEVECTOR:
        *sptr = (POINTER)ByteArray(*aString);
        *slen = strlen(ByteArray(*aString));
        return(TRUE);
        break;
         
    case TYTEXT:
        *sptr = (POINTER)aString->u.Text;
        *slen = strlen(aString->u.Text);
        return(TRUE);
        break;
    
    case TYSTRINGSUBSTR:
        *sptr = (POINTER)TStringSubstringT_GetStringPtr(gCP, gTP, *aString);
        *slen = asSubLen(aString);
        return(TRUE);
        break;

    default:
        *sptr = &c[0];
        *slen = 0;
        return(FALSE);
        break;
    }
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_ObjIDToTval

Converts an Integer Smartbase Object ID (OID) into a TVAL.

This conversion function is most often used to quickly convert a Lambda object's OID
into a callable TVAL for use with the FSmartbase_RunLambda function. This allows C/C++
objects to quickly call a Lisp Lambda whose OID was previously stored.

#endif

TVAL FSmartbase_ObjIDToTval(LpXCONTEXT gCP,LpTHREAD gTP,const NUM oid)
{
	TObject*		object;
	TVAL			result;

	gTP = gTP; // NOOP to hide unused parameter warning message

	if ((oid > 0) && (oid < gCP->TObject_MaxObjectCount) && (_TObject_ObjectFlag(oid) != _TObject_OfVOID))
	{
		object = _TObject_ObjectByIndex(oid);
		if (object->itsObjectIndex == oid)
			{
			result.Tag = object->itsObjectType;
			result.u.Object = (OBJ)object;
			}
		else
			{
			result = gCP->Tval_VOID;
			}
	}

	/* Return the final TVAL to the caller */
	return(result);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_StringLen

Returns the length of the character data portion of a ByteVector, Text, 
String, etc.

#endif

NUM FSmartbase_StringLen(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL aString)
{
NUM			slen;

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
/*  Return a Pointer to the character data for each of the following types. */

switch (aString->Tag)
    {
    case TYERROR:
        slen = strlen(ErrorArray(*aString));
        return(slen);
        break;
    
    case TYSTRING:
        slen = strlen(CharArray(*aString));
        return(slen);
        break;
    
    case TYSYMBOL:
    case TYQUOTEDSYMBOL:
        slen = strlen(SymbolArray(*aString));
        return(slen);
        break;
    
    case TYBYTEVECTOR:
        slen = strlen(ByteArray(*aString));
        return(slen);
        break;
         
    case TYTEXT:
        slen = strlen(aString->u.Text);
        return(slen);
        break;

    case TYSTRINGSUBSTR:
        slen = asSubLen(aString);
        return(slen);
        break;
      
    default:
        slen = 0;
        return(slen);
        break;
    }
}




/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_SetSymbolValue

Sets the globalvalue of the specified symbol.

#endif

TVAL    FSmartbase_SetSymbolValue(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const TVAL newValue)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
EndFrame

/*  Extract the symbol from the argument. */

aSymbol = TSymbol_MakeUnique(gCP,gTP,(LpCHAR)theSource);
    
/*  Set the symbol's global value; */

aSymbol->itsGlobalValue = newValue;
    
FrameExit(newValue);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_NewMemory

Returns a memory block of the specified size.

#endif

HMemory     FSmartbase_NewMemory(LpXCONTEXT gCP,LpTHREAD gTP,const NUM theSize)
{
return FMemory_New(gCP, gTP, theSize, TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_ResizeMemory

Resizes the specified memory block to the specified size.

#endif

HMemory     FSmartbase_ResizeMemory(LpXCONTEXT gCP,LpTHREAD gTP,HMemory theMemory,const NUM theSize)
{
return(FMemory_Resize(gCP, gTP, theMemory,theSize));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_FreeMemory

Frees the specified memory block.

#endif

void    FSmartbase_FreeMemory(LpXCONTEXT gCP,LpTHREAD gTP,HMemory theMemory)
{
FMemory_Free(gCP, gTP, theMemory);
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_SizeOfMemory

Returns the size of the specified memory block.

#endif

NUM     FSmartbase_SizeOfMemory(HMemory theMemory)
{
return(FMemory_SizeOf(theMemory));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_Error

Return the specified message as an error.  

#endif

TVAL FSmartbase_Error(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theMsg)
{
StartFrame
DeclareTVAL(err);
EndFrame

err->u.Error = TError_MakeUnique(gCP,gTP,theMsg);
err->Tag = TYERROR;

FrameExit(*err);
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_Perror

Return the specified message as an error and mark the string as permanent.  

#endif

TVAL FSmartbase_Perror(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theMsg)
{
StartFrame
DeclareTVAL(err);
EndFrame
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_Perror(), Call make Unique\r\n");	*/
err->u.Error = TError_MakeUnique(gCP,gTP,theMsg);
err->Tag = TYERROR;
  
/* DEBUG FSmartbase_Log(gCP, gTP, "FSmartbase_Perror(), Avoid garbage collection\r\n");	*/
FObject_Perm(gCP, gTP, err->u.Object, TRUE);  /* Protect the string from garbage collectin */

FrameExit(*err);
}



/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_ErrorTrace

Return the state of the SmartBase error trace on switch, and possibly set the value of
the SmartBase engine error trace switch.

	toggleSwitch	=	-1		Return the current value of the SmartBase error trace switch. 
	toggleSwitch	=	0		Turn OFF the SmartBase error trace switch. 
	toggleSwitch	=	1		Turn ON the SmartBase error trace switch. 

#endif

NUM FSmartbase_ErrorTrace(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch)
{

StartFrame
DeclareTVAL(arg);
DeclareTVAL(result);
EndFrame

/*	Turn OFF the value of the SmartBase error trace switch. */
if (toggleSwitch == 0)
	{
	*arg = TSYMBOL("off");	
	FDebug_Debug(gCP,gTP,1,arg);
	}

else	
/*	Turn ON the value of the SmartBase error trace switch. */
if (toggleSwitch == 1)
	{
	*arg = TSYMBOL("on");	
	FDebug_Debug(gCP,gTP,1,arg);
	}
	
/*	Return the value of the SmartBase error trace switch. */
*arg = TSYMBOL("getdebugon");
*result = FDebug_Debug(gCP,gTP,1,arg);
EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__;
return(asBool(result));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_InstructionTrace

Return the state of the SmartBase instruction trace on switch, and possibly set the value of
the SmartBase engine instruction trace switch.

	toggleSwitch	=	-1		Return the current value of the SmartBase instruction trace switch. 
	toggleSwitch	=	0		Turn OFF the SmartBase instruction trace switch. 
	toggleSwitch	=	1		Turn ON the SmartBase instruction trace switch. 

#endif

NUM FSmartbase_InstructionTrace(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch)
{
StartFrame
DeclareTVAL(arg);
DeclareTVAL(result);
EndFrame
//*args = FSmartbase_Eval(gCP,gTP,TGVALUE("new"),2,TSYMBOL("Vector"),TINT(1));	

/*	Turn OFF the value of the SmartBase instruction trace switch. */
if (toggleSwitch == 0)
	{
	//FSmartbase_Set(gCP,gTP,3,*args,TINT(0),TSYMBOL("traceoff"));
	*arg = TSYMBOL("traceoff");
	FDebug_Debug(gCP,gTP,1,arg);
	}
else	
/*	Turn ON the value of the SmartBase instruction trace switch. */
if (toggleSwitch == 1)
	{
//	FSmartbase_Set(gCP,gTP,3,*args,TINT(0),TSYMBOL("traceon"));
	*arg = TSYMBOL("traceon");
	FDebug_Debug(gCP,gTP,1,arg);
	}
	
/*	Return the value of the SmartBase instruction trace switch. */
//FSmartbase_Set(gCP,gTP,3,*args,TINT(0),TSYMBOL("gettraceon"));
*arg = TSYMBOL("gettraceon");
*result = FDebug_Debug(gCP,gTP,1,arg);
EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__;
return(asBool(result));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_SystemCheck

Return the state of the SmartBase system check on switch, and possibly set the value of
the SmartBase engine system check switch.

	toggleSwitch	=	-1		Return the current value of the SmartBase system check switch. 
	toggleSwitch	=	0		Turn OFF the SmartBase system check switch. 
	toggleSwitch	=	1		Turn ON the SmartBase system check switch. 

#endif

NUM FSmartbase_SystemCheck(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch)
{
StartFrame
DeclareTVAL(arg);
DeclareTVAL(result);
EndFrame

/*	Turn OFF the value of the SmartBase system check switch. */
if (toggleSwitch == 0)
	{
	*arg = TSYMBOL("checkoff");	
	FDebug_Debug(gCP,gTP,1,arg);
	}
else	
/*	Turn ON the value of the SmartBase system check switch. */
if (toggleSwitch == 1)
	{
	*arg = TSYMBOL("checkon");	
	FDebug_Debug(gCP,gTP,1,arg);
	}
	
/*	Return the value of the SmartBase error trace switch. */
*arg = TSYMBOL("getcheckon");	
*result = FDebug_Debug(gCP,gTP,1,arg);
EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__;
return(asBool(result));
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_Jit

Return the state of the SmartBase jit on switch, and possibly set the value of
the SmartBase engine jit on switch.

	toggleSwitch	=	-1		Return the current value of the SmartBase jit on switch. 
	toggleSwitch	=	0		Turn OFF the SmartBase jit on switch. 
	toggleSwitch	=	1		Turn ON the SmartBase jit on switch. 

#endif

NUM FSmartbase_Jit(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch)
{
StartFrame
DeclareTVAL(arg);
DeclareTVAL(result);
EndFrame

/*	Turn OFF the value of the SmartBase jit on switch. */
if (toggleSwitch == 0)
	{
	*arg = TSYMBOL("jitoff");	
	FDebug_Debug(gCP,gTP,1,arg);
	}
else	
/*	Turn ON the value of the SmartBase jit on switch. */
if (toggleSwitch == 1)
	{
	*arg = TSYMBOL("jiton");	
	FDebug_Debug(gCP,gTP,1,arg);
	}
	
/*	Return the value of the SmartBase error trace switch. */
*arg = TSYMBOL("getjiton");	
*result = FDebug_Debug(gCP,gTP,1,arg);
EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__;
return(asBool(result));
}


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_SetEngineFlags

Return the state of the SmartBase engine flags, and possibly set the value of
the SmartBase engine flags

	engineflags	=	-1		Return the current value of the SmartBase engine flags
	engineflags				see #defines in FSmartbase.h:
							_FSmartbase_DEBUGON,	_FSmartbase_TRACEON
							_FSmartbase_SYSCHECKON, _FSmartbase_JITON
#endif

#if 0
// This implementation has potential bugs because of the StartFrame and EndFrame without
// FrameExit ending the function.
NUM FSmartbase_SetEngineFlags(LpXCONTEXT gCP,LpTHREAD gTP,const NUM engineflags)
{
StartFrame
DeclareTVAL(arg);
DeclareTVAL(result);
EndFrame
								
/*	Set the smartbase engine flags according to the or'd flags*/
if (engineflags >= 0)
	{
	*arg = TINT(engineflags);	
	FDebug_Debug(gCP,gTP,1,arg);
	}
	
/*	Return the value of the SmartBase engine flags. */
*arg = TSYMBOL("getstate");	
*result = FDebug_Debug(gCP,gTP,1,arg);
EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__;
return(asInt(result));
}
#endif

#if 1
NUM FSmartbase_SetEngineFlags(LpXCONTEXT gCP,LpTHREAD gTP,const NUM engineflags)
{
TVAL arg;
TVAL result;
								
/*	Set the smartbase engine flags according to the or'd flags*/
if (engineflags >= 0)
	{
	arg = TINT(engineflags);	
	FDebug_Debug(gCP,gTP,1,&arg);
	}
	
/*	Return the value of the SmartBase engine flags. */
arg.Tag = TYTEXT;
/* gcc's built-in strcpy implementation detects buffer overflow. */
strncpy(arg.u.Text, "getstate", 8);
arg.TextOverflow = 0;
result = FDebug_Debug(gCP,gTP,1,&arg);
return(asInt(&result));
}
#endif


/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_recNumber

Recognize a numeric constant in a SmartLisp source string.

Note:   If the number is terminated with a % sign, treat the number
        as a percentage. 

#endif

TVAL  FSmartbase_recNumber(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const LpNUM iChar)
{
LpCHAR              sp;
NUM                 count;
NUM                 m;
NUM                 n;
NUM                 sn;
REAL                x;
REAL                y;
REAL                e;
REAL                f;
REAL                sign;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(isInt);
DeclareTVAL(isReal);
EndFrame

/* Initialize. */

sp = &theSource[*iChar];                
m  = 0;

/* Test for signed quantity.  */

if (*sp == '-')
    {
    sign = -1.0;
    ++m;
    ++sp;
    }
else
if (*sp == '+')
    {
    sign = 1.0;
    ++m;
    ++sp;
    }
 else
    sign = 1.0;
 
/* A real numeric constant may begin with an integer.  */

*isInt = FProcedure_recInt(gCP,gTP,&x,&count,sp);
if (asBool(isInt) == TRUE)
    sp += count;
else
    x = 0.0;

/* All real constants are sandwiched with a '.'.  */

if (*sp == '.')
    {
    ++count;
    *isReal = FProcedure_recFrac(gCP,gTP,&y,&n,++sp);
    if(asBool(isReal) == TRUE)
        {
        sp += n;
        count += n;
        x += y;
        if ((*sp == 'e') || (*sp == 'E'))
            {
            ++count;
            sn = 1.0;
            if (*(++sp) == '-')
                {
                ++count;
                sn = -1.0;
                ++sp;
                }
            else
            if (*sp == '+')
                {
                ++count;
                sn = 1.0;
                ++sp;
                }
            *ret = FProcedure_recInt(gCP,gTP,&y,&n,sp);
            if (asBool(ret) == TRUE)
                {
                sp += n;
                count += n;
                if (y != 0.0)
                    {
                    e = 10.0;
                    f = (REAL)sn;
                    y *= f;
                    f = log(e);
                    e = f * y;
                    f = exp(e);
                    x *= f;
                    }
                }
            }
        }
    else
        y = 0.0;
    }

/* All percent constants are terminated with a '%'.  */

if ((*sp == '%') && (count > 0))
    {
    *iChar += (m + ++count);
    ret->u.Real = (x * sign) / 100;   
    ret->Tag = TYREAL;
    }
else
if (count > 0)
    {
    *iChar += (m + count);
    n = x = x * sign;   
    if (n != x)
        {
        ret->Tag = TYREAL;
        ret->u.Real = x;
        }
    else
    if (isReal->u.Bool == TRUE)
        {
        ret->Tag = TYREAL;
        ret->u.Real = x;
        }
    else
        {
        ret->Tag = TYNUM;
        ret->u.Int = n;
        }
    }
else
    {
    ret->u.Bool = FALSE;  
    ret->Tag = TYBOLE;
    }
    
FrameExit(*ret);
}

#if 0  
/*--------------------------------------------------------------------------------------- */

FSmartbase_NewType

Add a new type registration to the typing system.

#endif


TYPE FSmartbase_NewType(
LpXCONTEXT		gCP,
LpTHREAD		gTP,
TYPE            theType,
LpCHAR          aTypeName,
CHAR            aTypeFlag,
NUM             aTypeSize,
LpFNEW          aNewFunction,
LpFMARK         aMarkFunction,
LpFGMARK        aGlobalMarkFunction,
LpFCONVERT      aCnvFunction,
LpF2TVALS       aCmpFunction,
LpF3TVALS       aSetIV1Function,
LpF4TVALS       aSetIV2Function,
LpF5TVALS       aSetIV3Function,
LpF2TVALS       aGetIV1Function,
LpF3TVALS       aGetIV2Function,
LpF4TVALS       aGetIV3Function,
LpF2TVALS       aMapFunction,
LpF2TVALS       aMapcFunction,
LpFPRINT        aPrintFunction,
LpFNLOAD        aLoadFunction,
LpFNSAVE		aSaveFunction,
LpFCOMPUTESIZE	aComputeSizeFunction,
LpFCOPY			aCopyFunction,
LpFDOOM			aDoomFunction)
{
NUM             newSize = TYMAXVALIDTYPE + 1;
BOLE			thisIsANewType = TRUE;


/*  Manage the first type as a special case */
if (gCP->TObject_MaxTypes == 0)
    {
    /*  Create the type name vector */
    gCP->TObject_TypeName = (HMName)FMemory_New(gCP, gTP, newSize * sizeof(NAME),TRUE);
    strcpy((char*)_TObject_TypeName(theType),(const char*)aTypeName);

    /*  Create the type flag vector */
    gCP->TObject_TypeFlag = (HMChar)FMemory_New(gCP, gTP, newSize * sizeof(CHAR),TRUE);
    _TObject_TypeFlag(theType) = aTypeFlag;

    /*  Create the type size vector */
    gCP->TObject_TypeSize = (HMShort)FMemory_New(gCP, gTP, newSize * sizeof(SHORT),TRUE);
    _TObject_TypeSize(theType) = aTypeSize;

    /*  Create the type new vector */
    gCP->TObject_TypeNew = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeNew(theType) = aNewFunction;

    /*  Create the type mark vector */
    gCP->TObject_TypeMark = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeMark(theType) = aMarkFunction;

    /*  Create the type Globals mark vector */
    gCP->TObject_TypeGlobalMark = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeGlobalMark(theType) = aGlobalMarkFunction;

    /*  Create the type convert vector */
    gCP->TObject_TypeConvert = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeConvert(theType) = aCnvFunction;

    /*  Create the type compare vector */
    gCP->TObject_TypeCompare = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeCompare(theType) = aCmpFunction;
    
    /*  Create the setindexvalue1 vector */
    gCP->TObject_TypeSetIV1 = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeSetIV1(theType) = aSetIV1Function;
    
    /*  Create the setindexvalue2 vector */
    gCP->TObject_TypeSetIV2 = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeSetIV2(theType) = aSetIV2Function;
    
    /*  Create the setindexvalue3 vector */
    gCP->TObject_TypeSetIV3 = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeSetIV3(theType) = aSetIV3Function;
    
    /*  Create the getindexvalue vector */
    gCP->TObject_TypeGetIV1 = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeGetIV1(theType) = aGetIV1Function;

    /*  Create the getindexvalue vector */
    gCP->TObject_TypeGetIV2 = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeGetIV2(theType) = aGetIV2Function;

    /*  Create the getindexvalue vector */
    gCP->TObject_TypeGetIV3 = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeGetIV3(theType) = aGetIV3Function;

    /*  Create the Map vector */
    gCP->TObject_TypeMap = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeMap(theType) = aMapFunction;

    /*  Create the Mapc vector */
    gCP->TObject_TypeMapc = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeMapc(theType) = aMapcFunction;

    /*  Create the Print vector */
    gCP->TObject_TypePrint = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypePrint(theType) = aPrintFunction;
    
    /*  Create the Load vector */
   gCP-> TObject_TypeLoad = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeLoad(theType) = aLoadFunction;

    /*  Create the Save function vector */
    gCP->TObject_TypeSave = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeSave(theType) = aSaveFunction;

    /*  Create the computeSize function vector */
    gCP->TObject_TypeComputeSize = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeComputeSize(theType) = aComputeSizeFunction;

	    /*  Create the copy function vector */
    gCP->TObject_TypeCopy = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);
    _TObject_TypeCopy(theType) = aCopyFunction;

	gCP->TObject_TypeDoom = (NUM)FMemory_New(gCP, gTP, newSize * sizeof(NUM),TRUE);

    _TObject_TypeDoom(theType) = aDoomFunction;
	

    /*  Create the Methods vector */
    gCP->TObject_TypeMethods = (HMTval)FMemory_New(gCP, gTP, newSize * sizeof(TVAL),TRUE);
    _TObject_TypeMethods(theType) = gCP->Tval_VOID;

    /*  Create the Fields vector */
    gCP->TObject_TypeFields = (HMTval)FMemory_New(gCP, gTP, newSize * sizeof(TVAL),TRUE);
    _TObject_TypeFields(theType) = gCP->Tval_VOID;

    /*  Create the Parent vector */
    gCP->TObject_TypeParent = (HMType)FMemory_New(gCP, gTP, newSize * sizeof(TYPE),TRUE);
    _TObject_TypeParent(theType) = TYVOID;
    }
else
    {
	 /* Check if the type has already been registered */

	if (strlen(_TObject_TypeName(theType)) != 0)
		{
		thisIsANewType = FALSE;
		}

   /*  Set the type name vector */
    strcpy((char*)_TObject_TypeName(theType),(const char*)aTypeName);

    /*  Set the type flag vector */
    _TObject_TypeFlag(theType) = aTypeFlag;

    /*  Set the type size vector */
    _TObject_TypeSize(theType) = aTypeSize;

    /*  Set the type mark vector */
    _TObject_TypeMark(theType) = aMarkFunction;

    /*  Set the type global mark vector */
    _TObject_TypeGlobalMark(theType) = aGlobalMarkFunction;

    /*  Set the type new vector */
    _TObject_TypeNew(theType) = aNewFunction;

    /*  Set the type convert vector */
    _TObject_TypeConvert(theType) = aCnvFunction;

    /*  Set the type compare vector */
    _TObject_TypeCompare(theType) = aCmpFunction;
    
    /*  Set the setindexvalue vector */
    _TObject_TypeSetIV1(theType) = aSetIV1Function;
    
    /*  Set the setindexvalue vector */
    _TObject_TypeSetIV2(theType) = aSetIV2Function;
    
    /*  Set the setindexvalue vector */
    _TObject_TypeSetIV3(theType) = aSetIV3Function;
    
    /*  Set the getindexvalue vector */
    _TObject_TypeGetIV1(theType) = aGetIV1Function;
    
    /*  Set the getindexvalue vector */
    _TObject_TypeGetIV2(theType) = aGetIV2Function;
    
    /*  Set the getindexvalue vector */
    _TObject_TypeGetIV3(theType) = aGetIV3Function;
    
    /*  Set the Map vector */
    _TObject_TypeMap(theType) = aMapFunction;
    
    /*  Set the Mapc vector */
    _TObject_TypeMapc(theType) = aMapcFunction;

    /*  Set the Print vector */
    _TObject_TypePrint(theType) = aPrintFunction;

    /*  Set the load vector */
    _TObject_TypeLoad(theType) = aLoadFunction;

    /*  Set the save vector */
    _TObject_TypeSave(theType) = aSaveFunction;

   /*  Set the computeSize vector */
    _TObject_TypeComputeSize(theType) = aComputeSizeFunction;

     /*  Set the copy vector */
    _TObject_TypeCopy(theType) = aCopyFunction;

	/*  Set the doom vector */
    _TObject_TypeDoom(theType) = aDoomFunction;

	/*  Set the Methods vector */
    _TObject_TypeMethods(theType) = gCP->Tval_VOID;

    /*  Set the Fields vector */
    _TObject_TypeFields(theType) = gCP->Tval_VOID;


    /*  Set the Parent vector */
    _TObject_TypeParent(theType) = TYVOID;
    }

/*  Promote the max number of registered types, and return this type. */
if (thisIsANewType == TRUE)
	gCP->TObject_MaxTypes++;
return(theType);
}


#if 0
/*--------------------------------------------------------------------------------------- */

FSmartbase_AlignMe

Allign a structure according to the native word size  

#endif


NUM FSmartbase_AlignMe(LpXCONTEXT gCP,LpTHREAD gTP,NUM isize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return((((isize + ALLIGNMENTFACTOR - 1) / ALLIGNMENTFACTOR) * ALLIGNMENTFACTOR));
}

#if 0
/*--------------------------------------------------------------------------------------- */

FSmartbase_OperatorNew

Create a new object of this Class

#endif


void*  FSmartbase_OperatorNew(LpXCONTEXT gCP,LpTHREAD gTP,long objSize)
{
objSize = objSize; // NOOP to hide unused parameter warning message
return (TObject_OperatorNew (gCP,gTP));
}


#if 0
/*--------------------------------------------------------------------------------------- */

FSmartbase_VectorDelete

FSmartbase_VectorDelete destructively deletes the specified index element 
from the vector.  The vector is decreased in size to accomodate the lost value. 

Arguments:

argc                The number of arguments in the argument list.

arg ...             The optional direct argument list.

Return Argument:

                    The result vector after deleting the specified element

#endif


TVAL FSmartbase_VectorDelete(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... )

{
va_list         args;
NUM             i;
StartFrame
DeclareTVALArray(argv,_FSmartbase_MAXARGUMENTS);
DeclareTVAL(ret);
EndFrame

/* Gather the variable arguments into an array. */

if (argc > __MAXPARMS) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
va_start(args,argc);
for (i = 0; i < argc; i++)
    {
    argv[i] = va_arg(args,TVAL);
    }
va_end(args);
    

/*  Set the specified object using the argument list. */
    
*ret = FUtil3_VectorDelete(gCP,gTP,argc,&argv[0]);
FrameExit(*ret);
}

#if 0
/*--------------------------------------------------------------------------------------- */

FSmartbase_MarkTval

Mark the specified type of data. This function is usually called during a mark
and sweep phase.  This is a deep mark, i.e., it marks all the elements in an 
aggregate type.


#endif


void FSmartbase_MarkTval(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval )
{

TObject_MarkTval(gCP,gTP,aTval);

}

#if 0
/*--------------------------------------------------------------------------------------- */

FSmartbase_MarkTvalShallow

Mark the specified type of data. This function is usually called during a mark
and sweep phase.  This is a shallow mark, i.e., it marks only the top level object.


#endif


void FSmartbase_MarkTvalShallow(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval )

{
LpFMARK     aFunction;

/* If the type is a descendent of TObject send it a mark message. */
/* otherwise, call its appropriate mark function from the Mark vector. */

if ((aTval.Tag < TYVOID) || (aTval.Tag > TYMAXVALIDTYPE))
    {
    FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
    }   
if (_TObject_TypeFlag(aTval.Tag) == _TObject_TfTOBJECT)
    {
    /*  Don't mark the nil object */
    if (aTval.u.Object == NIL)
		{
		FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_BAD_DATATYPE);
		}
    else
    /*  Mark the top level object only */
		{
		_TObject_MarkFlag(aTval.u.Object) |= _TObject_OfMARK;
		}
    }
else
    {
    aFunction = (LpFMARK)_TObject_TypeMark(aTval.Tag);

    if (aFunction != TObject_MarkNever)
        {
        (*aFunction)(gCP,gTP,aTval);
        }
    }
}


/*--------------------------------------------------------------------------------------- */
#if 0

FSmartbase_SendMsg

Send a message to a target along with an argument list. Return the result to the caller.

Arguments:


msg                 The message Symbol to be sent.

target              The object to receive the message.

argc                The number of arguments in the argument list.

argv                The optional argument list.

Return Argument:

                    The value of the evaluating the message against the argument list.


Note:   For speed of execution, this code has been duplicated in several
        locations. Any change to this code should also be accompanied by
        similar changes in the all locations, which currently are: 

            FSmartbase_SendMsg
            FSmartbase_SendMsgv
            FVmScript_Eval

Note:   The result is not protected from garbage collection as in FSmartbase_Evals!!

#endif

TVAL FSmartbase_SendMsg(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL msg,const TVAL target,const NUM argc,...)
{
#define         MaxParms    50
LpTVAL          argv;
NUM             i;
va_list         args;
StartFrame
DeclareTVALArray(argp,MaxParms+1);
EndFrame

/* Gather the variable arguments into an array.  On risc */
/* machines, because they are register machines, we must. */
/* use the ansii standard arg_list approach. On cisc stack */
/* machines we just pass the variable arguments address. */

if (argc > __MAXPARMS) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
va_start(args,argc);
for (i = 0; i < argc; i++)
    {
    argp[i] = va_arg(args,TVAL);
    }
va_end(args);
argv = &argp[0];   

/*  Send the specified message to the target along with the argument list. */
    
FrameExit(FSmartbase_SendMsgv(gCP,gTP,msg,target,argc,argv));
#undef          MaxParms
}

/*--------------------------------------------------------------------------------------- */
#if 0
FSmartbase_SendMsgv

Send a message to a target along with an argument list. Return the result to the caller.

Arguments:

msg                 The message Symbol to be sent.

target              The object to receive the message.

argc                The number of arguments in the argument list.

argv                The vectored argument list.

Return Argument:

                    The value of the evaluating the message against the argument list.


Note:   For speed of execution, this code has been duplicated in several
        locations. Any change to this code should also be accompanied by
        similar changes in the all locations, which currently are: 

            FSmartbase_SendMsg
            FSmartbase_SendMsgv
            FVmScript_Eval

Note:   The result is not protected from garbage collection as in FSmartbase_Evals!!

#endif

TVAL    FSmartbase_SendMsgv(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL msg, const TVAL target, const NUM argc, const TVAL argv[])
{
#define         MaxParms    50
TYPE			type;
NUM             i;
COMPARE			selection;
NUM             __tvalf__;
NUM             __objsf__;
NUM             __recur__;
TVAL			member;
TVAL			methodsList;
TVAL			prmv[MaxParms];
StartFrame
DeclareTVAL(ret);
EndFrame

/*  Evaluate the specified procedure against the argument list. */
    
__tvalf__ = gTP->TvalStackIdx;
__objsf__ = gTP->ObjStackIdx;
__recur__ = gTP->RecursionCount;


/*  Load the source message procedure by combining the */
/*  type of the target and the message symbol */
/*  to look up the procedure in the methods table for the type. */
/*  Note:   If a quoted symbol is the message, then the method */
/*          Procedure is chosen from the parent's methods Dictionary. */
if (msg.Tag == TYSYMBOL)
    { 
    /*  Load the type of the target. */
    type = target.Tag;
	/*  Modify the send if the target object has properties */
	/*  which may may take prescedence over the class methods */
	switch (type)
		{
		case TYLAMBDA:
			/*  If target is an Lambda, and the message matches one of its  child Lambdas, */
			/*	then the matching child Lambda takes prescedence over any class methods. */
			/*	Does the message match a child Lambda of the target? */
			member = TLambda_GetPvMember(gCP,gTP,target,FSmartbase_ObjectPtr(gCP,gTP,(LpTVAL)&msg));
			if (member.Tag != TYVOID)
				{
				/* Call the child of the target directly instead of using the class method. */
				/* Note: Do not pass the target as an argument. */
				*ret = FSmartbase_Evalv(gCP,gTP,member,argc,argv);
				goto ReturnToCaller;
				}
			goto UseClassMethod;
			break;
		
		case TYSTRUCTURE:
			/*  If target is a Structure, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the target? */
			member = TStructure_GetIV1(gCP, gTP, target,msg);
			if ((member.Tag != TYVOID) && (member.Tag != TYERROR))
				{
				/* Call the attribute of the target directly instead using the class method. */
				/* Note: Do not pass the target as an argument. */
				*ret = FSmartbase_Evalv(gCP,gTP,member,argc,argv);
				goto ReturnToCaller;
				}
			goto UseClassMethod;
			break;
		
		case TYVECTOR:
			/*  If target is a Vector, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the target? */
			member = TVector_GetIV1(gCP,gTP,target,msg);
			if ((member.Tag != TYVOID) && (member.Tag != TYERROR))
				{
				/* Call the attribute of the target directly instead using the class method. */
				/* Note: Do not pass the target as an argument. */
				*ret = FSmartbase_Evalv(gCP,gTP,member,argc,argv);
				goto ReturnToCaller;
				}
			goto UseClassMethod;
			break;
		
		case TYDICTIONARY:
			/*  If target is a Dictionary, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the target? */
			member = TDictionary_GetIV1(gCP,gTP,target,msg);
			if ((member.Tag != TYVOID) && (member.Tag != TYERROR))
				{
				/* Call the attribute of the target directly instead using the class method. */
				/* Note: Do not pass the target as an argument. */
				*ret = FSmartbase_Evalv(gCP,gTP,member,argc,argv);
				goto ReturnToCaller;
				}
			goto UseClassMethod;
			break;
		
		case TYDIRECTORY:
			/*  If target is a Directory, and the message matches one of its attributes, */
			/*	then the matching attribute takes prescedence over any class methods. */
			/*	Is the message an attribute of the target? */
			member = TDirectory_GetIV1(gCP,gTP,target,msg);
			if ((member.Tag != TYVOID) && (member.Tag != TYERROR))
				{
				/* Call the attribute of the target directly instead using the class method. */
				/* Note: Do not pass the target as an argument. */
				*ret = FSmartbase_Evalv(gCP,gTP,member,argc,argv);
				goto ReturnToCaller;
				}
			goto UseClassMethod;
			break;
		
		default:
			UseClassMethod:
			/*  Load the methods Dictionary for the target type. */
			if ((type == TYSTRUCTURE) && (Structure(target)->itsMethods != NIL))
				{
				methodsList = TOBJ(Structure(target)->itsMethods->itsUserTypeMethods);
				}
			else
				{
				methodsList = _TObject_TypeMethods(type);
				}
			break;
		}	/* end switch */
    }	/* end if */
else
/*  If the message is a quoted symbol, then this is treated the same as a super. */
/*  Note:   If a quoted symbol is the message, then the method */
/*          Procedure is chosen from the parents methods Dictionary. */
if (msg.Tag == TYQUOTEDSYMBOL)
    { 
    /*  Load the type of the target. */
    type = target.Tag;
		
	/*  If target is an Lambda, and message is its child, then call the child Lambda directly. */
	if (type == TYLAMBDA)
		{
		/* Is the message a child of the target? */
		member = TLambda_GetPvMember(gCP,gTP,target,SymbolArray(msg));
		if (member.Tag != TYVOID)
			{
			/* Call the child of the target directly instead of sending a message. */
			/* Note: Do not pass the target as an argument. */
			*ret = FSmartbase_Evalv(gCP,gTP,member,argc,argv);
			goto ReturnToCaller;
			}
		}

	/*  Load the methods Dictionary for the receiving type. */
    if ((type == TYSTRUCTURE) && (Structure(target)->itsMethods != NIL) &&
        (Structure(target)->itsMethods->itsUserTypeParent != NIL))
        {
        methodsList = TOBJ(Structure(target)->itsMethods->itsUserTypeParent->itsUserTypeMethods);
        }
    else
    if ((type == TYSTRUCTURE) && (Structure(target)->itsMethods != NIL))
        {
        methodsList = TOBJ(Structure(target)->itsMethods->itsUserTypeMethods);
        }
    else
        {
        methodsList = _TObject_TypeMethods(type);
        }
    }
else
    { 
    if (msg.Tag == TYERROR)
        *ret = msg;
    else
        *ret = TObject_Error(gCP,gTP,"!vmsendValue!");
    goto ReturnToCaller;
    }	/* end else */

/*  If the methods Dictionary is empty, issue an error. */
if (methodsList.Tag != TYDICTIONARY)
    {
    *ret = TObject_Error(gCP,gTP,"!vmsendValue!");
    goto ReturnToCaller;
    }
/*  Load the method from the methods Dictionary for the target type. */
member = TDictionary_BSearchKey(gCP,gTP,(TDictionary*)methodsList.u.Object, msg.u.Object, &selection);
if (selection != 0)
    {
    member = gCP->Tval_VOID;
    }
else
    {
    member = BondArray(methodsList)[member.u.Int].Value;
    }

/* Call the method passing the target as an argument */

for (i = 0; i < argc; ++i) prmv[i+1] = argv[i];
prmv[0] = target;
*ret = FSmartbase_Evalv(gCP,gTP,member,argc+1,prmv);

/* Return the result value to the caller. */
ReturnToCaller:
gTP->TvalStackIdx = __tvalf__;
gTP->ObjStackIdx = __objsf__;
gTP->RecursionCount = __recur__;
FrameExit(*ret);
#undef          MaxParms
}

/*--------------------------------------------------------------------------------------- */
#if 0
globalReferences

Return a Dictionary of any global symbols which are referenced by the pcode
vector of the Lambda passed as an argument..

#endif

TVAL    FSmartbase_GlobalReferences(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
OPCODE              ExtendedOpcode;     
NUM                 modifier[3];
NUM                 modIndex;
NUM                 vecIndex;
StartFrame
DeclareOBJ(TPcodeVector,self);
DeclareOBJ(TSymbol,aSymbol);
DeclareTVAL(result);
EndFrame

/* The first argument must be an Lambda */
if ((argc != 1) || (argv[0].Tag != TYLAMBDA)) 
	{
	FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/* Reference the Pcode Vector and set the result Dictionary */
self = Procedure(argv[0])->PcodeVector;
result->u.Dictionary = TDictionary_New(gCP,gTP);
result->Tag = TYDICTIONARY;

/*  Examine all pcodes in the Pcode Vector looking at the argument modifiers */
/*  for any Global Symbol references, record to symbol and displacement. */
vecIndex = 0;
while (vecIndex < self->itsMaxItemIndex)
    {
    /*  Separate the opcode into the six modifiers. */
    ExtendedOpcode.Opcode = atHMInt(self->itsInstructionArray,vecIndex++);
    modifier[0] = ExtendedOpcode.u.Am1;
    modifier[1] = ExtendedOpcode.u.Am2;
    modifier[2] = ExtendedOpcode.u.Am3;

    /*  Loop through the modifier patterns for arguments which need */
    /*  to be marked. */
    for (modIndex = 0; modIndex < 3; ++modIndex)
        {
        switch (modifier[modIndex])
            {
            case AMVOID:
                break;
                
            case AMAVOFFSET:
            case AMTVOFFSET:
            case AMPVOFFSET:
            case AMCVOFFSET:
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
            case AMINTEGER:
                if (vecIndex < self->itsMaxItemIndex) vecIndex++;
                break;
                
            case AMREGISTER:
                if ((ExtendedOpcode.Opcode < VMSTARTREGISTERINS) && (vecIndex < self->itsMaxItemIndex)) vecIndex++;
                break;
                
            case AMGVOFFSET:
                if (vecIndex < self->itsMaxItemIndex)
                    {
                    aSymbol = *(TSymbol**)&atHMInt(self->itsInstructionArray,vecIndex);
                    TDictionary_SetIV1(gCP,gTP,*result,TOBJ(aSymbol),TINT(vecIndex));
					++vecIndex;
                    }
                break;
                                
            default:
                break;
            }       
        }
    }

FrameExit(*result);
}


