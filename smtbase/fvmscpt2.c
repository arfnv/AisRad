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

#define _C_FVMSCRIPT2
#define _SMARTBASE

#if 0
FVmScript2.c

Implementation of the Virtual Machine Procedure engine utility functions.

This source file contains the main utility functions for the VmScript 
Procedure object. A Procedure object contains a compiled VmScript formula. 
Adopted from Scheme and other dialects of Lisp, it is one of the main 
mechanisms for storing Lisp functions or scripts in SmartLisp.

PARENT:             None. 

AUTHORS:            Michael F. Korns

#endif

#include "fvmscpt2.h"
#include "fdebug.h"
#include "tdirect.h"
#include "terror.h"


extern  BOLE    FConio_sprintf      (LpXCONTEXT gCP, LpTHREAD gTP, LpCHAR dest,LpCHAR fmt, ...);


/*--------------------------------------------------------------------------------------- */
#if 0
FVmScript2_UpdateBindings

Update the value bindings in the Tv of the passed procedure object to contain the correct
values as extracted from the Fb given.
        
#endif

TVAL FVmScript2_UpdateBindings(LpXCONTEXT gCP,LpTHREAD gTP,TLambda* self,NUM argc,TVAL argv[], LpTVAL Fb)
{
NUM                     cn;
StartFrame
DeclareOBJ(TStructure,tmpEnv);
EndFrame

argc = argc; // NOOP to hide unused parameter warning message
if (self->TemporaryVariables != NIL)
    {
    tmpEnv = self->TemporaryVariables;
    for (cn = 0; cn < tmpEnv->itsMaxItemIndex; ++cn)
        {
        atHMBind(tmpEnv->itsDictionaryArray,cn).Value = Fb[cn];
        }
    }
    
if (self->ArgumentVariables != NIL)
    {
    tmpEnv = self->ArgumentVariables;
    for (cn = 0; cn < tmpEnv->itsMaxItemIndex; ++cn)
        {
        atHMBind(tmpEnv->itsDictionaryArray,cn).Value = argv[cn];
        }
    }
    

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
/*
FVmScript2_DebugManager

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
				(debugDetective Lambda CodeL Recursion Rv Av Tv Pv Cv Sv Iv errorMsg)	

*/        
#endif

void FVmScript2_DebugManager(LpXCONTEXT gCP,	/*  Context Pointer */
							 LpTHREAD gTP,		/*  Thread Pointer */						 
							 TLambda* self,		/*  Procedure object being evaluated */
                             NUM argc,          /*  Count of Arguments passed to procedure */
                             TVAL argv[],       /*  Arguments passed to procedure */
                             TPcodeVector* Pc,  /*  Pcode vector register */
                             TStructure* Pv,	/*  Persistent variable register */
                             TStructure* Sv,	/*  Class variable register */
                             LpTVAL Rb,			/*  Register base register */
                             LpREAL Vs,	        /*  Vector Processing Number Stack */
                             NUM VsTopOfStack,	/*  Vector Processing Number Stack */
                             LpTVAL Fb,         /*  Frame base register */
                             LpNUM Ip,          /*  Instruction pointer */
                             TVAL errorMsg)     /*  Error message String (or #void if no error )*/
{
/*  VM DEBUGGER LOCAL VARIABLE DECLARATIONS */
/*  Note:   These variables should be kept to an absolute necessary */
/*          minimum, as they eat up large (approc 132 bytes) of C  */
/*          system stack space with each SmartLisp recursion.  */
NUM                     n;                      /*  Temporary index variable */
NUM                     CodeL;					/*  Debugger instruction counter */
NUM                     SafeSI;                 /*  Debugger gc safe temp storage */
BOLE                    saveSwt;                /*  Debugger save error switch */

StartFrame
DeclareTVAL(_Rv);
DeclareTVAL(_Tv);
DeclareTVAL(_Sv);
DeclareTVAL(_Av);
EndFrame

/*  ===================================================================== */
/*  Here we manage the virtual machine's debugger and trace facility.     */
/*  ===================================================================== */
argc = argc;	// NOOP to hide unused parameter warning message
Pv = Pv;		// NOOP to hide unused parameter warning message

gTP->DebugTraceON = TRUE;
gTP->DebugSuspended = TRUE;

/*  Save gc protected space on the stack for temporary results. */
SafeSI = TopOfStack;
TopOfStack += 2;
saveSwt = gTP->TObject_ErrorSwt;

/* Compute the current IP location */
CodeL = (Ip - 1 - &atHMInt(Pc->itsInstructionArray,0));

/*  Save the procedure's register variables into the Rv variable. */
if (self->RegisterVariables == NIL)
    {
    *_Rv = TOBJ(self->RegisterVariables);
    }
else
    {
    _Rv->u.Object = (TObject*)TStructure_Copy(gCP,gTP,TOBJ(self->RegisterVariables));
    _Rv->Tag = _Rv->u.Object->itsObjectType;
    
    for (n = 0; n < _Rv->u.Structure->itsMaxItemIndex; n++)
        {
		if (atHMBind(_Rv->u.Structure->itsDictionaryArray,n).Value.DeclaredType == TYREAL)
			{
			atHMBind(_Rv->u.Structure->itsDictionaryArray,n).Value.Tag = TYREAL;
			atHMBind(_Rv->u.Structure->itsDictionaryArray,n).Value.u.Real = Rb[n].u.Real;
			}
		else
			{
			atHMBind(_Rv->u.Structure->itsDictionaryArray,n).Value.Tag = TYNUM;
			atHMBind(_Rv->u.Structure->itsDictionaryArray,n).Value.u.Int = Rb[n].u.Int;
			}
        }
    }


/*  Save the procedure's temporary variables into the global "tv" variable. */
if (self->TemporaryVariables == NIL)
    {
    *_Tv = TOBJ(self->TemporaryVariables);
    }
else
    {
    _Tv->u.Object = (TObject*)TStructure_Copy(gCP,gTP,TOBJ(self->TemporaryVariables));
    _Tv->Tag = _Tv->u.Object->itsObjectType;    
    for (n = 0; n < asStructure(_Tv)->itsMaxItemIndex; n++)
        {
        atHMBind(asStructure(_Tv)->itsDictionaryArray,n).Value = Fb[n]; 
        }
    }

/*  Save the procedure's self variables into the global "sv" variable. */
if (Sv == NIL) 
    {
    *_Sv = gCP->Tval_VOID;
    }
else
    {
    _Sv->u.Structure = Sv;
    _Sv->Tag = TYSTRUCTURE;
    }

/*  Save the procedure's argument variables into the global "av" variable. */
if (self->ArgumentVariables == NIL)
    {
    *_Av = TOBJ(self->ArgumentVariables);
    }
else
    {
    _Av->u.Object = (TObject*)TStructure_Copy(gCP,gTP,TOBJ(self->ArgumentVariables));
    _Av->Tag = _Av->u.Object->itsObjectType;    
    for (n = 0; n < asStructure(_Av)->itsMaxItemIndex; n++)
        {
        atHMBind(asStructure(_Av)->itsDictionaryArray,n).Value = argv[n]; 
        }
    }


// (debugDetective Lambda CodeL Recursion Rv Av Tv Pv Cv Sv Iv)	
CallDebugDetective:
gTP->DebugTraceON = FALSE;
gTP->DebugSuspended = TRUE;
if (gCP->FVmscript_Symbol_debugDetective->itsGlobalValue.Tag != TYVOID)
{
	FSmartbase_Eval(gCP,gTP,gCP->FVmscript_Symbol_debugDetective->itsGlobalValue,
							11,
							TOBJ(self),
							TINT(CodeL),
							TINT(gTP->RecursionCount-1),
							*_Rv,
							*_Av,
							*_Tv,
							TOBJ(self->PersistantVariables),
							TOBJ(self->ConstantVariables),
							*_Sv,
							TOBJ(self->Interfaces),
							errorMsg);
}

/*  Return gc safe temporary storage to the stack. */
ResumeNextInstruction:
TopOfStack = SafeSI;
gTP->TObject_ErrorSwt = saveSwt;
gTP->DebugTraceON = TRUE;
gTP->DebugSuspended = FALSE;
/* Check for a client escape request. */
if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) 
	FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);

FrameReturn

}
