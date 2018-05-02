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

#define _C_TNUMMATRIX
#define _SMARTBASE
#if 0
TNumMat.c

Implementation of the number matrix CLASS which stores a variable number of items of number
type in a rank dimensioned matrix. The manifest typing system is used to control 
and size the data items and the matrix itself.

PARENT:             TObject 

AUTHORS:            Michael F. Korns

#endif

#include "tmatrix.h"
#include "tnummat.h"
#include "fconio.h"
#include "fmake.h"
#include "tsymbol.h"
#include "futil3.h"
#include "fproc.h"

/*--------------------------------------------------------------------------------------- */
#if 0
TNumMatrix_Init

Initialize the TNumMatrix class and extend the typing system.

Note:   This function should only be called once at the beginning of the application.

#endif

void TNumMatrix_Init(LpXCONTEXT gCP,LpTHREAD gTP)
{
StartFrame
DeclareOBJ(TSymbol,aSymbol);
EndFrame
 
/*  Don't initialize more than once. */
if (gCP->TNumMatrix_Initialized) FrameReturn;
gCP->TNumMatrix_Initialized = TRUE;

/*  Initialize the new type for this class. */
FSmartbase_NewType (gCP,
					gTP,
					TYNUMMATRIX,
					(LpCHAR)"NumMatrix",
					_TObject_TfTOBJECT,
					sizeof(OBJ),
					(LpFNEW)&FMake_Matrix,
					&TNumMatrix_Mark,
					&TObject_GlobalMarkNever,
					&FObject_ObjAnyCnv,
					&FObject_CompareNever,
					&TNumMatrix_SetIV1,
					&TNumMatrix_SetIV2,
					&TNumMatrix_SetIV3,
					&TNumMatrix_GetIV1,
					&TNumMatrix_GetIV2,
					&TNumMatrix_GetIV3,
					&TNumMatrix_Map, 
					&TNumMatrix_Mapc,
					&TNumMatrix_Print,
					&TNumMatrix_Load,
					&TNumMatrix_Save,
					&TNumMatrix_ComputeSize,
					&TNumMatrix_Copy,
					&TNumMatrix_Doomed);

FSmartbase_NewType	(gCP,
					 gTP,					 
					 TYNUMMATRIXROW,
                    (LpCHAR)"NumMatrixRow",
                    _TObject_TfIOBJECT,
                    sizeof(TVAL),
                    (LpFNEW)&TObject_NewNever,
                    &TObject_MarkTval,
                    &TObject_GlobalMarkNever,
                    &FObject_ObjAnyCnv,
                    &FObject_CompareNever,
                    &TNumMatrixRow_SetIV1,
                    &FObject_SetIV2Never,
                    &FObject_SetIV3Never,
                    &TNumMatrixRow_GetIV1,
                    &FObject_GetIV2Never,
                    &FObject_GetIV3Never,
                    &TObject_MapText,
                    &TObject_MapcText,
                    &TNumMatrixRow_Print,
                    &TObject_LoadNever,
					&FObject_SaveNever,
					&FObject_ComputeSizeNever,
					&FObject_CopyNever,
					&FObject_DoomNever);

/* Register the SmartLisp cProcedures contained in this package */

FrameReturn;
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNumMatrix_Load

The specified OODBMS manager is about to load this object. Convert yourself from
a handle into a properly initialized object.

#endif

TVAL TNumMatrix_Load(LpXCONTEXT gCP,LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve)
{
NUM                 cn;
StartFrame
DeclareOBJ(TNumMatrix,it);
DeclareTVAL(retTval);
EndFrame

*retTval = gCP->TObject_VOID;
it = NULL;

if(bResolve == 0)
    {
    it = TNumMatrix_New(gCP,gTP);
    *retTval = TObject_RegisterLoad(gCP,gTP,theFileID,(TObject*)it);
    }
else
    {
    it = (TNumMatrix*)TObject_CheckRegistration(gCP,gTP,theFileID);
    if(it != NULL)
        {
        FObject_SetMaxIndex(gCP,gTP,(TObject*)it,TNumMatrixOnDiskPtr(aHMemory,0)->itsMaxItemIndex);
        it->itsRank = TNumMatrixOnDiskPtr(aHMemory,0)->itsRank;
		for (cn = 0; cn < _MAXDIMENSIONS; ++cn)
			{
			it->itsDimensions[cn] = TNumMatrixOnDiskPtr(aHMemory,0)->itsDimensions[cn];
			}

		/* Load the "itsCdr" from the disk stream */
        it->itsCdr = TObject_LoadTval(gCP,gTP,TNumMatrixOnDiskPtr(aHMemory,0)->itsCdr);
                
        for(cn = 0;cn < it->itsMaxItemIndex; cn++)
            {
            atHMReal(it->itsRealMatrix,cn) = TNumMatrixOnDiskPtr(aHMemory,0)->itsItemArray[cn];
            }
        
        asTag(retTval) = it->itsObjectType;
        asObject(retTval) = (TObject*)it;
        }
    else
        *retTval = gCP->TObject_ERROR_INVALID;
    }

FrameExit(*retTval);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Map

Make a copy of *this and call the given proc on each element, storing the result in place.

#endif

TVAL TNumMatrix_Map(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, TVAL proc)
{
TNumMatrix*    self = (TNumMatrix*)asObject(&selfTval);

NUM         index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TNumMatrix,copyMatrix);
EndFrame

/* Make a copy of myself since map returns an object of the same type */
copyMatrix = (TNumMatrix*)TNumMatrix_Copy(gCP,gTP,selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    tmp->u.Real = atHMReal(copyMatrix->itsRealMatrix,index);
	tmp->Tag = TYREAL;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        {
        FrameExit(*ret);
        }
    else
        atHMReal(copyMatrix->itsRealMatrix,index) = asNumIndex(ret);
    }
asObject(ret) = (TObject*)copyMatrix;
asTag(ret) = TYNUMMATRIX;
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Mapc

Loop through itsRealMatrix and call the specified function.

#endif

TVAL TNumMatrix_Mapc(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL proc)
{
NUM         index;
StartFrame
DeclareTVAL(ret);
DeclareTVAL(tmp);
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

for(index=0; index < self->itsMaxItemIndex; index++)
    {
    tmp->u.Real = atHMReal(self->itsRealMatrix,index);
    tmp->Tag = TYREAL;
    *ret = FSmartbase_Evalv(gCP,gTP,proc, 1, tmp);
    if(isERROR(ret))
        break;
    }
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
IMatrix

Initialize a TNumMatrix object with a new tail(cdr) and an array of items.

#endif

TVAL TNumMatrix_IMatrix(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM argc,REAL argv[],TVAL newCdr)
{
NUM             indexOf;
StartFrame
DeclareTVAL(ret);
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  Reshape the Matrix's array to be the correct size. */
TNumMatrix_SetMaxIndex(gCP, gTP, selfTval, argc);

/*  Set the matrix's array items. */
for (indexOf = 0; indexOf < self->itsMaxItemIndex; ++indexOf)
    atHMReal(self->itsRealMatrix,indexOf) = argv[indexOf];

/*  Set the matrix's tail(cdr). */
self->itsCdr = newCdr;

asTag(ret) = TYNUMMATRIX;
asObject(ret) = (TObject*)self;
FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Mark

Garbage collection is marking this object. Mark any objects which you reference.

Note:   This method should only be called by mark and sweep garbage collection!
        Do send mark messages to any referenced objects,

#endif

void TNumMatrix_Mark(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
StartFrame
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  Mark the matrix's Lisp tail(cdr) so its won't be garbage collected. */
TObject_MarkTval(gCP,gTP,self->itsCdr);

FrameReturn;
}


/*--------------------------------------------------------------------------------------- */
#if 0
Doomed

Garbage collection is about to delete this object. Dispose of the array data.

Note:   This method should only be called by mark and sweep garbage collection!
        This method warns the object that it is about to be deleted. Garbage
        collection first warns all the doomed objects, then it deletes all doomed
        objects.
        
        Do close any files and clean up anything necessary here.
        Do free any resources which you have allocated of which you are the sole owner.

        Do not send delete or dispose messages to any referenced objects,
        Let garbage collection do this.

        Do not delete or release any of your own storage here.
        Let garbage collection do this.

#endif

void TNumMatrix_Doomed(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TNumMatrix*    self = (TNumMatrix*)asObject(&selfTval);

/* If immediate data space is used, do NOT try to release a memory handle. */
if (self->itsImmediatePtr != NULL)
	{
	self->itsMaxItemIndex = 0;																				 
	self->itsRealMatrix = NULL;
	self->itsImmediatePtr = NULL;
	return;
	}

FMemory_Free(gCP, gTP, (HMemory)self->itsRealMatrix);
self->itsMaxItemIndex = 0;	
self->itsRealMatrix = NULL;
}


/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TNumMatrix_ComputeSize(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, NUM* aSize)
{
TNumMatrix*        self = (TNumMatrix*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk;
*aSize += SIZEOF_TNumMatrixOnDisk + (self->itsMaxItemIndex * sizeof(REAL));
ALLIGNME(*aSize);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Save

The specified OODBMS manager is about to save this object. Convert yourself into 
a handle and return the handle.

#endif

HMemory TNumMatrix_Save(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval, HMemory aHMemory)
{
long                theOffset;
NUM                 cn;
StartFrame
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  Save itsObjectOnDisk */

TObjectOnDiskPtr(aHMemory,0)->itsObjectType = self->itsObjectType;
theOffset = SIZEOF_TObjectOnDisk;

/*  Save TNumMatrixOnDisk */

TNumMatrixOnDiskPtr(aHMemory,theOffset)->itsMaxItemIndex = self->itsMaxItemIndex;
TNumMatrixOnDiskPtr(aHMemory,theOffset)->itsCdr = TObject_RegisterTval(gCP,gTP,self->itsCdr);
TNumMatrixOnDiskPtr(aHMemory,theOffset)->itsRank = self->itsRank;

for (cn = 0; cn < _MAXDIMENSIONS; ++cn)
	{
	TNumMatrixOnDiskPtr(aHMemory,theOffset)->itsDimensions[cn] = self->itsDimensions[cn];
	}

/*  Save the real matrix in a compressed form into a stream in disk record */

for(cn = 0;cn < self->itsMaxItemIndex; cn++)
    {
    /*  Save temp into stream in disk record */
    TNumMatrixOnDiskPtr(aHMemory,theOffset)->itsItemArray[cn] = atHMReal(self->itsRealMatrix,cn);
    }

FrameExit(aHMemory);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Copy

Make a copy of a TNumMatrix.

#endif

TObject* TNumMatrix_Copy(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
NUM				cn;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareOBJ(TNumMatrix,theCopy);
DeclareTVAL(tmpTval);
EndFrame

self = (TNumMatrix*)selfTval.u.Object;

theCopy = TNumMatrix_New(gCP,gTP);

tmpTval->u.Object = (TObject*)theCopy;
tmpTval->Tag = theCopy->itsObjectType;

if (self->itsRealMatrix != NULL)
	{
	TNumMatrix_IMatrix(gCP, gTP, *tmpTval, self->itsMaxItemIndex,&atHMReal(self->itsRealMatrix,0),self->itsCdr);
	}

theCopy->itsRank = self->itsRank;
for (cn = 0; cn < _MAXDIMENSIONS; ++cn)
	theCopy->itsDimensions[cn] = self->itsDimensions[cn];

FrameExit((TObject*)theCopy);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetMaxIndex

Return the maximum size of the repeating portion of this object.

#endif

NUM TNumMatrix_GetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TNumMatrix*    self = (TNumMatrix*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsMaxItemIndex);
}


/*--------------------------------------------------------------------------------------- */
/*
SetMaxIndex

Set the maximum size of the repeating portion of this object.

Note: This is a private internal function only. The Matrix object's published
	  resize function requires a rank and dimension list.

*/

TVAL TNumMatrix_SetMaxIndex(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,NUM newRepeats)
{
NUM			dn;
NUM			n;
NUM			oldMaxItemIndex;
LpREAL		ptr;
StartFrame	
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  Do not allow a resize for negative lengths */
if (newRepeats < 0)	FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
oldMaxItemIndex = self->itsMaxItemIndex;

/* Use immediate data space (if the requested size will allow). */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if (newRepeats <= (NUM)(_TNumMatrix_ImmediateSpace/(NUM)sizeof(REAL)))
	{
	if (self->itsRealMatrix == NULL) 
		{
		_FMemory_memset(self->itsImmediateSpace,0,newRepeats*sizeof(REAL));
		}
	else
		{
		_FMemory_memcpy(self->itsImmediateSpace,(char*)RealMatrix(selfTval),min(newRepeats,self->itsMaxItemIndex)*sizeof(REAL));
		if ((self->itsRealMatrix != NULL) && (self->itsImmediatePtr == NULL)) FMemory_Free(gCP,gTP,(HMemory)self->itsRealMatrix);
		}
	self->itsRealMatrix = (HMReal)&self->itsImmediatePtr;
	self->itsImmediatePtr = (CHAR*)&self->itsImmediateSpace[0];
    self->itsMaxItemIndex = newRepeats;
	}
else
/*  Either create or resize the item array handle. */
if (self->itsRealMatrix == NULL)
    {
    self->itsRealMatrix = (HMReal)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(REAL)),TRUE);
    self->itsMaxItemIndex = newRepeats;
    }
else
/* Existing data is in immediate data space. */
/* Note: The immediate data space is at the trailing end of the */
/*       standard sized object header (_FSmartbase_ObjectHeaderMaxSize). */
if ((self->itsImmediatePtr != NULL) && (newRepeats != self->itsMaxItemIndex))
	{
    self->itsRealMatrix = (HMReal)FMemory_New(gCP, gTP, (LONG)(newRepeats*sizeof(REAL)),TRUE);
	_FMemory_memcpy((char*)RealMatrix(selfTval),self->itsImmediateSpace,min(newRepeats,self->itsMaxItemIndex)*sizeof(REAL));
	self->itsImmediatePtr = NULL;
    self->itsMaxItemIndex = newRepeats;
	}
else
if (newRepeats != self->itsMaxItemIndex)
    {
    self->itsRealMatrix = (HMReal)FMemory_Resize(gCP, gTP, (HMemory)self->itsRealMatrix,(LONG)(newRepeats*sizeof(REAL)));
    self->itsMaxItemIndex = newRepeats;
    }

/* Initialize any skipped items (if necesssary). */
if (oldMaxItemIndex < newRepeats)
	{
	ptr = (LpREAL)&RealMatrix(selfTval)[oldMaxItemIndex];
	for (n = oldMaxItemIndex; n < newRepeats; ++n)
		{
		*(ptr++) = 0.0;
		}
	}
	
/* Reset the rank and dimensions. */
self->itsRank = 1;
for (dn = 0; dn < _MAXDIMENSIONS; ++dn) self->itsDimensions[dn] = 1;
self->itsDimensions[0] = newRepeats;

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TNumMatrix_GetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(ret);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  We accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
	{
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

if (self->itsRank == 1)
    {
    *ret = TREAL(atHMReal(self->itsRealMatrix,indexOf));
    }
else
if (self->itsRank > 1)
    {
    if (indexOf >= self->itsDimensions[0])
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    ret->Tag = TYNUMMATRIXROW;
    asObjIdx(ret) = self->itsObjectIndex;
    asRowIdx(ret) = indexOf;
    asFldIdx(ret) = -1;
    }
else
    *ret = gCP->TObject_ERROR_INVALID;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetIV2

Return the indexed value from the repeating portion of this object.

#endif

TVAL TNumMatrix_GetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  We accept numeric indices. */
if ((self->itsRank == 2) && 
	(isNumIndex(&index1)) && 
	(isNumIndex(&index2)) &&
	(asNumIndex(&index1) >= 0) && 
	(asNumIndex(&index1) < self->itsDimensions[0]) && 
	(asNumIndex(&index2) >= 0) && 
	(asNumIndex(&index2) < self->itsDimensions[1]))
    indexOf = (asNumIndex(&index1) * self->itsDimensions[1]) + asNumIndex(&index2);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

FrameExit(TREAL(atHMReal(self->itsRealMatrix,indexOf)));
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetIV3

Return the indexed value from the repeating portion of this object.

#endif

TVAL TNumMatrix_GetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  We accept numeric indices. */
if ((self->itsRank == 3) && 
	(isNumIndex(&index1)) && 
	(isNumIndex(&index2)) &&
	(asNumIndex(&index1) >= 0) && 
	(asNumIndex(&index1) < self->itsDimensions[0]) && 
	(asNumIndex(&index2) >= 0) && 
	(asNumIndex(&index2) < self->itsDimensions[1]) &&
	(asNumIndex(&index3) >= 0) && 
	(asNumIndex(&index3) < self->itsDimensions[2]))
    indexOf = (asNumIndex(&index1) * self->itsDimensions[1] * self->itsDimensions[2]) + (asNumIndex(&index2) * self->itsDimensions[2]) + asNumIndex(&index3);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

FrameExit(TREAL(atHMReal(self->itsRealMatrix,indexOf)));
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.  The index may be symbolic or
numeric

Note:   

#endif

TVAL TNumMatrix_SetIV1(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL newValue)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(ret);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  We accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
	{
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
	}

/*  Save the new value in the matrix. */
atHMReal(self->itsRealMatrix,indexOf) = asNumIndex(&newValue);

asTag(ret) = TYNUMMATRIX;
asObject(ret) = (TObject*)self;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV2

Set the indexed value in the repeating portion of this object.  The index may be symbolic or
numeric

Note:   

#endif

TVAL TNumMatrix_SetIV2(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL newValue)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(ret);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  We accept numeric indices. */
if ((self->itsRank == 2) && 
	(isNumIndex(&index1)) && 
	(isNumIndex(&index2)) &&
	(asNumIndex(&index1) >= 0) && 
	(asNumIndex(&index1) < self->itsDimensions[0]) && 
	(asNumIndex(&index2) >= 0) && 
	(asNumIndex(&index2) < self->itsDimensions[1]))
    indexOf = (asNumIndex(&index1) * self->itsDimensions[1]) + asNumIndex(&index2);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Save the new value in the matrix. */
atHMReal(self->itsRealMatrix,indexOf) = asNumIndex(&newValue);

asTag(ret) = TYNUMMATRIX;
asObject(ret) = (TObject*)self;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetIV3

Set the indexed value in the repeating portion of this object.  The index may be symbolic or
numeric

Note:   

#endif

TVAL TNumMatrix_SetIV3(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index1,TVAL index2,TVAL index3,TVAL newValue)
{
NUM             indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(ret);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  We accept numeric indices. */
if ((self->itsRank == 3) && 
	(isNumIndex(&index1)) && 
	(isNumIndex(&index2)) &&
	(asNumIndex(&index1) >= 0) && 
	(asNumIndex(&index1) < self->itsDimensions[0]) && 
	(asNumIndex(&index2) >= 0) && 
	(asNumIndex(&index2) < self->itsDimensions[1]) &&
	(asNumIndex(&index3) >= 0) && 
	(asNumIndex(&index3) < self->itsDimensions[2]))
    indexOf = (asNumIndex(&index1) * self->itsDimensions[1] * self->itsDimensions[2]) + (asNumIndex(&index2) * self->itsDimensions[2]) + asNumIndex(&index3);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/*  Make sure array index is in range. */
if ((indexOf < 0) || (indexOf >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/*  Save the new value in the matrix. */
atHMReal(self->itsRealMatrix,indexOf) = asNumIndex(&newValue);

asTag(ret) = TYNUMMATRIX;
asObject(ret) = (TObject*)self;

FrameExit(*ret);
}


/*--------------------------------------------------------------------------------------- */
#if 0
AddNewValue

Add a new value to the repeating portion of this object.

Note: This is a private internal function only. The Matrix object 
	  does not support a published append function.

#endif

TVAL TNumMatrix_AddNewValue(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(index);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

asInt(index) = self->itsMaxItemIndex;
asTag(index) = TYNUM;

FrameExit(TNumMatrix_SetIV1(gCP,gTP,selfTval, *index, newValue));
}

/*--------------------------------------------------------------------------------------- */
#if 0
Delete

Delete the indexed value from the repeating portion of this object.

Note:   All of the remaining values are moved down one position and
        the Matrix is resized.

#endif

TVAL TNumMatrix_Delete(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index)
{
register LpREAL         targetPtr;
register LpREAL         sourcePtr;
register LpREAL         haltPtr;
NUM                     deleteIndex;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(err);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/* This operation supported only for Matrices of rank one. */

if (self->itsRank != 1) 
	{
	*err = TERROR("!delete: NumMatrix must be rank one!");
	FrameExit(*err);
	}

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    deleteIndex = asNumIndex(&index);
else
    FrameExit(gCP->TObject_ERROR_INVALID);
    
/*  Make sure Matrix index is in range. */
if ((deleteIndex < 0) || (deleteIndex >= self->itsMaxItemIndex))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Move all of the remaining values in the Matrix down one position */

sourcePtr = &atHMReal(self->itsRealMatrix,deleteIndex+1);
targetPtr = &atHMReal(self->itsRealMatrix,deleteIndex);
haltPtr = &atHMReal(self->itsRealMatrix,self->itsMaxItemIndex);
while (sourcePtr < haltPtr)
    {
    *(targetPtr++) = *(sourcePtr++);
    }
    
/*  Resize the Matrix down one position */
TNumMatrix_SetMaxIndex(gCP,gTP,selfTval,self->itsMaxItemIndex-1);
self->itsDimensions[0] = self->itsMaxItemIndex;

FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
Insert

Insert the indexed value in the repeating portion of this object. 

Note:   All of the haigher values are moved up one position and
        the Matrix is resized.

#endif

TVAL TNumMatrix_Insert(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL index,TVAL newValue)
{
register LpREAL         targetPtr;
register LpREAL         sourcePtr;
register LpREAL         insertPtr;
NUM                     insertIndex;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(err);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/* This operation supported only for Matrices of rank one. */

if (self->itsRank != 1) 
	{
	*err = TERROR("!insert: NumMatrix must be rank one!");
	FrameExit(*err);
	}

/*  We only accept numeric indices. */
if (isNumIndex(&index))
    insertIndex = asNumIndex(&index);
else
	{
	*err = TERROR("!insert: Invalid Index Argument!");
    FrameExit(*err);
	}
    
/*  Make sure Matrix index is in range. */
if ((insertIndex < 0) || (insertIndex > self->itsMaxItemIndex))
	{ 
	*err = TERROR("!insert: Index out of range!");  
	FrameExit(*err);
	}

/*  Resize the Matrix up one position */
TNumMatrix_SetMaxIndex(gCP, gTP, selfTval, self->itsMaxItemIndex+1);
self->itsDimensions[0] = self->itsMaxItemIndex;

/*  Move all of the remaining values in the Matrix up one position */

sourcePtr = &atHMReal(self->itsRealMatrix,(self->itsMaxItemIndex-2));
targetPtr = &atHMReal(self->itsRealMatrix,(self->itsMaxItemIndex-1));
insertPtr = &atHMReal(self->itsRealMatrix,insertIndex);
while (sourcePtr >= insertPtr)
    {
    *(targetPtr--) = *(sourcePtr--);
    }
    
/*  Insert the new value in the Matrix at the specified position */
atHMReal(self->itsRealMatrix,insertIndex) = asNumIndex(&newValue);
    
FrameExit(gCP->TObject_OK);
}


/*--------------------------------------------------------------------------------------- */
#if 0
GetCdr

Return the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TNumMatrix_GetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval)
{
TNumMatrix*    self = (TNumMatrix*)asObject(&selfTval);

gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
return(self->itsCdr);
}


/*--------------------------------------------------------------------------------------- */
#if 0
SetCdr

Set the Lisp tail(cdr) of this object.

Note:   All TObject children must have this method. Even objects which do
        not contain a Lisp head(car) or tail(cdr), must at least respond
        to this message.
 
#endif

TVAL TNumMatrix_SetCdr(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,TVAL newValue)
{
TNumMatrix*    self = (TNumMatrix*)asObject(&selfTval);

gTP = gTP; // NOOP to hide unused parameter warning message
self->itsCdr = newValue;
return(gCP->TObject_OK);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a NumMatrix object into an ascii string and append it to an output buffer. 

#endif

TVAL TNumMatrix_Print(LpXCONTEXT gCP,LpTHREAD gTP,TVAL selfTval,LpNUM size, LpCHAR buf)
{
NUM                     indexOf;
StartFrame	
DeclareOBJ(TNumMatrix,self);
DeclareTVAL(ec);
DeclareTVAL(err);
EndFrame

self = (TNumMatrix*)asObject(&selfTval);

/*  Quit if the output string is already too long */

if (*size + 200 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Matrix prefix */

buf[(*size)++]  = '#';
buf[(*size)++]  = '(';
buf[(*size)++]  = 'n';
buf[(*size)++]  = 'u';
buf[(*size)++]  = 'm';
buf[(*size)++]  = 'm';
buf[(*size)++]  = 'a';
buf[(*size)++]  = 't';
buf[(*size)]	= 0;

if (self->itsRank <= 1)
	{
	buf[(*size)++]  = '|';
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	}
else
if (self->itsRank == 2)
	{
	buf[(*size)++]  = '[';
	buf[(*size)]	= 0;
	FConio_sprintn(gCP,gTP,buf,size,TINT(self->itsDimensions[0]));
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	FConio_sprintn(gCP,gTP,buf,size,TINT(self->itsDimensions[1]));
	buf[(*size)++]  = ']';
	buf[(*size)++]  = '|';
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	}
else
if (self->itsRank == 3)
	{
	buf[(*size)++]  = '[';
	buf[(*size)]	= 0;
	FConio_sprintn(gCP,gTP,buf,size,TINT(self->itsDimensions[0]));
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	FConio_sprintn(gCP,gTP,buf,size,TINT(self->itsDimensions[1]));
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	FConio_sprintn(gCP,gTP,buf,size,TINT(self->itsDimensions[2]));
	buf[(*size)++]  = ']';
	buf[(*size)++]  = '|';
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	}
else
	{
	*err = TERROR("!nummatrix object with invalid rank!");  
	FrameExit(*err);
	}


for(indexOf = 0; indexOf < self->itsMaxItemIndex; indexOf++)
    {
    *ec = FConio_sprintn(gCP,gTP,buf,size,TREAL(atHMReal(self->itsRealMatrix,indexOf)));
    _TObject_ErrorChk(*ec);
    
     if (*size + 200 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[(*size)++]  = ' ';
    buf[(*size)]	= 0;
    }

/*  Show the Matrix's cdr if it is not void */

if (asTag(&self->itsCdr))
    {
     if (*size + 200 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
    buf[(*size)++]  = ' ';
    buf[(*size)++]  = '.';
    buf[(*size)++]  = ' ';
    buf[(*size)]	= 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,self->itsCdr);
    _TObject_ErrorChk(*ec);
    }


/*  Show Matrix suffix */

 if (*size + 200 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[(*size)++]  = ')';
buf[(*size)]	= 0;

FrameExit(gCP->TObject_TRUE);
}


/*--------------------------------------------------------------------------------------- */
#if 0
TNumMatrix_Resize

Resize a TNumMatrix.

#endif

TVAL TNumMatrix_Resize(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[])
{
NUM                     rank = 1;
NUM                     size = 0;
NUM                     dimensions[_MAXDIMENSIONS];
NUM                     dn = 0;
NUM                     startIndex = 0;
StartFrame
DeclareTVAL(self);
DeclareTVAL(ret);
DeclareOBJ(TNumMatrix,mp);
EndFrame
 
/*  The first argument should be the matrix object itself. */

if ((argc < 1) || (argv[0].Tag != TYNUMMATRIX)) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
*self = argv[0];
mp = argv[0].u.NumMatrix;
startIndex = 1;


/*  The next argument should be the requested rank. */
/*  Note: If there is no rank argument, then the matrix defaults to a rank of one. */

for (dn = 0; dn < _MAXDIMENSIONS; ++dn) dimensions[dn] = 1;

/* Get the rank and size of the new matrix. */
/* Note: This may be either size alone or rank dim1 ... dim3. */

if ((argc - startIndex) == 0)
	{
	/* No Matrix size was specified. Default to size zero, rank one. */
	size = 0;
	rank = 1;
	}
else
if ((argc - startIndex) == 1)
	{
	/* Only a Matrix size was specified. Default to rank of one. */
	if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	size = asNumIndex(&argv[startIndex]);
	++startIndex;
	if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	rank = 1;
	}
else
	{
	/* A Matrix rank and a dimension list have been specified. */
	if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	rank = asNumIndex(&argv[startIndex]);
	++startIndex;
	if ((rank < 1) || (rank >= _MAXDIMENSIONS))  FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	if ((argc - startIndex) < rank)  FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	size = 1;
	for (dn = 0; dn < rank; ++dn) 
		{
		if (!isNumIndex(&argv[startIndex])) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		dimensions[dn] = asNumIndex(&argv[startIndex]);
		++startIndex;
		if (dimensions[dn] <= 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
		size *= dimensions[dn];
		}
	if (size < 0) FrameExit(gCP->TObject_ERROR_INVALID_ARGLIST);
	}

/*  Resize the Matrix object */

*ret = TNumMatrix_SetMaxIndex(gCP,gTP,*self,size);
NumMatrix(*self)->itsRank = rank;
for (dn = 0;dn < _MAXDIMENSIONS;++dn) NumMatrix(*self)->itsDimensions[dn] = dimensions[dn];
    
FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
TNumMatrix_New

Create a new TNumMatrix.

#endif

TNumMatrix* TNumMatrix_New(LpXCONTEXT gCP,LpTHREAD gTP)
{
NUM				dn;
StartFrame
DeclareOBJ(TNumMatrix,self);
EndFrame

/*  This class must be initialized. */
if (!gCP->TNumMatrix_Initialized) TNumMatrix_Init(gCP,gTP);

self = (TNumMatrix*)TObject_OperatorNew(gCP,gTP);
self->itsObjectType = TYNUMMATRIX;
self->itsMaxItemIndex = 0;
self->itsCdr = gCP->Tval_VOID;
self->itsRealMatrix = NULL;
self->itsImmediatePtr = NULL;
self->itsRank = 1;
for (dn = 0; dn <= self->itsRank; ++dn) self->itsDimensions[dn] = 1;

FrameExit(self);
}

/*--------------------------------------------------------------------------------------- */
#if 0
ComputeSize

The oodbms is trying to compute the size of HMemory required to store all of your data. 
Add your size requirements to the input size argument.

#endif

void TNumMatrixRow_ComputeSize(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, NUM* aSize)
{
gCP = gCP; // NOOP to hide unused parameter warning message
gTP = gTP; // NOOP to hide unused parameter warning message
selfTval = selfTval; // NOOP to hide unused parameter warning message
*aSize = SIZEOF_TObjectOnDisk + SIZEOF_TNumMatrixRowOnDisk;
ALLIGNME(*aSize);
}

/*--------------------------------------------------------------------------------------- */
#if 0
GetIV1

Return the indexed value from the repeating portion of this object.

#endif

TVAL TNumMatrixRow_GetIV1(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL index1)
{
NUM         indexOf = 0;
NUM         actualIndex = 0;
TNumMatrix* matrixObj = NIL;

StartFrame
DeclareTVAL(ret);
EndFrame

/* The MatrixRow immediate type contains 3 values:
 * 1. Object Index of the Matrix
 * 2. 1st dimension
 * 3. 2nd dimension
 *
 * A MatrixRow instance can represent the 2nd level or 3rd level of the Matrix
 * If the 2nd dimension is less than 0, then it is a 2nd level MatrixRow
 * Otherwise, it is a 3rd level MatrixRow
 */

/* Get the pointer to our matrix object. */
matrixObj = (TNumMatrix*)_TObject_MainObjectList(ObjIdx(selfTval));
if ((matrixObj == NULL) || (matrixObj->itsObjectType != TYNUMMATRIX))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  We accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
/* Compute for the maximum range. */
if (FldIdx(selfTval) >= 0)
    {
    /* We're on the 3rd dimension */
    if ((indexOf < 0) || (indexOf >= matrixObj->itsDimensions[2]) || (matrixObj->itsRank < 3))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /* actualIndex = (row * 2dim * 3dim)) + (field * 3dim) + index */
    actualIndex = (RowIdx(selfTval) * matrixObj->itsDimensions[1] * matrixObj->itsDimensions[2]) + (FldIdx(selfTval) * matrixObj->itsDimensions[2]) + indexOf;
    *ret = TREAL(atHMReal(matrixObj->itsRealMatrix,actualIndex));
    }   
else
if (RowIdx(selfTval) >= 0)
    {
    /* We're on the 2nd dimension */
    if ((indexOf < 0) || (indexOf >= matrixObj->itsDimensions[1]) || (matrixObj->itsRank < 2))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
        
    /* If the no. of dimensions is greater than 2, return another MatrixRow */
    if (matrixObj->itsRank > 2)
        {
        asTag(ret) = TYNUMMATRIXROW;
        asObjIdx(ret) = matrixObj->itsObjectIndex;
        asRowIdx(ret) = RowIdx(selfTval);
        asFldIdx(ret) = indexOf;
        }
    else
        {
        /* actualIndex = (row * 2dim) + index */
        actualIndex = (RowIdx(selfTval) * matrixObj->itsDimensions[1]) + indexOf;
        *ret = TREAL(atHMReal(matrixObj->itsRealMatrix,actualIndex));
        }
    }
else
    /* MatrixRow object is invalid */
    FrameExit(gCP->TObject_ERROR_INVALID);

FrameExit(*ret);
}

/*--------------------------------------------------------------------------------------- */
#if 0
Print

Convert a Matrix object into an ascii string and append it to an output buffer. 

#endif

TVAL TNumMatrixRow_Print(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, LpNUM size, LpCHAR buf)
{
NUM         indexOf = 0;
NUM         startIndex = 0;
NUM         endIndex = 0;
TNumMatrix* matrixObj = NIL; 

StartFrame	
DeclareTVAL(ec);
DeclareTVAL(err);
EndFrame

/* Get the pointer to our matrix object. */
matrixObj = (TNumMatrix*)_TObject_MainObjectList(ObjIdx(selfTval));
if ((matrixObj == NULL) || (matrixObj->itsObjectType != TYNUMMATRIX))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  Quit if the output string is already too long */
if (*size + 12 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);

/*  Show Matrix prefix */
buf[(*size)++]  = '{';
buf[(*size)++]  = 'N';
buf[(*size)++]  = 'u';
buf[(*size)++]  = 'm';
buf[(*size)++]  = 'M';
buf[(*size)++]  = 'a';
buf[(*size)++]  = 't';
buf[(*size)++]  = 'r';
buf[(*size)++]  = 'i';
buf[(*size)++]  = 'x';
buf[(*size)++]  = 'R';
buf[(*size)++]  = 'o';
buf[(*size)++]  = 'w';
buf[(*size)++]  = '|';
buf[(*size)++]  = ' ';
buf[(*size)]	= 0;

if (matrixObj->itsRank == 2)
	{
    if (*size + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
	buf[(*size)++]  = '[';
	buf[(*size)]	= 0;

	if (RowIdx(selfTval) >= 0)
	    {
		FConio_sprintn(gCP, gTP, buf, size, TINT(matrixObj->itsDimensions[1]));
		}
    else
        FrameExit(gCP->TObject_ERROR_INVALID);

    if (*size + 4 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);

	buf[(*size)++]  = ']';
	buf[(*size)++]  = '|';
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;

	startIndex = RowIdx(selfTval) * matrixObj->itsDimensions[1];
	endIndex = startIndex + matrixObj->itsDimensions[1];
	}
else
if (matrixObj->itsRank == 3)
	{
    if (*size + 2 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
        
	buf[(*size)++]  = '[';
	buf[(*size)]	= 0;

	if (FldIdx(selfTval) >= 0)
	    {
        FConio_sprintn(gCP, gTP, buf, size, TINT(matrixObj->itsDimensions[2]));

        startIndex = (RowIdx(selfTval) * matrixObj->itsDimensions[1] * matrixObj->itsDimensions[2]) + (FldIdx(selfTval) * matrixObj->itsDimensions[2]);
        endIndex = startIndex + matrixObj->itsDimensions[2];
	    }
	else
	if (RowIdx(selfTval) >= 0)
	    {
	    FConio_sprintn(gCP, gTP, buf, size, TINT(matrixObj->itsDimensions[1]));
	    buf[(*size)++]  = ' ';
	    buf[(*size)]	= 0;
	    FConio_sprintn(gCP, gTP, buf, size, TINT(matrixObj->itsDimensions[2]));
	    
        startIndex = RowIdx(selfTval) * matrixObj->itsDimensions[1] * matrixObj->itsDimensions[2];
	    endIndex = startIndex + (matrixObj->itsDimensions[1] * matrixObj->itsDimensions[2]);
	    }
	else
	    FrameExit(gCP->TObject_ERROR_INVALID);

    if (*size + 4 > gCP->TObject_MaxOutputLen) 
        FrameExit(gCP->TObject_FALSE);
	
	buf[(*size)++]  = ']';
	buf[(*size)++]  = '|';
	buf[(*size)++]  = ' ';
	buf[(*size)]	= 0;
	}
else
	{
	*err = TERROR("!matrix object with invalid rank!");  
	FrameExit(*err);
	}

for(indexOf = startIndex; indexOf < endIndex; indexOf++)
    {
	buf[(*size)]	= 0;
    *ec = FConio_sprintn(gCP,gTP,buf,size,TREAL(atHMReal(matrixObj->itsRealMatrix,indexOf)));
    ExitOnError(*ec);
    
    if ((indexOf + 1) < endIndex)
        {
        if (*size + 2 > gCP->TObject_MaxOutputLen) 
            FrameExit(gCP->TObject_FALSE);
            
        buf[(*size)++]  = ' ';
        buf[(*size)]	= 0;
        }
    }

/*  Show Matrix suffix */
if (*size + 2 > gCP->TObject_MaxOutputLen) 
    FrameExit(gCP->TObject_FALSE);
    
buf[(*size)++]  = '}';
buf[(*size)]	= 0;

FrameExit(gCP->TObject_TRUE);
}

/*--------------------------------------------------------------------------------------- */
#if 0
SetIV1

Set the indexed value in the repeating portion of this object.  The index may be symbolic or
numeric

Note:   

#endif

TVAL TNumMatrixRow_SetIV1(LpXCONTEXT gCP, LpTHREAD gTP, TVAL selfTval, TVAL index1, TVAL newValue)
{
NUM         indexOf = 0;
NUM         actualIndex = 0;
TNumMatrix* matrixObj = NIL;

StartFrame	
DeclareTVAL(ret);
EndFrame

/* Get the pointer to our matrix object. */
matrixObj = (TNumMatrix*)_TObject_MainObjectList(ObjIdx(selfTval));
if ((matrixObj == NULL) || (matrixObj->itsObjectType != TYNUMMATRIX))
    FrameExit(gCP->TObject_ERROR_INVALID);

/*  We accept numeric indices. */
if (isNumIndex(&index1))
    indexOf = asNumIndex(&index1);
else
    FrameExit(gCP->TObject_ERROR_BADIDXORKEY);

/* Compute for the maximum range. */
if (FldIdx(selfTval) >= 0)
    {
    /* We're on the 3rd dimension */
    if ((indexOf < 0) || (indexOf >= matrixObj->itsDimensions[2]) || (matrixObj->itsRank < 3))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
    
    /* actualIndex = (row * 2dim * 3dim)) + (field * 3dim) + index */
    actualIndex = (RowIdx(selfTval) * matrixObj->itsDimensions[1] * matrixObj->itsDimensions[2]) + (FldIdx(selfTval) * matrixObj->itsDimensions[2]) + indexOf;
    atHMReal(matrixObj->itsRealMatrix,actualIndex) = asNumIndex(&newValue);
    }   
else
if (RowIdx(selfTval) >= 0)
    {
    /* We're on the 2nd dimension */
    if ((indexOf < 0) || (indexOf >= matrixObj->itsDimensions[1]) || (matrixObj->itsRank < 2))
        FrameExit(gCP->TObject_ERROR_BADIDXORKEY);
        
    /* actualIndex = (row * 2dim) + index */
    actualIndex = (RowIdx(selfTval) * matrixObj->itsDimensions[1]) + indexOf;
    atHMReal(matrixObj->itsRealMatrix,actualIndex) = asNumIndex(&newValue);
    }
else
    /* MatrixRow object is invalid */
    FrameExit(gCP->TObject_ERROR_INVALID);

*ret = selfTval;

FrameExit(*ret);
}
