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
FSmtbase.h

Declarations  of the top level API procedures required for the Smartbase
High Speed Engine.

AUTHORS:            Michael F. Korns

REFERENCES:

    [1]     "THINK C (5.0) Object Oriented Programming Manual", Philip Borenstein
            & Jeff Mattson, Symantec Corporation, Cupertino California, 1991.

    [2]     "THINK C (5.0) Standard Libraries Reference", Philip Borenstein
            & Jeff Mattson, Symantec Corporation, Cupertino California, 1991.

    [3]     "THINK C (5.0) User`s Manual", Philip Borenstein & Jeff Mattson,
            Symantec Corporation, Cupertino California, 1991.

    [4]     "Structure and Interpretation of Computer Programs", Harold Abelson
            Gerald Sussman & Julie Sussman, McGraw-Hill Book Company, New York,
            1985.

    [5]     "Revised(3) Report on the Algorithmic Language Scheme", Harold Abelson
            et. al., MIT AI Laboratory, AI Memo 848a, September 1986.

    [6]     "Common Lisp: The Reference", Franz Inc., Addison-Wesley Publishing
            Company, Menlo Park California, 1988.

    [7]     "Inside Macintosh Vols I - V", Apple Inc., Addison-Wesley Publishing
            Company, Menlo Park California, 1985.

    [8]     "Techniques Of Artificial Intelligence", Stuart C. Shapiro, D Van Nostrand
            Company, New York, 1979.

    [9]     "Lisp", Patrick Winston & Berthold Horn, Addison-Wesley Publishing Company,
            Menlo Park California, 1981.

    [10]    "Common Lisp: A Tutorial", Wendy L. Milner, Prentice Hall, Englewood Cliffs
            New Jersey, 1988.

    [11]    "Natural Language Processing in Lisp", Gerald Gazdar & Chris Mellish,
            Addison-Wesley Publishing Company, Menlo Park California, 1989.


CHANGE HISTORY
Version	 Date		Who		Change
5.005    11/16/2008 rca     Added vmregRefWord,vmregRefXWord,vmregSetWord,vmregSetXWord instructions.
5.0000   8/20/2008  fchua   MySQL Support.
5.0000   8/20/2008  fchua   New object types (TBrickRow, TBrickField, TStringSubstring).
5.0000   8/20/2008  fchua   New immediate types (TBrickRowT, TBrickFieldT, TStringSubstringT).
5.000    8/20/2008  mfk     Updated TLambda, TDatabase structure, Object Disk Header definitions.
3.0000	 6/26/2007	mfk		Beta Release New Class Lisp language features for 32bit architectures
2.0001	 1/3/2007	tlw		Add MAX_INT, MIN_INT, MAX_LONG, MIN_LONG defines.
1.0101	 8/03/2006	tlw		Define BIGENDIAN, bit-sizes for AMD64
		 12/9/2006  TM		Added FSMARTBASE_ERR_BADBUFFERSIZE

Compiler/Build switches -- please set these in the makefile (or IDE), dont set them here as it makes
cross platform version control difficult. You must set one of each to be assured of predictable behavior

OS selection Switches
_MACOS, _SUN, _LINUX, _WIN

Compiler/Toolchain Switches
_MSVC, _GCC

CPU Selection Switches
_PENTIUM, _AMD64, _UNKNOWN32

AIS Configuration Switches
_JIT		// Include Just In Time Compiler code

Typical Configurations By Target Environment
WindowsXP with MSVC
_WIN _MSVC _PENTIUM _JIT	// Windows XP Pro 32 with MS Visual Studio 2003
_WIN _MSVC _AMD64 _JIT		// Windows XP Pro 64 with MS Visual Studio 2005

_LINUX _GCC _AMD64       	// SUSE Linux 10.1 with GCC, no JIT
_LINUX _GCC _AMD64 _JIT  	// SUSE Linux 10.1 with GCC, with JIT

#endif

/*  Make sure that we skip this include file if it has already been seen. */
/*  Also, make sure that all common variables are declared "extern" if */
/*  the common keyword has not explicitly been declared. */

#ifndef _H_FSmartbase
#define _H_FSmartbase

#ifdef _LINUX
#define __int32 long
#endif

/*  SmartBase Engine Extended Configuration Options. */
/*  Notes:  A value of 1 indicates the specified extended */
/*          configuration be included in this build.  */

#define __EXMYSQL	        1       /* MySQL Database Support */

//  SmartBase Engine Version ID.
//  Notes:  Returned by the Lisp (version) function. 
//          This is the current version of the
//          Smartbase engine including all of
//          the new Lisp registered Functions and
//          features available in this version.
#define CURRENT_VERSION		"20170330"


//  SmartBase Disk File Version Id.
//  Warning: Not related to the AIS Version above.
//           Multiple AIS versions my share the
//           same digital Disk File Version Id.
//  Notes:   First digit is major release Number.
//           Second is revision number.
//           The Disk File Version is not meant
//           to match the Smartbase version and
//           release numbers because not ever
//           release changes the disk signature.

// ************************************************************************************************
// NOTE: THIS NUMERIC _BASEVERSION SHOULD ALWAYS BE DIVISIBLE BY 4 AND MUST BE BETWEEN 000 AND 400!
//       The _BASEVERSION changes ONLY when the Object Repository disk footprint changes!
//       When this _BASEVERSION number changes, all existing Smartbase Object Repositories will
//       be unreadable, and will have to be recreated from original data backups.
// ************************************************************************************************
#define _BASEVERSION 56

#ifdef _LINUX

#ifdef _M64
// NOTE: THIS VALUE IS FOR LINUX 64-bit and SHOULD ALWAYS BE EQUAL TO THE _BASEVERSION
#define _VERSION     _BASEVERSION + 0
#else
// NOTE: THIS VALUE IS FOR LINUX 32-bit and SHOULD ALWAYS BE +1 OF THE _BASEVERSION
#define _VERSION     _BASEVERSION + 1
#endif
#else
#ifdef _M64
// NOTE: THIS VALUE IS FOR WINDOWS 64-bit and SHOULD ALWAYS BE +2 OF THE _BASEVERSION
#define _VERSION     _BASEVERSION + 2
#else
// NOTE: THIS VALUE IS FOR WINDOWS 32-bit and SHOULD ALWAYS BE +3 OF THE _BASEVERSION
#define _VERSION     _BASEVERSION + 3
#endif
#endif

/*  Host system external symbol definition (How API symbols are exported). */
#ifndef PUBLIC
#ifdef _MSVC
#define PUBLIC              extern _declspec(dllexport)
#else
#define PUBLIC              extern
#endif
#endif


/*  Declare the bitsize of the register data types */
#ifdef _M32
#define BITSIZEOFFLOAT		2
#define BITSIZEOFNUM32		2
#define BITSIZEOFNUM		2
#define BITSIZEOFREAL		3
#define BITSIZEOFTVAL		4
#define BITSIZEOFSHORT		1
#define _FSmartbase_HeapBlockSize	128
#elif defined _M64
#define BITSIZEOFFLOAT		2
#define BITSIZEOFNUM32		2
#define BITSIZEOFNUM		3
#define BITSIZEOFREAL		3
#define BITSIZEOFTVAL		4
#define BITSIZEOFSHORT		1
#define _FSmartbase_HeapBlockSize	256
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif

/*  Declare the arithmetic overflow macros for each machine. */
/*  Note:   These macros must reflect the machine language */
/*          check for the host machine. The default is not to */
/*          check for arithmetic overflow. */
#define JumpNoOverflow(label)		goto label;
#define JumpOnOverflow(label)
#ifdef _MSVC
#define JumpOverflowSet 1
#ifdef _M32
#undef JumpNoOverflow
#define JumpNoOverflow(label)		_asm { jno label }
#undef JumpOnOverflow
#define JumpOnOverflow(label)		_asm { jo label }
#elif defined _M64
#define JumpNoOverflow(label)		goto label;
#define JumpOnOverflow(label)
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif
#endif


/* Declare some derived types */
#undef BIGENDIAN		// So far, we support no big endian platforms.

/*  Host system pragma declarations (How Needless warnings are turned off). */
#ifdef _MSVC
#pragma warning(disable: 4001)	// nonstandard extension 'single line comment' was used
#pragma warning(disable: 4100)	// unreferenced formal parameter
#pragma warning(disable: 4101)	// unreferenced local variable
#pragma warning(disable: 4201)	// nameless unions are part of C++
#pragma warning(disable: 4209)	// nonstandard extension used : benign typedef redefinition
#pragma warning(disable: 4214)	// long double is the same precision as double
#pragma warning(disable: 4699)	// Creating new precompiled header WinDebug/SMTBASE.pch
#endif

/*  To avoid prepending the current Global path to a file spec. that is already an absolute
	path, uncomment the following definition. Otherwise, smtbase works as it always has. */
#define _DETECTABSPATH 1

/*  Include these header files for ANSI Standard C on most platforms. */
/*  Set compiler on ANSII settings, but turn off check pointer types */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#ifndef __cplusplus
	#include <math.h>
#endif
#include <float.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <time.h>

#ifdef MEMTEST
#include <assert.h>	/* TM -- added to help check new FMemory.c */
// #include <crtdbg.h>
#endif

/*  MAXIMUM NUMBER OF ARGUMENTS */
/* */
/*  FSmartbase supports function calls between C programs and Lambdas. */
/*  This defines the maximum number of arguments in a function call. */

#define __MAXPARMS			50		/* Maximum number of arguments in function calls */


/*  COMPILER ALLIGNMENT FACTOR */
/* */
/*  FSmartbase supports automatic calculation of the C compiler allignment */
/*  factor for the current compilation. Each host computer may require   */
/*  allignment of structures to one, two, four, or eight byte boundries. */

typedef struct {char x;double y;} allignment;
#define ALLIGNMENTFACTOR  (sizeof(allignment) - sizeof(double))
#define ALLIGNME(isize)   isize = (((isize + ALLIGNMENTFACTOR - 1) / ALLIGNMENTFACTOR) * ALLIGNMENTFACTOR)

/*  When _FVMScript_ESCAPECHECK may be defined as an integer counter indicative of the number of */
/*  instructions which may execute between calling the host to check for an escape (user-abort) */
/*  for the current operation. */
/*  Note: On the Macintosh the new escape code is (command-period). */
#define _FSMARTBASE_ESCAPECHECK  1000

/*  CATCH & THROW ERROR RETURN CODE MACROS */
/* */
/*  FSmartbase supports catch & throw macros. These macros allow resumption */
/*  of execution from a standard reference point after an error condition. */
/* */
/*  Note:   Do not return status codes of 0 or 1 with the THROW macro!!!  */
/*          While our product uses these codes, ANSII C does not */
/*          support them with CATCH and THROW. */

#define FSMARTBASE_ERR_OUT_OF_MEMORY					1001
#define FSMARTBASE_ERR_FRAME_ERROR						1002
#define FSMARTBASE_ERR_INVALID							1003
#define FSMARTBASE_ERR_STACK							1004
#define FSMARTBASE_ERR_ESCAPE							1005
#define FSMARTBASE_ERR_PCODE							1006
#define FSMARTBASE_ERR_BAD_DATATYPE						1007
#define FSMARTBASE_ERR_RECURSION						1008
#define FSMARTBASE_ERR_FRAME_RELEASE					1009
#define FSMARTBASE_ERR_STACK_RELEASE					1010
#define FSMARTBASE_ERR_RECURSION_RELEASE				1011
#define FSMARTBASE_ERR_QUIT								1012
#define FSMARTBASE_ERR_WRONG_VERSION					1013
#define FSMARTBASE_ERR_ENGINE_BUSY						1014
#define FSMARTBASE_ERR_REPOSITORY_GC					1015
#define FSMARTBASE_ERR_REPOSITORY_LOCKOUT				1016
#define FSMARTBASE_ERR_MEMORY_REQUEST_TOO_LARGE			1017
#define FSMARTBASE_ERR_OUT_OF_MEMORY_FRAGMENTED			1018
#define FSMARTBASE_ERR_TOO_MANY_OBJECTS					1019
#define FSMARTBASE_REQUEST_SUSPEND						1020
#define FSMARTBASE_ERR_BADBUFFERSIZE					1021


/*  Definition of CStack size and maxi mum recursions allowed. */
/*  Note: The Smartbase engine executes, C functions and Lambda functions */
/*		  all recursing on the C Stack, the Tval Stack, and the Object Stack. */
/*		  These C Stack sizes and maximum recursion sizes have been emperically */
/*        derrived to allow a large number of recursions WITHOUT causing */
/*		  an actual C Stack overflow. The gTP->_MaxRecursions will trigger */
/*        a system error long before the C Stack actually overflows (hopefully). */
#define		FSMARTBASE_MAINCONTEXTSTACK			80000000
#define		FSMARTBASE_MAINCONTEXTMAXRECURSIONS	4000

/* Maximum number of OS provide memory allocation blocks for a context */
#define		FSMARTBASE_MAXCONTEXTMEMORYBLOCKS	20

/* Minimum memory allocation block size for a context */
#define		FSMARTBASE_MINBLOCKSIZE				10000000
#define		FSMARTBASE_BLOCKDECREMENT			10000000

/*  PORTABLE TYPES */
/* */
/*  FSmartbase supports a number of portable types for Lisp and C. */
/* */
/*  Note: When porting or changing the size of NUM, LONG, REAL, or TOKEN  */
/*        make sure to also change the necessary code in the FSmartbase  */
/*        subsystems. */

#define NIL             0
typedef char*           CPTR,   * PCPTR;
typedef CPTR*           HDL,    * PHDL;
#define HNIL            (HDL)0L
#define PNIL            (CPTR)0L
#define LpNIL           (char *)0L
#define LhNIL           (char **)0L
#define VOID            void
typedef void                    * LpVOID;           /*  Really means void: a nothing indicator */
typedef char			BOLE,   * LpBOLE;
typedef short           COMPARE,* LpCOMPARE;
typedef short           ERR,    * LpERR;
typedef char            CHAR,   * LpCHAR;
typedef unsigned char   UCHAR,  * LpUCHAR;
typedef unsigned short  UINT16, * LpUINT16;
typedef unsigned int    UINT32, * LpUINT32;
typedef short			INT16,  * LpINT16;
typedef int				INT32,  * LpINT32;
#ifdef _M64
#ifdef _GCC
typedef unsigned long   UNUM,   * LpUNUM;
typedef int             NUM32,  * LpNUM32;
typedef long            NUM,    * LpNUM;
typedef long            OBJ,    * LpOBJ;            /*  Really refers to any TObject* or descendent */
typedef unsigned long	ULONG;
typedef long			LONG;
#elif defined _MSVC
typedef unsigned __int64 UNUM,  * LpUNUM;
typedef __int32	        NUM32,  * LpNUM32;
typedef __int64         NUM,    * LpNUM;
typedef long			LONG;
typedef unsigned long	ULONG;
typedef __int64			OBJ,    * LpOBJ;            /*  Really refers to any TObject* or descendent */
#else
#error "The toolset for this build is not defined in the preprocessor defines for this configuration"
#endif
#define MAXNUM			 9223372036854775807L
#define MINNUM			-9223372036854775808L
#define _MAXNUM			 9223372036854775807.0
#define _MINNUM			-9223372036854775808.0
#elif defined _M32
typedef unsigned long   UNUM,   * LpUNUM;
typedef long            NUM32,  * LpNUM32;
typedef long            NUM,    * LpNUM;
typedef unsigned __int32 ULONG;
typedef __int32			 LONG;
typedef long            OBJ,    * LpOBJ;            /*  Really refers to any TObject* or descendent */
#define MAXNUM			 2147483647
#define MINNUM			-2147483648
#define _MAXNUM			 2147483647.0
#define _MINNUM			-2147483648.0
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif
typedef short			FLAG;
typedef char*           POINTER;
typedef POINTER         * LpPOINTER;
typedef long*           PNTR;
typedef PNTR            * LpPNTR;
typedef float           FLOAT,  * LpFLOAT;
typedef double          REAL,   * LpREAL;
typedef short           SHORT,  * LpSHORT;
typedef char			TEXT,   * LpTEXT;
typedef short           TYPE,   * LpTYPE;
typedef char            NAME[20];
typedef char            STREAM[1];					/*  Type delimited variable length data stream */
typedef STREAM			* LpSTREAM;
typedef NAME            * LpNAME;
#ifdef _M64
#ifdef _MSVC
#define INTFORMAT       "%I64d"
#define UINTFORMAT      "%I64u"
#define POINTERFORMAT   "0x%p"
#elif defined _GCC
#define INTFORMAT       "%ld"
#define UINTFORMAT      "%lu"
#define POINTERFORMAT   "%p"
#else
#error "The toolset for this build is not defined in the preprocessor defines for this configuration"
#endif
#elif defined _M32
#define INTFORMAT       "%ld"
#define UINTFORMAT      "%lu"
#define POINTERFORMAT   "0x%p"
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif

#define REALFORMAT      "%.12e"
#define SHORTFORMAT     "%d"
#define HEXFORMAT       "%lx"
#define MAX_INT			 2147483647
#define MIN_INT			-2147483648
#ifdef _M64
#define MAX_LONG		9223372036854775807L
#define MIN_LONG		-9223372036854775808L
#define _MAXLONG		 9223372036854775807.0
#define _MINLONG		-9223372036854775808.0
#elif defined _M32
#define MIN_LONG		0x80000000
#define MAX_LONG		0x7fffffff
#define _MAXLONG		 2147483647.0
#define _MINLONG		-2147483648.0
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif
#define _MAXREAL        DBL_MAX
#define _MINREAL        DBL_MIN
#define _MAXDIMENSIONS  3

#ifndef LONG_MAX
#define LONG_MAX MAXNUM
#endif

#define NAN_32    0x7FC00000
#define NAN_64_HI 0x7FF80000
#define NAN_64_LO 0x00000000
#define INF_64_HI 0x7FF00000
#define INF_64_LO 0x00000000

#define REALCHECK(r)    ((_MAXREAL < (r)) ? _MAXREAL : ((-_MAXREAL > (r)) ? -_MAXREAL : (((-_MINREAL < (r)) && (_MINREAL > (r))) ? 0.0 : (r))))
#define INTEGER(r)		((_MAXLONG < (r)) ? (MAX_LONG) : ((_MINLONG > (r)) ? (MIN_LONG) : ((NUM)(r))))

/*  FSMARTBASE ROOT CLASS DISK LAYOUT */
/*  FSmartbase supports a root class known as TObject. */
/*  The TObject root class forms the basis for all other   */
/*  FSmartbase class structures. These definitions include */
/*  both the memory and the disk root class structure layout. */

typedef struct
    {
    LONG                itsVariableLength;  /*  Inclusive disk size */
    TYPE                itsObjectType;      /*  Dependent on init order! */
    LONG                itsObjectData[1];
    }TObjectOnDisk;
#define TObjectOnDiskPtr(h,n)   ((TObjectOnDisk*)(_HMemoryPtr(h)+n))
#define SIZEOF_TObjectOnDisk    ((NUM)&((TObjectOnDisk*)0)->itsObjectData)


// DEBUG
#define _FSMTBASE_LOG(msg) {FILE *apFile;if ((apFile = fopen("SysLog.txt", "a"))!= NULL){fwrite(msg,1,strlen(msg),apFile);fclose(apFile);}}

/*  Define the memory handle structure used by the memory manager */

#define		_MEMMAXFRAMES           500L
// TLW extend the marker in mh to an unsigned.  Since pointers are on 4 or 8 byte boundaries, no more space is consumed.
#define		_MARKERVALUE			(unsigned)0x89abcdef

typedef struct mh
    {
        POINTER         fwd;            /*  Pointer to next free handle (free), to data (busy)	*/
#if MEMTEST
        unsigned	marker;				/*  Marker used in test for overwrite */
        unsigned	marker2;			/*  Second marker used in test for overwrite */
#endif
        struct mh*      back;           /*  Pointer to previous free frame handle (free)		*/
        struct mh*      next;           /*  Pointer to next allocated block						*/
        struct mh*      prev;           /*  Pointer to previously allocated block				*/
        LONG            userSize;       /*  Buffer size as last requested by user (<= true size)*/
        SHORT	        Frame;          /*  Frame Index For Handle								*/
        BOLE            Free;           /*  Free Switch (true iff block is free)				*/
        BOLE            Fixed;          /*  Fixed frame Switch (true iff block is fixed)		*/
        LONG            data[1];        /*  Data Block											*/
     }   MHANDLEINFO, *LpMHANDLEINFO;
/* The following macro returns the size of the header up to but */
/* not including any portion of the data[] element */
/* Note: Please keep this total size to an even multiple of eight bytes. */
#define SIZEOF_MHANDLEINFO  ((NUM)&((MHANDLEINFO*)0)->data)


/* CLASS CONSTANT DECLARATIONS */

/* The TObject CLASS supports several CLASS constants for use */
/* in marking, locking, and pinning the object list entries.  */

#define _TObject_OfVOID             0
#define _TObject_OfOBJECT           1
#define _TObject_OfMARK             2
#define _TObject_OfLOCK             4
#define _TObject_OfPERM             8
#define _TObject_OfIGNORE           16

#define _TObject_TfNATIVE           0
#define _TObject_TfTOBJECT          1
#define _TObject_TfCOBJECT          2
#define _TObject_TfIOBJECT          3

#define _TObject_NILSavedObject     -1


/*  LINEBREAKS */
/* */
/*  FSmartbase supports a a portable test for ascii line breaks. */
/* */
/*  Note: When porting between MSDOS, Unix, Windows, Macintosh, etc.  */
/*        one must make sure to check for either a line feed (10) or  */
/*        a carriage return (13). */

#define LINEBREAK       10
#define RETURNCHR       13
#define ISLINEBREAK(c)  (((c) == 10) || ((c) == 13))

/*  BOLE VALUES */
/* */
/*  SmartBase contains its own values for the BOLE data type. The values for  */
/*  BOLE include not only true and false, but also comparison and error */
/*  status values. These values are declared herein to maximize portability. */

#ifndef TRUE
#define TRUE            1
#endif
#ifndef FALSE
#define FALSE           0
#endif
#define YES             1
#define NO              0
#define ON              1
#define OK              1
#define OFF             0
#define HIGH            1
#define EQUAL           0
#define LOW             -1

/*  *********************************************************************  */
/*  TVAL DATA TYPE                                                         */
/*                                                                         */
/*  Smartbase manages a word data type which is the union of a number of   */
/*  native data types. The format of the word data type is defined below.  */
/*  Note: The word data type is known as a TVAL because WORD has too many  */
/*        C language macro collisions with Microsoft Windows, and other    */
/*        legacy systems. The term TVAL stands for Typed VALue and is      */
/*        used to identify a basic Smartbase dynamically typed word.       */
/*  *********************************************************************  */

#define MAXREGISTERLEN  16
#define MAXREGISTERCNT  50
#define MAXTVALTEXTLEN  10
typedef struct {union {NUM						Bool;
                       NUM						Char;
                       COMPARE					Compare;
                       NUM32					Index[2];		/* The Modifier and Offset can be overloaded to become a third index (see asTail)	*/
                       NUM						Int;
                       UNUM                     UInt;
                       NUM32					Long;
                       FLAG						Flag[4];
                       OBJ						Obj;
					   POINTER					Pointer;
                       REAL						Real;
                       SHORT					Short;
                       CHAR						Text[8];		/* Text may extend into the TextOverflow and QuoteCnt areas (see MAXTVALTEXTLEN)	*/
                       TYPE						Type;
                       REAL						Void;
                       struct TLambda*			Lambda;
                       struct TBitVector*		BitVector;
                       struct TByteVector*		ByteVector;
                       struct TContinuation*	Continuation;
					   struct TCpx*				Complex;
					   struct TCpxVector*		CpxVector;
                       struct TDatabase*		Repository;
                       struct TDictionary*		Dictionary;
                       struct TDirectory*		Directory;
                       struct TError*			Error;
                       struct TFltVector*		FltVector;
                       struct TIntVector*		IntVector;
                       struct TLongVector*		LongVector;
                       struct TMatrix*			Matrix;
                       struct TNumMatrix*		NumMatrix;
                       struct TNumVector*		NumVector;
                       struct TObject*			Object;
                       struct TObjVector*		ObjVector;
                       struct TPair*			Pair;
                       struct TPcodeVector*		PcodeVector;
                       struct TBrick*			Brick;
                       struct TBrickRow*        BrickRow;
                       struct TBrickField*      BrickField;
                       struct TShtVector*		ShortVector;
                       struct TString*			String;
                       struct TStructure*		Structure;
                       struct TSymbol*			Symbol;
                       struct TVector*			Vector;
                       struct TWorkspace*		Workspace;
                      } u;
                unsigned char			TextOverflow;			/* Text may extend into the TextOverflow bytes (see MAXTVALTEXTLEN) */
                unsigned char			QuoteCnt;				/* A word may not be evaluated until its quote count is reduced to zero (Note: text may also extend into the QuoteCnt field making the QuoteCnt zero). */
                unsigned short			Modifier;				/* The Modifier and Offset can be overloaded to become the Tail (see asTail) */
                unsigned short			Offset;					/* The Modifier and Offset can be overloaded to become the Tail (see asTail) */
                signed char				DeclaredType;			/* This word was declared with the specified type. */
                signed char				Tag;					/* This word currently contains a value of the specified type. */
                } TVAL, *LpTVAL;


#define     asBool(a)       (((LpTVAL)(a))->u.Bool)
#define     asChar(a)       (((LpTVAL)(a))->u.Char)
#define     asCompare(a)    (((LpTVAL)(a))->u.Compare)
#define     asData(a)       (((LpTVAL)(a))->u)
#define     asQuoteCnt(a)   (((LpTVAL)(a))->QuoteCnt)
#define     asGoto(a)       (((LpTVAL)(a))->u.Field[2])
#define     asInt(a)        (((LpTVAL)(a))->u.Int)
#define     asUInt(a)       (((LpTVAL)(a))->u.UInt)
#define     asLong(a)       (((LpTVAL)(a))->u.Long)
#define     asError(a)      (((LpTVAL)(a))->u.Error)
#define     asObj(a)        (((LpTVAL)(a))->u.Obj)
#define     asObject(a)     (((LpTVAL)(a))->u.Object)
#define     asPointer(a)    (((LpTVAL)(a))->u.Pointer)
#define     asReal(a)       (((LpTVAL)(a))->u.Real)
#define     asShort(a)      (((LpTVAL)(a))->u.Short)
#define     asText(a)       (((LpTVAL)(a))->u.Text)
#define     asType(a)       (((LpTVAL)(a))->u.Type)
#define     asFunction(a)   ((LpFUNC)((LpTVAL)(a))->u.Pointer)
#define     asEvaluator(a)  ((LpVMEVALUATOR)((LpTVAL)(a))->u.Pointer)
#define     asLambda(a)      (((LpTVAL)(a))->u.Lambda)
#define     asProcedure(a)  (((LpTVAL)(a))->u.Lambda)
#define		Lambda(tval)		((tval).u.Lambda)
#define		Procedure(tval)	((tval).u.Lambda)

#define     asTag(a)        (((LpTVAL)(a))->Tag)
#define     asTail(a)       (*((LpINT32)(&((LpTVAL)(a))->Modifier)))
#define     asMemo(a)		(*((LpINT32)(&((LpTVAL)(a))->Modifier)))
#define		asOffset(a)     (((LpTVAL)(a))->Offset)
#define		asModifier(a)   (((LpTVAL)(a))->Modifier)
#define     asDeclaredType(a) (((LpTVAL)(a))->DeclaredType)
#define     asQuoteCnt(a)   (((LpTVAL)(a))->QuoteCnt)
#define     asQuoteTag(a)   (*((LpUINT16)((&((LpTVAL)(a))->Tag)-1)))

#define     asObjIdx(a)     (((LpTVAL)(a))->u.Index[0])
#define     asRowIdx(a)     (((LpTVAL)(a))->u.Index[1])
#define     asFldIdx(a)     (*((LpINT32)(&((LpTVAL)(a))->Modifier)))

#define     ObjIdx(tval)    ((tval).u.Index[0])
#define     RowIdx(tval)    ((tval).u.Index[1])
#define     FldIdx(tval)    (*((LpINT32)(&(tval).Modifier)))

#define     asSubOff(a)     (((LpTVAL)(a))->u.Index[1])
#define     asSubLen(a)     (*((LpINT32)(&((LpTVAL)(a))->Modifier)))
#define     SubOff(tval)    ((tval).u.Index[1])
#define     SubLen(tval)    (*((LpINT32)(&(tval).Modifier)))

#define     asNumIndex(a)   ((a)->Tag==TYREAL?(a)->u.Real:(a)->Tag==TYNUM?(a)->u.Int:(a)->Tag==TYUNUM?(a)->u.UInt:(a)->Tag==TYSHORT?(a)->u.Short:(a)->Tag==TYCHAR?((unsigned char)(a)->u.Char):(a)->Tag==TYMONEY?(a)->u.Real:(a)->Tag==TYDATE?(a)->u.Real:(a)->Tag==TYCPX?(a)->u.Complex->itsReal:0)
#define     isNumIndex(a)   ((asTag(a)==TYREAL) || (asTag(a)==TYNUM) || (asTag(a)==TYUNUM) || (asTag(a)==TYSHORT) || (asTag(a)==TYCHAR) || (asTag(a)==TYCPX))

#define     asRealIndex(a)   ((a)->Tag==TYFRAME?(a)->u.Real:(a)->Tag==TYREAL?(a)->u.Real:0.0)
#define     isRealIndex(a)   ((asTag(a)==TYFRAME) || (asTag(a)==TYREAL))

#define     isNullTval(a)   (((LpTVAL)(a))->Tag == TYVOID)
#define		isObject(a)		(_TObject_TypeFlag((a)->Tag) == _TObject_TfTOBJECT)
#define     isCompareEQ(a)  ((asTag(a) == TYCOMPARE) && (asCompare(a) == EQUAL))
#define     isCompareNE(a)  ((asTag(a) == TYCOMPARE) && (asCompare(a) != EQUAL))
#define     isCompareLT(a)  ((asTag(a) == TYCOMPARE) && (asCompare(a) == LOW))
#define     isCompareLE(a)  ((asTag(a) == TYCOMPARE) && (asCompare(a) < HIGH))
#define     isCompareGT(a)  ((asTag(a) == TYCOMPARE) && (asCompare(a) == HIGH))
#define     isCompareGE(a)  ((asTag(a) == TYCOMPARE) && (asCompare(a) > LOW))

#define     isTRUE(a)       ((asTag(a) == TYBOLE) && (asBool(a) == TRUE))
#define     isFALSE(a)      ((asTag(a) == TYBOLE) && (asBool(a) == FALSE))

#define     isERROR(a)		(asTag(a) == TYERROR)

#define     inRange(v,s,e)  (((v) >= (s)) &&((v) < (e)))

#ifndef _MIN
	#define     _MIN(a,b)       ((a < b) ? a : b)
#endif

#ifndef _MAX
	#define     _MAX(a,b)       ((a > b) ? a : b)
#endif

/*  MAXIMUM VM INSTRUCTIONS DECLARATION */
/*  This key macro is ALSO defined in "tpcodvec.h".	*/
/*  WARNING: Keep these two definitions identical	*/
#define     _VMMAXINSTRS    266

/*  MEMORY BLOCK DECLARATIONS */
/*   */
/*  The data structures, macros, and coding conventions used in  */
/*  this source file are designed to isolate and protect the  */
/*  Smartbase High Speed Engine from the low level differences */
/*  between the Macintosh, Windows 3.1, Windows NT, and Unix  */
/*  memory management structures.  */

typedef struct
    {
    TVAL            Value;              /*  Binding value   (result) */
    struct TObject* Key;                /*  Binding key     (index) */
    } BIND, *LpBIND;

typedef struct
    {
    TVAL            Value;              /*  Binding value   (result) */
    TVAL            Key;                /*  Binding key     (index) */
    } PBIND, *LpPBIND;

typedef struct  {
                CHAR        Char[1];
                } FMemory, **HMemory;
#define _HMemoryPtr(h)      (*(char **)(h))
#define atHMemory(h,n)      ((*(HMemory)(h))->Char[(n)])

typedef struct  {
                CHAR        Char[1];
                } FMChar, **HMChar;
#define atHMChar(h,n)       ((*(HMChar)(h))->Char[(n)])

typedef struct  {
                BIND        Bind[1];
                } FMBind, **HMBind;
#define atHMBind(h,n)       ((*(HMBind)(h))->Bind[(n)])

typedef struct  {
                PBIND       PBind[1];
                } FMPBind, **HMPBind;
#define atHMPBind(h,n)      ((*(HMPBind)(h))->PBind[(n)])

typedef struct  {
                BOLE        Bool[1];
                } FMBool, **HMBool;
#define atHMBool(h,n)       ((*(HMBool)(h))->Bool[(n)])

typedef struct  {
                COMPARE     Compare[1];
                } FMCompare, **HMCompare;
#define atHMCompare(h,n)    ((*(HMCompare)(h))->Compare[(n)])

typedef struct  {
                POINTER      Handle[1];
                } FMHandle, **HMHandle;
#define atHMHandle(h,n)     ((*(HMHandle)(h))->Handle[(n)])

typedef struct  {
                NUM         Int[1];
                } FMInt, **HMInt;
#define atHMInt(h,n)        ((*(HMInt)(h))->Int[(n)])

typedef struct  {
                UNUM        UInt[1];
                } FMUInt, **HMUInt;
#define atHMUInt(h,n)        ((*(HMUInt)(h))->UInt[(n)])

typedef struct  {
                NUM32       Long[1];
                } FMLong, **HMLong;
#define atHMLong(h,n)       ((*(HMLong)(h))->Long[(n)])

typedef struct  {
                SHORT       Short[1];
                } FMShort, **HMShort;
#define atHMShort(h,n)      ((*(HMShort)(h))->Short[(n)])

typedef struct  {
                NAME        Name[1];
                } FMName, **HMName;
#define atHMName(h,n)       ((*(HMName)(h))->Name[(n)])

typedef struct  {
                OBJ         Obj[1];
                } FMObj, **HMObj;
#define atHMObj(h,n)        ((*(HMObj)(h))->Obj[(n)])

typedef struct  {
                struct TObject* Object[1];
                } FMObject, **HMObject;
#define atHMObject(h,n)     ((*(HMObject)(h))->Object[(n)])

typedef struct  {
                OBJ*        Objpntr[1];
                } FMObjpntr, **HMObjpntr;
#define atHMObjpntr(h,n)    ((*(HMObjpntr)(h))->Objpntr[(n)])

typedef struct  {
                POINTER     Pointer[1];
                } FMPointer, **HMPointer;
#define atHMPointer(h,n)    ((*(HMPointer)(h))->Pointer[(n)])

typedef struct  {
                REAL        Real[1];
                } FMReal, **HMReal;
#define atHMReal(h,n)       ((*(HMReal)(h))->Real[(n)])

typedef struct  {
                FLOAT       Float[1];
                } FMFloat, **HMFloat;
#define atHMFloat(h,n)      ((*(HMFloat)(h))->Float[(n)])

typedef struct  {
                TYPE        Type[1];
                } FMType,   **HMType;
#define atHMType(h,n)       ((*(HMType)(h))->Type[(n)])

typedef struct  {
                TVAL        Tval[1];
                } FMTval,   **HMTval;
#define atHMTval(h,n)       ((*(HMTval)(h))->Tval[(n)])

#define	TVALARRAYITEMSIZE			sizeof(FMTval)
#define	BINDARRAYITEMSIZE			sizeof(FMBind)


/*  Define some fundamental SmartBase engine parametrics */

#define _FSmartbase_MAXIMPORTBUFFERLEN 2500000	/* Max size of import buffer */
#define _FSmartbase_MAXBUFFERLEN        512000	/* Max size of working buffer */
#define _FSmartbase_MAXARGUMENTS        40      /* Max number of arguments */

/*  Thread Structure  */
//  The following structure contains the variables necessary to perserve a thread

typedef struct {TVAL*				TvalStack;									/* Smartbase operations stack */
				NUM					TvalStackIdx;								/* Operations stack current index */
				NUM					MaxTvalStackSize;							/* Maximum size of the operations stack */
				NUM					escapeSW;									/* 0=None, 1=Escape Requested, 2=Quit Requested */
				NUM					SessionID;									/* Caller's session identifier */
				NUM					RequestID;									/* Caller's request identifier */
				NUM					RecursionCount;								/* Current recursion count */
				NUM					MaxRecursions;								/* Maximum recursions allowed */
				NUM					CStackSpace;								/* Space allocated for C stack */
				NUM					ObjStackIdx;								/* Protected object stack current index */
				NUM					MaxObjStackSize;							/* Maximum size of the protected object stack */
				OBJ**				ObjStack;									/* Smartbase protected object stack */
				BOLE				busySW;										/* FALSE=Engine waiting, TRUE=Engine executing (i.e. RecursionCount > 0) */									
																				/* BEG Engine Debugger Trace Switches & Settings */
				BOLE				DebugJitON;									/* Just-in-time compiler ON/OFF switch */
				BOLE				DebugTraceON;								/* Debugger instruction trace ON/OFF switch */
				BOLE				DebugSuspended;								/* The debugDetective function is processing a trace break */
				BOLE				DebugErrorON;								/* The trace break on error ON/OFF switch */
				BOLE				EngineStateChanged;							/* Set by calls to FDebug_Debug whenever the engine debug state has changed */
				TVAL				DebugTraceLambda;							/* Current trace Lambda object OR (-1 for *, -2 for !) */
				NUM					DebugTraceCodeL;							/* Current trace CodeL OR (-1 for *, -2 for !) */
				NUM					DebugTraceRecursion;						/* Current trace recursion level  OR (-1 for *, -2 for !) */
																				/* END Engine Debugger Trace Switches & Settings */
				CHAR				TempBuffer[_FSmartbase_MAXBUFFERLEN+2];		/* Temporary task scratchpad buffer */
				BOLE				FConio_LogConsoleSW;						/* Log Console writes to AIS Log file */
				BOLE				FCompile_GenerateDebugInfo;					/* Compile function and Lisp parser should generate debugging information. */
				struct TVector*		FCompile_DebugSourceVector;					/* Current Source Vector object being compiled with debugging information. */
				struct TStructure*	FCompile_DebugListLines;					/* Current List ==> Line Numbers Structure being compiled with debugging information. */
				TVAL				FCompile_DebugInterfaces;					/* Current Interfaces Structure being compiled with debugging information. */
				TVAL				FCompile_DebugCurrentSourceLine;			/* Current source line providing debug information to the compile function. */
				TVAL				FCompile_DebugCurrentSourceLineIndex;		/* Index of current source line providing debug information to the compile function. */
				NUM					FCompile_tmpCount;
				NUM					FCompile_lambdaSwitch;
				NUM					FCompile_LambdaNest;
				NUM					FCompile_LastProcID;
				NUM					FCompile_LastProcDisp;
				BOLE				FCompile_OptimizeSW;
				TVAL				FDatabas_IOerr;
				TVAL				FDatabas_OutOfMemory;
				TVAL				FDatabas_PageSize;
				char*				FDebug_fmtStr77;
				BOLE				FDebug_ShowShort;
				REAL				FMath2_realResult;
				REAL				FMath2_imagResult;
				NUM					FMath2_intTotal;
				BOLE				FMath2_boolInitial;
				TVAL				FMath2_CallBackTval;
				NUM					FOpt2_labelCounter;
				NUM					FVmscript_facs[8];
				BOLE				FVmscript_definiteArgs;						/*  Definite argument switch */
				NUM					FVmscript_SizeOfFormals;					/*  Number of formal arguments */
				CHAR				FVmscript_targetAm;							/*  Assignment target modifier */
				NUM					FVmscript_escapeCheck;						/*  Escape check counter */
				NUM					FVmscript_nArgument;						/*  Integer argument operand */
				REAL				FVmscript_Integer;							/*  Modulus math integer part */
				REAL				FVmscript_Fraction;							/*  Modulus math fraction part */
				NUM                 FVmscript_Modifier;							/*  Modifier pointer for push */
				TVAL                FVmscript_isource;							/*  Tval source operand */
				TVAL				FVmscript_iargument;						/*  Tval argument operand */
				TVAL				FVmscript_iindex;							/*  Tval index operand */
				struct TSymbol*		FVmscript_aSymbol;							/*  Temporary symbol variable */
				BOLE				FVmscript_NestedDebugSwt;					/*  Nested debug required switch */
				NUM					FVmscript_cacheIndex;						/*  Temporary cache index */
				TYPE				FVmscript_type;								/*  Temporary type variable */
				NUM					FVmscript_Selection;						/*  Debugger source line selection */
				NUM					FVmscript2_DisassemblyStyle;
				NUM					FVmscript2_OldDisassemblyStyle;
				NUM					FVmscript2_VariablesToShow;
				struct TLambda*		FVmscript2_OldSelf;							/*  Saved copy of self */
				struct TLambda*		TLambda_TheProc;
				struct TLambda*		TLambda_saveProc;
				struct TLambda*		TLambda_CurrentProcedure;
				struct TContinuation*  TLambda_TheContinuation;
				TVAL				TLambda_ThePropList;
				BOLE				TObject_ErrorSwt;
				TVAL				TObject_ErrorTree;
				struct	TObjVector*	TObject_SaveVector;
				struct	TIntVector*	TObject_SaveNdx;
				REAL                startOfMemoryBlocks[1];
				 /* The array of memory blocks follows the,    */
				 /* last of the thread blocks. The variable is */
				 /* REAL for alignment purposes only.          */
                } THREAD, *LpTHREAD;
#define THREADBLOCKLENGTH         (NUM)(sizeof(THREAD))

/*  Declare the SmartLisp Lisp Native execution functions here. */
/*  Note:   These variables and macros can be used to access the  */
/*          SmartLisp native execution environment at run time  */
/*          from compiled Lambdas which are not interpreted.  */

typedef     TVAL            (*LpFUNTVAL)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     TVAL*           (*LpFUNTVALPTR)	(POINTER gCP,LpTHREAD gTP, ...);
typedef     POINTER	        (*LpFUNPOINTER)	(POINTER gCP,LpTHREAD gTP, ...);
//typedef     UNUM            (*LpATOI)       (CHAR * s);
typedef		NUM             (*LpATOI)       (CHAR * s);
typedef		LONG			(*LpATOL)       (CHAR * s);
typedef		REAL			(*LpATOF)       (CHAR * s);
typedef		REAL			(*LpMODF)       (REAL v,LpNUM n);

/*  Execution Context Structure  */
//  The following structure contains the variables necessary to perserve a context

typedef struct  {BOLE				FSmartbase_Initialized;
				 CHAR				ContextName[64];
				 NUM				ContextIndex;
				 POINTER			SessionMgrContextThread;
				 POINTER			ContextBlocks[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
				 NUM				ContextBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
				 NUM				JIT_codeBloatFactor;
				 POINTER			JIT_JumpTable[256];
				 TVAL				Tval_TRUE;
				 TVAL				Tval_FALSE;
				 TVAL				Tval_VOID;
				 BOLE				FSmartbase_SilentMode;
				 struct TSymbol*	FSmartbase_errorMsg;
				 struct TSymbol*	FSmartbase_errorSym;
				 struct TSymbol*	FSmartbase_sysErrorSym;
				 TVAL				FSmartbase_sysError;
				 jmp_buf			FSmartbase_CatchEnv;
				 LpFUNTVAL			FSmartbase_RegisterCProcedure;
				 LpFUNPOINTER		FSmartbase_ObjectPtr;
				 LpFUNTVALPTR		FSmartbase_GetSymbolValuePtr;
				 LpFUNTVAL			FSmartbase_Eval;
				 LpFUNTVAL			FSmartbase_Evals;
				 LpFUNTVAL			FSmartbase_Evalv;
				 LpFUNTVAL			FSmartbase_Ref;
				 LpFUNTVAL			FSmartbase_Refv;
				 LpFUNTVAL			FSmartbase_Set;
				 LpFUNTVAL			FSmartbase_Setv;
				 LpFUNTVAL			FSmartbase_CnvFromChar;
				 LpFUNTVAL			FSmartbase_CnvFromBool;
				 LpFUNTVAL			FSmartbase_Error;
				 LpFUNTVAL			FSmartbase_Perror;
				 LpFUNTVAL			FSmartbase_MakeCFunction;
				 LpFUNTVAL			FSmartbase_CnvFromInt;
				 LpFUNTVAL			FSmartbase_CnvFromPtr;
				 LpFUNTVAL			FSmartbase_CnvFromObj;
				 LpFUNTVAL			FSmartbase_CnvFromReal;
				 LpFUNTVAL			FSmartbase_CnvToFrame;
				 LpFUNTVAL			FSmartbase_CnvFromText;
				 LpFUNTVAL			FSmartbase_CnvToSymbol;
				 LpFUNTVAL			FSmartbase_CnvToQSymbol;
				 LpFUNTVAL			FSmartbase_GetSymbolValue;
				 NUM				(*_Host_Display)(POINTER gCP, LpTHREAD gTP, char* string, NUM newline);
				 NUM				(*_Host_Escape)(POINTER gCP, LpTHREAD gTP);
				 NUM				(*_Host_UpdateState)(POINTER gCP, LpTHREAD gTP);
				 NUM				(*_Host_Openf)(POINTER gCP, LpTHREAD gTP,char* name, NUM mode, NUM type);
				 NUM				(*_Host_Readf)(POINTER gCP, LpTHREAD gTP, NUM fileID, NUM length, char* bufptr);
				 NUM				(*_Host_Writef)(POINTER gCP, LpTHREAD gTP, NUM fileID, NUM length, const char* bufptr);
				 REAL			    (*_Host_Seekf)(POINTER gCP, LpTHREAD gTP, NUM fileID, REAL adjustment, NUM opcode);
				 REAL				(*_Host_Resizef)(POINTER gCP, LpTHREAD gTP, NUM fileID, REAL newsize);
				 NUM				(*_Host_Closef)(POINTER gCP, LpTHREAD gTP, NUM fileID, NUM opcode);
				 LpATOI				atoi;
				 LpATOL				atol;
				 LpATOF				atof;
				 LpMODF				modf;
				 BOLE               FCompile_Initialized;
				 struct TSymbol*    FCompile_CharPointerSym;
				 struct TSymbol*    FCompile_FloatPointerSym;
				 struct TSymbol*    FCompile_IntegerSym;
				 struct TSymbol*    FCompile_IntPointerSym;
				 struct TSymbol*    FCompile_JumpPointerSym;
				 struct TSymbol*    FCompile_NumberSym;
				 struct TSymbol*    FCompile_NumPointerSym;
				 struct TSymbol*    FCompile_ShortPointerSym;
				 struct TSymbol*    FCompile_dbgSourceLinesSYM;
				 struct TSymbol*    FCompile_dbgSourceLinesInstrSYM;
				 struct TSymbol*    FCompile_dbgParseTreeSYM;
				 struct TSymbol*    FCompile_dbgListLinesSYM;
				 struct TSymbol*    FCompile_gotoLE;
				 struct TSymbol*    FCompile_gotoLT;
				 struct TSymbol*    FCompile_gotoEQ;
				 struct TSymbol*    FCompile_gotoNE;
				 struct TSymbol*    FCompile_gotoGT;
				 struct TSymbol*    FCompile_gotoGE;
				 struct TSymbol*    FCompile_gotoLEb;
				 struct TSymbol*    FCompile_gotoLTb;
				 struct TSymbol*    FCompile_gotoEQb;
				 struct TSymbol*    FCompile_gotoNEb;
				 struct TSymbol*    FCompile_gotoGTb;
				 struct TSymbol*    FCompile_gotoGEb;
				 struct TSymbol*    FCompile_gotoLEc;
				 struct TSymbol*    FCompile_gotoLTc;
				 struct TSymbol*    FCompile_gotoEQc;
				 struct TSymbol*    FCompile_gotoNEc;
				 struct TSymbol*    FCompile_gotoGTc;
				 struct TSymbol*    FCompile_gotoGEc;
				 struct TSymbol*    FCompile_gotoLEi;
				 struct TSymbol*    FCompile_gotoLTi;
				 struct TSymbol*    FCompile_gotoEQi;
				 struct TSymbol*    FCompile_gotoNEi;
				 struct TSymbol*    FCompile_gotoGTi;
				 struct TSymbol*    FCompile_gotoGEi;
				 struct TSymbol*    FCompile_gotoLEn;
				 struct TSymbol*    FCompile_gotoLTn;
				 struct TSymbol*    FCompile_gotoEQn;
				 struct TSymbol*    FCompile_gotoNEn;
				 struct TSymbol*    FCompile_gotoGTn;
				 struct TSymbol*    FCompile_gotoGEn;
				 struct TSymbol*    FCompile_return;
				 struct TSymbol*    FCompile_notSym;
				 struct TSymbol*    FCompile_andSym;
				 struct TSymbol*    FCompile_orSym;
				 struct TSymbol*    FCompile_ifSym;
				 struct TSymbol*    FCompile_ieqSym;
				 struct TSymbol*    FCompile_igtSym;
				 struct TSymbol*    FCompile_iltSym;
				 struct TSymbol*    FCompile_igeSym;
				 struct TSymbol*    FCompile_ileSym;
				 struct TSymbol*    FCompile_ineSym;
				 struct TSymbol*    FCompile_neqSym;
				 struct TSymbol*    FCompile_ngtSym;
				 struct TSymbol*    FCompile_nltSym;
				 struct TSymbol*    FCompile_ngeSym;
				 struct TSymbol*    FCompile_nleSym;
				 struct TSymbol*    FCompile_nneSym;
				 struct TSymbol*    FCompile_neSym;
				 struct TSymbol*    FCompile_eqSym;
				 struct TSymbol*    FCompile_gtSym;
				 struct TSymbol*    FCompile_ltSym;
				 struct TSymbol*    FCompile_geSym;
				 struct TSymbol*    FCompile_leSym;
				 struct TSymbol*    FCompile_refSym;
				 struct TSymbol*    FCompile_setSym;
				 struct TSymbol*    FCompile_addSym;
				 struct TSymbol*    FCompile_addiSym;
				 struct TSymbol*    FCompile_iaddSym;
				 struct TSymbol*    FCompile_idivSym;
				 struct TSymbol*    FCompile_imodSym;
				 struct TSymbol*    FCompile_imulSym;
				 struct TSymbol*    FCompile_isubSym;
				 struct TSymbol*    FCompile_naddSym;
				 struct TSymbol*    FCompile_ndivSym;
				 struct TSymbol*    FCompile_nmodSym;
				 struct TSymbol*    FCompile_nmulSym;
				 struct TSymbol*    FCompile_nsubSym;
				 struct TSymbol*    FCompile_divSym;
				 struct TSymbol*    FCompile_diviSym;
				 struct TSymbol*    FCompile_divrSym;
				 struct TSymbol*    FCompile_divriSym;
				 struct TSymbol*    FCompile_mulSym;
				 struct TSymbol*    FCompile_muliSym;
				 struct TSymbol*    FCompile_shlSym;
				 struct TSymbol*    FCompile_shrSym;
				 struct TSymbol*    FCompile_subSym;
				 struct TSymbol*    FCompile_subiSym;
				 struct TSymbol*    FCompile_xorSym;
				 struct TSymbol*    FCompile_nandSym;
				 struct TSymbol*    FCompile_norSym;
				 struct TSymbol*    FCompile_nxorSym;
				 struct TSymbol*    FCompile_nandbSym;
				 struct TSymbol*    FCompile_norbSym;
				 struct TSymbol*    FCompile_nxorbSym;
				 struct TSymbol*    FCompile_add1Sym;
				 struct TSymbol*    FCompile_sub1Sym;
				 struct TSymbol*    FCompile_bandSym;
				 struct TSymbol*    FCompile_borSym;
				 struct TSymbol*    FCompile_selfSym;
				 struct TSymbol*    FCompile_argcountSym;
				 struct TSymbol*    FCompile_defineSym;
				 struct TSymbol*    FCompile_defunSym;
				 struct TSymbol*    FCompile_defmacroSym;
				 struct TSymbol*    FCompile_defchildSym;
				 struct TSymbol*    FCompile_defChildLambdaSym;
				 struct TSymbol*    FCompile_deforphanSym;
				 struct TSymbol*    FCompile_defriendSym;
				 struct TSymbol*    FCompile_defclassSym;
				 struct TSymbol*    FCompile_defcloneSym;
				 struct TSymbol*    FCompile_defvmSym;
				 struct TSymbol*    FCompile_defmethodSym;
				 struct TSymbol*    FCompile_lambdaSym;
				 struct TSymbol*    FCompile_whileSym;
				 struct TSymbol*    FCompile_loopSym;
				 struct TSymbol*    FCompile_beginSym;
				 struct TSymbol*    FCompile_sendSym;
				 struct TSymbol*    FCompile_argfetchSym;
				 struct TSymbol*    FCompile_onerrorSym;
				 struct TSymbol*    FCompile_letSym;
				 struct TSymbol*    FCompile_caseSym;
				 struct TSymbol*    FCompile_condSym;
				 struct TSymbol*    FCompile_setqSym;
				 struct TSymbol*    FCompile_setfSym;
				 struct TSymbol*    FCompile_setvSym;
				 struct TSymbol*    FCompile_addnSym;
				 struct TSymbol*    FCompile_divnSym;
				 struct TSymbol*    FCompile_mulnSym;
				 struct TSymbol*    FCompile_subnSym;
				 struct TSymbol*    FCompile_beqSym;
				 struct TSymbol*    FCompile_bgtSym;
				 struct TSymbol*    FCompile_bltSym;
				 struct TSymbol*    FCompile_bgeSym;
				 struct TSymbol*    FCompile_bleSym;
				 struct TSymbol*    FCompile_bneSym;
				 struct TSymbol*    FCompile_caddSym;
				 struct TSymbol*    FCompile_cdivSym;
				 struct TSymbol*    FCompile_cmulSym;
				 struct TSymbol*    FCompile_csubSym;
				 struct TSymbol*    FCompile_ceqSym;
				 struct TSymbol*    FCompile_cgtSym;
				 struct TSymbol*    FCompile_cltSym;
				 struct TSymbol*    FCompile_cgeSym;
				 struct TSymbol*    FCompile_cleSym;
				 struct TSymbol*    FCompile_cneSym;
				 struct TSymbol*    FCompile_plusPlusSym;
				 struct TSymbol*    FCompile_minusMinusSym;
				 struct TSymbol*    FCompile_plusEqualsSym;
				 struct TSymbol*    FCompile_refmacroSym;
				 struct TSymbol*    FCompile_minusEqualsSym;
				 struct TSymbol*    FCompile_timesEqualsSym;
				 struct TSymbol*    FCompile_divEqualsSym;
				 struct TSymbol*    FCompile_refTextSym;
				 struct TSymbol*    FCompile_refStringSym;
				 struct TSymbol*    FCompile_setStringSym;
				 struct TSymbol*    FCompile_refSymbolSym;
				 struct TSymbol*    FCompile_refVectorSym;
				 struct TSymbol*    FCompile_setVectorSym;
				 struct TSymbol*    FCompile_refStrValueSym;
				 struct TSymbol*    FCompile_setStrValueSym;
				 struct TSymbol*    FCompile_refStrKeySym;
				 struct TSymbol*    FCompile_setStrKeySym;
				 struct TSymbol*    FCompile_refDicValueSym;
				 struct TSymbol*    FCompile_setDicValueSym;
				 struct TSymbol*    FCompile_refDicKeySym;
				 struct TSymbol*    FCompile_setDicKeySym;
				 struct TSymbol*    FCompile_refDirValueSym;
				 struct TSymbol*    FCompile_setDirValueSym;
				 struct TSymbol*    FCompile_refDirKeySym;
				 struct TSymbol*    FCompile_setDirKeySym;
				 struct TSymbol*    FCompile_refBitVectorSym;
				 struct TSymbol*    FCompile_setBitVectorSym;
				 struct TSymbol*    FCompile_refBytVectorSym;
				 struct TSymbol*    FCompile_setBytVectorSym;
				 struct TSymbol*    FCompile_refCpxVectorSym;
				 struct TSymbol*    FCompile_setCpxVectorSym;
				 struct TSymbol*    FCompile_refPcdVectorSym;
				 struct TSymbol*    FCompile_setPcdVectorSym;
				 struct TSymbol*    FCompile_refObjVectorSym;
				 struct TSymbol*    FCompile_setObjVectorSym;
				 struct TSymbol*    FCompile_refIntVectorSym;
				 struct TSymbol*    FCompile_setIntVectorSym;
				 struct TSymbol*    FCompile_refNumVectorSym;
				 struct TSymbol*    FCompile_setNumVectorSym;
				 struct TSymbol*    FCompile_refFltVectorSym;
				 struct TSymbol*    FCompile_setFltVectorSym;
				 struct TSymbol*    FCompile_refMatrixSym;
				 struct TSymbol*    FCompile_setMatrixSym;
				 struct TSymbol*    FCompile_refNumMatrixSym;
				 struct TSymbol*    FCompile_setNumMatrixSym;
				 BOLE				FConio_Initialized;
				 BOLE				FControl_Initialized;
				 BOLE				FConvert_Initialized;
				 BOLE				FDatabas_Initialized;
				 TVAL*				FDatabas_pCompress;
				 TVAL*				FDatabas_pUncompress;
				 TVAL*				FDatabas_pEncode;
				 TVAL*				FDatabas_pDecode;
				 BOLE				FDatefnc_Initialized;
				 BOLE				FDebug_Initialized;
				 BOLE				FDebug_ShowRegVars;
				 BOLE				FDebug_ShowTempVars;
				 BOLE				FDebug_ShowArgVars;
				 BOLE				FDebug_ShowClassVars;
				 BOLE				FDebug_ShowConstVars;
				 BOLE				FDebug_ShowInterfaceVars;
				 BOLE				FDebug_ShowPersistentVars;
				 struct TSymbol*    FDebug_exit;
				 struct TSymbol*    FDebug_short;
				 struct TSymbol*    FDebug_bp;
				 struct TSymbol*    FDebug_bc;
				 struct TSymbol*    FDebug_bl;
				 struct TSymbol*    FDebug_src;
				 struct TSymbol*    FDebug_srcOnly;
				 struct TSymbol*    FDebug_srcCnt;
				 struct TSymbol*    FDebug_asm;
				 struct TSymbol*    FDebug_envs;
				 struct TSymbol*    FDebug_dbgVec;
				 struct TSymbol*    FDebug_dbgSym;
				 struct TSymbol*    FDebug_traceon;
				 struct TSymbol*    FDebug_traceoff;
				 struct TSymbol*    FDebug_on;
				 struct TSymbol*    FDebug_off;
				 struct TSymbol*    FDebug_jiton;
				 struct TSymbol*    FDebug_jitoff;
				 struct TSymbol*    FDebug_until;
				 struct TSymbol*    FDebug_go;
				 struct TSymbol*    FDebug_currentResult;
				 struct TObjVector* FDebug_Space;
				 struct TSymbol*    FDebug_getGlobalValue;
				 BOLE				FDefine_Initialized;
				 BOLE				FFinance_Initialized;
				 BOLE				FLisp_Initialized;
				 BOLE				FList_Initialized;
				 BOLE				FMacro_Initialized;
				 struct TSymbol*    FMacro_macro;
				 struct TSymbol*    FMacro_vm;
				 struct TSymbol*    FMacro_refSym;
				 struct TSymbol*    FMacro_makevecSym;
				 struct TSymbol*    FMacro_makeenvSym;
				 struct TSymbol*    FMacro_quoteSym;
				 struct TSymbol*    FMacro_boolean;
				 BOLE				FMake_Initialized;
				 BOLE				FMath1_Initialized;
				 BOLE				FMath2_Initialized;
				 NUM			    FMath2_mask[32];
				 BOLE				FMath3_Initialized;
				 BOLE				FMatrix1_Initialized;
				 BOLE				FXml_Initialized;
				 NUM				FMemory_BlockedMemoryBytes; /* see (inspect BlockedMemoryBytes:) */
                 BOLE				FMemory_Initialized;
				 BOLE               FMemory_SelfTest;
				 NUM                FMemory_UsedBlockCount;		/* see (inspect BlockCount:) */
				 NUM                FMemory_UsedMemoryBytes;	/* see (inspect UseMemoryBytes:) */
                 NUM                FMemory_ParentSize;
				 NUM				FMemory_myForcedGCCounter;
                 POINTER            FMemory_ParentPtr;
				 NUM                FMemory_MemoryAllocated;	/* see (inspect MemoryAllocated:) */
				 LpMHANDLEINFO		FMemory_PrevPtr;
				 NUM                FMemory_FrameMaxIndex;
				 NUM                FMemory_FrameSize[_MEMMAXFRAMES];
				 LpMHANDLEINFO		FMemory_FrameNextMHandle[_MEMMAXFRAMES];
				 NUM				FMemory_FreeBlockCount;		/* number of free blocks - see (inspect FreeBlockCount:) */
				 NUM				FMemory_FreeMemoryBytes;	/* number of free memory bytes - see (inspect FreeBlockCount:) */
				 NUM				FMemory_BlockedBlockCount;	/* number of blocked blocks (these map out gaps in the os supplied memory) - see (inspect BlockedBlockCount:)*/
				 NUM				FMemory_SystemCheckCount;	/* number of times systemCheck was performed - see (inspect SystemCheckCount:) */
				 NUM				FMemory_JoinCount;			/* number of times blocks were joined - see (inspect JoinCount:) */
				 NUM				FMemory_SplitCount;			/* number of times blocks were split - see (inspect SplitCount:) */
				 NUM				FMemory_NewCount;			/* number of times blocks were created from new memory - see (inspect NewCount:) */
				 NUM				FMemory_CopyCount;			/* number of times FMemory_copy called - see (inspect CopyCount:) */
				 NUM				FMemory_ReuseCount;			/* number of times blocks were reused in whole - see (inspect ReuseCount:) */
				 NUM				FMemory_FreeListHitCount;	/* number of times blocks on a free list were found first time. - see (inspect FreeListHitCount:) */
				 NUM				FMemory_LargerFrameHitCount;/* number of times blocks on a larger frame free list were found. - see (inspect LargerFrameHitCount:) */
				 NUM				FMemory_TimerFrequency;		/* cycles per second of time counters */
				 NUM				FMemory_NewTime;
				 NUM				FMemory_FreeTime;
				 NUM				FMemory_ResizeTime;
                 BOLE               FMySQL1_Initialized;
                 BOLE               FMySQL1_Enabled;
                 BOLE               FMySQL1_UseWordInt;
                 BOLE               FMySQL1_UseWordFlt;
                 BOLE               FMySQL1_UseWordDat;
				 BOLE               FMySQL1_UseDecimalMoney;
                 struct TIntVector* FMySQL1_IntVector;
                 struct TVector*    FMySQL1_InfoVector;
                 NUM				TObject_initMaxObjects;
				 BOLE				FOpt1_Initialized;
				 BOLE				FOpt2_Initialized;
				 BOLE				FPred_Initialized;
				 BOLE				FPred2_Initialized;
				 BOLE				FProperty_Initialized;
				 BOLE				FStatfnc_Initialized;
				 NUM				FStatfnc_count;
				 REAL				FStatfnc_kurtosis;
				 REAL				FStatfnc_minimum;
				 REAL				FStatfnc_maximum;
				 REAL				FStatfnc_xbar;
				 REAL				FStatfnc_stdev;
				 REAL				FStatfnc_skew;
				 TVAL				FStatfnc_CallBackTval;
				 struct TNumVector* FStatfnc_NumVector;
				 BOLE				FString_Initialized;
				 BOLE				FText_Initialized;
				 BOLE				FText2_Initialized;
				 BOLE				FTextfnc_Initialized;
				 BOLE				FUtil1_Initialized;
				 BOLE				FUtil2_Initialized;
				 BOLE				FUtil3_Initialized;
				 BOLE				FVmappend_Initialized;
				 BOLE				FVmscript_Initialized;
				 struct TSymbol*	FVmscript_Symbol_debugDetective;
				 TVAL				FVmScript_ERROR_ILLEGAL_INSTRUCTION;
				 TVAL				FVmScript_ERROR_ILLEGAL_VALUE;
				 TVAL				FVmScript_ERROR_DIVIDE_BY_ZERO;
				 TVAL				FVmScript_ERROR_OVERFLOW;
				 TVAL				FVmScript_ERROR_MISSING_FUNCTION_NAME;
				 TVAL				FVmScript_ERROR_NOT_AN_Lambda;
				 TVAL				FVmScript_ERROR_MISSING_PCODES;
				 TVAL				FVmScript_ERROR_VMADD_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMDIV_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMDIVR_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMMUL_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMSUB_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMADDI_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMSUBI_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMDIVI_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMDIVRI_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMMULI_BAD_VALUE;
				 TVAL				FVmScript_ERROR_VMARGFETCH_MOD;
				 TVAL				FVmScript_ERROR_VMARGFETCH_VALUE;
				 TVAL				FVmScript_ERROR_VMONERROR_MOD;
				 TVAL				FVmScript_ERROR_VMPUSH_MOD;
				 TVAL				FVmScript_ERROR_VMREF_MOD;
				 TVAL				FVmScript_ERROR_VMRETURN_MOD;
				 TVAL				FVmScript_ERROR_VMSEND_MESSAGE;
				 TVAL				FVmScript_ERROR_VMSET_MOD;
				 TVAL				FVmScript_ERROR_VMTARGET_MOD;
				 BOLE				FXmlLoad_Initialized;
				 BOLE				FWorkspace_Initialized;
				 BOLE				TLambda_Initialized;
				 NUM				TLambda_MaxInputLen;
				 struct TSymbol*    TLambda_assign;
				 struct TSymbol*    TLambda_by;
				 struct TSymbol*    TLambda_close;
				 struct TSymbol*    TLambda_console;
				 struct TSymbol*    TLambda_data;
				 struct TSymbol*    TLambda_do;
				 struct TSymbol*    TLambda_EditWindow;
				 struct TSymbol*    TLambda_else;
				 struct TSymbol*    TLambda_end;
				 struct TSymbol*    TLambda_for;
				 struct TSymbol*    TLambda_from;
				 struct TSymbol*    TLambda_globals;
				 struct TSymbol*    TLambda_include;
				 struct TSymbol*    TLambda_nil;
				 struct TSymbol*    TLambda_otherwise;
				 struct TSymbol*    TLambda_pi;
				 struct TSymbol*    TLambda_quote;
				 struct TSymbol*    TLambda_save;
				 struct TSymbol*    TLambda_script;
				 struct TSymbol*    TLambda_select;
				 struct TSymbol*    TLambda_then;
				 struct TSymbol*    TLambda_to;
				 struct TSymbol*    TLambda_until;
				 struct TSymbol*    TLambda_title;
				 struct TSymbol*    TLambda_workspace;
				 struct TSymbol*    TLambda_dotcdr;
				 struct TSymbol*    TLambda_faces;
				 struct TSymbol*    TLambda_self;
				 struct TSymbol*    TLambda_vars;
				 struct TSymbol*    TLambda_args;
				 struct TSymbol*    TLambda_svars;
				 struct TSymbol*    TLambda_pvars;
				 struct TSymbol*    TLambda_cvars;
				 struct TSymbol*    TLambda_rvars;
				 struct TSymbol*    TLambda_label;
				 struct TSymbol*    TLambda_goto;
				 struct TSymbol*    TLambda_Sv;
				 struct TSymbol*    TLambda_Av;
				 struct TSymbol*    TLambda_Tv;
				 struct TSymbol*    TLambda_Pv;
				 struct TSymbol*    TLambda_Cv;
				 struct TSymbol*    TLambda_Rv;
				 struct TSymbol*    TLambda_Pc;
				 struct TSymbol*    TLambda_Sc;
				 struct TSymbol*    TLambda_Nc;
				 struct TSymbol*    TLambda_In;
				 struct TSymbol*    TLambda_Binding;
				 struct TSymbol*    TLambda_BreakList;
				 struct TSymbol*    TLambda_Vm;
				 struct TSymbol*    TLambda_LispVirtualMachine;
				 struct TSymbol*    TLambda_EvalWhenDoomed;
				 struct TSymbol*    TLambda_Doomed;
				 TVAL				TLambda_TvalIn;
				 TVAL				TLambda_TvalBinding;
				 BOLE				TBitVector_Initialized;
				 BOLE				TBitVector_Functions_Initialized;
				 CHAR				TBitVector_OrMasks[8];
				 CHAR				TBitVector_AndMasks[8];
				 BOLE				TByteVector_Initialized;
				 BOLE				TBrick_Initialized;
				 BOLE				TContinuation_Initialized;
				 BOLE				TCpx_Initialized;
				 BOLE				TCpxVector_Initialized;
				 BOLE				TDatabase_Initialized;
				 NUM				TDatabase_MinBlockSize;
				 BOLE				TDictionary_Initialized;
				 BOLE				TDirectory_Initialized;
				 BOLE				TError_Initialized;
				 struct	TError*     TError_NIL;
				 BOLE				TFltVector_Initialized;
				 BOLE				TLongVector_Initialized;
				 BOLE				TIntVector_Initialized;
				 BOLE				TShtVector_Initialized;
				 BOLE				TMatrix_Initialized;
				 BOLE				TNeuralNet_Initialized;
				 struct TSymbol*	TNeuralNet_NeuralNetSym;
				 struct TSymbol*    TNeuralNet_NeuralLayerSym;
				 struct TSymbol*    TNeuralNet_ContinuousSym;
				 struct TSymbol*    TNeuralNet_SigmoidalSym;
				 struct TSymbol*    TNeuralNet_BinarySym;
				 struct TSymbol*    TNeuralNet_BipolarSym;
				 BOLE				TNumMatrix_Initialized;
				 BOLE				TNumVector_Initialized;
				 NUM				TObject_MaxOutputLen;
				 BOLE				TObject_Initialized;
				 NUM				TObject_MaxObjectCount;
				 NUM				TObject_UsedObjectCount;
				 NUM				TObject_MaxUsedObjectCount;
				 NUM				TObject_FreeObjectIndex;
				 HMChar				TObject_ObjectFlag;
				 HMObj				TObject_MainObjectList;
				 HMObj				TObject_MainObjectHeaderArray;
				 SHORT				TObject_GarbageInhibit;
				 BOLE				TObject_GarbageON;
				 BOLE				TObject_GarbageCollectInProgress;
				 SHORT				TObject_MaxTypes;
				 HMName				TObject_TypeName;
				 HMChar				TObject_TypeFlag;
				 HMShort			TObject_TypeSize;
				 NUM				TObject_TypeNew;
				 NUM				TObject_TypeMark;
				 NUM				TObject_TypeGlobalMark;
				 NUM				TObject_TypeConvert;
				 NUM				TObject_TypeCompare;
				 NUM				TObject_TypeSetIV1;
				 NUM				TObject_TypeSetIV2;
				 NUM				TObject_TypeSetIV3;
				 NUM				TObject_TypeGetIV1;
				 NUM				TObject_TypeGetIV2;
				 NUM				TObject_TypeGetIV3;
				 NUM				TObject_TypeMap;
				 NUM				TObject_TypeMapc;
				 NUM				TObject_TypePrint;
				 NUM				TObject_TypeLoad;
				 NUM				TObject_TypeSave;
				 NUM				TObject_TypeDoom;
				 NUM				TObject_TypeCopy;
				 NUM				TObject_TypeComputeSize;
				 HMTval				TObject_TypeMethods;
				 HMTval				TObject_TypeFields;
				 HMType				TObject_TypeParent;
				 NUM				TObject_Ctype[256];
				 TVAL				TObject_VOID;
				 TVAL				TObject_OK;
				 TVAL				TObject_TRUE;
				 TVAL				TObject_FALSE;
				 TVAL				TObject_HIGH;
				 TVAL				TObject_EQUAL;
				 TVAL				TObject_LOW;
				 TVAL				TObject_NAN;
				 TVAL				TObject_ERROR_OUT_OF_MEMORY;
				 TVAL				TObject_ERROR_FRAME_ERROR;
				 TVAL				TObject_ERROR_INVALID;
				 TVAL				TObject_ERROR_DIVIDE_BY_ZERO;
				 TVAL				TObject_ERROR_INVALID_ARGLIST;
				 TVAL				TObject_ERROR_RUNTIME;
				 TVAL				TObject_ERROR_SYNTAX;
				 TVAL				TObject_ERROR_BADOPERATOR;
				 TVAL				TObject_ERROR_BADSYMBOL;
				 TVAL				TObject_ERROR_BADINDEX;
				 TVAL				TObject_ERROR_BADTYPE;
				 TVAL				TObject_ERROR_BADMSG;
				 TVAL				TObject_ERROR_BADCONTINUATION;
				 TVAL				TObject_ERROR_CONTINUATION;
				 TVAL				TObject_ERROR_WINDOWACTIVE;
				 TVAL				TObject_ERROR_BADCELL;
				 TVAL				TObject_ERROR_SYMBOLMUSTFOLLOW;
				 TVAL				TObject_ERROR_PAIRINDEXREQ;
				 TVAL				TObject_ERROR_SYMBOLINDEXREQ;
				 TVAL				TObject_ERROR_NUMINDEXREQ;
				 TVAL				TObject_ERROR_BADIDXORKEY;
				 TVAL				TObject_ERROR_BADIVAL;
				 TVAL				TObject_ERROR_ACCESSDENIED;
				 BOLE               TObjVector_Initialized;
				 BOLE				TPair_Initialized;
				 BOLE				TPcodeVector_Initialized;
				 unsigned char		TPcodeVector_LegalModifiers[_VMMAXINSTRS][3];
				 BOLE				TString_Initialized;
				 struct TString*	TString_NIL;
				 BOLE				TStructure_Initialized;
				 struct TSymbol*    TStructure_key;
				 struct TSymbol*    TStructure_value;
				 BOLE				TSVector_Initialized;
				 BOLE				TSymbol_Initialized;
				 struct TSymbol*	TSymbol_NIL;
				 struct TObjVector*	TSymbol_SymbolTable;
				 NUM				TSymbol_SymbolTable_ActiveCount;
				 NUM				TSymbol_SymbolTable_MaxEmptyCount;
				 NUM				TSymbol_SymbolTable_MinEmptyCount;
				 NUM				TSymbol_SymbolTable_NewAllocationSize;
				 struct TSymbol*	TSymbol_Currentworkspace;
				 struct TSymbol*	TSymbol_Path;
				 struct TSymbol*	TSymbol_SaveTypes;
				 struct TSymbol*	TSymbol_Formula;
				 struct TSymbol*	TSymbol_Script;
				 struct TSymbol*	TSymbol_Value;
				 struct TSymbol*	TSymbol_Viewer;
				 struct TSymbol*	TSymbol_Key;
				 struct TSymbol*	TSymbol_hostSend;
				 struct TSymbol*	TSymbol_BreakManager;
				 struct TSymbol*	TSymbol_add;
				 struct TSymbol*	TSymbol_addi;
				 struct TSymbol*	TSymbol_compare;
				 struct TSymbol*	TSymbol_compareLT;
				 struct TSymbol*	TSymbol_compareLE;
				 struct TSymbol*	TSymbol_compareEQ;
				 struct TSymbol*	TSymbol_compareNE;
				 struct TSymbol*	TSymbol_compareGE;
				 struct TSymbol*	TSymbol_compareGT;
				 struct TSymbol*	TSymbol_div;
				 struct TSymbol*	TSymbol_divi;
				 struct TSymbol*	TSymbol_divr;
				 struct TSymbol*	TSymbol_divri;
				 struct TSymbol*	TSymbol_mul;
				 struct TSymbol*	TSymbol_muli;
				 struct TSymbol*	TSymbol_new;
				 struct TSymbol*	TSymbol_ref;
				 struct TSymbol*	TSymbol_set;
				 struct TSymbol*	TSymbol_sub;
				 struct TSymbol*	TSymbol_subi;
				 struct TSymbol*	TSymbol___send;
				 struct TSymbol*    TSymbol_eol;
				 NUM				TSymbol_MakeUniqueTime; /* time spend in the Make_Unique call - see (inspect MakeUnique:) */
				 BOLE				TVector_Initialized;
				 BOLE				TWorkspace_Initialized;
                 NUM                maxNumberOfThreads;
				 NUM				Custom01;	/* used with perfmon - see Fperfmon.h and (inspect Custom01:) */
				 NUM				Custom02;	/* ditto */
				 NUM				Custom03;	/* ditto */
				 NUM				Custom04;	/* ditto */
				 BOLE				logMode;
				 NUM				logFileID;
				 BOLE				logTime;
				 time_t				logStartTime;
				 BOLE				logNewLineFlag;
				 POINTER			hostRegisterLispFunctions; /* Must be cast to LpFUNC which is defined immediately after here */	
                 THREAD				ThreadBlocks[1];
				 /* The array of thread blocks follows here,   */
				 /* and after the array of thread blocks, the  */
				 /* main memory blocks follow. The array is    */
				 /* dimension one only for compilation. At     */
                 /* initialization time FMemory allocates the  */
                 /* space requested in the maxNumberOfThreads  */
                 /* variable.*/
                } XCONTEXT, *LpXCONTEXT;

#define XCONTEXTBLOCKLENGTH         (NUM)(sizeof(XCONTEXT))

/*  Declare the SmartLisp Lisp operations Stack and framing macros. */
/*  Note:   These variables and macros can be used to access the  */
/*          SmartLisp operations stack so direct inline C tagged  */
/*          variables in C function calls can be protected from */
/*          garbage collection.  */

typedef     TVAL            (*LpVMEVALUATOR)	(LpXCONTEXT gCP, LpTHREAD gTP,struct TLambda* proc,NUM argc,LpTVAL argP);
typedef     TVAL            (*LpFUNC)			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,LpTVAL argP);
typedef     TVAL            (*LpFNEW)			(LpXCONTEXT gCP, LpTHREAD gTP,NUM argc,LpTVAL argP);

typedef     TVAL            ( *LpFPRINT )		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL self, LpNUM size, LpCHAR buf);
typedef     void            ( *LpFMARK )		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL srcTval);
typedef     TVAL            ( *LpFGMARK )		(LpXCONTEXT gCP, LpTHREAD gTP);
typedef     TVAL            ( *LpF2TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL v1, TVAL v2);
typedef     TVAL            ( *LpF3TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL v1, TVAL v2, TVAL v3);
typedef     TVAL            ( *LpF4TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL v1, TVAL v2, TVAL v3, TVAL v4);
typedef     TVAL            ( *LpF5TVALS )		(LpXCONTEXT gCP, LpTHREAD gTP,TVAL v1, TVAL v2, TVAL v3, TVAL v4, TVAL v5);
typedef     TVAL            ( *LpFCONVERT )		(LpXCONTEXT gCP, LpTHREAD gTP,TYPE tTarget, TVAL oldValue);

typedef     TVAL            (*LpREGCPROC)		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpFUNC lpFunc);
typedef     POINTER         (*LpOBJECTPTR)		(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL anObject);
typedef     TVAL*           (*LpGETSYMPTR)		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE perm);
typedef     TVAL            (*LpEVAL)			(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL proc,const NUM argc, ... );

typedef struct  {
                LpFUNC      Lpfunc[1];
                } FMLpfunc, **HMLpfunc;
#define atHMLpfunc(h,n) ((*(HMLpfunc)(h))->Lpfunc[n])

typedef struct  {
                LpFNEW      Lpfnew[1];
                } FMLpfnew, **HMLpfnew;
#define atHMLpfnew(h,n) ((*(HMLpfnew)(h))->Lpfnew[n])

typedef struct  {
                LpFMARK     Lpfmark[1];
                } FMLpfmark, **HMLpfmark;
#define atHMLpfmark(h,n)    ((*(HMLpfmark)(h))->Lpfmark[n])

typedef struct  {
                LpFPRINT        Lpfprint[1];
                } FMLpfprint, **HMLpfprint;
#define atHMLpfprint(h,n)   ((*(HMLpfprint)(h))->Lpfprint[n])

typedef struct  {
                LpFCONVERT      Lpfconvert[1];
                } FMLpfconvert, **HMLpfconvert;
#define atHMLpfconvert(h,n) ((*(HMLpfconvert)(h))->Lpfconvert[n])

typedef struct  {
                LpF2TVALS       Lpf2tvals[1];
                } FMLpf2tvals, **HMLpf2tvals;
#define atHMLpf2tvals(h,n)  ((*(HMLpf2tvals)(h))->Lpf2tvals[n])

typedef struct  {
                LpF3TVALS       Lpf3tvals[1];
                } FMLpf3tvals, **HMLpf3tvals;
#define atHMLpf3tvals(h,n)  ((*(HMLpf3tvals)(h))->Lpf3tvals[n])

typedef struct  {
                LpF4TVALS       Lpf4tvals[1];
                } FMLpf4tvals, **HMLpf4tvals;
#define atHMLpf4tvals(h,n)  ((*(HMLpf4tvals)(h))->Lpf4tvals[n])

typedef struct  {
                LpF5TVALS       Lpf5tvals[1];
                } FMLpf5tvals, **HMLpf5tvals;
#define atHMLpf5tvals(h,n)  ((*(HMLpf5tvals)(h))->Lpf5tvals[n])

typedef struct  {
                LpFGMARK        Lpfgmark[1];
                } FMLpfgmark, **HMLpfgmark;
#define atHMLpfgmark(h,n)   ((*(HMLpfgmark)(h))->Lpfgmark[n])

typedef     TVAL ( *LpFNLOAD )(LpXCONTEXT gCP, LpTHREAD gTP,HMemory aHMemory, NUM theFileID, NUM bResolve);
typedef struct  {
                LpFNLOAD        Lpfnload[1];
                } FMLpfnload, **HMLpfnload;
#define atHMLpfnload(h,n)   ((*(HMLpfnload)(h))->Lpfnload[n])

// Define the prototype and access macros for the Save function vector
typedef HMemory ( *LpFNSAVE )(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, HMemory aHMemory);
typedef struct  {
                LpFNSAVE        Lpfnsave[1];
                } FMLpfnsave, **HMLpfnsave;
#define atHMLpfnsave(h,n)   ((*(HMLpfnsave)(h))->Lpfnsave[n])


// Define the prototype and access macros for the computesize function vector
typedef void    ( *LpFCOMPUTESIZE )(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval, NUM* aSize);
typedef struct  {
                LpFCOMPUTESIZE  Lpfcomputesize[1];
                } FMLpfcomputesize, **HMLpfcomputesize;
#define atHMLpfcomputesize(h,n)   ((*(HMLpfcomputesize)(h))->Lpfcomputesize[n])

// Define the prototype and access macros for the copy function vector
typedef  struct TObject*  ( *LpFCOPY )(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
typedef struct  {
                LpFCOPY        Lpfcopy[1];
                } FMLpfcopy, **HMLpfcopy;
#define atHMLpfcopy(h,n)   ((*(HMLpfcopy)(h))->Lpfcopy[n])

// Define the prototype and access macros for the doom function vector
typedef  void   ( *LpFDOOM )(LpXCONTEXT gCP, LpTHREAD gTP,TVAL selfTval);
typedef struct  {
                LpFDOOM        Lpfdoom[1];
                } FMLpfdoom, **HMLpfdoom;
#define atHMLpfdoom(h,n)   ((*(HMLpfdoom)(h))->Lpfdoom[n])

#define TopOfStack                  gTP->TvalStackIdx
#define StartRecursion\
					if ((++gTP->RecursionCount) >= gTP->MaxRecursions)\
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_RECURSION);\
					else\
					if (((TopOfStack - 20) >= gTP->MaxTvalStackSize) || (TopOfStack < 0))\
						FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK)
#define EndRecursion ((gTP->RecursionCount > 0) ? --gTP->RecursionCount : (gTP->RecursionCount = 0))
#define ResetRecursion gTP->RecursionCount = 0
#define _TvalStackReset()\
    {for (gTP->TvalStackIdx = gTP->MaxTvalStackSize; gTP->TvalStackIdx > 0; gTP->TvalStack[--gTP->TvalStackIdx] = gCP->Tval_VOID);}
#define StartFrame\
    NUM __tvals__ = gTP->TvalStackIdx;NUM __tval__ = gTP->TvalStackIdx;		\
    NUM __objs__ =  gTP->ObjStackIdx; NUM __obj__ = gTP->ObjStackIdx;
#define EndFrame                                                            \
    {                                                                       \
    NUM cn;                                                                 \
    if(__tval__ > gTP->MaxTvalStackSize)									\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_STACK);                     \
    for(cn=__tvals__;cn<__tval__;cn++)                                      \
        gTP->TvalStack[cn] = gCP->Tval_VOID;								\
    gTP->TvalStackIdx = __tval__;                                           \
    if(__obj__ > gTP->MaxObjStackSize)										\
        FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_FRAME_ERROR);				\
    for(cn=__objs__;cn<__obj__;cn++)                                        \
        *gTP->ObjStack[cn] = NIL;											\
    gTP->ObjStackIdx = __obj__;												\
	StartRecursion;															\
	ThrowOnEscape;															\
    }
#define DeclareTVAL(var)				TVAL* var = (TVAL *)&gTP->TvalStack[__tval__++]
#define DeclareTVALArray(var,dim)		TVAL* var = (TVAL *)FSmartbase_InitTVALArray(gCP,gTP,&__tval__,dim)
#define DeclareOBJ(type,var)            type* var = (type *)(gTP->ObjStack[__obj__++] = (OBJ *)((type*)&var))
#define DeclareOBJArray(type,var,dim)	type* var[dim] = {(type *)FSmartbase_InitOBJArray(gCP,gTP,&__obj__,(OBJ**)var,dim)}

#define FrameReset                  ThrowOnEscape;EndRecursion;gTP->TvalStackIdx = __tvals__;gTP->ObjStackIdx = __objs__
#define FrameExit(value)            {FrameReset; return(value);}
#define FrameReturn		            {FrameReset; return;}
#define LeaveOnError(value)			if (asTag(&value) == TYERROR) FrameReturn
#define ExitOnError(value)          if (asTag(&value) == TYERROR) FrameExit(value)
#define ReturnOnError(value)        if (asTag(&value) == TYERROR) return(value)


/*  Declare the direct C value to SmartLisp tagged value conversion macros. */
/*  Note:   These macros can be used to return SmartLisp tagged values for */
/*          direct inline C to SmartLisp function calls.  */
#define     TVOID           gCP->Tval_VOID
#define     TCHAR(c)        gCP->FSmartbase_CnvFromChar((POINTER)gCP,gTP,(char *)c)
#define     TBOOL(b)        gCP->FSmartbase_CnvFromBool((POINTER)gCP,gTP,(BOLE)b)
#define     TERROR(s)       gCP->FSmartbase_Error((POINTER)gCP,gTP,(char *)s)
#define		TPERROR(s)		gCP->FSmartbase_Perror((POINTER)gCP,gTP,(char *)s) /* make error permanent */
#define     TFUNCTION(f)    gCP->FSmartbase_MakeCFunction((POINTER)gCP,gTP,(LpFUNC)f)
#define     TPOINTER(p)     gCP->FSmartbase_CnvFromPtr((POINTER)gCP,gTP,(POINTER)p)
#define     TINT(i)         gCP->FSmartbase_CnvFromInt((POINTER)gCP,gTP,(NUM)i)
#define     TOBJ(o)         gCP->FSmartbase_CnvFromObj((POINTER)gCP,gTP,(OBJ)o)
#define		TOID(oid)		FSmartbase_ObjIDToTval(gCP,gTP,(NUM)oid)
#define     TREAL(r)        gCP->FSmartbase_CnvFromReal((POINTER)gCP,gTP,(REAL)r)
#define     TFRAME(r)       gCP->FSmartbase_CnvToFrame((POINTER)gCP,gTP,(REAL)r)
#define     TSTRING(s)      gCP->FSmartbase_CnvFromText((POINTER)gCP,gTP,(char *)s)
#define     TSYMBOL(s)      gCP->FSmartbase_CnvToSymbol((POINTER)gCP,gTP,(char *)s)
#define     TQSYMBOL(s)     gCP->FSmartbase_CnvToQSymbol((POINTER)gCP,gTP,(char *)s)
#define     TGVALUE(s)      gCP->FSmartbase_GetSymbolValue((POINTER)gCP,gTP,(char *)s)

/* Define the test to determine whether the automatic system self test is on . */
#define TESTON (gCP->FMemory_SelfTest == TRUE)

/*  **********************************************************  */
/*  Declare the built-in data types for the Smartbase engine.   */
/*  Note:   These are the fundamental types available for use   */
/*          in the Smartbase engine. All user defined types     */
/*          are constructed by combining these basic types.     */
/*  **********************************************************  */

								/* The following types describe Native types */
								/* whose value is contained entirely within  */
								/* immediate memory. These types are stored  */
								/* in a TVAL(TVAL) as immediate values. They */
								/* do NOT require HEAP space and do NOT      */
								/* require HEAP garbage collection.          */

#define TYVOID          0
#define TYTVAL          1
#define TYBOLE          2
#define TYCHAR          3
#define TYCFUNCTION     4
#define TYCOMPARE       5
#define TYDATE          6
#define TYELLIPSES      7
#define TYFRAME	        8
#define TYLEX           9
#define TYMONEY         10
#define TYNUM           11
#define TYUNUM          12
#define TYPCODE         13
#define TYPOINTER       14
#define TYREAL          15
#define TYREPOINSERT    16
#define TYSHORT         17
#define TYLONG			18
#define TYTYPE          19

								/* The following types are ONLY valid in the */
								/* DeclaredType field of the bindings in the */
								/* Av, Rv, Tv, Pv, & Cv elements of an Lambda */
								/* to be used only for strong typing by the  */
								/* DTRTL virtual machine compiler. They may  */
								/* NOT be found in the Tag field of a TVAL.  */

#define TYCHARPOINTER	20
#define TYFLOAT         21
#define TYFLOATPOINTER	22
#define TYINTPOINTER	23
#define TYJUMPPOINTER	24
#define TYREALPOINTER	25
#define TYSHORTPOINTER	26
#define TYLONGPOINTER	27
#define TYWORDPOINTER	28

								/* The following types are the immediate     */
								/* repeating types with a non zero length    */
								/* whose value is an indexed reference into  */
								/* another object in AIS HEAP memory. They   */
								/* cannot be processed correctly by the      */
								/* vmregObjLength and the vmregObjPointer    */
								/* instructions */
#define TYBRICKFIELD	29
#define TYBRICKROW		30
#define TYSTRINGSUBSTR	31
#define TYMATRIXROW		32
#define TYNUMMATRIXROW  33

								/* The following type is the only native     */
								/* repeating type with a non zero length     */
								/* whose value is NOT a pointer into the     */
								/* AIS HEAP memory. Its type code must       */
								/* immediately preceed the HEAP object type  */
								/* codes so that the vmregObjLength and the  */
								/* vmregObjPointer instructions will be fast */
#define TYTEXT          34

								/* The following types describe Object types */
								/* whose value is a pointer into AIS HEAP    */
								/* memory. These types cannot be stored in   */
								/* a TVAL(TVAL) as can the Native types.     */
								/* All types, shown before this note, are    */
								/* Native types.                             */

#define TYLAMBDA		35
#define TYBITVECTOR     36
#define TYBYTEVECTOR    37
#define TYCMACRO        38
#define TYCONTINUATION  39
#define TYCPX           40
#define TYCPROCEDURE    41
#define TYCPXVECTOR     42
#define TYDICTIONARY    43
#define TYDIRECTORY     44
#define TYERROR         45
#define TYFLTVECTOR     46
#define TYGLOBALS       47
#define TYINTVECTOR     48
#define TYLABELSYMBOL   49
#define TYLONGVECTOR	50
#define TYMACRO         51
#define TYMATRIX		52
#define TYNUMMATRIX		53
#define TYNUMVECTOR     54
#define TYOBJ           55
#define TYOBJREPOSITORY 56
#define TYOBJVECTOR     57
#define TYPAIR          58
#define TYQUOTEDPAIR    59
#define TYQUOTEDSYMBOL  60
#define TYBRICK			61
#define TYSHORTVECTOR	62
#define TYSPECIALFORM   63
#define TYSTRING        64
#define TYSTRUCTURE		65
#define TYSYMBOL        66
#define TYVECTOR	    67
#define TYVMEVALUATOR   68
#define TYWORKSPACE     69
#define TYPCODEVECTOR   70
#define TYMAXVALIDTYPE  70


/*  The initialization structure containing Pointers to HOST supplied functions. */

struct  FSmartbase_HostCallBackFunctions
    {
	CHAR							ContextName[64];
    NUM     (*_Host_Display)        (LpXCONTEXT gCP, LpTHREAD gTP, char*  string,NUM  newline);
    NUM     (*_Host_Escape)         (LpXCONTEXT gCP, LpTHREAD gTP);
	NUM		(*_Host_UpdateState)	(LpXCONTEXT gCP, LpTHREAD gTP);
    NUM     (*_Host_OpenF)          (LpXCONTEXT gCP, LpTHREAD gTP, char* name, NUM mode, NUM type);
    NUM     (*_Host_ReadF)          (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, NUM length, char* bufptr);
    NUM     (*_Host_WriteF)         (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, NUM length, const char* bufptr);
    REAL    (*_Host_SeekF)          (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, REAL adjustment, NUM opcode);
    REAL    (*_Host_ResizeF)        (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, REAL newsize);
    NUM     (*_Host_CloseF)         (LpXCONTEXT gCP, LpTHREAD gTP, NUM fileID, NUM opcode);
    NUM								_Host_memorySize;
    NUM								_Host_memoryObjHdrSize;
	NUM								_Host_MaxThreadCount;
    POINTER							_Host_memBlockPtr[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
	NUM								_Host_memBlockSize[FSMARTBASE_MAXCONTEXTMEMORYBLOCKS];
    NUM								_Host_OperationsStackWords;
    NUM								_Host_GarbageStackWords;
	NUM								_Host_RequiredStackSpace;
	NUM								_MaxRecursions;
    BOLE                            _EmbeddedMySQLEnabled;
	LpFUNC							_Host_RegisterLispFunctions;
    };

typedef NUM  (*LpHOST_DISPLAY)      (POINTER gCP, LpTHREAD gTP, char*  string,NUM  newline);
typedef NUM  (*LpHOST_ESCAPE)       (POINTER gCP, LpTHREAD gTP);
typedef NUM  (*LpHOST_UPDATESTATE)	(POINTER gCP, LpTHREAD gTP);
typedef NUM  (*LpHOST_OPENF)        (POINTER gCP, LpTHREAD gTP, char*  name, NUM mode, NUM type);
typedef NUM  (*LpHOST_READF)        (POINTER gCP, LpTHREAD gTP, NUM  fileID,NUM length,char*  bufptr);
typedef NUM  (*LpHOST_WRITEF)       (POINTER gCP, LpTHREAD gTP, NUM  fileID,NUM length,const char*  bufptr);
typedef REAL (*LpHOST_SEEKF)        (POINTER gCP, LpTHREAD gTP, NUM  fileID,REAL adjustment,NUM  opcode);
typedef REAL (*LpHOST_RESIZEF)      (POINTER gCP, LpTHREAD gTP, NUM  fileID,REAL newsize);
typedef NUM  (*LpHOST_CLOSEF)       (POINTER gCP, LpTHREAD gTP, NUM  fileID,NUM opcode);

/*  API Object Disk Record Header declaration */

typedef struct  {
				CHAR	AisIdentifier[4];
				CHAR	VersionIdentifier[4];
                SHORT   recordVersion;
                CHAR    recordType;
                LONG    recordLength;
                REAL    SaveTimeStamp;
                } OBRECORDHEADER;

/* ************************************************************************************************************/
/* *** API Function declarations ******************************************************************************/
/* ************************************************************************************************************/

/* *** Basic (most often used) Caller API Function Declarations ***********************************************/
PUBLIC  LpXCONTEXT	FSmartbase_MainContextStart(LpCHAR name,NUM heapSize,NUM stackSize,NUM objHdrSize,struct FSmartbase_HostCallBackFunctions* Funcs,LpCHAR errorMsg);
PUBLIC  NUM			FSmartbase_MainContextStop(LpXCONTEXT gCP, LpTHREAD gTP);
PUBLIC  TVAL		FSmartbase_RegisterCProcedure(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpFUNC lpFunc);
PUBLIC  TVAL		FSmartbase_RegisterVMEvaluator(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR funcSymbolName,const LpVMEVALUATOR lpFunc);
PUBLIC  TVAL		FSmartbase_Run(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE print);
PUBLIC  TVAL		FSmartbase_RunLambda(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL lambda, const NUM argc, const TVAL argv[],const BOLE print);
PUBLIC  TVAL		FSmartbase_RunLambdaMember(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL lambda, const TVAL member, const NUM argc, const TVAL argv[],const BOLE print);

/* *** Basic (most often used) Caller Type Conversion Function Declarations ***********************************/
PUBLIC  NUM			FSmartbase_ObjectLen(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL anObject);
PUBLIC  POINTER		FSmartbase_VectorPtr(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL aVector);
PUBLIC  NUM			FSmartbase_VectorLen(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL aVector);
PUBLIC  BOLE		FSmartbase_StringPtr(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL aString,LpPOINTER strptr,LpNUM strlen);
PUBLIC  NUM			FSmartbase_StringLen(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL aString);
PUBLIC  TVAL		FSmartbase_ObjIDToTval(LpXCONTEXT gCP,LpTHREAD gTP,const NUM oid);

/* *** Inter Context Function Declarations ********************************************************************/
PUBLIC  LpXCONTEXT	FSmartbase_ContextInit(struct FSmartbase_HostCallBackFunctions* Funcs);
PUBLIC  NUM			FSmartbase_CalculateStackSpace(NUM memorySize);

/* *** Intra Context Function Declarations ********************************************************************/
PUBLIC  TVAL*	FSmartbase_InitTVALArray	(LpXCONTEXT gCP,LpTHREAD gTP,NUM* stackIndexPtr,const NUM arraySize);
PUBLIC  OBJ**	FSmartbase_InitOBJArray		(LpXCONTEXT gCP,LpTHREAD gTP,NUM* stackIndexPtr,OBJ* variables[],const NUM arraySize);
PUBLIC  TVAL	FSmartbase_Convert			(LpXCONTEXT gCP,LpTHREAD gTP,const TYPE targetType,const TVAL oldValue);
PUBLIC  TVAL	FSmartbase_CnvFromBool		(LpXCONTEXT gCP,LpTHREAD gTP,const BOLE inputBool);
PUBLIC  TVAL    FSmartbase_CnvFromChar		(LpXCONTEXT gCP,LpTHREAD gTP,const CHAR inputChar);
PUBLIC  TVAL    FSmartbase_CnvFromInt		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM inputInt);
PUBLIC  TVAL    FSmartbase_CnvFromPtr    	(LpXCONTEXT gCP,LpTHREAD gTP,const POINTER inputPtr);
PUBLIC  TVAL    FSmartbase_CnvFromObj		(LpXCONTEXT gCP,LpTHREAD gTP,const OBJ obj);
PUBLIC  TVAL    FSmartbase_CnvFromReal		(LpXCONTEXT gCP,LpTHREAD gTP,const REAL inputReal);
PUBLIC  TVAL	FSmartbase_CnvToFrame		(LpXCONTEXT gCP,LpTHREAD gTP,const REAL inputReal);
PUBLIC  TVAL    FSmartbase_CnvFromText		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputText);
PUBLIC  TVAL    FSmartbase_CnvFromSubstring	(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputText,const NUM start,const NUM length);
PUBLIC  TVAL    FSmartbase_CnvToSymbol		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputSymbol);
PUBLIC  TVAL    FSmartbase_CnvToQSymbol		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR inputSymbol);
PUBLIC  TVAL    FSmartbase_CnvToText		(LpXCONTEXT gCP,LpTHREAD gTP,LpCHAR buf, const NUM maxLen, const TVAL source);
PUBLIC  NUM     FSmartbase_DebugOn			(LpXCONTEXT gCP,LpTHREAD gTP);
PUBLIC  TVAL    FSmartbase_Eval				(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL proc,const NUM argc, ... );
PUBLIC  TVAL    FSmartbase_Evals			(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE print);
PUBLIC  TVAL    FSmartbase_Evalv			(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL proc, const NUM argc, const TVAL argv[]);
PUBLIC  NUM     FSmartbase_GetObjectID		(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL objectTval);
PUBLIC  TVAL    FSmartbase_GetSymbolValue	(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource);
PUBLIC  TVAL*   FSmartbase_GetSymbolValuePtr(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const BOLE perm);
PUBLIC  TVAL    FSmartbase_SetSymbolValue	(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const TVAL newValue);
PUBLIC  void    FSmartbase_MarkAndSweep		(LpXCONTEXT gCP,LpTHREAD gTP);
PUBLIC  TVAL    FSmartbase_recNumber		(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theSource,const LpNUM iChar);
PUBLIC  TVAL    FSmartbase_Ref				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  TVAL    FSmartbase_Refv				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,const TVAL argv[]);
PUBLIC  TVAL    FSmartbase_MakeCFunction	(LpXCONTEXT gCP,LpTHREAD gTP,const LpFUNC lpFunc);
PUBLIC  TVAL    FSmartbase_SendMsg			(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL msg,const TVAL target,const NUM argc, ... );
PUBLIC  TVAL    FSmartbase_SendMsgv			(LpXCONTEXT gCP,LpTHREAD gTP,const TVAL msg,const TVAL target, const NUM argc, const TVAL argv[]);
PUBLIC  TVAL    FSmartbase_Set				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  TVAL    FSmartbase_Setv				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc,const TVAL argv[]);
PUBLIC  TVAL    FSmartbase_Throw			(LpXCONTEXT gCP,LpTHREAD gTP,const NUM ret);
PUBLIC  TVAL    FSmartbase_Unimplemented	(void);
PUBLIC  TVAL    FSmartbase_Writeln			(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  HMemory FSmartbase_NewMemory		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM theSize);
PUBLIC  void    FSmartbase_FreeMemory		(LpXCONTEXT gCP,LpTHREAD gTP,HMemory theMemory);
PUBLIC  HMemory FSmartbase_ResizeMemory		(LpXCONTEXT gCP,LpTHREAD gTP,HMemory theMemory,const NUM newSize);
PUBLIC  TVAL    FSmartbase_Error			(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theMsg);
PUBLIC  TVAL    FSmartbase_Perror			(LpXCONTEXT gCP,LpTHREAD gTP,const LpCHAR theMsg);
PUBLIC  POINTER FSmartbase_ObjectPtr		(LpXCONTEXT gCP,LpTHREAD gTP,const LpTVAL anObject);
PUBLIC  POINTER FSmartbase_NilPtr			(void);
PUBLIC  HMemory FSmartbase_NilHandle		(HMemory handle);
PUBLIC  NUM     FSmartbase_ErrorTrace		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  TVAL	FSmartbase_GlobalReferences	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);
PUBLIC  TVAL    FSmartbase_LoadExtension	(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc,TVAL argv[]);

/* engine state flag management functions */
PUBLIC  NUM     FSmartbase_InstructionTrace	(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  NUM     FSmartbase_SystemCheck		(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  NUM     FSmartbase_Jit				(LpXCONTEXT gCP,LpTHREAD gTP,const NUM toggleSwitch);
PUBLIC  NUM     FSmartbase_SetEngineFlags	(LpXCONTEXT gCP,LpTHREAD gTP,const NUM engineFlags);

PUBLIC TYPE		FSmartbase_NewType (LpXCONTEXT gCP,
									LpTHREAD gTP,
									TYPE   theType,
									LpCHAR aTypeName,
									CHAR flag,
									NUM aTypeSize,
									LpFNEW aNewFunction,
									LpFMARK aMarkFunction,
									LpFGMARK aGlobalMarkFunction,
									LpFCONVERT aCnvFunction,
									LpF2TVALS aCmpFunction,
									LpF3TVALS aSetIV1Function,
									LpF4TVALS aSetIV2Function,
									LpF5TVALS aSetIV3Function,
									LpF2TVALS aGetIV1Function,
									LpF3TVALS aGetIV2Function,
									LpF4TVALS aGetIV3Function,
									LpF2TVALS aMapFunction,
									LpF2TVALS aMapcFunction,
									LpFPRINT aPrintFunction,
									LpFNLOAD aLoadFunction,
									LpFNSAVE aSaveFunction,
									LpFCOMPUTESIZE aComputeSizeFunction,
									LpFCOPY	  aCopyFunction,
									LpFDOOM	  aDoomFunction);

PUBLIC  NUM FSmartbase_AlignMe			(LpXCONTEXT gCP,LpTHREAD gTP,NUM isize);
PUBLIC  void* FSmartbase_OperatorNew	(LpXCONTEXT gCP,LpTHREAD gTP,long objSize);
PUBLIC  TVAL  FSmartbase_VectorDelete	(LpXCONTEXT gCP,LpTHREAD gTP,const NUM argc, ... );
PUBLIC  void  FSmartbase_MarkTval		(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);
PUBLIC  void  FSmartbase_MarkTvalShallow(LpXCONTEXT gCP,LpTHREAD gTP,TVAL aTval);

PUBLIC  LpNUM FSmartbase_FillWordArray(LpNUM var,LpNUM __tval__,NUM repeats);


/*  API Macro declarations */

#ifndef min
#define min(a,b)    (a < b ? a : b )
#endif
#ifndef max
#define max(a,b)    (a > b ? a : b )
#endif
#ifndef abs
#define abs(a)      (a < 0 ? -a : a )
#endif

#define _FSmartbase_Catch(sts,lbl)  {if ((sts = setjmp(gCP->FSmartbase_CatchEnv)) != 0) goto lbl;}
#define _FSmartbase_ECatch(xCP,sts,lbl)  {if ((sts = setjmp(xCP->FSmartbase_CatchEnv)) != 0) goto lbl;}
#define _FSmartbase_Throw(sts)      {longjmp(gCP->FSmartbase_CatchEnv,(sts));}

/*  File seek codes. */

#define _FSmartbase_fromStart       -1
#define _FSmartbase_fromCurPos       0
#define _FSmartbase_fromEOF      1

/*  File open codes. (To be added (ord) together). */

#define _FSmartbase_read        1
#define _FSmartbase_write       2
#define _FSmartbase_create      4
#define _FSmartbase_binary      8
#define _FSmartbase_text        16
#define _FSmartbase_perm        32

/*  Declare debug mode flags which are used to control execution during debugging */
// If you change these flags be sure to modify the corrosponding SBGLUE_ defines (/
// in asbglue.h
#define _FSmartbase_DEBUGON     0x01	/* 1st bit - ErrorTrace */
#define _FSmartbase_TRACEON     0x02	/* 2nd bit - InstructionTrace */
#define _FSmartbase_SYSCHECKON	0x04	/* 3rd bit - System Checking */
#define _FSmartbase_JITON		0x10	/* 4th bit - Just In Time Processing */
/* Note: Please reserve the 7th and 8th bit for use by the glue layer. */

#define _FSmartbase_TRACEASM    0x04	//Not used - TM, Remove?

/***********************************************************************************************/
/* Declaration of ALL object header definitions required for the creation of Smartbase objects */
/* Note1: All new object headers MUST be defined here.                                         */
/* Note2: (_FSmartbase_ObjectHeaderMaxSize) MUST reflect the largest possible object header	   */
/* Note3: On each new host computer these object layouts must be coordinated with a slot	   */
/*        size in the FMemory_FrameSize table in the Context so that no space is wasted.	   */
/***********************************************************************************************/

/* Note: Please keep this total size to an even multiple of eight bytes. */
/*       Current size of the maximum Object Header is sizeof(TDatabase) = 88 bytes on 32-bit systems. */
/*       Current size of the maximum Object Header is sizeof(TLambda) = 144 bytes on 64-bit systems. */
#ifdef _M32
#define _FSmartbase_ObjectHeaderMaxSize			sizeof(TDatabase)
#elif defined _M64
#define _FSmartbase_ObjectHeaderMaxSize			sizeof(TLambda)
#else
#error "The platform for this build is not defined in the preprocessor defines for this configuration"
#endif

typedef struct TLambda
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
struct TStructure*		ClassVariables;			/* Sv: The Lambda's self variables structure (see OOP paradygm). */
struct TStructure*		ArgumentVariables;		/* Av: The Lambda's argument variables structure. */
struct TStructure*		TemporaryVariables;		/* Tv: The Lambda's temporary variables structure. */
struct TStructure*		PersistantVariables;	/* Pv: The Lambda's persistent variables structure (see AOP paradygm). */
struct TStructure*		ConstantVariables;		/* Cv: The Lambda's persistent  constant variables structure (see OOP paradygm). */
struct TStructure*		RegisterVariables;		/* Rv: The Lambda's register variable structure */
struct TPcodeVector*	PcodeVector;			/* Pv: The Lambda's pseudo codes vector. */
struct TObject*			DebuggerSource;			/* Sc: The Lambda's source code. */
struct TByteVector*		NativeCodeVector;		/* Nc: The Lambda's native code vector. */
struct TSymbol*			VirtualMachine;			/* Vm: The Lambda's virtual machine evaluator. */
struct TStructure*		Interfaces;				/* In: The Lambda's interfaces structure. */
NUM						EvalWhenDoomed;			/* EvalWhenDoomed: The Lambda's evaluate when doomed switch. */
NUM						InUse;					/* InUse: Count of the number of times the Lambda is actively invoked */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
} TLambda;

typedef struct TBitVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMChar					itsBitArray;            /* HMemory to variable length bit array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TBitVector;
#define	_TBitVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBitVector) - sizeof(NUM)))
#define BitArray(tval)      ((LpCHAR)*((TBitVector*)((tval).u.Object))->itsBitArray)
#define BitVector(tval)     ((TBitVector*)((tval).u.Object))
#define asBitVector(atval)  ((TBitVector*)(((TVAL*)atval)->u.Object))
    

typedef struct TBrick
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index of items in the array (the byte count of the Brick data area) */
HMChar                  itsFieldArray;          /* HMemory to variable length byte array */
TVAL                    itsCdr;                 /* The Lisp tail(cdr) for the Array */
TVAL					itsFieldList;			/* Definition of fields (name, type, offset, repeats) in record byte array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM                     itsFieldCount;			/* The number of fields per row (same as itsFieldList->itsMaxItemIndex) */
NUM                     itsRowCount;			/* The number of rows per record */
NUM                     itsRowByteCount;		/* Maximum byte count in a row (the byte count of each row in the Brick object) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TBrick;
#define	_TBrick_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBrick) - sizeof(NUM)))

/* NOTES:
 * A reference to the Brick object is required.
 */
typedef struct TBrickRow
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Must be set to the number of Fields in the indexed Brick row */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TBrick*                 itsBrickObj;            /* Brick object pointer */
NUM                     itsRowIndex;            /* Brick row index within the Brick object pointer (defined above)*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TBrickRow;
#define	_TBrickRow_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBrickRow) - sizeof(NUM)))

/* NOTES:
 * A reference to the Brick object is required.
 */
typedef struct TBrickField
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Must be set to the number of repeats in the indexed Brick field */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TBrick*                 itsBrickObj;            /* Brick object pointer */
NUM                     itsRowIndex;            /* Brick row index within the Brick object pointer (defined above)*/
NUM                     itsFieldIndex;          /* Brick field index within the Brick object pointer (defined above)*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TBrickField;
#define	_TBrickField_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TBrickField) - sizeof(NUM)))
#define FieldArray(tval)    ((LpCHAR)*((tval).u.Brick->itsFieldArray))
#define asFieldArray(obj)   ((LpCHAR)*((TBrick*)(obj))->itsFieldArray)
#define Brick(tval)			((tval).u.Brick)
#define asBrick(atval)		(((TVAL*)atval)->u.Brick)
#define BrickFromRow(tval) ((TBrick*)_TObject_MainObjectList(tval.u.Index[0]))
#define BrickFromFld(tval) ((TBrick*)_TObject_MainObjectList(tval.u.Index[0]))


typedef struct TByteVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index of items in the array */
HMChar                  itsByteArray;           /* HMemory to variable length byte array */
TVAL                    itsCdr;                 /* The Lisp tail(cdr) for the Array */
NUM                     itsMemoLength;			/* Memoized size of the text (used by appendWriteln function) */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TByteVector;
#define	_TByteVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TByteVector) + sizeof(NUM)))	// Must reserve fudge space
#define ByteArray(tval)     ((LpCHAR)*((TByteVector*)((tval).u.Object))->itsByteArray)
#define ByteVector(tval)    ((TByteVector*)((tval).u.Object))
#define asByteVector(atval) ((TByteVector*)(((TVAL*)atval)->u.Object))

typedef struct TContinuation
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TVAL					itsResult;
BOLE					inScope;
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TContinuation;
#define asContinuation(a)           ((TContinuation*)((LpTVAL)(a))->u.Object)

typedef struct  TCpx
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
REAL					itsReal;				/* Real part of complex number */
REAL					itsImag;				/* Imaginary part of complex number */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TCpx;

typedef struct TCpxVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;		/* Allocated array size */
HMReal					itsCpxArray;			/* -> -> Array of pairs of doubles */
TVAL					itsCdr;
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TCpxVector;
#define	_TCpxVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TCpxVector) - sizeof(NUM)))
#define CpxArray(tval)     ((LpREAL)*((TCpxVector*)((tval).u.Object))->itsCpxArray)
#define CpxVector(tval)    ((TCpxVector*)((tval).u.Object))
#define asCpxVector(atval) ((TCpxVector*)(((TVAL*)atval)->u.Object))

typedef struct TDatabase
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TVAL					itsCodeKey;             /* Numeric key for encryption and compression, 0.0 for compression only. */
struct TString*			itsFileName;            /* File name of the Database archive disk file. */
struct TByteVector*		itsOdbID;               /* Database Vector obdID object (#void if file not open). */
struct TDirectory*		itsIndex;               /* Database Directory/Record index (#void if file not open). */
struct TVector*			itsBufferKeys;          /* Database buffer of previous keys accessed. */
struct TVector*			itsBufferValues;        /* Database buffer of previous items accessed. */
NUM						itsBaseFilePos;			/* Starting position in the database file. */
BOLE					itsTransactionOn;       /* Extended transaction in progress. */
BOLE					itsTreeOn;				/* Tree indexing strategy on. */
NUM						itsBufferCount;         /* Count of buffered items in ObjectRepository. */
NUM						itsBufferIndex;         /* Next buffered item to replace index. */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
} TDatabase;
#define Database(tval)	((TDatabase*)(tval).u.Object)
#define asDatabase(a)	((TDatabase*)asObject(a))
#define COMFORTABLEBTREENODESIZE	1000


typedef struct TDirectory
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMPBind					itsDirectoryArray;      /* The array of object/value Bondings */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Directory */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TDirectory;
#define	_TDirectory_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TDirectory) - sizeof(NUM)))
#define BondArray(tval)     ((LpBIND)*((TDictionary*)((tval).u.Object))->itsDictionaryArray)
#define Dictionary(tval)    ((TDictionary*)((tval).u.Object))
#define asDictionary(a)     ((TDictionary*)((LpTVAL)(a))->u.Obj)

typedef struct TDictionary
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMBind					itsDictionaryArray;     /* The array of object/value Bondings */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Dictionary */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TDictionary;
#define	_TDictionary_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TDictionary) - sizeof(NUM)))
#define PBindArray(tval)   ((LpPBIND)*((TDirectory*)((tval).u.Object))->itsDirectoryArray)
#define Directory(tval)    ((TDirectory*)((tval).u.Object))
#define asDirectory(a)     ((TDirectory*)((LpTVAL)(a))->u.Obj)


typedef struct TError
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of variable length data */
HMChar					itsCString;             /* HMemory to variable length data */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TError;
#define	_TError_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TError) + sizeof(NUM)))	// Must reserve fudge space
#define ErrorArray(tval)    ((LpCHAR)*((TError*)((tval).u.Object))->itsCString)
#define Error(tval)         ((TError*)((tval).u.Object))

typedef struct  TFltVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMFloat					itsFloatArray;          /* HMemory to variable length float array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TFltVector ;
#define	_TFltVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TFltVector) - sizeof(NUM)))
#define FloatArray(tval)    ((LpFLOAT)*((TFltVector*)((tval).u.Object))->itsFloatArray)
#define FltVector(tval)     ((TFltVector*)((tval).u.Object))
#define asFltVector(atval)  ((TFltVector*)(((TVAL*)atval)->u.Object))


typedef struct TIntVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMInt					itsIntArray;            /* HMemory to variable length object array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TIntVector;
#define	_TIntVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TIntVector) - sizeof(NUM)))
#define IntArray(tval)      ((LpNUM)*((TIntVector*)((tval).u.Object))->itsIntArray)
#define IntVector(tval)     ((TIntVector*)((tval).u.Object))
#define asIntVector(atval)  ((TIntVector*)(((TVAL*)atval)->u.Object))


typedef struct TLongVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMLong					itsLongArray;			/* HMemory to variable length object array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TLongVector;
#define	_TLongVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TLongVector) - sizeof(NUM)))
#define LongArray(tval)		((LpNUM32)*((TLongVector*)((tval).u.Object))->itsLongArray)
#define LongVector(tval)	((TLongVector*)((tval).u.Object))
#define asLongVector(atval) ((TLongVector*)(((TVAL*)atval)->u.Object))


typedef struct TMatrix
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;		/* Maximum index of items in the array		*/
HMTval					itsTvalMatrix;			/* HMemory to variable length item array	*/
TVAL					itsCdr;					/* The Lisp tail(cdr) for the Matrix		*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM						itsRank;				/* The rank of the array					*/
NUM						itsDimensions[_MAXDIMENSIONS];	/* The number of items in each dimension	*/
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TMatrix;
#define	_TMatrix_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TMatrix) - sizeof(NUM)))
#define TvalMatrix(tval)    ((LpTVAL)*((TMatrix*)((tval).u.Object))->itsTvalMatrix)
#define Matrix(tval)        ((TMatrix*)((tval).u.Object))
#define asMatrix(a)         ((TMatrix*)((LpTVAL)(a))->u.Obj)
#define MatrixFromRow(tval) ((TMatrix*)_TObject_MainObjectList(tval.u.Index[0]))


typedef struct TNumMatrix
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;		/* Maximum index of items in the array		*/
HMReal					itsRealMatrix;			/* HMemory to variable length real array	*/
TVAL					itsCdr;					/* The Lisp tail(cdr) for the Matrix		*/
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM						itsRank;				/* The rank of the array					*/
NUM						itsDimensions[_MAXDIMENSIONS];	/* The number of items in each dimension	*/
CHAR					itsImmediateSpace[sizeof(NUM)];	/* Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TNumMatrix;
#define	_TNumMatrix_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TNumMatrix) - sizeof(NUM)))
#define RealMatrix(tval)	    ((LpREAL)*((TNumMatrix*)((tval).u.Object))->itsRealMatrix)
#define NumMatrix(tval)         ((TNumMatrix*)((tval).u.Object))
#define asNumMatrix(atval)      ((TNumMatrix*)(((TVAL*)atval)->u.Object))
#define NumMatrixFromRow(tval)  ((TNumMatrix*)_TObject_MainObjectList(tval.u.Index[0]))

typedef struct  TNumVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMReal					itsRealArray;           /* HMemory to variable length number array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TNumVector;
#define	_TNumVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TNumVector) - sizeof(NUM)))
#define RealArray(tval)     ((LpREAL)*((TNumVector*)((tval).u.Object))->itsRealArray)
#define NumVector(tval)     ((TNumVector*)((tval).u.Object))
#define asNumVector(atval)  ((TNumVector*)(((TVAL*)atval)->u.Object))


typedef struct TObject
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
}TObject;

typedef struct TObjVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMObject				itsObjectArray;         /* HMemory to variable length object array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TObjVector;
#define	_TObjVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TObjVector) - sizeof(NUM)))
#define ObjArray(tval)      ((struct TObject**)*((TObjVector*)((tval).u.Object))->itsObjectArray)
#define ObjVector(tval)     ((TObjVector*)((tval).u.Object))
#define asObjVector(atval)  ((TObjVector*)(((TVAL*)atval)->u.Object))
    

typedef struct TPair
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index (is always 0) */
HMChar                  itsNilArray;			/* HMemory NIL pointer (always points to itsImmediatePtr which is NIL) */
TVAL					itsCar;
TVAL					itsCdr;
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (is always NIL) */
}TPair;
#define asPair(a)       ((TPair*)((LpTVAL)(a))->u.Obj)
#define isPair(a)       (asTag((LpTVAL)(a)) == TYPAIR)
#define isObjPair(o)    (_VALIDOBJ(o) && ((o)->itsObjectType == TYPAIR))
#define CarOfPair(tv)   (((TPair*)(tv).u.Obj)->itsCar)
#define CdrOfPair(tv)   (((TPair*)(tv).u.Obj)->itsCdr)
    

typedef struct TPcodeVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMInt					itsInstructionArray;    /* HMemory to variable length object array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
NUM						itsCurItemIndex;        /* Used in append to improve allocation scheme */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TPcodeVector;
#define	_TPcodeVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TPcodeVector) - sizeof(NUM)))
#define PcodeArray(tval)        ((LpNUM)*((TPcodeVector*)((tval).u.Object))->itsInstructionArray)
#define PcodeVector(tval)       ((TPcodeVector*)((tval).u.Object))
#define asPcodeVector(atval)    ((TPcodeVector*)(((TVAL*)atval)->u.Object))

typedef struct TShtVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMShort					itsShortArray;          /* HMemory to variable length object array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TShtVector;
#define	_TShtVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TShtVector) - sizeof(NUM)))
#define ShortArray(tval)    ((LpSHORT)*((TShtVector*)((tval).u.Object))->itsShortArray)
#define ShtVector(tval)     ((TShtVector*)((tval).u.Object))
#define asShtVector(atval)  ((TShtVector*)(((TVAL*)atval)->u.Object))


typedef struct TSymbol
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM                     itsMaxItemIndex;        /* Maximum index of variable length data */
HMChar                  itsCString;             /* HMemory to variable length data */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
LpFUNC                  itsCProcedure;			/* Pointer to its built-in C function (if available) */
TVAL                    itsGlobalValue;         /* The global value assigned to this symbol (if available) */
unsigned long           itsGlobalLock : 1;      /* The global locking switch for this symbol (1 if locked) */
unsigned long           itNeedsBars   : 1;      /* The global needs-bars switch for this symbol (1 if bars are needed in display) */
struct TLambda*         itsVMEmulator;          /* Present if this symbol is an Lambda Virtual Machine */
struct TSymbol*         itsUserTypeParent;      /* Present if a user type (see defclass) */
struct TStructure*		itsUserTypeFields;      /* Present if a user type (see defclass) */
struct TDictionary*		itsUserTypeMethods;     /* Present if a user type (see defclass) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
} TSymbol;
#define	_TSymbol_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TSymbol) + sizeof(NUM)))	// Must reserve fudge space
#define objSymbolArray(obj) ((LpCHAR)*((TSymbol*)(obj))->itsCString)
#define SymbolArray(tval)   ((LpCHAR)*((TSymbol*)((tval).u.Object))->itsCString)
#define Symbol(tval)        ((TSymbol*)((tval).u.Object))
#define asSymbol(atval)     ((TSymbol*)((LpTVAL)(atval))->u.Obj)

typedef struct TString
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of variable length data */
HMChar					itsCString;             /* HMemory to variable length data */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TString;
#define	_TString_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TString) + sizeof(NUM)))	// Must reserve fudge space
#define CharArray(tval)     ((LpCHAR)*((TString*)((tval).u.Object))->itsCString)
#define String(tval)        ((TString*)((tval).u.Object))
#define asString(atval)     ((TString*)((LpTVAL)(atval))->u.Obj)

typedef struct TStructure
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMBind					itsDictionaryArray;     /* The array of object/value bindings */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Structure */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
TSymbol*				itsMethods;				/* The Lisp user type (see defclass) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TStructure;
#define	_TStructure_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TStructure) - sizeof(NUM)))
#define BindArray(tval)     ((LpBIND)*((TStructure*)((tval).u.Object))->itsDictionaryArray)
#define asBindArray(obj)    ((LpBIND)*((TStructure*)(obj))->itsDictionaryArray)
#define Structure(tval)		((TStructure*)((tval).u.Object))
#define asStructure(a)		((TStructure*)((LpTVAL)(a))->u.Obj)

typedef struct TVector
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMTval					itsTvalArray;           /* HMemory to variable length item array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Vector */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
struct TObjVector*		itsAttributes;          /* The column attributes for the Vector (optional) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TVector;
#define	_TVector_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TVector) - sizeof(NUM)))
#define TvalArray(tval)     ((LpTVAL)*((TVector*)((tval).u.Object))->itsTvalArray)
#define Vector(tval)        ((TVector*)((tval).u.Object))
#define asVector(a)         ((TVector*)((LpTVAL)(a))->u.Obj)

typedef struct TWorkspace
{
NUM						itsObjectIndex;			/* Index of the object header in the object header array. */
TYPE					itsObjectType;			/* Type code of this object header. */
SHORT					itsObjectFlag;			/* Type code of this object header. */
NUM						itsMaxItemIndex;        /* Maximum index of items in the array */
HMObject				itsObjectArray;         /* HMemory to variable length object array */
TVAL					itsCdr;                 /* The Lisp tail(cdr) for the Array */
LpCHAR					itsImmediatePtr;        /* Pointer to immediate data (if available) */
CHAR					itsImmediateSpace[sizeof(NUM)];	/*  Immediate data space used to fill out to _FSmartbase_ObjectHeaderMaxSize. */
}TWorkspace;
#define	_TWorkspace_ImmediateSpace	(_FSmartbase_ObjectHeaderMaxSize - (sizeof(TWorkspace) - sizeof(NUM)))


#define ExitWhenError(value)  if (value.Tag == TYERROR)\
                                 if (gTP->TvalStack[ErrorLambdaTval].Tag != TYVOID)\
									{\
									 value = FSmartbase_Evalv(gCP,gTP,gTP->TvalStack[ErrorLambdaTval],1,&value);\
							         FrameExit(value)\
									}\
								  else\
							         FrameExit(value)

#define ThrowOnEscape	if ((gCP->_Host_Escape!=NULL) && (++gTP->FVmscript_escapeCheck >= _FSMARTBASE_ESCAPECHECK)) {gTP->FVmscript_escapeCheck = 0;if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);}
#define CheckAndThrowOnEscape	if ((*gCP->_Host_Escape)((POINTER)gCP,gTP)) FSmartbase_Throw(gCP,gTP,FSMARTBASE_ERR_ESCAPE);
#define CheckEscapeOnly	(*gCP->_Host_Escape)((POINTER)gCP,gTP);

#endif


