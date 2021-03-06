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
fsim.h

PARENT:             None

AUTHORS:            Michael F. Korns

#endif
 
#ifndef _H_FSim
#define _H_FSim

#include    "tobject.h"
#include    "tstring.h"
#include    "tsymbol.h"

/*  Function declarations */

extern TVAL FSim_Init(void);
extern TVAL FSim_Simulate(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);   
extern TVAL FSim_TranSimToLisp(LpXCONTEXT gCP,LpTHREAD gTP,NUM argc, TVAL argv[]);  

#endif

