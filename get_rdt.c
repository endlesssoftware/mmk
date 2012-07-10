/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Revision date/time routines
**
**  MODULE DESCRIPTION:
**
**  	This module contains routines that obtain the RDT for
**  MMS objects (mainly for library modules).
**
**  AUTHOR: 	    M. Madison
**
**  Copyright (c) 2008, Matthew Madison.
**  
**  All rights reserved.
**  
**  Redistribution and use in source and binary forms, with or without
**  modification, are permitted provided that the following conditions
**  are met:
**  
**      * Redistributions of source code must retain the above
**        copyright notice, this list of conditions and the following
**        disclaimer.
**      * Redistributions in binary form must reproduce the above
**        copyright notice, this list of conditions and the following
**        disclaimer in the documentation and/or other materials provided
**        with the distribution.
**      * Neither the name of the copyright owner nor the names of any
**        other contributors may be used to endorse or promote products
**        derived from this software without specific prior written
**        permission.
**  
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
**  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
**  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
**  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
**  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
**  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
**  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
**  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
**  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
**  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
**  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
**  CREATION DATE:  21-AUG-1992
**
**  MODIFICATION HISTORY:
**
**  	21-AUG-1992 V1.0    Madison 	Initial coding.
**  	27-AUG-1992 V1.1    Madison 	Comments, cleanup.
**  	12-SEP-1992 V1.1-1  Madison 	Cache RDT's.
**  	09-APR-1993 V1.1-2  Madison 	Comments.
**  	01-JUL-1994 V1.2    Madison 	Update for CMS.
**  	12-JUL-1994 V1.2-1  Madison 	Set RDT to zero on failure.
**  	17-JUL-1995 V1.2-2  Madison 	Set have_rdt flag if successful.
**  	06-NOV-1995 V1.2-3  Madison 	Don't open too many libraries.
**  	27-DEC-1998 V1.3    Madison 	General cleanup.
**--
*/
#pragma module GET_RDT "V1.3"
#include "mmk.h"
#include <rms.h>
#include <mhddef.h>
#include <lbrdef.h>

/*
** Forward declarations
*/
    unsigned int lbr_get_rdt(char *, char *, TIME *);
    void lbr_flush(void);

/*
** Context structure and header for the list that tracks them
*/

    struct LBR {
    	struct LBR *flink, *blink;
    	struct NAM nam;
    	unsigned int lbrctx;
    	char espec[256];
    	char rspec[256];
    };

    static struct QUE lbrque = {&lbrque, &lbrque};
    static int lbrcount = 0;

/*
** External references
*/
    extern unsigned int lbr$ini_control(), lbr$open(), lbr$lookup_key();
    extern unsigned int lbr$set_module(), lbr$close();

#pragma nostandard
    globalvalue unsigned int LBR$_HDRTRUNC;
#pragma standard


/*
**++
**  ROUTINE:	lbr_get_rdt
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Gets the RDT of a library module.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	lbr_get_rdt(char *libfile, char *module, TIME *rdt);
**
**  libfile:	ASCIZ_string, read only, by reference
**  module: 	ASCIZ_string, read only, by reference
**  rdt:    	date_time, quadword (signed), write only, by reference
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	Any from the LBR$ routines.
**
**  SIDE EFFECTS:   	lbrque modified.
**
**--
*/
unsigned int lbr_get_rdt (char *lib, char *mod, TIME *rdt) {

    unsigned int lbrfunc=LBR$C_READ;
    char real_name[256];
    unsigned char fid[28];
    struct dsc$descriptor libdsc, moddsc;
    struct LBR *lbr;
    unsigned int status, len;
    unsigned short rfa[3];
    struct mhddef mhd;

/*
** First look up the library file
*/
    status = file_find(lib, 0, real_name, fid);
    if (!OK(status)) return status;

/*
** Already open?
*/
    for (lbr = (struct LBR *)lbrque.head; lbr != (struct LBR *)&lbrque;
    	    	    lbr = lbr->flink) {
    	if (memcmp(fid, &lbr->nam.nam$t_dvi, 28) == 0) break;
    }

/*
** If not open yet, construct a context block and open it.
*/
    if (lbr == (struct LBR *) &lbrque) {
/*
**  Already have max number of libraries open?  If so, close one.
*/
    	if (lbrcount >= LBR$C_MAXCTL) {
    	    queue_remove(lbrque.head, &lbr);
    	    lbr$close(&lbr->lbrctx);
    	    lbrcount -= 1;
    	} else {
    	    lbr = malloc(sizeof(struct LBR));
    	}
    	queue_insert(lbr, lbrque.tail);
    	lbr->lbrctx = 0;
    	lbr->nam = cc$rms_nam;
    	lbr->nam.nam$b_rss = sizeof(lbr->rspec)-1;
    	lbr->nam.nam$b_ess = sizeof(lbr->espec)-1;
    	lbr->nam.nam$l_esa = lbr->espec;
    	lbr->nam.nam$l_rsa = lbr->rspec;
    	status = lbr$ini_control(&lbr->lbrctx, &lbrfunc, 0, &lbr->nam);
    	if (!OK(status)) lib$signal(status);
    	INIT_SDESC(libdsc, strlen(real_name), real_name);
    	status = lbr$open(&lbr->lbrctx, &libdsc);
    	if (!OK(status)) return status;
    	lbrcount += 1;
    }

/*
** Look up the module in question...
*/
    INIT_SDESC(moddsc, strlen(mod), mod);
    status = lbr$lookup_key(&lbr->lbrctx, &moddsc, rfa);
    if (!OK(status)) return status;

/*
**  ... and get the RDT from the module header
*/
    INIT_SDESC(moddsc, sizeof(mhd), &mhd);
    status = lbr$set_module(&lbr->lbrctx, rfa, &moddsc, &len);
    if (!OK(status) && (status != LBR$_HDRTRUNC)) lib$signal(status);
    memcpy(rdt, &mhd.mhd$l_datim, 8);
    return SS$_NORMAL;

}

/*
**++
**  ROUTINE:	lbr_flush
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Closes any libraries we have left open.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	lbr_flush()
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	Any from LBR$ routines.
**
**  SIDE EFFECTS:   	lbrque modified.
**
**--
*/
void lbr_flush (void) {

    struct LBR *lbr;

    while (queue_remove(lbrque.head, &lbr)) {
    	lbr$close(&lbr->lbrctx);
    	free(lbr);
    }
    lbrcount = 0;
}

/*
**++
**  ROUTINE:	get_rdt
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Generic RDT fetch routine for MMS objects.  Fills in the
**  RDT field in the object structure.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	get_rdt(struct OBJECT *obj)
**
**  obj:    OBJECT structure, modify, by reference
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	Any from file_get_rdt or lbr_get_rdt
**  	    	    	0: object is not "tangible" - no revision date
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int get_rdt (struct OBJECT *obj) {

    unsigned int status;

    if (obj->have_rdt) return SS$_NORMAL;

    memset(&obj->rdt, 0, sizeof(obj->rdt));

    switch (obj->type) {
    	case MMK_K_OBJ_FILE:
    	case MMK_K_OBJ_LIB:
    	    status = file_get_rdt(obj->name, &obj->rdt);
    	    break;
    	case MMK_K_OBJ_LIBMOD:
       	    status = lbr_get_rdt(obj->libfile->name, obj->name, &obj->rdt);
    	    break;
    	case MMK_K_OBJ_CMSFILE:
    	    status = cms_get_rdt(obj->name, obj->cms_gen, &obj->rdt);
    	    break;
    	default:
    	    status = 0;
    	    break;
    }

    if (OK(status)) obj->have_rdt = 1;

    return status;
}
