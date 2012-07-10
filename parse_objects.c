/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Source/Target object parser.
**
**  MODULE DESCRIPTION:
**
**  	This module contains routines for parsing MMS object names,
**  which are in one of the following forms:
**
**  	OBJECT                     generic object - no suffix, not a file
**  	OBJECT.SFX                 file object
**      LIB.SFX(OBJECT)            library with library module object with
**                                     implicit file dependency
**  	LIB.SFX(OBJECT=FILE.SF2)   library with libmod object with explicit
**                                     file dependency
**
**  Lists of object names are parsed.  Library specifications may also
**  contain lists of module references.
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
**  CREATION DATE:  20-AUG-1992
**
**  MODIFICATION HISTORY:
**
**  	20-AUG-1992 V1.0    Madison 	Initial coding.
**  	01-SEP-1992 V1.1    Madison 	Comments.
**  	08-MAR-1993 V1.1-1  Madison 	Fix for library modules.
**  	23-APR-1993 V1.2    Madison 	Improve suffix handling.
**  	29-APR-1993 V1.2-1  Madison 	Handle some suffix-less file cases.
**  	27-SEP-1993 V1.2-2  Madison 	Assume libraries w/no suffix are .OLB.
**  	22-OCT-1993 V1.2-3  Madison 	Fix blank-line problem.
**  	02-DEC-1993 V1.2-4  Madison 	Back out intermediate-deletes.
**  	28-JUN-1994 V1.2-5  Madison 	Track modules' file objects.
**  	01-JUL-1994 V1.3    Madison 	Add CMS support.
**  	22-AUG-1994 V1.3-1  Madison 	Remove KILL_INTERMEDIATES stuff.
**  	28-AUG-1994 V1.4    Madison 	Uniform calling sequence for store rtn.
**  	12-JAN-1995 V1.4-1  Madison 	Defer target libmod dependencies.
**  	30-APR-1995 V1.4-2  Madison 	Don't prefix lib modules if they've
**  	    	    	    	        already got dev/dir specifications.
**  	29-MAY-1996 V1.4-3  Madison 	Special handling for macros beginning
**  	    	    	    	    	with dots.
**  	20-JUN-1997 V1.4-4  Madison 	Silently ignore null objects.
**  	27-DEC-1998 V1.5    Madison 	General cleanup.
**      03-MAY-2004 V1.6    Madison     Integrate IA64 changes.
**--
*/
#pragma module PARSE_OBJECTS "V1.6"
#include "mmk.h"
#pragma nostandard
    globalvalue int LIB$_SYNTAXERR;
#pragma standard
#include "globals.h"
#include <fscndef.h>
#define __NEW_STARLET
#include <tpadef.h>

/*
** Private extended TParse state info block
*/
#define TPA_C_LENGTH	(TPA$C_LENGTH0+4)
#define TPA_K_COUNT 	(TPA$K_COUNT0+1)

    struct TPABLK {
    	TPADEF tpa0;
    	int tpa_l_istarget;
    	struct QUE *tpa_l_objqptr;
    };

/*
** Forward declarations
*/

    void Parse_Objects(char *, int, struct QUE *, int);
    int  parse_obj_store(struct TPABLK *);
    static void process_sfx(struct OBJECT *, int);

/*
** Parse transition codes; must match corresponding values in PARSE_TABLE.MAR.
*/

#define PO_K_LIB_BEGIN	 1
#define PO_K_END_OBJ	 2
#define PO_K_APPNAM	 3
#define PO_K_APPNAM_CMS	 4
#define PO_K_LIB_END	 5
#define PO_K_MOD_END	 6
#define PO_K_APPMOD	 7
#define PO_K_APPFIL	 8
#define PO_K_OBJ_INIT	 9
#define PO_K_MOD_FILE	10
#define PO_K_APPGEN	11
#define PO_K_APPGENQ	12

/*
** External references
*/
#if defined(__ALPHA) || defined(__ia64__)
    extern int po_state, po_key;
    unsigned int lib$table_parse();
#define lib$tparse lib$table_parse
#else
    globalref po_state, po_key;
#endif

/*
**++
**  ROUTINE:	Parse_Objects
**
**  FUNCTIONAL DESCRIPTION:
**
**  RETURNS:	void (all errrors are signaled)
**
**  PROTOTYPE:
**
**  	Parse_Objects(char *line, int linelen, sturct OBJECT *object_queue)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
void Parse_Objects (char *line, int linelen, struct QUE *objque, int is_target) {
    
    struct TPABLK tpablk;
    int i, nllen;
    unsigned int status;

    memset(&tpablk, 0, TPA_C_LENGTH);
    tpablk.tpa0.tpa$l_count = TPA_K_COUNT;
    tpablk.tpa0.tpa$l_options = TPA$M_BLANKS;
    tpablk.tpa0.tpa$l_stringcnt = linelen;
    tpablk.tpa0.tpa$l_stringptr = line;
    tpablk.tpa_l_istarget = is_target;
    tpablk.tpa_l_objqptr = objque;

    status = lib$tparse(&tpablk, &po_state, &po_key);

    if (!OK(status)) lib$signal(MMK__PARSERR, 2, linelen, line, status);

} /* Parse_Objects */

/*
**++
**  ROUTINE:	PARSE_OBJ_STORE
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Character store routine for use with LIB$TPARSE.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	PARSE_OBJ_STORE  flag, inplen, inp, toklen, tok, char, number,
**  	    	    	usrarg
**
** The first eight arguments are the LIB$TPARSE standard argument block.
** (Note that LIB$TABLE_PARSE passes the address of the state array,
**  rather than the state array elements as arguments.)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	SS$_NORMAL: 	normal successful completion
**  	Other errors are signaled.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
int parse_obj_store (struct TPABLK *tpa) {

    int len;
    static struct OBJECT *obj = (struct OBJECT *) 0;
    static struct OBJECT *libobj = (struct OBJECT *) 0;
    static struct OBJECT *filobj = (struct OBJECT *) 0;
    struct OBJREF *or;
    struct OBJECT *obj2, *queued_object;
    struct DEPEND *dep;
    static char *namptr, *sfxptr, *genptr;

    switch (tpa->tpa0.tpa$l_param) {

    case PO_K_OBJ_INIT:
	obj = mem_get_object();
    	namptr = obj->name;
    	sfxptr = obj->sfx;
    	genptr = obj->cms_gen;
    	break;

    case PO_K_APPNAM_CMS:
    	obj->type = MMK_K_OBJ_CMSFILE;
    case PO_K_APPNAM:
    	*namptr++ = toupper(tpa->tpa0.tpa$b_char);
    	*namptr = '\0';
    	break;

    case PO_K_LIB_BEGIN:
    	process_sfx(obj, 0);
    	if (obj->sfx[0] == '\0') {
    	    strcpy(obj->sfx, ".OLB");
    	    strcat(obj->name, ".OLB");
    	}
    	obj->type = MMK_K_OBJ_LIB;
    	libobj = obj;
    	obj2 = Find_Object(libobj);
    	if (obj2 == NULL) {
    	    Insert_Object(libobj);
    	} else {
    	    mem_free_object(libobj);
    	    libobj = obj2;
    	}
    	obj = mem_get_object();
    	namptr = obj->name;
    	sfxptr = obj->sfx;
    	obj->libfile = libobj;
    	obj->type = MMK_K_OBJ_LIBMOD;
    	filobj = (struct OBJECT *) 0;
    	break;

    case PO_K_APPMOD:
    	*namptr++ = toupper(tpa->tpa0.tpa$b_char);
    	*namptr = '\0';
    	break;

    case PO_K_MOD_FILE:
    	filobj = mem_get_object();
    	namptr = filobj->name;
	sfxptr = filobj->sfx;
    	filobj->type = MMK_K_OBJ_FILE;
    	break;

    case PO_K_APPFIL:
    	*namptr++ = toupper(tpa->tpa0.tpa$b_char);
    	*namptr = '\0';
    	break;

    case PO_K_MOD_END:
    case PO_K_LIB_END: if (obj->name[0] != '\0') {

    	struct CMD *c;
    	char tmp[MMK_S_FILE], realobjname[MMK_S_FILE];

    	if (filobj) process_sfx(filobj, 0);
/*
**  Make sure that the library module's object name is just the module
**  name.
*/
    	strcpy(tmp, obj->name);
    	extract_name(realobjname, tmp);
    	extract_filename(obj->name, tmp);
    	extract_filetype(obj->sfx, tmp);
/*
**  Special case for macro names that begin with '.'
*/
    	if (realobjname[0] == '\0' && obj->sfx[0] != '\0') {
    	    strcpy(realobjname, obj->sfx);
    	    strcpy(obj->name, obj->sfx);
    	    obj->sfx[0] = '\0';
    	}

    	dep = mem_get_depend();
    	c = mem_get_cmd();
    	dep->cmdqptr = c->flink = c->blink = c;
/*
**  Assume the right suffix if one wasn't given.  Take it from the
**  =xxx file, if one was given...
*/
    	if (obj->sfx[0] == '\0' && filobj) {
    	    if (filobj->sfx[0] != '\0') strcpy(obj->sfx, filobj->sfx);
    	}

/*
**  Or make the assumption based on the library's suffix.
*/
    	if (obj->sfx[0] == '\0') {
    	    struct SFX *s;
    	    struct RULE *r;
    	    s = find_suffix(libobj->sfx);
    	    if (s) {
    	    	for (s = s->flink; s != &suffixes; s = s->flink) {
    	    	    r = find_rule(libobj->sfx, s->value);
    	    	    if (r) break;
    	    	}
    	    	if (r) strcpy(obj->sfx, s->value);
    	    }
    	}

/*
**  Now insert the object into the object tree...
*/
    	obj2 = Find_Object(obj);
    	if (obj2 == NULL) {
    	    Insert_Object(obj);
    	} else {
    	    mem_free_object(obj);
    	    obj = obj2;
    	}
    	queued_object = mem_get_object();
    	memcpy(queued_object, obj, sizeof(struct OBJECT));
    	queue_insert(queued_object, tpa->tpa_l_objqptr->tail);
    	dep->target = obj;
/*
**  Now make sure that the module's file object has the suffix on it,
**  if one was given.  If one wasn't given, fake one up.
*/
    	if (filobj) {
    	    if (filobj->sfx[0] == '\0') {
    	    	strcpy(filobj->sfx, obj->sfx);
    	    	strcat(filobj->name, obj->sfx);
    	    }
    	    obj2 = Find_Object(filobj);
    	    if (obj2 == NULL) {
    	    	Insert_Object(filobj);
    	    } else {
    	    	mem_free_object(filobj);
    	    	filobj = obj2;
    	    }
    	    or = mem_get_objref();
    	    or->obj = filobj;
    	    queue_insert(or, dep->sources.blink);
    	} else {
    	    char tmp[MMK_S_FILE];
    	    int i;
    	    filobj = mem_get_object();
    	    filobj->type = MMK_K_OBJ_FILE;
/*
**  If the library module does not have a device/directory specification,
**  assume it's located in the same directory as the library.
*/
    	    if (extract_prefix(tmp, realobjname) == 0) {
    	    	i = extract_prefix(tmp, libobj->name);
    	    	memcpy(filobj->name, tmp, i);
    	    } else {
    	    	i = 0;
    	    }
    	    strcpy(filobj->name+i,
    	    	    realobjname + (realobjname[0] == '.' ? 1 : 0));
    	    strcat(filobj->name+i, dep->target->sfx);
    	    strcpy(filobj->sfx, dep->target->sfx);
    	    obj2 = Find_Object(filobj);
    	    if (obj2 == NULL) {
    	    	Insert_Object(filobj);
    	    } else {
    	    	mem_free_object(filobj);
    	    	filobj = obj2;
    	    }
    	    or = mem_get_objref();
    	    or->obj = filobj;
    	    queue_insert(or, dep->sources.blink);
    	}
    	if (tpa->tpa_l_istarget) {
    	    queue_insert(dep, dep_deferred.blink);
    	} else {
    	    queue_insert(dep, dep_internal.blink);
    	}
    	obj->fileobj = queued_object->fileobj = filobj;
    	obj = filobj = (struct OBJECT *) 0;
    	if (tpa->tpa0.tpa$l_param == PO_K_MOD_END) {
    	    obj = mem_get_object();
    	    obj->type = MMK_K_OBJ_LIBMOD;
    	    obj->libfile = libobj;
    	    namptr = obj->name;
    	} else {
    	    libobj = NULL;
    	}
    	break;
    } else {  /* silently ignore null modules */
    	if (tpa->tpa0.tpa$l_param == PO_K_MOD_END) {
    	    namptr = obj->name;
    	    sfxptr = obj->sfx;
    	} else {
    	    mem_free_object(obj);
    	    obj = 0;
    	    libobj = 0;
    	}
    	if (filobj) mem_free_object(filobj);
    	filobj = 0;
    	break;
    }

    case PO_K_END_OBJ:
    	if (obj) {
    	    if (obj->name[0] != '\0') {
    	    	process_sfx(obj, 0);
    	    	if (obj->type == 0) {
    	    	    obj->type = obj->sfx[0] ? MMK_K_OBJ_FILE :
    	    	    	OK(file_exists(obj->name, "")) ? MMK_K_OBJ_FILE :
    	    	    	    MMK_K_OBJ_GENERIC;
    	    	}
    	    	queue_insert(obj, tpa->tpa_l_objqptr->tail);
    	    } else {  /* silently ignore null object */
    	    	mem_free_object(obj);
    	    }
    	    obj = NULL;
    	}
    	break;

    case PO_K_APPGEN:
    	if (genptr-obj->cms_gen < sizeof(obj->cms_gen)-1) {
    	    *genptr++ = tpa->tpa0.tpa$b_char;
    	    *genptr = '\0';
    	}
    	break;

    default:
    	lib$signal(MMK__PRSTBLERR, 0);

    }

    return SS$_NORMAL;

} /* parse_obj_store */

/*
**++
**  ROUTINE:	process_sfx
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Parses the suffix from an object name.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	tbs
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void process_sfx (struct OBJECT *obj, int chop_off) {

    struct dsc$descriptor fdsc;
    static struct {
    	unsigned short len, code;
    	char *ptr;
    	unsigned int term;
    } itmlst = {0, FSCN$_TYPE, 0, 0};
    unsigned int status;

    INIT_SDESC(fdsc, strlen(obj->name), obj->name);
    status = sys$filescan(&fdsc, &itmlst, 0);
    if (OK(status) && itmlst.len != 0) {
    	if (itmlst.ptr[itmlst.len] == '~') itmlst.len++;
    	if (itmlst.len > MMK_S_SFX-1) itmlst.len = MMK_S_SFX-1;
    	memcpy(obj->sfx, itmlst.ptr, itmlst.len);
    	obj->sfx[itmlst.len] = '\0';
    	if (chop_off) *itmlst.ptr = '\0';
    } else {
    	obj->sfx[0] = '\0';
    }

}
