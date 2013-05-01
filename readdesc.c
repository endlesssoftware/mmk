/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Description file reader
**
**  MODULE DESCRIPTION:
**
**  	This module contains the read_description routine and its
**  supporting routines.
**
**  AUTHOR: 	    M. Madison
**
**  Copyright (c) 2008, Matthew Madison.
**  Copyright (c) 2013, Endless Software Solutions.
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
**  	02-APR-1993 V1.2    Madison 	Don't strip comments off commands.
**  	04-JUN-1993 V1.3    Madison 	Support for .INCLUDE files.
**  	22-OCT-1993 V1.3-1  Madison 	Trim trailing blanks off lines.
**  	12-DEC-1993 V1.4    Madison 	Add Fill_In_Missing_Sources.
**  	01-JUL-1994 V1.5    Madison 	Support for CMS.
**  	06-JUL-1994 V1.5-1  Madison 	Add filename to default filespecs.
**  	14-JUL-1994 V1.6    Madison 	Update for prefixed inferences.
**  	15-JUL-1994 V1.6-1  Madison 	Fix broken CMS fallback logic.
**  	29-DEC-1994 V1.6-2  Madison 	Allow for comment lines with leading blanks.
**  	12-JAN-1995 V1.6-3  Madison 	Defer target-libmod dependencies.
**  	19-JAN-1995 V1.6-4  Madison 	Fill in missing sources only when there
**                                       are no explicit action lines.
**  	27-JUN-1995 V1.6-5  Madison 	CMS lib elements vs. file specs.
**  	21-JUL-1995 V1.6-6  Madison 	Fix negative index into xbuf.
**  	03-OCT-1995 V1.6-7  Madison 	Fix handling of continuations!
**  	21-FEB-1996 V1.6-8  Madison 	Fix bound of loop in strip_comments.
**  	27-DEC-1998 V1.7    Madison 	General cleanup.
**  	20-JAN-2001 V1.8    Madison 	Fixes for rule use, per Chuck Lane.
**      30-MAR-2001 V1.8-1  Madison     Fix for prefixed rules, per Chuck Lane.
**      08-APR-2001 V1.8-2  Madison     Fix Find_Usable_Object to scan the
**                                      source list for a match before
**                                      inferring any dependencies.  Also change
**                                      name comparisons to be case-blind.
**      11-JUL-2002 V1.8-3  Madison     Have Fill_In_Missing_Sources walk the
**                                      double-colon dependency list, too.
**      07-AUG-2002 V1.8-4  Madison     Fixed basename match bug in Find_Usable_Object.
**      09-DEC-2003 V1.8-5  Madison     CMS objects aren't always usable.
**      02-MAR-2008 V1.9    Madison     Make base-name-match the default; fetch
**                                      newer makefiles out of CMS, if present.
**	30-SEP-2009 V1.10   Sneddon	Added MMSDESCRIPTION_FILE.
**	07-APR-2010 V1.10-1 Sneddon 	Got ahead of myself with symbols.  Changed
**                                      MMSDESCRIPTION_FILE to MMK_K_SYM_BUILTIN.
**	12-JUL-2012 V1.11   Sneddon	Tweak strip_comments to support '!='.
**	09-APR-2013 V1.12   Sneddon	#57. Fix to support default filespec
**					correctly in Read_Description.
**	01-MAY-2013 V1.13   Sneddon	#68, undo V1.12 changes.
**--
*/
#pragma module READDESC "V1.13"
#include <ctype.h>
#include "mmk.h"
#include "globals.h"
#include <rmsdef.h>

    struct IO {
    	struct IO *flink, *blink;
    	char *linebuf, *stripbuf;
    	FILEHANDLE unit;
    	int maxlen, current_line;
    	char filespec[256];
    };

/*
** Forward declarations
*/
    void Read_Description(char *, char *, int);
    static void strip_comments(char *, char *);
    static void Process_Deferred_Dependencies(void);
    static void Fill_In_Missing_Sources(void);
    static int  Find_Usable_Object(struct SFX *, struct DEPEND *);


/*
**++
**  ROUTINE:	Read_Description
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Reads in a description file and forms lines that will be
**  parsed by parse_descrip.  Handles continuation lines, strips
**  comments.
**
**  	Although rules files and description files use identical
**  syntax, the third argument is a flag to indicate that this is
**  a rules file, since error handling is different in that case.
**
**  RETURNS:	void (errors are signaled)
**
**  PROTOTYPE:
**
**  	Read_Description(char *fspec, char *defspec, int rules_file)
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
void Read_Description (char *fspec, char *defspec, int rules_file) {

    struct QUE ioque;
    struct IO *io;
    char resspec[256];
    char *buf, *xbuf;
    FILEHANDLE unit;
    unsigned int status, cstatus;
    TIME frdt, crdt, junktime;
    int bufsize, len;
    int continuation, maxlen, xlen, itry;
    char element[256];
    static char *tryfile[] =
	{"SYS$DISK:[]DESCRIP.MMS", "SYS$DISK:[]MAKEFILE."};

    if (*fspec == '\0') {
        for (itry = 0; itry < 2; itry++) {
            status = file_get_rdt(tryfile[itry], &frdt);
            if (use_cms) {
    	        extract_nametype(element, tryfile[itry]);
                cstatus = cms_get_rdt(element, 0, &crdt);
                if (OK(cstatus)) {
                    if (!OK(status) || (!(frdt.long1 == crdt.long1 && frdt.long2 == crdt.long2) &&
                            OK(lib$sub_times(&crdt, &frdt, &junktime)))) {
                        status = cms_fetch_file(element, tryfile[itry]);
                    }
                }
            }
            if (OK(status)) {
                strcpy(fspec, tryfile[itry]);
                break;
            }
        }
    } else {
        status = file_get_rdt(fspec, &frdt);
        if (use_cms) {
    	    extract_nametype(element, fspec);
            cstatus = cms_get_rdt(element, 0, &crdt);
            if (OK(cstatus)) {
                if (!OK(status) || (!(frdt.long1 == crdt.long1 && frdt.long2 == crdt.long2) &&
                            OK(lib$sub_times(&crdt, &frdt, &junktime))))
                    status = cms_fetch_file(element, fspec);
            }
        } else {
	    status = SS$_NORMAL;
	}
    }

    if (OK(status) && *fspec != '\0')
    	status = file_open(fspec, &unit, defspec, resspec, &maxlen);

    if (!OK(status)) {
    	if (rules_file) {
    	    lib$signal(MMK__NOOPNRUL, 1, fspec, status);
    	    if (OK(exit_status)) exit_status = MMK__NOOPNRUL;
    	} else {
    	    lib$signal(MMK__NOOPNDSC, 1, fspec, status);
    	}
    	return;
    }

    if (!rules_file) {
	Define_Symbol(MMK_K_SYM_BUILTIN, "MMSDESCRIPTION_FILE", resspec, strlen(resspec));
    }

    if (do_log) {
    	lib$signal((rules_file ? MMK__OPENRULE : MMK__OPENDESC), 1, resspec);
    }

    ioque.head = ioque.tail = &ioque;
    io = malloc(sizeof(struct IO));
    io->unit = unit;
    io->maxlen = maxlen;
    io->linebuf = malloc(io->maxlen+1);
    io->stripbuf = malloc(io->maxlen+1);
    io->current_line = 0;
    strcpy(io->filespec, resspec);
    queue_insert(io, ioque.tail);
    buf = (char *) 0;
    bufsize = 0;
    unit = 0;

    while (queue_remove(ioque.head, &io)) {
    	while (OK(file_read(io->unit, io->linebuf, io->maxlen+1, &len))) {
    	    io->current_line++;
    	    while (len > 0 && isspace(io->linebuf[len-1])) len--;
    	    io->linebuf[len] = '\0';
	    if (continuation && isspace(*io->linebuf)) {
    	    	char *cp;
    	    	for (cp = io->linebuf; *cp; cp++) if (!isspace(*cp)) break;
    	    	strip_comments(io->stripbuf, cp-1);
    	    	xbuf = io->stripbuf;
    	    } else if (isspace(*io->linebuf)) {
    	    	char *cp;
    	    	for (cp = io->linebuf; *cp; cp++) if (!isspace(*cp)) break;
    	    	if (*cp == '!' || *cp == '#') *io->linebuf = '\0';
    	    	xbuf = cp - 1;
    	    } else {
    	    	strip_comments(io->stripbuf, io->linebuf);
    	    	xbuf = io->stripbuf;
    	    }

    	    xlen = strlen(xbuf);
    	    continuation = 0;

    	    if (xlen > 0) {
    	    	if (xbuf[xlen-1] == '-' || xbuf[xlen-1] == '\\') {
    	    	    continuation = 1;
    	    	    xbuf[xlen-1] = '\0';
    	    	    xlen -= 1;
    	    	}
    	    	if (bufsize > 0) {
    	    	    buf = realloc(buf, bufsize+xlen);
    	    	    strcpy(buf+bufsize-1,xbuf);
    	    	    bufsize += xlen;
    	    	} else {
    	    	    bufsize = xlen + 1;
    	    	    buf = malloc(bufsize);
    	    	    strcpy(buf, xbuf);
    	    	}
    	    }
    	    if (!continuation && bufsize > 1) {
    	    	parse_descrip(buf, bufsize-1, &unit, &maxlen, io->current_line, io->filespec);
    	    	free(buf);
    	    	bufsize = 0;
    	    	if (unit) {
    	    	    queue_insert(io, &ioque);
    	    	    io = malloc(sizeof(struct IO));
    	    	    io->unit = unit;
    	    	    io->maxlen = maxlen;
    	    	    io->linebuf = malloc(io->maxlen+1);
    	    	    io->stripbuf = malloc(io->maxlen+1);
    	    	    io->current_line = 0;
    	    	    file_get_filespec(unit, io->filespec, sizeof(io->filespec));
    	    	    unit = 0;
    	    	}
    	    }
    	}

    	if (bufsize > 0) {
    	    parse_descrip(buf, bufsize-1, &unit, &maxlen, io->current_line, io->filespec);
    	    free(buf);
    	    if (unit) {
    	    	struct IO *io;
    	    	io = malloc(sizeof(struct IO));
    	    	io->unit = unit;
    	    	io->maxlen = maxlen;
    	    	io->linebuf = malloc(io->maxlen+1);
    	    	io->stripbuf = malloc(io->maxlen+1);
    	    	io->current_line = 0;
    	    	file_get_filespec(unit, io->filespec, sizeof(io->filespec));
    	    	unit = 0;
    	    	queue_insert(io, &ioque);
    	    }
    	}

    	file_close(io->unit);
    	free(io->linebuf);
    	free(io->stripbuf);
    	free(io);
    }

    Process_Deferred_Dependencies();
    Fill_In_Missing_Sources();

}

/*
**++
**  ROUTINE:	strip_comments
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Strips comments from the end of a string.  A comment begins
**  with either an exclamation point (!) or a pound sign (#).  Quoted
**  strings are handled properly.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	strip_comments(char *dest, char *source)
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
static void strip_comments (char *dest, char *source) {

    int quote;
    register char *cp=source, *cp1=dest;

    quote = 0;
    for (cp = source; *cp; cp++) {
    	if (quote) {
    	    if (*cp == '"') quote = !quote;
    	} else {
	    if (*cp == '!' || *cp == '#') break;
	}
    	*cp1++ = *cp;
    }
    while (cp1 > dest && isspace(*(cp1-1))) cp1--;
    *cp1 = 0;
}

/*
**++
**  ROUTINE:	Process_Deferred_Dependencies
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Looks for dependencies that we created for library modules
**  that are targets, and adds them to the main dependencies queue if
**  there aren't dependencies there already for them.  The deferred
**  dependencies list is created by Parse_Objects().
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Process_Deferred_Dependencies()
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:   None.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void Process_Deferred_Dependencies (void) {

    struct DEPEND *dep;

    while (queue_remove(dep_deferred.flink, &dep)) {

    	if (find_dependency(dep->target, 0) == 0) queue_insert(dep, dependencies.blink);
    	else mem_free_depend(dep);

    }

} /* Process_Deferred_Dependencies */

/*
**++
**  ROUTINE:	Fill_In_Missing_Sources
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Add implied sources to dependency rules missing them.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Fill_In_Missing_Sources()
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
static void Fill_In_Missing_Sources (void) {

    struct DEPEND *dep, *dep2, *prev;
    struct OBJREF *o, *o2;
    struct SFX *s;

/*
**  We only need to fill in sources when the target is a file,
**  there are no explicit action lines for the dependency, and
**  the existing sources do not already match a rule (checked in
**  Find_Usable_Object).
*/
    for (dep = dependencies.flink; dep != &dependencies; dep = dep->flink) {
        if (dep->double_colon) {
            /*
            **  Treat extra double-colon dependencies with no commands as if
            **  they were just additions to the first dependency.
            */
            dep2 = dep->dc_flink;
            prev = dep;
            while (dep2 != 0) {
                if (dep2->cmdqptr == 0 || (dep2->cmdqptr->flink == dep2->cmdqptr)) {
                    while (queue_remove(dep2->sources.flink, &o)) {
                        for (o2 = dep->sources.flink;
                                o2 != &dep->sources && o2->obj != o->obj;
                                o2 = o2->flink);
                        if (o2 == &dep->sources)
                            queue_insert(o, dep->sources.blink);
                        else
                            mem_free_objref(o);
                    }
                    prev->dc_flink = dep2->dc_flink;
                    mem_free_depend(dep2);
                    dep2 = prev->dc_flink;
                } else {
                    prev = dep2;
                    dep2 = dep2->dc_flink;
                }
            }
        }
    	if (dep->target->type != MMK_K_OBJ_FILE)
            continue;
    	if (dep->cmdqptr != 0 && dep->cmdqptr->flink != dep->cmdqptr)
            continue;
        s = find_suffix(dep->target->sfx);
        if (s != 0)
            Find_Usable_Object(s, dep);
    }

} /* Fill_In_Missing_Sources */

/*
**++
**  ROUTINE:	Find_Usable_Object
**
**  FUNCTIONAL DESCRIPTION:
**
**  	
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	Find_Usable_Object  sfx, dep
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
static int Find_Usable_Object (struct SFX *s, struct DEPEND *dep) {

    struct SFX *s2;
    struct RULE *r, *xr;
    struct OBJECT *obj, *obj2;
    struct OBJREF *o;
    struct DEPEND *dep2;
    char trgnam[MMK_S_FILE], tfile[MMK_S_FILE], sfile[MMK_S_FILE];
    char trgbase[MMK_S_FILE];
    int trgnamlen, tfilelen, check_cms, slen, baselen;

/*
**  Don't use the CMS suffixes if we're not using CMS
*/
    slen = strlen(s->value);
    check_cms = s->value[slen-1] == '~';
    if (check_cms && !use_cms) return 0;

    tfilelen = extract_name(tfile, dep->target->name);
/*
**  Look through the sources first to see if there is a matching rule
#ifndef BASE_NAME_MATCH_NOT_REQUIRED
**  _and_ there is a base name match.
#endif
*/
    baselen = extract_filename(trgbase, dep->target->name);
    for (o = dep->sources.flink; o != &dep->sources; o = o->flink) {
        if (o->obj->type != MMK_K_OBJ_FILE) continue;
        r = find_rule_with_prefixes(dep->target, o->obj);
        if (r == 0) continue;
#ifndef BASE_NAME_MATCH_NOT_REQUIRED 
        trgnamlen = extract_filename(sfile, o->obj->name);
        if (baselen == trgnamlen && strneql_case_blind(trgbase, sfile, trgnamlen))
#endif
            return 1;
    }

/*
**  Go through suffixes, looking for one that will (a) get us to
**  the desired suffix, and (b) has a source file.  If (a) and not
**  (b) then recurse to find how to get the intermediate suffix
**  (implicit rule chaining).
*/
    for (s2 = s->flink; s2 != &suffixes; s2 = s2->flink) {

    /*
    ** Skip over any CMS element suffixes if we aren't using CMS on this build.
    */
        if (!use_cms) {
            slen = strlen(s2->value);
            if (s2->value[slen-1] == '~')
                continue;
        }
    /*
    **	First check for an explicit dependency
    */
    	for (dep2 = dependencies.flink; dep2 != &dependencies; dep2 = dep2->flink) {
    	    if (dep2->target->type != MMK_K_OBJ_FILE) continue;
    	    if (dep2->cmdqptr == 0 || dep2->cmdqptr->flink == dep2->cmdqptr) continue;
    	    if (strcmp(dep2->target->sfx, s2->value) != 0) continue;
    	    trgnamlen = extract_name(sfile, dep2->target->name);
    	    if (tfilelen != trgnamlen || !strneql_case_blind(tfile, sfile, tfilelen)) continue;

    	    obj = mem_get_object();
    	    memcpy(obj->name, tfile, trgnamlen);
    	    strcpy(obj->name+trgnamlen, s2->value);
    	    strcpy(obj->sfx, s2->value);
    	    obj->type = obj->sfx[strlen(obj->sfx)-1] == '~' ?
                          MMK_K_OBJ_CMSFILE : MMK_K_OBJ_FILE;
    	    if ((obj2 = Find_Object(obj)) == 0) {
    	    	Insert_Object(obj);
    	    } else {
    	    	mem_free_object(obj);
    	    	obj = obj2;
    	    }
    	    o = mem_get_objref();
    	    o->obj = obj;
    	    queue_insert(o, &dep->sources);
    	    return 1;
    	}

    /*
    **	Find a rule to get from s2 suffix to s suffix
    */
    	xr = find_rule(s->value, s2->value);
    	if (xr != 0) {
    	    r = scan_rule_list(xr, dep->target->name, 0);
    	    if (r == 0) {
    	    	if (!Find_Usable_Object(s2, dep)) continue;
    	    	r = scan_rule_list(xr, dep->target->name, 0x10);
    	    	if (r == 0) continue; /* XXX should never happen */
    	    }
    	    obj = mem_get_object();
            if (r->srcpfxlen != 0 || r->trgpfxlen != 0) {
    	        trgnamlen = extract_filename(trgnam, dep->target->name);
    	        memcpy(obj->name, r->srcpfx, r->srcpfxlen);
    	        memcpy(obj->name+r->srcpfxlen, trgnam, trgnamlen);
    	        strcpy(obj->name+(r->srcpfxlen+trgnamlen), s2->value);
            } else {
                memcpy(obj->name, tfile, tfilelen);
                strcpy(obj->name+tfilelen, s2->value);
            }
    	    strcpy(obj->sfx, s2->value);
    	    obj->type = obj->sfx[strlen(obj->sfx)-1] == '~' ?
                          MMK_K_OBJ_CMSFILE : MMK_K_OBJ_FILE;
    	    if ((obj2 = Find_Object(obj)) == NULL) {
    	    	Insert_Object(obj);
    	    } else {
    	    	mem_free_object(obj);
    	    	obj = obj2;
    	    }
    	    o = mem_get_objref();
    	    o->obj = obj;
    	    queue_insert(o, &dep->sources);
    	    return 1;
    	}
    }

    return 0;

} /* Find_Usable_Object */
