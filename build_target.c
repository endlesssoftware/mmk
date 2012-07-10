/*
**  FACILITY:	MMK
**
**  ABSTRACT:	Routines for performing builds.
**
**  MODULE DESCRIPTION:
**
**  	This module contains routines that are used to seek out
**  dependencies, determine if targets need to be updated, and
**  actually carry out those updates.
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
**  	27-AUG-1992 V1.1    Madison 	Comments.
**  	29-SEP-1992 V1.2    Madison 	Add support for /FORCE, /FROM, /OUT.
**  	12-OCT-1992 V1.2-1  Madison 	Handle generic sources case better.
**  	02-APR-1993 V1.3    Madison 	Handle ignore error flag.
**  	09-APR-1993 V1.3-1  Madison 	Comments.
**  	29-APR-1993 V1.3-2  Madison 	Fix SYS$OUTPUT conflicts.
**  	04-JUN-1993 V1.4    Madison 	Add IGNORE support.
**  	07-JUN-1993 V1.5    Madison 	Provide changed-source list.
**  	21-AUG-1993 V1.6    Madison 	Don't assume first source is main.
**  	24-SEP-1993 V1.6-1  Madison 	Set have_rdt on built targets when /FROM.
**  	17-OCT-1993 V1.7    Madison 	Eliminate need for permanent libfiles.
**  	20-OCT-1993 V1.8    Madison 	Add ctrl-T AST support.
**  	22-OCT-1993 V1.8-1  Madison 	Improve handling of intermediate files.
**  	22-NOV-1993 V1.8-2  Madison 	Have Build_Target use Parse_Objects,
**  	    	    	    	    	    fix trgobj setting.
**  	01-DEC-1993 V1.8-3  Madison 	Have built-in rules honor /IGNORE setting.
**  	02-DEC-1993 V1.8-4  Madison 	Backout intermediate-deletion stuff.
**  	09-DEC-1993 V1.8-5  Madison 	Remove extraneous extract_name call.
**  	12-DEC-1993 V1.9    Madison 	Support for $(MMS) macro.
**  	14-APR-1994 V1.9-1  Madison 	Fix build of changed-sources list.
**  	06-MAY-1994 V1.9-2  Madison 	Output @-prefixed command on /NOACTION.
**  	27-JUN-1994 V1.9-3  Madison 	Fix misplaced break.
**  	01-JUL-1994 V2.0    Madison 	Add CMS support.
**  	11-JUL-1994 V2.0-1  Madison 	Fix the way .DEFAULT is handled.
**  	12-JUL-1994 V2.1    Madison 	Fix some other MMS discrepancies.
**  	14-JUL-1994 V2.2    Madison 	Add prefixing on inference rules.
**  	22-JUL-1994 V2.2-1  Madison 	Let prefixed rules inherit actions from
**  	    	    	    	    	   generics.
**  	22-AUG-1994 V2.2-2  Madison 	Edit out the KILL_INTERMEDIATES stuff.
**  	28-DEC-1994 V2.3    Madison 	Allow for cmdqptr == 0 case; let
**  	    	    	    	    	  generic targets have no action lists.
**  	10-JAN-1995 V2.3-1  Madison 	Handle command echoing specially.
**  	12-JAN-1995 V2.3-2  Madison 	Handle line splits better.
**  	21-JUN-1995 V2.4    Madison 	Support /SKIP, /CHECK.
**  	21-JUL-1995 V2.4-1  Madison 	Fix for /SKIP.
**  	06-NOV-1995 V2.4-2  Madison 	Fix use of generic inference rules.
**  	23-DEC-1996 V2.5    Madison 	Support forced commands on rules.
**  	23-MAR-1997 V2.5-1  Madison 	Set symbol MMS$STATUS to action status.
**  	07-SEP-1998 V2.5-2  Madison 	Fix endless loop on long cmd tokens.
**  	27-DEC-1998 V2.6    Madison 	Prototype cleanup.
**--
*/
#pragma module BUILD_TARGET "V2.6"
#include "mmk.h"
#include "globals.h"
#include <rmsdef.h>
#include <libclidef.h>

/*
** Forward declarations
*/
    void Build_Target(char *);
    struct DEPEND *find_dependency(struct OBJECT *, int);
    static int needs_updating(struct DEPEND *, struct RULE **,
    	    	    	    	struct OBJREF **, struct QUE *);
    static int do_build(struct OBJECT *, int);
    static void perform_update(struct DEPEND *, struct RULE *,
    	    	    	    	struct OBJREF *, struct QUE *);
    void close_subprocess(void);
    static unsigned int echo_ast();
    static unsigned int send_cmd_and_wait(SPHANDLE *, char *, int, unsigned int *);
    static void execute_command(SPHANDLE *, struct CMD *, char *);

/*
** Statics used with the subprocess and SP_MGR stuff
*/
    static SPHANDLE spctx = 0;
    static unsigned int static_status, command_complete;
    static $DESCRIPTOR(ini_dsc1, "MMK___OPEN = \"OPEN\" !'F$VERIFY(0,0)'");
    static $DESCRIPTOR(ini_dsc2, "MMK___SET  = \"SET\" !'F$VERIFY(0,0)'");
    static $DESCRIPTOR(ini_dsc3, "MMK___SET NOON");
    static $DESCRIPTOR(ini_dsc4, "MMK___OPEN/WRITE MMK___OUTPUT SYS$OUTPUT:");
#define EOM_TEXT "MMK____status="
#define EOM_LEN  14
#define EOM_CMD1 "MMK____status = F$INTEGER($STATUS) !'F$VERIFY(0,0)'"
#define EOM_CMD2 "MMK___WRITE = \"WRITE\""
#define EOM_CMD3 "MMK___WRITE MMK___OUTPUT \"MMK____status=\",MMK____status"

/*
**++
**  ROUTINE:	Build_Target
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Builds a target specified by name.  If the name is null,
**  we just build the first target in the dependency list.
**
**  RETURNS:	void (errors are signaled)
**
**  PROTOTYPE:
**
**  	Build_Target(char *name)
**
**  name:   ASCIZ_string, read only, by reference
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
void Build_Target (char *name) {

    struct DEPEND *dep;

    if (*name == '\0') {
    	dep = dependencies.flink;
    	if (dep == &dependencies) {
    	    lib$signal(MMK__NOTARGETS, 0);
    	    return;
    	}
    	make_object_name(name, dep->target);
    	do_build(dep->target, 0);
    } else {
    	struct QUE objque;
    	struct OBJECT *obj, *obj2;
    	char *cp;

    	objque.head = objque.tail = &objque;

    	Parse_Objects(name, strlen(name), &objque, 1);
    	if (objque.head == &objque) {
    	    lib$signal(MMK__NOSUCHTRG, 1, name);
    	    return;
    	}
    	queue_remove(objque.head, &obj);
    	dep = find_dependency(obj, 1);
    	if (dep == (struct DEPEND *) 0) {
    	    lib$signal(MMK__NOSUCHTRG, 1, name);
    	    return;
    	}
    	make_object_name(name, dep->target);
    	do_build(dep->target, 0);
    }
} /* Build_Target */

/*
**++
**  ROUTINE:	find_dependency
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Locates a dependency rule for a particular object.  If one
**  doesn't exist, we use the suffixes list and build rules to try
**  and fake one based on the target suffix.
**
**  RETURNS:	struct DEPEND *
**
**  PROTOTYPE:
**
**  	find_dependency(struct OBJECT *xobj, int fakeit)
**
**  xobj:   OBJECT structure, read only, by reference
**
**  IMPLICIT INPUTS:	dependencies, dep_internal
**
**  IMPLICIT OUTPUTS:	dep_internal may be updated.
**
**  COMPLETION CODES:
**  	0:	no dependency found
**  non-0:  	pointer to found dependency rule
**
**  SIDE EFFECTS:   	None.
**
**--
*/
struct DEPEND *find_dependency (struct OBJECT *xobj, int fakeit) {

    struct DEPEND *dep;
    struct SFX *s;
    struct RULE *r, *xr;
    struct CMD *cmd;
    struct OBJECT *obj, *obj2;
    struct OBJREF *or;
    char fspec[MMK_S_FILE], *cp, ftype[MMK_S_SFX], basename[MMK_S_FILE];
    int bnlen;

/*
** First search the real dependencies list
*/
    for (dep = dependencies.flink; dep != &dependencies; dep = dep->flink) {
    	if (xobj->type == dep->target->type) {
    	    if (strcmp(dep->target->name, xobj->name) == 0) {
    	    	if (xobj->type == MMK_K_OBJ_LIBMOD) {
    	    	    if (strcmp(xobj->libfile->name,
    	    	    	    	dep->target->libfile->name) == 0) {
    	    	    	return dep;
    	    	    }
    	    	} else return dep;
    	    }
    	}
    }

/*
**  If we're not supposed to fake one up, don't bother checking the
**  internal dependency list, and don't try to fake one, either.
*/

    if (!fakeit) return 0;

/*
** Now search the internal list, which we built ourselves during description
** parsing time (for library modules).
*/
    for (dep = dep_internal.flink; dep != &dep_internal; dep = dep->flink) {
    	if (xobj->type == dep->target->type) {
    	    if (strcmp(dep->target->name, xobj->name) == 0) {
    	    	if (xobj->type == MMK_K_OBJ_LIBMOD) {
    	    	    if (strcmp(xobj->libfile->name,
    	    	    	    	dep->target->libfile->name) == 0) {
    	    	    	return dep;
    	    	    }
    	    	} else return dep;
    	    }
    	}
    }

/*
** Nothing in our lists; see if we can fake one up.
*/
    extract_filetype(ftype, xobj->name);

    s = (ftype[0] == '\0') ? 0 : find_suffix(ftype);

    if (s != 0) {

    	for (s = s->flink; s != &suffixes; s = s->flink) {
    	    int check_cms, slen;
/*
**  Don't use the CMS suffixes if we're not using CMS
*/
    	    slen = strlen(s->value);
    	    check_cms = s->value[slen-1] == '~';
    	    if (check_cms && !use_cms) continue;

    	    xr = find_rule(ftype, s->value);
    	    if (xr != 0) {
    	    	r = scan_rule_list(xr, xobj->name, 0);

    	    	if (r != 0) {
    	    	    dep = mem_get_depend();
    	    	    cmd = mem_get_cmd();
    	    	    dep->cmdqptr = cmd->flink = cmd->blink = cmd;
    	    	    obj = mem_get_object();
    	    	    dep->target = obj;
    	    	    obj->type = MMK_K_OBJ_FILE;
    	    	    strcpy(obj->name, xobj->name);
    	    	    strcpy(obj->sfx, ftype);
    	    	    if ((obj2 = Find_Object(obj)) == NULL) {
    	    	    	Insert_Object(obj);
    	    	    } else {
    	    	    	mem_free_object(obj);
    	    	    	obj = obj2;
    	    	    }
    	    	    dep->target = obj;
    	    	    obj = mem_get_object();
    	    	    obj->type = check_cms ? MMK_K_OBJ_CMSFILE : MMK_K_OBJ_FILE;
    	    	    strcpy(obj->sfx, s->value);
    	    	    if (r->srcpfxlen == 0 && r->trgpfxlen == 0) {
    	    	    	bnlen = extract_name(basename, xobj->name);
    	    	    } else {
    	    	    	bnlen = extract_filename(basename, xobj->name);
    	    	    	memcpy(obj->name, r->srcpfx, r->srcpfxlen);
    	    	    }
    	    	    memcpy(obj->name+r->srcpfxlen, basename, bnlen);
    	    	    strcpy(obj->name+(r->srcpfxlen+bnlen), r->src);
    	    	    if ((obj2 = Find_Object(obj)) == NULL) {
    	    	    	Insert_Object(obj);
    	    	    } else {
    	    	    	mem_free_object(obj);
    	    	    	obj = obj2;
    	    	    }
    	    	    or = mem_get_objref();
    	    	    or->obj = obj;
    	    	    queue_insert(or, dep->sources.blink);
    	    	    queue_insert(dep, dep_internal.blink);
    	    	    return dep;
    	    	} /* if (r) */
    	    } /* if (r) */
    	} /* for */
    } /* if (s) */

    if (default_rule) {
    	dep = mem_get_depend();
    	cmd = mem_get_cmd();
    	dep->cmdqptr = cmd->flink = cmd->blink = cmd;
    	obj = mem_get_object();
    	dep->target = xobj;
    	return dep;
    }

    return (struct DEPEND *) 0;

} /* find_dependency */

/*
**++
**  ROUTINE:	needs_updating
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Determines if a target needs to be updated, and locates
**  a build rule for the target if the dependency doesn't include
**  any commands for performing the update.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	needs_updating(struct DEPEND *dep, struct RULE **rule);
**
**  dep:    DEPEND structure, read only, by refernce
**  rule:   pointer to RULE structure, write only, by reference
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	0: no update needed
**  	1: update needed
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static int needs_updating (struct DEPEND *dep, struct RULE **rule,
    	    	    	    	struct OBJREF **srcref, struct QUE *chgque) {

    struct OBJECT *tobj, *trgobj;
    struct OBJREF *obj, *obj2;
    struct RULE *r;
    struct SFX *sfx;
    TIME junktime;
    char srcnam[40], tmp[256], target_name[256];
    int doit, target_noexist, set_rdt;
    unsigned int status;

    if (do_log) {
    	make_object_name(target_name, dep->target);
    	lib$signal(MMK__CHECKUPD, 1, target_name);
    }

/*
** /FORCE causes unconditional build.  /FROM_SOURCES does too, but
** we use the have_rdt flag to indicate that a target has already been
** built.
*/
    doit = force;
    if (from_sources) {
    	if (dep->target->have_rdt) {
    	    return 0;
    	} else {
    	    doit = 1;
    	}
    }

/*
** Generic targets are always updated
*/
    if (!doit) doit = (dep->target->type == MMK_K_OBJ_GENERIC);

/*
** Special handling for library modules; the file name we use is
** the library's filename.
*/
    tobj = (dep->target->type == MMK_K_OBJ_LIBMOD ? dep->target->libfile :
    	    	dep->target);

/*
** For non-generic targets, we get the revision date/time (RDT) for
** the target.  If the RDT lookup fails, we assume we should build (probably
** because the target doesn't exist).  The get_rdt routine handles both
** files and library modules.
**
** If the file is locked, we don't try and build, to try and emulate
** some real weirdiness in MMS.
*/
    trgobj = dep->target;

    if (!doit) {
    	status = get_rdt(trgobj);
    	doit = !OK(status) && (status != RMS$_FLK);
    	target_noexist = doit;
    	if (do_log) {
    	    if (doit) {
    	    	lib$signal(MMK__TRGLKUF, 1, target_name, status);
    	    } else {
    	    	lib$signal(MMK__TRGRDT, 2, target_name, &trgobj->rdt);
    	    }
    	}
    } else target_noexist = 1;

/*
** Now we recursively descend the list of dependencies, making sure that
** all the sources we depend upon are also up-to-date (unless /FORCE was
** specified).
*/
    for (obj = dep->sources.flink; obj != &dep->sources; obj = obj->flink) {
    	char source_name[256];
    	if (!force) {
    	    int i;
    	    struct OBJREF *o;

    	    if (obj->obj->build_in_progress) {
    	    	char tmp[256];
    	    	make_object_name(tmp, obj->obj);
    	    	lib$signal(MMK__CIRCDEP, 1, tmp);
    	    	return -2;
    	    }

/*
**  For /SKIP_INTERMEDIATES, non-existent sources inherit the RDT of their
**  targets.
*/
    	    set_rdt = 0;
    	    if (skip_intermediates) {
    	    	if (!target_noexist && obj->obj->type != MMK_K_OBJ_GENERIC
    	    	    	    	    && !OK(get_rdt(obj->obj))) {
    	    	    obj->obj->rdt = trgobj->rdt;
    	    	    obj->obj->have_rdt = 3; /* flag for later removal */
    	    	    set_rdt = 1;
    	    	}
    	    }

    	    i = do_build(obj->obj, 1);
    	    if (i == -2) return i;
    	    if ((i < 0) && (set_rdt || !OK(get_rdt(obj->obj)))) {
    	    	while (queue_remove(chgque->head, &o)) mem_free_objref(o);
    	    	lib$signal(MMK__CANTUPD, 2, strlen(obj->obj->name), 
    	    	    	    	obj->obj->name);
    	    	return -1;
    	    }
    	}

/*
** If we still haven't decided to do the update yet, we compare the source's
** RDT with the target's.
**
** No matter what we still look up the RDT's so we can build the
** changed-sources queue (to define MMS$CHANGED_LIST).
*/
    	make_object_name(source_name, obj->obj);
    	status = get_rdt(obj->obj);
    	if (!OK(status)) {
    	    if (noaction || obj->obj->type == MMK_K_OBJ_GENERIC) {
    	    	doit = 1;
    	    } else if (!doit && obj->obj->libmodobj != trgobj) {
    	    	lib$signal(MMK__SRCERR, 1, source_name, status);
    	    }
    	} else {
    	    int xdoit;
    	    if (!doit && do_log) {
    	    	lib$signal(MMK__SRCRDT, 2, source_name, &obj->obj->rdt);
    	    }
    	    xdoit = target_noexist;
    	    if (!xdoit) {
    	    	if (!(obj->obj->rdt.long1 == trgobj->rdt.long1 &&
    	    	      obj->obj->rdt.long2 == trgobj->rdt.long2) &&
    	    	    OK(lib$sub_times(&obj->obj->rdt, &trgobj->rdt, &junktime))) xdoit = 1;
    	    }
    	    if (xdoit) {
    	    	for (obj2 = chgque->head; obj2 != (struct OBJREF *) chgque;
    	    	    	    	    	    obj2 = obj2->flink) {
    	    	    if (obj2->obj == obj->obj) break;
    	    	}
    	    	if (obj2 == (struct OBJREF *) chgque) {
    	    	    obj2 = mem_get_objref();
    	    	    obj2->obj = obj->obj;
    	    	    queue_insert(obj2, chgque->tail);
    	    	}
    	    	if (!doit) doit = 1;
    	    }
    	}
/*
**  If we faked the source's RDT due to /SKIP_INTERMEDIATES processing, remove
**  that setting now, so if the file is a non-intermediate in some other
**  dependency, it will get built.
*/
    	if (set_rdt && obj->obj->have_rdt == 3) obj->obj->have_rdt = 0;

    	if (doit && do_log) {
    	    lib$signal(MMK__TRGNUPD, 1, target_name);
    	}
    }

/*
** If we have to perform the update, but we haven't any commands
** to do it with, we search the build rules list, and return the first
** one we find.
*/
    if (!doit) return doit;
    *rule = (struct RULE *) 0;
    *srcref = (struct OBJREF *) 0;
    sfx = find_suffix(tobj->sfx);
    if (sfx) {
    	struct OBJREF *o;
    	for (o = dep->sources.flink; o != &dep->sources; o = o->flink) {
    	    if (o->obj->type == MMK_K_OBJ_LIBMOD) {
    	    	r = find_rule_with_prefixes(tobj, o->obj->libfile);
    	    	if (r != 0 && r->cmdque.flink == &r->cmdque) r = find_rule(tobj->sfx, o->obj->libfile->sfx);
    	    } else if (o->obj->type == MMK_K_OBJ_FILE ||
    	    	       o->obj->type == MMK_K_OBJ_CMSFILE) {
    	    	r = find_rule_with_prefixes(tobj, o->obj);
    	    	if (r != 0 && r->cmdque.flink == &r->cmdque) r = find_rule(tobj->sfx, o->obj->sfx);
    	    } else continue;
    	    if (r) {
    	    	*srcref = o;
    	    	*rule = r;
    	    	break;
    	    }
    	}
    }

    return doit;

} /* needs_updating */

/*
**++
**  ROUTINE:	do_build
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Performs a build on the specified object.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	do_build(struct OBJECT *obj, int intermediate);
**
**  obj:    OBJECT structure, read only, by reference
**  intermediate: int, read only, by value
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
static int do_build (struct OBJECT *obj, int intermediate) {

    struct DEPEND *dep;
    struct RULE *rule;
    struct QUE chgque;
    struct OBJREF *or, *srcref;
    int doit, really_doit;

    dep = find_dependency(obj, 1);
    if (dep == (struct DEPEND *) 0) return -1;
    really_doit = 0;

/*
**  Loop to handle double-colon dependencies
*/
    while (dep != 0) {

    	dep->target->build_in_progress = 1;
/*
** Even if /FORCE is specified, we call on needs_updating
** to look up any rules we might need.
*/
    	chgque.head = chgque.tail = &chgque;
    	doit = needs_updating(dep, &rule, &srcref, &chgque);
    	if ((doit > 0) || ((doit == 0) && force)) {
    	    really_doit = 1;
    	    if (check_status) {
    	    	did_an_update = 1;
    	    } else {
    	    	perform_update(dep, rule, srcref, &chgque);
    	    }
    	} else if (doit == -2) {
    	    char tmp[256];
    	    make_object_name(tmp, dep->target);
    	    lib$signal(MMK__CONNEXION, 1, tmp);
    	}
    	while (queue_remove(chgque.head, &or)) mem_free_objref(or);
    	dep->target->build_in_progress = 0;
    	if (doit == -2) return -2;
    	dep = dep->dc_flink;
    }

    return really_doit;

} /* do_build */

/*
**++
**  ROUTINE:	perform_update
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Executes the commands to perform an update on a target.
**
**  RETURNS:	void  (errors are signaled)
**
**  PROTOTYPE:
**
**  	perform_update(struct DEPEND *dep, struct RULE *rule);
**
**  dep:    DEPEND structure, read only, by reference
**  rule:   RULE structure, read only, by reference
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
static void perform_update (struct DEPEND *dep, struct RULE *xrule,
    	    	    	    struct OBJREF *srcref, struct QUE *chgque) {

    struct RULE *rule;
    struct CMD *cmd;
    char target_name[256];
    unsigned int status;
    int did_one;

/*
** If no commands and no build rule, what can we do??
*/

    rule = (xrule == 0) ? default_rule : xrule;

    make_object_name(target_name, dep->target);
    if ((dep->cmdqptr == 0 || dep->cmdqptr->flink == dep->cmdqptr) &&
    	    ((rule == 0) || (rule->cmdque.flink == &rule->cmdque))) {
/*
**  Allow generic targets to have null action lists -- just assume
**  they have been updated.  But don't set the "did_an_update" flag --
**  let the updates of the generic's sources determine that.
*/
    	if (dep->target->type == MMK_K_OBJ_GENERIC) {
    	    dep->target->have_rdt = 1;
    	    sys$gettim(&(dep->target->rdt));
    	    return;
    	}
    	lib$signal(MMK__NOACTION, 1, target_name);
    	return;
    }

    if (do_log) lib$signal(MMK__PERFUPD, 1, target_name);

    did_one = 0;

/*
** Close any libraries that were opened by the GET_RDT module
*/
    lbr_flush();

/*
** Create the subprocess if it doesn't exist, and pass it the commands
** from the .FIRST directive, if there was one.
*/
    if (spctx == 0) {
    	status = sp_open(&spctx, &ini_dsc1, echo_ast, 0);
    	if (!OK(status)) lib$signal(status);
    	status = sp_send(&spctx, &ini_dsc2);
    	if (!OK(status)) lib$signal(status);
    	status = sp_send(&spctx, &ini_dsc3);
    	if (!OK(status)) lib$signal(status);
    	status = sp_send(&spctx, &ini_dsc4);
    	if (!OK(status)) lib$signal(status);
    	for (cmd = do_first.flink; cmd != &do_first; cmd = cmd->flink) {
    	    execute_command(&spctx, cmd, target_name);
    	}
    }
/*
** Create the local symbols for this dependency (MMS$SOURCE, MMS$TARGET, etc.),
** then pass the command strings (with symbols resolved) to the subprocess.
*/
    Create_Local_Symbols(dep, srcref, chgque);
/*
**  Check to see if there are any forced setup commands from the rule
**  (or its parent)
*/
    did_one = 0;
    if (rule != 0) {
    	for (cmd = rule->cmdque.flink; cmd != &rule->cmdque; cmd = cmd->flink) {
    	    if (!(cmd->flags & CMD_M_FORCED_FIRST)) continue;
    	    did_one = 1;
    	    execute_command(&spctx, cmd, target_name);
    	}
    }

    if (!did_one && rule != 0 && rule->parent != 0) {
    	for (cmd = rule->parent->cmdque.flink; cmd != &rule->parent->cmdque; cmd = cmd->flink) {
    	    if (!(cmd->flags & CMD_M_FORCED_FIRST)) continue;
    	    execute_command(&spctx, cmd, target_name);
    	}
    }
    did_one = 0;

/*
**  Execute the dependency's commands, if any
*/
    if (dep->cmdqptr != 0) {
    	for (cmd = dep->cmdqptr->flink; cmd != dep->cmdqptr; cmd = cmd->flink) {
    	    did_one = 1;
    	    execute_command(&spctx, cmd, target_name);
    	}
    }

/*
** If we haven't executed a command yet, but there is a build rule available,
** execute those commands.
*/
    if (!did_one && rule != 0) {
    	for (cmd = rule->cmdque.flink; cmd != &rule->cmdque; cmd = cmd->flink) {
    	    if (cmd->flags & (CMD_M_FORCED_FIRST|CMD_M_FORCED_LAST)) continue;
    	    execute_command(&spctx, cmd, target_name);
    	}
    }

/*
**  Check to see if there are any forced cleanup commands from the rule
**  (or its parent)
*/
    did_one = 0;
    if (rule != 0) {
    	for (cmd = rule->cmdque.flink; cmd != &rule->cmdque; cmd = cmd->flink) {
    	    if (!(cmd->flags & CMD_M_FORCED_LAST)) continue;
    	    did_one = 1;
    	    execute_command(&spctx, cmd, target_name);
    	}
    }

    if (!did_one && rule != 0 && rule->parent != 0) {
    	for (cmd = rule->parent->cmdque.flink; cmd != &rule->parent->cmdque; cmd = cmd->flink) {
    	    if (!(cmd->flags & CMD_M_FORCED_LAST)) continue;
    	    execute_command(&spctx, cmd, target_name);
    	}
    }
/*
** Clean up after ourselves, and flag the fact that we've actually updated
** something.  Also flush the revision date/time status, since the target
** may actually have changed.
*/
    Clear_Local_Symbols();
    did_an_update = 1;

/*
**  Used to do this only if NOACTION or FROM_SOURCES was specified,
**  but it may make sense to do it the way MMS does instead.
**
**  Let's check to see if the target object was actually updated first.
*/
    if (!noaction && dep->target->type != MMK_K_OBJ_GENERIC) {
    	TIME old_rdt;
    	memcpy(&old_rdt, &dep->target->rdt, sizeof(old_rdt));
    	dep->target->have_rdt = 0;
    	get_rdt(dep->target);
    	if ((old_rdt.long1 == dep->target->rdt.long1) &&
    	    	 (old_rdt.long2 == dep->target->rdt.long2)) {
    	    lib$signal(MMK__ACTNOUPD, 1, target_name);
    	}
    }
    dep->target->have_rdt = 1;
    sys$gettim(&(dep->target->rdt));

} /* perform_update */

/*
**++
**  ROUTINE:	close_subprocess
**
**  FUNCTIONAL DESCRIPTION:
**
**  	If we have created a subprocess, this closes it up.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	close_subprocess(void)
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
void close_subprocess (void) {

    struct CMD *cmd;
    struct dsc$descriptor tmpdsc;
    unsigned int status;
    char *tmp;
    int tmplen;

/*
** If we actually created a subprocess, close it down by sending out
** the commands from the .LAST directive (if there was one), then
** use sp_close to have it logged out. 
*/

    if (spctx) {
    	lbr_flush();
    	for (cmd = do_last.flink; cmd != &do_last; cmd = cmd->flink) {
    	    Resolve_Symbols(cmd->cmd, strlen(cmd->cmd), &tmp, &tmplen, 0);
    	    if (verify && (noaction || !(cmd->flags & CMD_M_NOECHO))) {
    	    	INIT_SDESC(tmpdsc, tmplen, tmp);
    	    	put_command(&tmpdsc);
    	    }
    	    if (noaction) {
    	    	status = SS$_NORMAL;
    	    } else {
    	    	unsigned int cmdstat;
    	    	status = send_cmd_and_wait(&spctx, tmp, tmplen, &cmdstat);
    	    	if (OK(status)) status = cmdstat;
    	    }
    	    free(tmp);
    	    if (!OK(status)) break;
    	}
    	if (!noaction) {
    	    sp_close(&spctx);
    	}
    }
    return;
} /* close_subprocess */

/*
**++
**  ROUTINE:	echo_ast
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Write-attention AST routine for the subprocess's output mailbox.
**  Keeps reading the output and echoing it until it gets the magic
**  end-of-command text.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	echo_ast(void)  (called from AST level)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	SS$_NORMAL always returned.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int echo_ast (void) {

    struct dsc$descriptor rcvstr;
    $DESCRIPTOR(end_marker,EOM_TEXT);

    INIT_DYNDESC(rcvstr);
    while (OK(sp_receive(&spctx, &rcvstr, 0))) {
    	if (rcvstr.dsc$w_length > EOM_LEN &&
    	    	strncmp(rcvstr.dsc$a_pointer, EOM_TEXT, 14) == 0) {
/*
** Fetch the command's exit status value from the end-of-command-marker text.
*/
    	    lib$cvt_dtb(rcvstr.dsc$w_length-EOM_LEN,
    	    	    rcvstr.dsc$a_pointer+EOM_LEN, &static_status);
    	    str$free1_dx(&rcvstr);
    	    command_complete = 1;
    	    sys$wake(0,0);
    	    break;
    	}
    	put_output(&rcvstr);
    	str$free1_dx(&rcvstr);
    }
    return SS$_NORMAL;
} /* echo_ast */

/*
**++
**  ROUTINE:	send_cmd_and_wait
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Issues a command to the subprocess and waits until it
**  completes.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	send_cmd_and_wait(SPHANDLE *spctxp, char *cmd, int cmdlen, unsigned int *cmdstat)
**
**  spctxp: 	context, longword (unsigned), read only, by reference
**  cmd:    	ASCIZ_string, read only, by reference
**  cmdlen: 	signed_longword, longword (signed), read only, by value
**  cmdstat:	cond_value, longword (unsigned), write only, by reference
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	Any status from the command executed in the subprocess.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int send_cmd_and_wait (SPHANDLE *spctxp, char *cmd, int cmdlen, unsigned int *cmdstat) {

    struct dsc$descriptor sdsc, hdsc;
    char *cp, ch, hexbuf[32];
    int i, inquotes, last_quote;
    static $DESCRIPTOR(eom_cmd1, EOM_CMD1);
    static $DESCRIPTOR(eom_cmd2, EOM_CMD2);
    static $DESCRIPTOR(eom_cmd3, EOM_CMD3);
    static $DESCRIPTOR(hexfao, "%X!XL");
    static $DESCRIPTOR(mms$status, "MMS$STATUS");
    static unsigned int gsym = LIB$K_CLI_GLOBAL_SYM;

    static_status = SS$_NORMAL;
    command_complete = 0;
    set_ctrlt_ast(sp_show_subprocess, *spctxp);

    cp = cmd;
    while (cmdlen > 254) {
    	inquotes = 0;
    	for (i = 0; i < 254; i++) {
    	    if (cp[i] == '"') {
    	    	inquotes = !inquotes;
    	    	last_quote = i;
    	    }
    	}
    	i = inquotes ? last_quote : 254;
    	if (i == 0) {
    	    clear_ctrlt_ast();
    	    return MMK__CMDLENERR;
    	}
    	ch = *(cp+i);
    	*(cp+i) = '-';
    	INIT_SDESC(sdsc, i+1, cp);
    	sp_send(spctxp, &sdsc);
    	*(cp+i) = ch;
    	cp += i;
    	cmdlen -= i;
    }
    INIT_SDESC(sdsc, cmdlen, cp);
    sp_send(spctxp, &sdsc);
    sp_send(spctxp, &eom_cmd1);  /* end-of-command marker command #1 */
    sp_send(spctxp, &eom_cmd2);  /* end-of-command marker command #2 */
    sp_send(spctxp, &eom_cmd3);  /* end-of-command marker command #3 */

    do {
    	sys$hiber();
    } while (!command_complete);

    clear_ctrlt_ast();

/*
**  Set the symbol MMS$STATUS to match the returned status of the
**  command
*/
    INIT_SDESC(hdsc, sizeof(hexbuf), hexbuf);
    if (OK(sys$fao(&hexfao, &hdsc.dsc$w_length, &hdsc, static_status))) {
    	lib$set_symbol(&mms$status, &hdsc, &gsym);
    }

    *cmdstat = static_status;

    return SS$_NORMAL;

} /* send_cmd_and_wait */

/*
**++
**  ROUTINE:	execute_command
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handles execution of a command stored in a CMD
**  structure.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	execute_command(unsigned int *spctxP, struct CMD *cmd)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	See code.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void execute_command (SPHANDLE *spctxP, struct CMD *cmd, char *target_name) {

    int is_MMS_command, tmplen;
    unsigned int status, s, cmdstat;
    char *tmp;
    struct dsc$descriptor tmpdsc;

    is_MMS_command = Resolve_Symbols(cmd->cmd, strlen(cmd->cmd), &tmp, &tmplen, 0);
    if (verify && (noaction || !(cmd->flags & CMD_M_NOECHO))) {
    	INIT_SDESC(tmpdsc, tmplen, tmp);
    	put_command(&tmpdsc);
    }
    if (!noaction || is_MMS_command) {
    	status = send_cmd_and_wait(&spctx, tmp, tmplen, &cmdstat);
    	if (!OK(status)) {
    	    lib$signal(MMK__ERRUPD, 2, status, target_name, status, 0);
    	} else {
    	    status = cmdstat;
    	    if (!OK(status) && !(cmd->flags & CMD_M_IGNERR)) {
    	    	s = $VMS_STATUS_SEVERITY(status);
    	    	if (ignore == 0 || (ignore == 2 && s == STS$K_SEVERE) ||
    	    	    	(ignore == 1 && (s == STS$K_SEVERE || s == STS$K_ERROR)))
    	    	    lib$signal(MMK__ERRUPD, 2, status, target_name);
    	    }
    	}
    }
    free(tmp);

} /* execute_command */
