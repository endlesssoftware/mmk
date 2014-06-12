/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Miscellaneous routines for MMK.
**
**  MODULE DESCRIPTION:
**
**  	Miscellaneous utility routines.
**
**  AUTHOR: 	    M. Madison
**
**  Copyright (c) 2008, Matthew Madison.
**  Copyright (c) 2014, Endless Software Solutions.
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
**  	12-JAN-1993 V1.1-1  Madison 	Fix make_object_name.
**  	04-JUN-1993 V1.2    Madison 	Add default rule support.
**  	17-OCT-1993 V1.3    Madison 	Add strneql_case_blind.
**  	20-OCT-1993 V1.4    Madison 	Add ctrlt_ast routines.
**  	28-OCT-1993 V1.4-1  Madison 	Fix behavior of .SUFFIXES directive.
**  	09-DEC-1993 V1.4-2  Madison 	Fix extract_name to use $FILESCAN.
**  	04-APR-1994 V1.4-3  Madison 	Fix extract_name again!
**  	14-APR-1994 V1.4-4  Madison 	Fix extract_name again!
**  	11-JUL-1994 V1.4-5  Madison 	Have find_rule return 0 if none found.
**  	14-JUL-1994 V1.5    Madison 	New extract routines, scan_rule_list.
**  	11-AUG-1994 V1.5-1  Madison 	Convert suffixes to upper case.
**  	27-JUN-1995 V1.6    Madison 	Add extract_nametype.
**  	06-NOV-1995 V1.6-1  Madison 	Fix behavior of scan_rule_list.
**  	27-DEC-1998 V1.7    Madison 	General cleanup.
**  	20-JAN-2001 V1.8    Madison 	More fixes for rule application, from
**  	    	    	    	    	Chuck Lane.
**      17-DEC-2010 V1.9    Sneddon     Add cat.
**	10-FEB-2011 V1.10   Sneddon	Add itoa.
**	11-APR-2011 V1.10-1 Sneddon     Minor change to help cat cope with
**					null source strings.
**	12-APR-2011 V1.11   Sneddon	Add trim.
**	02-JUL-2012 V1.12   Sneddon	Change find_char to find first out of
**					a list of characters.
**	29-AUG-2012 V1.13   Sneddon	Improve cat.
**	09-JUN-2014 V1.14   Sneddon	Add length argument to find_suffix.
**	10-JUN-2014 V1.14-1 Sneddon	make find_suffix match case-insensitive
**	12-JUN-2014 V1.15   Sneddon	Add create_suffix.
**--
*/
#pragma module MISC "V1.15"
#include "mmk.h"
#include "globals.h"
#include <lnmdef.h>
#include <iodef.h>
#include <dvidef.h>
#include <fscndef.h>
#include <builtins.h>
#include <stdarg.h>
#include <stdio.h>

/*
** Forward declarations
*/
    void Build_Suffix_List(char *, int);
    char *itoa(int);
    char *cat(char *, ...);
    char *trim(char *);
    char *find_char(char *, char *, char *);
    void upcase(char *);
    int extract_name(char *, char *);
    int extract_prefix(char *, char *);
    int extract_filetype(char *, char *);
    int extract_filename(char *, char *);
    int extract_nametype(char *, char *);
    static int split_path(char *, char *, unsigned int);
    int prefix_match(char *, char *);
    int create_suffix(char *, int, struct SFX *);
    struct SFX *find_suffix(char *, int);
    struct RULE *find_rule(char *, char *);
    struct RULE *find_rule_with_prefixes(struct OBJECT *, struct OBJECT *);
    struct RULE *scan_rule_list(struct RULE *, char *, int);
    int make_object_name(char *, struct OBJECT *);
    int logical_present(char *);
    int get_logical(char *, char *, int);
    int strneql_case_blind(char *, char *, int);
    void set_ctrlt_ast(unsigned int (*)(void *), void *);
    void clear_ctrlt_ast(void);
    static unsigned int ctrlt_ast(void);
    unsigned int find_image_symbol(char *, char *, void *);
    static unsigned int x_find_image_symbol(struct dsc$descriptor *,
    	    	    	    struct dsc$descriptor *, void *);

/*
**  Local statics
*/
    static unsigned short sysinput_chan = 0;
    static unsigned int (*ctrlt_ast_rtn)(void *) = 0;
    static void *ctrlt_ast_arg = 0;

/*
**++
**  ROUTINE:	Build_Suffix_List
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Builds the queue of suffixes from the right-hand side
**  of a .SUFFIXES directive.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Build_Suffix_List(char *line, int linelen)
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
void Build_Suffix_List (char *line, int linelen) {

    struct SFX *sfx;
    char *lp, *lpmax, *sp;
    int i;

    if (linelen == 0) {
    	while (queue_remove(suffixes.flink, &sfx)) mem_free_sfx(sfx);
    	return;
    }

    lp = line;
    lpmax = line+linelen;
    while (1) {
    	while (lp < lpmax && isspace(*lp)) lp++;
    	if (lp >= lpmax) break;
   	sp = lp;
    	while (lp < lpmax && !isspace(*lp)) lp++;
    	/*
    	** The behaviour here is different from .SUFFIXES_AFTER and
    	** .SUFFIXES_BEFORE to retain compatibility with previous versions
    	** of MMK.  However, beware that if the suffix is already in the
    	** list, it will NOT be appended to the end as in previous versions.
   	** However, that said the functionality will not change as the list 
    	** is scanned from suffixes.flink, so duplicate entries will never
    	** be reached anyway.
    	*/
    	create_suffix(sp, lp-sp, suffixes.blink);
    }
}

/*
**++
**  ROUTINE:	itoa
**
**  FUNCTIONAL DESCRIPTION:
**
**	Converts an integer into its string representation.  Anything
**  returned by this function will need to be free()'d by the caller.
**
**  RETURNS:	pointer to char
**
**  PROTOTYPE:
**
**  	itoa(int i)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    0: memory allocation error
**  	non-0: pointer to the character
**
**  SIDE EFFECTS:   	None.
**
**--
*/
char *itoa(int i) {
    char s[12];

    sprintf(s, "%d", i);

    return strdup(s);
}

/*
**++
**  ROUTINE:	cat
**
**  FUNCTIONAL DESCRIPTION:
**
**	Concatenates strings, dynamically.  Arguments following the input
**  string (which must be a pointer of zero or to a null-terminated, malloc'd
**  string) must be a string, length pair.  If the length is -1, the string
**  is assumed to be null-terminated and the length taken from the string
**  with strlen.
**
**	There is a special case that if the last argument is odd (as int,
**  no length) it is assumed that the last string is already null-terminated
**  and the length is taken with strlen.
**
**  RETURNS:	pointer to char
**
**  PROTOTYPE:
**
**  	cat(char *in, [ char *, int , ... [ char *])
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    0: memory allocation error
**  	non-0: pointer to the character
**
**  SIDE EFFECTS:   	Uses malloc/realloc to allocate all storage, the
**			caller needs to free this themselves using free.
**
**--
*/
char *cat(char *in, ...) {

    int actualcount;
    va_list ap;
    char *str, *out, *outp;
    int i, inlen, len, outlen;

    outlen = inlen = (in == (char *)0) ? 0 : strlen(in);
    va_count(actualcount);
    va_start(ap, in);
    i = 1;
    while (i < actualcount) {
	str = va_arg(ap, char *);
	if (++i < actualcount) {
	    i++;
	    len = va_arg(ap, int);
	    outlen += (len != -1) ? len : strlen(str);
	} else {
	    outlen += strlen(str);
	}
    }
    va_end(ap);
    out = (in == 0) ? malloc(outlen+1) : realloc(in, outlen+1);
    va_start(ap, in);
    i = 1;
    outp = out + inlen;
    while (i < actualcount) {
	str = va_arg(ap, char *);
	if (++i < actualcount) {
	    i++;
	    len = va_arg(ap, int);
	    if (len == -1) len = strlen(str);
	} else {
	    len = strlen(str);
	}
	memcpy(outp, str, len);
	outp += len;
    }
    va_end(ap);
    *outp = '\0';

    return out;
}

/*
**++
**  ROUTINE:	trim
**
**  FUNCTIONAL DESCRIPTION:
**
**	Trims the leading and trailing space characters of of a
**  NUL-terminated string.
**
**  RETURNS:	pointer to char
**
**  PROTOTYPE:
**
**	trim(char *s)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    0: Illegal string address.
**  	non-0: pointer to the trimmed string.
**
**  SIDE EFFECTS:
**	This function edits the string in place.  To preserve
**  the original string, use strdup (or something similar).
**  However, beware that the pointer returned by this function
**  may not necessarily be the same as the pointer in.
**
**--
*/
char *trim(char *s) {
    char *sp, *tp;

    if (!s) return NULL;

    for (sp = s; isspace(*sp) && (*sp != '\0'); sp++);
    for (tp = sp + strlen(sp) - 1; isspace(*tp) && (tp > sp); tp--);
    *++tp = '\0';

    return sp;
}

/*
**++
**  ROUTINE:	find_char
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Locates a character in a string, given the start and end
**  address of the string.
**
**  RETURNS:	pointer to char
**
**  PROTOTYPE:
**
**  	find_char(char *base, char *end, char *str)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    0: not found
**  	non-0: pointer to the character
**
**  SIDE EFFECTS:   	None.
**
**--
*/
char *find_char (char *base, char *end, char *charset) {

    register char *cp, *csp;

    for (cp = base; cp < end; cp++) {
	for (csp = charset; *csp; csp++) {
	    if (*cp == *csp) return cp;
	}
    }

    return (char *) 0;
}

/*
**++
**  ROUTINE:	upcase
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Converts a string in-place to upper case.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	upcase(char *str)
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
void upcase (char *str) {

    register char *cp;

    for (cp = str; *cp; cp++) {
    	*cp = islower(*cp) ? toupper(*cp) : *cp;
    }

    return;
}

/*
**++
**  ROUTINE:	extract_name
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a VMS file specification, returns everything up to,
**  but not including, the file type.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	extract_name(char *dest, char *src)
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
int extract_name (char *dest, char *src) {

   return split_path(dest, src, FSCN$M_DEVICE|FSCN$M_ROOT|FSCN$M_DIRECTORY|FSCN$M_NAME);

} /* extract_name */

/*
**++
**  ROUTINE:	extract_prefix
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a VMS file specification, returns the device and directory.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	extract_prefix(char *dest, char *src)
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
int extract_prefix (char *dest, char *src) {

    return split_path(dest, src, FSCN$M_DEVICE|FSCN$M_ROOT|FSCN$M_DIRECTORY);

} /* extract_prefix */

/*
**++
**  ROUTINE:	extract_filetype
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a VMS file specification, returns just the file type.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	extract_filetype(char *dest, char *src)
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
int extract_filetype (char *dest, char *src) {

    return split_path(dest, src, FSCN$M_TYPE);

} /* extract_filetype */

/*
**++
**  ROUTINE:	extract_filename
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a VMS file specification, returns just the file name.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	extract_filename(char *dest, char *src)
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
int extract_filename (char *dest, char *src) {

    return split_path(dest, src, FSCN$M_NAME);

} /* extract_filename */

/*
**++
**  ROUTINE:	extract_nametype
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a VMS file specification, returns just the file name and type.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	extract_nametype(char *dest, char *src)
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
int extract_nametype (char *dest, char *src) {

    return split_path(dest, src, FSCN$M_NAME|FSCN$M_TYPE);

} /* extract_nametype */

/*
**++
**  ROUTINE:	split_path
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Workhorse routine for the extract_xxx routines.  Uses the
**  $FILESCAN system service to parse a file specification, and
**  copies in just the requested parts.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	split_path(char *dest, char *src, unsigned int flags)
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
static int split_path (char *dest, char *src, unsigned int flags) {

    static struct {
    	unsigned short len, code;
    	char *ptr;
    } itmlst[] = {0, FSCN$_DEVICE, 0,
                  0, FSCN$_ROOT, 0,
    	    	  0, FSCN$_DIRECTORY, 0,
    	    	  0, FSCN$_NAME, 0,
    	    	  0, FSCN$_TYPE, 0,
    	    	  0, 0, 0};
    static unsigned int part[] = {FSCN$M_DEVICE, FSCN$M_ROOT,
    	    	    	    	  FSCN$M_DIRECTORY, FSCN$M_NAME, FSCN$M_TYPE};
    unsigned int status;
    struct dsc$descriptor fdsc;
    char *cp;
    int i;

    INIT_SDESC(fdsc, strlen(src), src);
    status = sys$filescan(&fdsc, itmlst, 0);
    cp = dest;
    if (OK(status)) {
    	for (i = 0; i < sizeof(part)/sizeof(unsigned int); i++) {
    	    if ((flags & part[i]) && itmlst[i].len > 0) {
    	    	if (part[i] == FSCN$M_TYPE) {
    	    	    if (itmlst[i].ptr[itmlst[i].len] == '~') itmlst[i].len++;
    	    	}
    	    	memcpy(cp, itmlst[i].ptr, itmlst[i].len);
    	    	cp += itmlst[i].len;
    	    }
    	}
    }
    *cp = '\0';

    return (cp - dest);

} /* split_path */

/*
**++
**  ROUTINE:	prefix_match
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a "prefix" (i.e., a device+directory specification) and
**  a file specification, checks to see if the file spec has the given
**  prefix.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	prefix_match(char *pfx, char *fspec)
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
int prefix_match(char *pfx, char *fspec) {

    char tmp[256];
    int len;

    len = split_path(tmp, fspec, FSCN$M_DEVICE|FSCN$M_ROOT|FSCN$M_DIRECTORY);
    if (len != strlen(pfx)) return 0;

    return strneql_case_blind(tmp, pfx, len);

} /* prefix_match */

/*
**++
**  ROUTINE:	create_suffix
**
**  FUNCTIONAL DESCRIPTION:
**
**	Create a suffix in the suffix queue (at the specified position).
**
**  RETURNS:	longword status code
**
**  PROTOTYPE:
**
**  	create_suffix(char *str, int, struct SFX *)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    1:	suffix was inserted into the queue.
**  	    0:  suffix already exists.
**
**  SIDE EFFECTS:   	None.
**--
*/
int create_suffix (char *name, int len, struct SFX *pos) {
    struct SFX *sfx;

    if (len == -1) len = strlen(name);
    len = len > MMK_S_SFX ? MMK_S_SFX : len;

    sfx = find_suffix(name, len);
    if (sfx != 0) return 0;

    sfx = mem_get_sfx();
    memcpy(sfx->value, name, len);
    sfx->value[len] = '\0';
    upcase(sfx->value);
    queue_insert(sfx, pos);

    return 1;
}

/*
**++
**  ROUTINE:	find_suffix
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Locates a suffix in the suffix queue.
**
**  RETURNS:	pointer to a SFX structure
**
**  PROTOTYPE:
**
**  	find_suffix(char *str, int)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	non-0:	address of entry in suffixes queue.
**  	    0:  not found
**
**  SIDE EFFECTS:   	None.
**
**--
*/
struct SFX *find_suffix (char *name, int len) {

    struct SFX *sfx;

    if (len == -1) len = strlen(name);
    for (sfx = suffixes.flink; sfx != &suffixes; sfx = sfx->flink) {
    	if (strncasecmp(name, sfx->value, len) == 0) return sfx;
    }

    return (struct SFX *) 0;
}

/*
**++
**  ROUTINE:	find_rule
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given source and target suffix strings, locates the base default
**  build rule for those suffixes.
**
**  RETURNS:	pointer to struct RULE
**
**  PROTOTYPE:
**
**  	find_rule(char *target, char *source)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	non-0:	pointer to rule entry
**  	    0:  not found
**
**  SIDE EFFECTS:   	None.
**
**--
*/
struct RULE *find_rule (char *trg, char *src) {

    struct RULE *r;

    for (r = rules.flink; r != &rules; r = r->flink) {
    	if (strcmp(trg, r->trg) == 0 && strcmp(src, r->src) == 0) return r;
    }

    return 0;

} /* find_rule */

/*
**++
**  ROUTINE:	find_rule_with_prefixes
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given two objects, locates a build rule for those objects,
**  taking including scanning for prefixes.
**
**  RETURNS:	pointer to struct RULE
**
**  PROTOTYPE:
**
**  	find_rule_with_prefixes(struct OBJECT *target, struct OBJECT *source)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	non-0:	pointer to rule entry
**  	    0:  not found
**
**  SIDE EFFECTS:   	None.
**
**--
*/
struct RULE *find_rule_with_prefixes (struct OBJECT *trg, struct OBJECT *src) {

    struct RULE *xr, *r;

    for (xr = rules.flink; xr != &rules; xr = xr->flink) {
    	for (r = xr; r != 0; r = r->next) {
    	    if (strcmp(trg->sfx, r->trg) == 0 && strcmp(src->sfx, r->src) == 0) {
    	    	if (prefix_match(r->trgpfx, trg->name) &&
    	    	    prefix_match(r->srcpfx, src->name)) return r;
    	    }
    	}
    	for (r = xr; r != 0; r = r->next) {
    	    if (strcmp(trg->sfx, r->trg) == 0 && strcmp(src->sfx, r->src) == 0) {
    	    	if (r->trgpfx[0] == '\0' && r->srcpfx[0] == '\0') return r;
    	    }
    	}
    }

    return 0;

} /* find_rule_with_prefixes */

static unsigned int
object_or_file_exists (const char *fspec)
{
    struct OBJECT tobj;

    memset(&tobj, 0, sizeof(tobj));
    strcpy(tobj.name, fspec);
    tobj.type = MMK_K_OBJ_FILE;
    if (Find_Object(&tobj) != NULL)
        return SS$_NORMAL;
    return file_exists((char *)fspec, 0);
}

/*
**++
**  ROUTINE:	scan_rule_list
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a base rule from a find_rule() call, scans
**  the list of rules (with prefixes, possibly) hanging off
**  the base, looking for a rule that might be used to
**  build a target.
**
**  RETURNS:	struct RULE *
**
**  PROTOTYPE:
**
**  	scan_rule_list(struct RULE *base, char *target_name, int generalize)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    0:	    	No rule found.
**  	    non-0:  	Matching rule found.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
struct RULE *scan_rule_list (struct RULE *base, char *target_name, int generalize) {

    TIME rdt ;
    struct RULE *r, *tmpr, *r_maybe[128], *tmpr_maybe[128];
    struct SFX *s;
    char trgnam[MMK_S_FILE], tmpsfx[MMK_S_SFX], fspec[MMK_S_FILE], *cp;
    unsigned int status;
    int nofileok, pass, passmax, check_cms, trgnamlen, maybes, i;

    nofileok = (generalize & 0x10) != 0;
    generalize = generalize & 0x0F;
    maybes = 0;

/*
**  When there's just one rule for this suffix pair, we want to avoid
**  checking it twice.  Hence this rather convoluted-looking logic,
**  which prevents us from doing a prefix-based scan plus a non-prefix-based
**  scan in the 1-rule case.
**
**  This is further complicated by our recursive inference check for files
**  residing in CMS.  When we call with generalize == 1, we want to check
**  _just_ the prefixed rules.  When we call with generalize == 2, we
**  want to check _just_ the non-prefixed rules.
**
**  When the NOFILEOK flag is set (generalize & 0x10), we don't care
**  if the file exists.
**
**  Phew! Now I understand why NMAKE doesn't have generic rules that
**  work across directories.
*/
    if (base->next == 0) {
    	if (base->trgpfx[0] == '\0' && base->srcpfx[0] == '\0') {
    	    pass = 1;
    	    passmax = 2;
    	} else {
    	    pass = 0;
    	    passmax = 1;
    	}
    } else {
    	pass = generalize < 2 ? 0 : 1;
    	passmax = (generalize & 1) ? 1 : 2;
    }

/*
**  Now scan the base rule and any rules hanging off it.  We may do two
**  passes over the list -- one with prefixes being used, the other without.
*/
    for (r = base; pass < passmax; pass++, r = base) {
    	while (r) {
    	    /*
    	    **	The first pass is the prefix-based scan.  For
    	    **	this, we use just the file name part of the target.
    	    */
    	    if (pass == 0) {
    	    	if (!prefix_match(r->trgpfx, target_name)) {
    	    	    r = r->next;
    	    	    continue;
    	    	}
   	    	trgnamlen = extract_filename(trgnam, target_name);
     	    /*
    	    **	The second pass is the non-prefix-based scan.
    	    **	For this, we check to see if the source file is in the
    	    ** 	same directory as the target file.
    	    */
    	    } else {
    	    	if (r->trgpfxlen != 0 || r->srcpfxlen != 0) {
    	    	    r = r->next;
    	    	    continue;
    	    	}
    	    	trgnamlen = extract_name(trgnam, target_name);
    	    }
    	    check_cms = r->src[strlen(r->src)-1] == '~';
    	    memcpy(fspec, r->srcpfx, r->srcpfxlen);
    	    memcpy(fspec+r->srcpfxlen, trgnam, trgnamlen);
    	    strcpy(fspec+(r->srcpfxlen+trgnamlen), r->src);
    	    if (check_cms) status = cms_get_rdt(fspec, 0, &rdt);
    	    else if (nofileok) status = SS$_NORMAL;
    	    else status = object_or_file_exists(fspec);
    	    if (OK(status)) break;
/*
**  OK, so the source file doesn't exist.  If we're using
**  CMS, let's see if we can infer the existence of the
**  source we want from its presence in the CMS library.
*/
    	    if (!check_cms && use_cms) {
    	    	strcpy(tmpsfx, r->src);
    	    	strcat(tmpsfx, "~");
    	    	s = find_suffix(tmpsfx, -1);
    	    	if (s != 0) {
    	    	    tmpr = find_rule(r->src, s->value);
    	    	    if (tmpr != 0) {
    	    	    	if (scan_rule_list(tmpr, fspec, 1)) break;
/*
**  We schedule a later check on the generic rules if the rule we found
**  has both prefixed and generic ones.
*/
    	    	    	if (tmpr->next != 0) {
    	    	    	    r_maybe[maybes] = r;
    	    	    	    tmpr_maybe[maybes++] = tmpr;
    	    	    	}
    	    	    }
    	    	}
    	    }
    	    r = r->next;
    	}
    	if (r != 0) break;
    }

    if ((pass >= passmax || r == 0) && maybes > 0) {
    	for (i = 0; i < maybes; i++) {
    	    r = r_maybe[i];
    	    tmpr = tmpr_maybe[i];
    	    memcpy(fspec, r->srcpfx, r->srcpfxlen);
    	    memcpy(fspec+r->srcpfxlen, trgnam, trgnamlen);
    	    strcpy(fspec+(r->srcpfxlen+trgnamlen), r->src);
    	    if (scan_rule_list(tmpr, fspec, 2) != 0) break;
    	}
    	if (i >= maybes) r = 0;
    }

    return (pass < passmax) ? r : 0;

} /* scan_rule_list */

/*
**++
**  ROUTINE:	make_object_name
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given an object structure, formats the name in a string.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	make_object_name(char *name, struct OBJECT *object)
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
int make_object_name (char *name, struct OBJECT *obj) {

    char *cp;
    int len;

    if (obj->type == MMK_K_OBJ_LIBMOD) {
    	cp =  name;
    	len = strlen(obj->libfile->name);
    	memcpy(cp, obj->libfile->name, len);
    	cp += len;
    	*cp++ = '(';
    	len = strlen(obj->name);
    	memcpy(cp, obj->name, len);
    	cp+= len;
    	if (obj->fileobj) {
    	    *cp++ = '=';
    	    len = strlen(obj->fileobj->name);
    	    memcpy(cp, obj->fileobj->name, len);
    	    cp += len;
    	}
    	*cp++ = ')';
    	*cp = '\0';
    	len = cp - name;
    } else {
    	len = strlen(obj->name);
    	memcpy(name, obj->name, len);
    	name[len] = '\0';
    }

    return len;

} /* make_object_name */

/*
**++
**  ROUTINE:	logical_present
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Checks to see if a logical name exists in one of the
**  LNM$FILE_DEV tables.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	logical_present(char *lognam)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	1: logical present
**  	0: logical not present
**
**  SIDE EFFECTS:   	None.
**
**--
*/
int logical_present (char *lognam) {

    struct dsc$descriptor namdsc;
    $DESCRIPTOR(tabdsc, "LNM$FILE_DEV");
    unsigned int attr = LNM$M_CASE_BLIND;

    INIT_SDESC(namdsc, strlen(lognam), lognam);
    return OK(sys$trnlnm(&attr, &tabdsc, &namdsc, 0, 0));

}

/*
**++
**  ROUTINE:	get_logical
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Checks to see if a logical name exists in one of the
**  LNM$FILE_DEV tables.
**
**  RETURNS:	int
**
**  PROTOTYPE:
**
**  	get_logical(char *lognam, char *buf, int bufsize)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	1: logical present
**  	0: logical not present
**
**  SIDE EFFECTS:   	None.
**
**--
*/
int get_logical (char *lognam, char *buf, int bufsize) {

    struct dsc$descriptor namdsc;
    $DESCRIPTOR(tabdsc, "LNM$FILE_DEV");
    unsigned int attr = LNM$M_CASE_BLIND;
    ITMLST lnmlst[2];
    unsigned short retlen;
    unsigned int status;

    INIT_SDESC(namdsc, strlen(lognam), lognam);
    ITMLST_INIT(lnmlst[0], LNM$_STRING, bufsize-1, buf, &retlen);
    ITMLST_INIT(lnmlst[1], 0, 0, 0, 0);
    status = sys$trnlnm(&attr, &tabdsc, &namdsc, 0, lnmlst);
    if (OK(status)) buf[retlen] = '\0';
    return OK(status);

} /* get_logical */

/*
**++
**  ROUTINE:	strneql_case_blind
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Counted case-blind string equality function.
**
**  RETURNS:	boolean
**
**  PROTOTYPE:
**
**  	strneql_case_blind(char *str1, char *str2, int len)
**
**  str1: character string, read only, by reference (ASCIZ)
**  str2: character string, read only, by reference (ASCIZ)
**  len:  integer, read only, by value
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	    0:	strings are not equal up to the specified length (case-blind)
**  	non-0:	strings are equal up to the specified length (case-blind)
**
**  SIDE EFFECTS:   	None.
**
**--
*/
int strneql_case_blind (char *s1, char *s2, int len) {

    register unsigned char c1, c2;

    while (len > 0) {
        c1 = _toupper(*s1);
        c2 = _toupper(*s2);
        if (c1 != c2) return 0;
        if (c1 == '\0') return 1;
        s1++; s2++;
    	len--;
    }

    return 1;
}

/*
**++
**  ROUTINE:	set_ctrlt_ast
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Sets up an out-of-band AST for ctrl/T.
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
void set_ctrlt_ast (unsigned int (*routine)(void *), void *arg) {

    static $DESCRIPTOR(sysinput, "SYS$INPUT:");
    static unsigned int dvi_trm = DVI$_TRM;
    static unsigned int trm_mask[2] = {0, 1<<('T'-'@')};
    unsigned int status, is_term;

    status = sys$assign(&sysinput, &sysinput_chan, 0, 0);
    if (!OK(status)) {
    	sysinput_chan = 0;
    	return;
    }
    status = lib$getdvi(&dvi_trm, &sysinput_chan, 0, &is_term);
    if (!OK(status)) {
    	sys$dassgn(sysinput_chan);
    	sysinput_chan = 0;
    	return;
    }
    if (is_term) {
    	ctrlt_ast_rtn = routine;
    	ctrlt_ast_arg = arg;
    	sys$qiow(0, sysinput_chan, IO$_SETMODE|IO$M_OUTBAND,
    	    0, 0, 0, ctrlt_ast, trm_mask, 0, 0, 0, 0);
    } else {
    	sys$dassgn(sysinput_chan);
    	sysinput_chan = 0;
    	ctrlt_ast_rtn = 0;
    	ctrlt_ast_arg = 0;
    }

    return;

} /* set_ctrlt_ast */

/*
**++
**  ROUTINE:	clear_ctrlt_ast
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Clears the out-of-band AST for ctrl/T.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	clear_ctrlt_ast()
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
void clear_ctrlt_ast (void) {

    static unsigned int trm_mask[2] = {0, 1<<('T'-'@')};

    if (sysinput_chan == 0) return;
    sys$qiow(0, sysinput_chan, IO$_SETMODE|IO$M_OUTBAND,
    	    0, 0, 0, 0, trm_mask, 0, 0, 0, 0);
    sys$dassgn(sysinput_chan);
    sysinput_chan = 0;
    ctrlt_ast_rtn = 0;
    ctrlt_ast_arg = 0;

    return;

} /* clear_ctrlt_ast */

/*
**++
**  ROUTINE:	ctrlt_ast
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Ctrl/T AST routine.
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
static unsigned int ctrlt_ast (void) {

    return ctrlt_ast_rtn ? (*ctrlt_ast_rtn)(ctrlt_ast_arg)
    	    	    	 : SS$_NORMAL;

} /* ctrlt_ast */

/*
**++
**  ROUTINE:	find_image_symbol
**
**  FUNCTIONAL DESCRIPTION:
**
**  	C interface to LIB$FIND_IMAGE_SYMBOL.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	find_image_symbol(char *image, char *symbol, void *symval)
**
**  image:  file_spec, read only, by reference (ASCIZ string)
**  symbol: character string, read only, by reference (ASCIZ)
**  symval: unspecified, write only, by reference
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	SS$_NORMAL: 	Normal successful completion.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int find_image_symbol (char *image, char *symbol, void *symval) {

    struct dsc$descriptor idsc, sdsc;

    lib$establish(lib$sig_to_ret);
    INIT_SDESC(idsc, strlen(image), image);
    INIT_SDESC(sdsc, strlen(symbol), symbol);
    return x_find_image_symbol(&idsc, &sdsc, symval);

} /* find_image_symbol */

static unsigned int x_find_image_symbol (struct dsc$descriptor *imgnam,
    	    	    	struct dsc$descriptor *symnam, void *symval) {

    return lib$find_image_symbol(imgnam, symnam, symval);

}
