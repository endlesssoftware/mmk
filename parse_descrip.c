/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Description file parser.
**
**  MODULE DESCRIPTION:
**
**  	This module contains routine PARSE_DESCRIP.
**
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
**  	29-SEP-1992 V1.1    Madison 	Support OBJREFs.
**  	12-OCT-1992 V1.1-1  Madison 	Fix some dependency rule parsing problems.
**  	02-APR-1993 V1.2    Madison 	Support '-' cmd pfx, comments in cmds.
**  	29-APR-1993 V1.2-1  Madison 	Fix dependency target handling.
**  	04-JUN-1993 V1.3    Madison 	Support for new directives.
**  	28-OCT-1993 V1.3-1  Madison 	Fix behavior of .SUFFIXES directive,
**  	    	    	    	    	    symbol definitions.
**  	12-DEC-1993 V1.4    Madison 	Support multiple dependencies for
**  	    	    	    	    	    single object.
**  	15-DEC-1993 V1.4-1  Madison 	Fix @-, -@ in action lines.
**  	03-MAR-1994 V1.4-2  Madison 	Resolve symbols on all lines.
**  	04-APR-1994 V1.4-3  Madison 	Don't resolve symbols on _all_ lines (!)
**  	08-APR-1994 V1.4-4  Madison 	Fix space-sep lists on lhs of dep rules.
**  	28-JUN-1994 V1.4-5  Madison 	Link object module to file in target spec.
**  	12-JUL-1994 V1.5    Madison 	Fix conditionals, add :: support.
**  	14-JUL-1994 V1.6    Madison 	Add prefixing on inference rules.
**  	22-AUG-1994 V1.6-1  Madison 	Remove LIBMOD check on dependency creation.
**  	18-OCT-1994 V1.6-2  Madison 	Don't free line until after we signal error!
**  	02-DEC-1994 V1.7    Madison 	Add line numbers to syntax error msgs.
**  	28-DEC-1994 V1.8    Madison 	Allow multiple dependencies for a target
**  	    	    	    	    	  as long as only one has an action list.
**  	    	    	    	    	  Add deferred symbol references.
**  	    	    	    	    	  Add .IF directive.
**  	10-JAN-1995 V1.8-1  Madison 	"defined" means set to non-null value.
**  	11-JAN-1995 V1.8-2  Madison 	Clear current_deplist on rule or .FIRST/.LAST.
**  	21-JUN-1995 V1.8-3  Madison 	Change to Define_Symbol.
**  	22-JUN-1995 V1.8-4  Madison 	Fix for CMD_TEXT ACCVIO.
**  	09-OCT-1995 V1.8-5  Madison 	Fix erroneous free(), 0-length malloc()s.
**  	29-MAY-1996 V1.8-6  Madison 	Fix ${} handling.
**	22-DEC-1996 V1.9    Madison 	Add support for FIRST/LAST on rules.
**  	27-DEC-1998 V2.0    Madison 	Cleanup, add support for .IFNDEF.
**      30-MAR-2001 V2.0-1  Madison     Fix comma in SYM2DEP handling.
**      03-MAY-2004 V2.1    Madison     Integrate IA64 changes.
**	10-OCT-2008 V2.2    Sneddon	Added support for more MMS features
**					  as well as .CASE_SENSITIVE. This feature
**					  was part of another fork of MMK by
**					  Kris Clippeleyr.
**	01-JUL-2009 V2.3    Sneddon	Changed definition for tpa0.  Now
**					 works better with older compilers.
**	16-APR-2010 V2.3-1  Sneddon	Fix symnam to be length of MMK_S_SYMBOL.
**	07-JUL-2012 V2.4    Sneddon	Added support for '|='.
**	25-JUL-2012 V2.4-1  Sneddon	Add some comments to sym_do_actrtn.
**	21-FEB-2013 V2.5    Sneddon	Change .IF handling to call
**					 Resolve_Symbol so we catch function
**					 calls.
**	05-MAR-2013 V2.6    Sneddon	Add support for immediate evaluation
**					 assignment.
**	01-MAY-2013 V2.6-1  Sneddon	#68: Updated all mention of '!=' to
**					 be '|='.
**	08-JUN-2014 V2.7    Sneddon     #82: Add support for .SUFFIXES_*
**  	13-JUN-2014 V2.8    Sneddon	Changes to Define_Symbol args.
**--
*/
#pragma module PARSE_DESCRIP "V2.7"
#include "mmk.h"
#include "globals.h"
#include <tpadef.h>

#pragma nostandard
    globalvalue unsigned int LIB$_SYNTAXERR;
#pragma standard

/*
** TPARSE context block.  A basic TPARSE block plus some
** extras of our own.
*/

#define TPA_C_LENGTH	(TPA$C_LENGTH0+16)
#define TPA_K_COUNT 	(TPA$K_COUNT0+4)

    struct TPABLK {
    	struct tpadef	   tpa0;
    	char    	  *tpa_l_stringbase;
    	char    	  *tpa_l_upbase;
    	FILEHANDLE  	  *tpa_l_unit;
    	int 	    	  *tpa_l_maxlen;
    };

/*
** Forward declarations
*/
    void parse_descrip(char *, int, FILEHANDLE *, int *, int, char *);
    int parse_store(struct TPABLK *);
    static void make_objrefs(struct QUE *, struct QUE *);
    static void copy_objrefs(struct OBJREF *, struct QUE *);
    static void sym_do_actrtn(void *, struct dsc$descriptor *);

/*
** Parse function codes.  Must match counterparts in PARSE_TABLE.MAR.
*/
#define PRS_K_CHECK_COND     0
#define PRS_K_CMD_INIT	     1
#define PRS_K_SYM_INIT	     2
#define PRS_K_DEP_INIT	     3
#define PRS_K_DIR_SFX	     4
#define PRS_K_DIR_FIRST	     5
#define PRS_K_DIR_LAST	     6
#define PRS_K_RULE_INIT	     7
#define PRS_K_DIR_RHS	     8
#define PRS_K_RULE_NEWSFX    9
#define PRS_K_RULE_SFX	    10
#define PRS_K_CMD_NOECHO    11
#define PRS_K_CMD_TEXT	    12
#define PRS_K_SYM2DEP	    13
#define PRS_K_SYM_VALUE	    14
#define PRS_K_DEP_TRGAPP    15
#define PRS_K_DEP_RHS	    16
#define PRS_K_RULE_END      17
#define PRS_K_DIR_IFDEF     18
#define PRS_K_DIR_ELSE      19
#define PRS_K_DIR_ENDIF	    20
#define PRS_K_SYM2DEP2	    21
#define PRS_K_CMD_IGNERR    22
#define	PRS_K_DIR_SILENT    23
#define PRS_K_DIR_IGNORE    24
#define PRS_K_DIR_DEFAULT   25
#define PRS_K_DIR_INCLUDE   26
#define PRS_K_CMD_FFORCED   27
#define PRS_K_DEP_TRGAPP2   28
#define PRS_K_DEP_DC	    29
#define PRS_K_RULE_INIPFX   30
#define PRS_K_RULE_NEWPFX   31
#define PRS_K_DIR_IFLHS	    32
#define PRS_K_DIR_IFEQL	    33
#define PRS_K_DIR_IFNEQ	    34
#define PRS_K_DIR_IFRHS	    35
#define	PRS_K_CMD_LFORCED   36
#define PRS_K_CMD_SETFLAGS  37
#define PRS_K_DIR_IFNDEF    38
#define PRS_K_DIR_IFGEQ     39
#define PRS_K_DIR_IFLEQ     40
#define PRS_K_DIR_IFGTR     41
#define PRS_K_DIR_IFLSS     42
#define PRS_K_DIR_NOT       43
#define PRS_K_DIR_AND	    44
#define PRS_K_DIR_OR	    45
#define PRS_K_DIR_BUILTIN   46
#define PRS_K_DIR_CASE	    47
#define PRS_K_DIR_ELSIF	    48
#define PRS_K_CHECK_GNU     49
#define PRS_K_DIR_GNU	    50
#define PRS_K_SYM_DEFINED   51
#define PRS_K_SYM_APPEND    52
#define PRS_K_SYM_DO	    53
#define PRS_K_SYM_EVAL      54
#define PRS_K_DIR_SFX_AFTER  55
#define PRS_K_DIR_SFX_BEFORE 56
#define PRS_K_DIR_SFX_DELETE 57

/*
** .IFDEF context block.  Used for tracking when we're in and out
** of .IFDEF/.ELSE/.ENDIF constructs in description files.
*/

    static struct IF {
    	struct IF *flink, *blink;
    	int do_it, in_else, matched;
    } ifque = {&ifque,&ifque,0,0};

    static int just_did_rule = 0;

/*
** External references
*/
#if defined(__ALPHA) || defined(__ia64__)
    extern int parse_state, parse_key;
    unsigned int lib$table_parse();
#define lib$tparse lib$table_parse
#else
    globalref parse_state, parse_key;
#endif

/*
**++
**  ROUTINE:	parse_descrip
**
**  FUNCTIONAL DESCRIPTION:
**
**  RETURNS:	void (errors are signaled)
**
**  PROTOTYPE:
**
**  	parse_descrip(char *line, int linelen, FILEHANDLE *newu, int *newmaxl)
**
**  IMPLICIT INPUTS:	All the globals.
**
**  IMPLICIT OUTPUTS:	All the globals.
**
**  COMPLETION CODES:
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
void parse_descrip (char *xline, int xlinelen, FILEHANDLE *newu, int *newmaxl,
    	    	    	int current_line, char *current_file) {
    
    struct TPABLK tpablk;
    char *upline, *line;
    int i, linelen;
    unsigned int status;

/*
** Resolve symbols, leaving untouched any symbols that cannot be
** resolved at this point.
*/
    if (just_did_rule && xlinelen > 0 && isspace(*xline)) {
    	line = xline;
    	linelen = xlinelen;
    } else {
    	Resolve_Symbols(xline, xlinelen, &line, &linelen, 1);
    }

/*
** To be able to match the keywords in the parse table, we must be
** upper case.  For those items that are case sensitive, parse_store
** uses the token's offset from the beginning of the upcase string
** as the offset into the mixed-case string for extracting the token
** value.
*/
    upline = malloc(linelen+1);
    memcpy(upline,line,linelen);
    *(upline+linelen) = '\0';
    upcase(upline);

    memset(&tpablk, 0, TPA_C_LENGTH);

    tpablk.tpa0.tpa$l_count = TPA_K_COUNT;
    tpablk.tpa0.tpa$l_options = TPA$M_BLANKS;
    tpablk.tpa0.tpa$l_stringcnt = linelen;
    tpablk.tpa0.tpa$l_stringptr = (unsigned int)upline;
    tpablk.tpa_l_stringbase = line;
    tpablk.tpa_l_upbase = upline;
    tpablk.tpa_l_unit = newu;
    tpablk.tpa_l_maxlen = newmaxl;

    status = lib$tparse(&tpablk, &parse_state, &parse_key);
    free(upline);
    if (!OK(status) && status != MMK__CONDSKIP) {
	lib$signal(MMK__PARSERR, 2, linelen, line,
    	    	   MMK__ERRLOC,  3, current_line, strlen(current_file),
				    current_file,
		   status);
    }
    if (line != xline) free(line);

} /* parse_descrip */

/*
**++
**  ROUTINE:	PARSE_STORE
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Action routine for use with LIB$TPARSE.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	PARSE_STORE  flag, inplen, inp, toklen, tok, char, number,
**  	    	    	usrarg
**
** The first eight arguments are the LIB$TPARSE standard argument block.
** LIB$TABLE_PARSE just passes the address of the context block, rather
** than passing the context fields as the routine arguments.
**
** This routine is non-reentrant.
**
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**  	
**
**  SIDE EFFECTS:   	None.
**
**--
*/
int parse_store (struct TPABLK *tpa) {

    int len, i, do_it;
    char *append = 0, *cp, *cp1, *vl_cp;
    int vl_sb, vl_tp, vl_ub;
    struct SYMBOL *s;
    unsigned int status;
    struct dsc$descriptor cmd, result;
    static struct CMD *current_cmd;
    static struct SYMBOL *current_sym;
    static struct QUE *current_cmdque, trgque, srcque, refque;
    static struct RULE *current_rule, *r, *xr;
    static struct DEPEND *current_deplist = 0;
    static int current_dirtype;
    static char *sfx_ptr;
    static char *trg_str, *src_str;
    static struct dsc$descriptor iflhs, ifrhs;
    static unsigned int iftype, tmp_cmdflags;
    static unsigned int and = 0, not = 0, or = 0, elsif = 0;
    static int trg_str_size, trg_str_len;
    static int double_colon;
    struct OBJECT *obj;
    struct DEPEND *dep;
    struct IF *ifent;

/*
** We handle .IFDEF/.ELSE/.ENDIF sequences first, since we don't want
** to be interpreting stuff that's inside a dead block.
*/

    switch (tpa->tpa0.tpa$l_param) {
    	char symnam[MMK_S_SYMBOL];

    case PRS_K_CHECK_COND:
    	for (ifent = ifque.flink; ifent != &ifque; ifent = ifent->flink) {
    	    if (!ifent->do_it) return MMK__CONDSKIP;
    	}
    	return SS$_NORMAL;
    	break;

    case PRS_K_CHECK_GNU:
	if (gnu_syntax) return SS$_NORMAL;
	return 0;
	break;

    case PRS_K_DIR_IFDEF:
    case PRS_K_DIR_IFNDEF:
	if (elsif) {
	    elsif = 0;
	    ifent = ifque.flink;
	    if (ifent->matched) return SS$_NORMAL;
	} else {
	    ifent = malloc(sizeof(struct IF));
	    ifent->matched = 0;
	    queue_insert(ifent, &ifque);
	}
    	strncpy(symnam, (char *)tpa->tpa0.tpa$l_tokenptr,
		    tpa->tpa0.tpa$l_tokencnt);
    	*(symnam+tpa->tpa0.tpa$l_tokencnt) = 0;
    	s = Lookup_Symbol(symnam);
    	ifent->do_it = (s != 0) && (s->value != 0) && (s->value[0] != '\0');
    	if (tpa->tpa0.tpa$l_param == PRS_K_DIR_IFNDEF)
    	    ifent->do_it = !ifent->do_it;
    	ifent->in_else = 0;
    	return SS$_NORMAL;

    case PRS_K_DIR_NOT:
        not = 1;
        return SS$_NORMAL;

    case PRS_K_DIR_IFLHS:
	Resolve_Symbols(tpa->tpa_l_stringbase + (((char *)tpa->tpa0.tpa$l_tokenptr)-tpa->tpa_l_upbase),
			tpa->tpa0.tpa$l_tokencnt, &cp, &len, 0);
	iflhs.dsc$w_length = (unsigned short)len;
    	iflhs.dsc$b_dtype = DSC$K_DTYPE_T;
    	iflhs.dsc$b_class = DSC$K_CLASS_S;
	iflhs.dsc$a_pointer = cp;
    	return SS$_NORMAL;

    case PRS_K_DIR_IFEQL:
    case PRS_K_DIR_IFNEQ:
    case PRS_K_DIR_IFGEQ:
    case PRS_K_DIR_IFLEQ:
    case PRS_K_DIR_IFGTR:
    case PRS_K_DIR_IFLSS:
        iftype = tpa->tpa0.tpa$l_param;
        return SS$_NORMAL;

    case PRS_K_DIR_IFRHS:
	if (and || or || elsif) {
	    ifent = ifque.flink;
	} else {
	    ifent = malloc(sizeof(struct IF));
	    ifent->matched = 0;
            queue_insert(ifent, &ifque);
	}
	if (ifent->matched) {
	    elsif = 0;
	    return SS$_NORMAL;
	}
        /*
	** If the length of the rhs token is zero then this is the symbol form of
	** .IF. MMS has obsoleted .IF[N]DEF and replaced it with '.IF [.NOT] symbol'.
	** So, we do a lookup instead. If the symbol exists, then the test result
	** is positive.
        */
	if (tpa->tpa0.tpa$l_tokencnt == 0) {
            strncpy(symnam, iflhs.dsc$a_pointer, iflhs.dsc$w_length);
            *(symnam+iflhs.dsc$w_length) = 0;
            s = Lookup_Symbol(symnam);
            do_it = (s != 0) && (s->value != 0) && (s->value[0] != '\0');
        } else {
	    Resolve_Symbols(tpa->tpa_l_stringbase + (((char *)tpa->tpa0.tpa$l_tokenptr)-tpa->tpa_l_upbase),
			    tpa->tpa0.tpa$l_tokencnt, &cp, &len, 0);
            ifrhs.dsc$w_length = (unsigned short)len;
            ifrhs.dsc$a_pointer = cp;
            ifrhs.dsc$b_dtype = DSC$K_DTYPE_T;
            ifrhs.dsc$b_class = DSC$K_CLASS_S;
            i = str$case_blind_compare(&iflhs, &ifrhs);
            switch (iftype) {

            case PRS_K_DIR_IFEQL:
                do_it = (i == 0) ? 1 : 0;
                break;
            case PRS_K_DIR_IFNEQ:
                do_it = (i != 0) ? 1 : 0;
                break;
            case PRS_K_DIR_IFGEQ:
                do_it = (i >= 0) ? 1 : 0;
                break;
            case PRS_K_DIR_IFLEQ:
                do_it = (i <= 0) ? 1 : 0;
                break;
            case PRS_K_DIR_IFGTR:
                do_it = (i >  0) ? 1 : 0;
                break;
            case PRS_K_DIR_IFLSS:
                do_it = (i <  0) ? 1 : 0;
                break;
            }
	    free(ifrhs.dsc$a_pointer);
        }
	free(iflhs.dsc$a_pointer);
        do_it = (not) ? !do_it : do_it;
	do_it = (and) ? ifent->do_it && do_it : do_it;
	do_it = (or ) ? ifent->do_it || do_it : do_it;
	ifent->do_it = do_it;
        ifent->in_else = 0;
	or = not = and = elsif = 0;
        return SS$_NORMAL;

    case PRS_K_DIR_AND:
	and = 1;
	return SS$_NORMAL;

    case PRS_K_DIR_OR:
	or = 1;
	return SS$_NORMAL;

    case PRS_K_DIR_ELSE:
    	ifent = ifque.flink;
    	if (ifent == &ifque) return MMK__ELSENOIF;
    	if (ifent->in_else) return MMK__ELSENOIF;
	if (!ifent->matched) ifent->do_it = !ifent->do_it;
    	ifent->in_else = 1;
    	return SS$_NORMAL;

    case PRS_K_DIR_ELSIF:
	ifent = ifque.flink;
	if (ifent == &ifque) return MMK__ELSIFNOIF;
	if (ifent->in_else) return MMK__ELSIFAFTELSE;
	elsif = 1;
	if (ifent->do_it) {
	     ifent->matched = 1;
	     ifent->do_it = 0;
	}
	return SS$_NORMAL;

    case PRS_K_DIR_ENDIF:
    	if (!queue_remove(ifque.flink, &ifent)) return MMK__ENDIFNOIF;
    	free(ifent);
    	return SS$_NORMAL;

    default:
    	break;
    }

    switch (tpa->tpa0.tpa$l_param) {

    case PRS_K_CMD_INIT:
    	current_cmd = mem_get_cmd();
    	tmp_cmdflags = 0;
    	break;

    case PRS_K_SYM_INIT:
    	current_sym = mem_get_symbol();
    	Resolve_Symbols((char *)tpa->tpa0.tpa$l_tokenptr,
			    tpa->tpa0.tpa$l_tokencnt, &cp, &len, 0);
    	if (len >= sizeof(current_sym->name)) len = sizeof(current_sym->name)-1;
    	strncpy(current_sym->name, cp, len);
    	*(current_sym->name+len) = '\0';
    	free(cp);
    	current_sym->value = (char *) 0;
    	break;

    case PRS_K_DEP_INIT:
    	trg_str = malloc(64);
    	trg_str_len = 0;
    	trg_str_size = 64;
    	double_colon = 0;
    	break;

    case PRS_K_DIR_SFX:
    case PRS_K_DIR_SFX_AFTER:
    case PRS_K_DIR_SFX_BEFORE:
    case PRS_K_DIR_SFX_DELETE:
    case PRS_K_DIR_FIRST:
    case PRS_K_DIR_LAST:
    	current_dirtype = tpa->tpa0.tpa$l_param;
    	break;

    case PRS_K_DIR_RHS:
    	switch (current_dirtype) {

    	case PRS_K_DIR_SFX:
    	    if (tpa->tpa0.tpa$l_stringcnt > 0) {
    	    	Resolve_Symbols((tpa->tpa_l_stringbase+
    	    	    (((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
    	    	    tpa->tpa0.tpa$l_stringcnt, &cp, &len, 0);
    	    	while (len > 0 && isspace(cp[len-1])) len--;
    	    	Build_Suffix_List(cp, len);
    	    	free(cp);
    	    } else Build_Suffix_List("", 0);
    	    current_dirtype = 0;
    	    break;
        case PRS_K_DIR_SFX_AFTER:
        case PRS_K_DIR_SFX_BEFORE:
            if (tpa->tpa0.tpa$l_stringcnt > 0) {
                char *rhs, *rhsend;
                int rhslen;

                Resolve_Symbols((tpa->tpa_l_stringbase+
                    (((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
                    tpa->tpa0.tpa$l_stringcnt, &rhs, &rhslen, 0);

                cp = rhs;
                rhsend = rhs+(rhslen-1);

                /*
                ** Skip leading whitespace and then count the following
                ** non-whitespace characters (the suffix).
                */
                while ((cp <= rhsend) && isspace(*cp)) cp++;
                for (len = 0; (cp+len <= rhsend) && !isspace(cp[len]); len++)
    	    	    ;
		if (len == 0) {
    	    	    lib$signal(MMK__NOSUFFLST, 1,
    	    	    	       (current_dirtype == PRS_K_DIR_SFX_AFTER) ?
    	    	    	    	   ".SUFFIXES_AFTER" : ".SUFFIXES_BEFORE");
    	    	} else {
    	    	    struct SFX *sfx;

    	    	    sfx = find_suffix(cp, len);
    	    	    if (sfx == 0) {
    	    	    	lib$signal(MMK__NOTINSUFFLST, 2, len, cp);
    	    	    	sfx = suffixes.blink;
    	    	    } else {
    	    	    	if (current_dirtype == PRS_K_DIR_SFX_BEFORE)
    	    	    	    sfx = sfx->blink;
    	    	    }

    	    	    cp += len;
                    while (cp <= rhsend) {
                    	while ((cp <= rhsend) && isspace(*cp)) cp++;
                    	for (len = 0; (cp+len <= rhsend) && !isspace(cp[len]);
    	    	    	    	len++)
    	    	    	    ;
    	    	    	if (len > 0) {
    	    	    	    if (create_suffix(cp, len, sfx) == 0)
    	    	    	    	lib$signal(MMK__ALRINSUFFLST, 2, len, cp);
    	    	    	}
	    	    	cp += len;
    	    	    }
                }
                free(rhs);
    	    	set_mmssuffixes();
            } else {
    	    	lib$signal(MMK__NOSUFFLST, 1,
    	    	    	   (current_dirtype == PRS_K_DIR_SFX_AFTER) ?
    	    	    	       ".SUFFIXES_AFTER" : ".SUFFIXES_BEFORE");
    	    }
            current_dirtype = 0;
            break;
        case PRS_K_DIR_SFX_DELETE:
            if (tpa->tpa0.tpa$l_stringcnt > 0) {
                char *rhs, *rhsend;
                int rhslen;

                Resolve_Symbols((tpa->tpa_l_stringbase+
                    (((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
                    tpa->tpa0.tpa$l_stringcnt, &rhs, &rhslen, 0);

                cp = rhs;
                rhsend = rhs+(rhslen-1);
                while (cp <= rhsend) {
                    struct SFX *sfx;

                    /*
                    ** Skip leading whitespace and then count the following
                    ** non-whitespace characters (the suffix).
                    */
                    while ((cp <= rhsend) && isspace(*cp)) cp++;
                    for (len = 0; (cp+len <= rhsend) && !isspace(cp[len]);
    	    	    	    len++)
    	    	    	;
    	    	    if (len > 0) {
    	    	    	sfx = find_suffix(cp, len);
    	    	    	if (sfx != 0) {
    	    	    	    queue_remove(sfx, &sfx);
    	    	    	    mem_free_sfx(sfx);
    	    	    	}
    	    	    }
    	    	    cp += len;
                }
                free(rhs);
    	    	set_mmssuffixes();
            } else Build_Suffix_List("", 0);
            current_dirtype = 0;
            break;
    	case PRS_K_DIR_FIRST:
    	    current_cmdque = (struct QUE *) &do_first;
    	    current_dirtype = 0;
    	    current_deplist = 0;
    	    break;
    	case PRS_K_DIR_LAST:
    	    current_cmdque = (struct QUE *) &do_last;
    	    current_dirtype = 0;
    	    current_deplist = 0;
    	    break;
    	}
    	just_did_rule = 0;
    	break;

    case PRS_K_DIR_SILENT:
    	if (!override_silent) verify = 0;
    	just_did_rule = 0;
    	break;

    case PRS_K_DIR_IGNORE:
    	if (!override_ignore) ignore = 3;
    	just_did_rule = 0;
    	break;

    case PRS_K_DIR_CASE:
	if (!override_case) case_sensitive = 1;
	just_did_rule = 0;
	break;

    case PRS_K_DIR_BUILTIN:
	if (!override_builtins) builtins = 1;
	just_did_rule = 0;
	break;

    case PRS_K_DIR_GNU:
	if (!override_gnu_syntax) gnu_syntax = 1;
	just_did_rule = 0;
	break;

    case PRS_K_DIR_DEFAULT:
    	default_rule = mem_get_rule();
    	current_cmdque = (struct QUE *) &default_rule->cmdque;
    	current_deplist = 0;
    	just_did_rule = 0;
    	break;

    case PRS_K_DIR_INCLUDE:
    	i = tpa->tpa0.tpa$l_stringcnt;
    	cp = (char *)tpa->tpa0.tpa$l_stringptr;
    	while (i > 0 && isspace(*cp)) {
    	    cp++; i--;
    	}
    	if (i > 0) {
    	    while (i > 1 && isspace(*(cp+(i-1)))) i--;
    	}
    	if (i > 0) {
    	    char tmp[256];
    	    status = file_open(cp, tpa->tpa_l_unit, "SYS$DISK:[].MMS",
    	    	    	    tmp, tpa->tpa_l_maxlen);
    	    if (OK(status)) {
    	    	if (do_log) lib$signal(MMK__OPENINCL, 1, tmp);
    	    } else {
    	    	lib$signal(MMK__NOOPNINCL, 1, cp, status);
    	    	*(tpa->tpa_l_unit) = 0;
    	    }
    	}
    	just_did_rule = 0;
    	break;

    case PRS_K_RULE_INIT:
    case PRS_K_RULE_INIPFX:
    	current_rule = mem_get_rule();
    	current_deplist = 0;
    	sfx_ptr = current_rule->src;
    	*sfx_ptr++ = '.';
    	*sfx_ptr = '\0';
    	current_cmdque = (struct QUE *) &current_rule->cmdque;
    	if (tpa->tpa0.tpa$l_param == PRS_K_RULE_INIPFX) {
    	    current_rule->srcpfxlen = tpa->tpa0.tpa$l_tokencnt-2;
    	    if (current_rule->srcpfxlen > sizeof(current_rule->srcpfx)-1)
    	    	current_rule->srcpfxlen = sizeof(current_rule->srcpfx)-1;
    	    memcpy(current_rule->srcpfx, (char *)tpa->tpa0.tpa$l_tokenptr+1,
    	    	    	current_rule->srcpfxlen);
    	    current_rule->srcpfx[current_rule->srcpfxlen] = '\0';
    	}
    	break;

    case PRS_K_RULE_SFX:
    	*sfx_ptr++ = tpa->tpa0.tpa$b_char;
    	*sfx_ptr   = '\0';
    	break;

    case PRS_K_RULE_NEWSFX:
    	sfx_ptr = current_rule->trg;
    	*sfx_ptr++ = '.';
    	*sfx_ptr = '\0';
    	break;

    case PRS_K_RULE_NEWPFX:
    	current_rule->trgpfxlen = tpa->tpa0.tpa$l_tokencnt-2;
    	if (current_rule->trgpfxlen > sizeof(current_rule->trgpfx)-1)
    	    current_rule->trgpfxlen = sizeof(current_rule->trgpfx)-1;
    	memcpy(current_rule->trgpfx, (char *)tpa->tpa0.tpa$l_tokenptr+1,
    	    	    	current_rule->trgpfxlen);
    	current_rule->trgpfx[current_rule->trgpfxlen] = '\0';
    	break;

    case PRS_K_RULE_END:
    	if (current_rule->trg[0] == '\0' ||
    	    current_rule->src[0] == '\0') return LIB$_SYNTAXERR;
    	xr = find_rule(current_rule->trg, current_rule->src);
    	if (xr != 0) {
    	    struct RULE *lastr;
    	    lastr = 0;
    	    current_rule->parent = xr;
    	    for (r = xr; r != 0; r = r->next) {
    	    	if (r->srcpfxlen == current_rule->srcpfxlen &&
    	    	    	r->trgpfxlen == current_rule->trgpfxlen &&
    	    	    	strneql_case_blind(r->srcpfx, current_rule->srcpfx,
    	    	    	    	r->srcpfxlen) &&
    	    	    	strneql_case_blind(r->trgpfx, current_rule->trgpfx,
    	    	    	    	r->trgpfxlen)) {
    	    	    break;
    	    	}
    	    	lastr = r;
    	    }
    	    if (r == 0) {
    	    	lastr->next = current_rule;
    	    	current_rule->next = 0;
    	    } else {
    	    	current_rule->next = r->next;
    	    	if (r == xr) {
    	    	    queue_insert(current_rule, r);
    	    	    queue_remove(r, &r);
    	    	} else {
    	    	    lastr->next = current_rule;
    	    	}
    	    }
    	} else {   /* ensure that a non-prefixed rule exists ahead of any prefixed rule in the list */
    	    if (current_rule->trgpfxlen > 0 || current_rule->srcpfxlen > 0) {
    	    	struct RULE *r;
    	    	struct CMD *c, *c2;
    	    	r = mem_get_rule();
    	    	memcpy(r, current_rule, sizeof(*r));
    	    	r->cmdque.flink = r->cmdque.blink = &r->cmdque;
    	    	r->srcpfxlen = r->trgpfxlen = 0;
    	    	for (c = current_rule->cmdque.flink; c != &current_rule->cmdque; c = c->flink) {
    	    	    c2 = mem_get_cmd();
    	    	    memcpy(c2, c, sizeof(*c));
    	    	    c2->cmd = malloc(strlen(c->cmd)+1);
    	    	    strcpy(c2->cmd, c->cmd);
    	    	    queue_insert(c2, r->cmdque.blink);
    	    	}
    	    	queue_insert(r, rules.blink);
    	    	r->next = current_rule;
    	    	current_rule->parent = r;
    	    } else {
    	    	queue_insert(current_rule, rules.blink);
    	    }
    	}
    	just_did_rule = 1;
    	break;

    case PRS_K_CMD_NOECHO:
    	tmp_cmdflags |= CMD_M_NOECHO;
    	break;

    case PRS_K_CMD_IGNERR:
    	tmp_cmdflags |= CMD_M_IGNERR;
    	break;

    case PRS_K_CMD_FFORCED:
    	tmp_cmdflags |= CMD_M_FORCED_FIRST;
    	break;

    case PRS_K_CMD_LFORCED:
    	tmp_cmdflags |= CMD_M_FORCED_LAST;
    	break;

    case PRS_K_CMD_SETFLAGS:
    	current_cmd->flags = tmp_cmdflags;
    	tmp_cmdflags = 0;
    	break;

    case PRS_K_CMD_TEXT:
    	if (current_cmdque == 0) return MMK__ACTNODEPRULE;
    	current_cmd->cmd = malloc(tpa->tpa0.tpa$l_stringcnt+1);
    	strncpy(current_cmd->cmd, (tpa->tpa_l_stringbase+
    	    	(((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
    	    	tpa->tpa0.tpa$l_stringcnt);
    	*(current_cmd->cmd+tpa->tpa0.tpa$l_stringcnt) = '\0';
    	queue_insert(current_cmd, current_cmdque->tail);
    	current_cmd = (struct CMD *) 0;
/*
**  If we just did a dependency line, set up the pointers to the command
**  queue for each dependency processed, and make sure we don't have
**  multiple sets of action lines for multiple single-colon dependencies
**  for one target.
*/
    	while (current_deplist != 0) {
    	    dep = current_deplist;
    	    if (dep->cmdqptr != 0 && !dep->double_colon) {
    	    	char target[256];
    	    	i = make_object_name(target, dep->target);
    	    	lib$signal(MMK__MULACTION, 2, i, target);
    	    	return MMK__MULACTION;
    	    }
    	    dep->cmdqptr = (struct CMD *) current_cmdque;
    	    current_deplist = dep->deplist_flink;
    	}
    	break;

    case PRS_K_SYM2DEP:
    	trg_str_size = 64;
	trg_str = malloc(64);
	strcpy(trg_str, current_sym->name);
	trg_str_len = strlen(current_sym->name);
    	*(trg_str+trg_str_len) = tpa->tpa0.tpa$b_char;
    	trg_str_len++;
    	double_colon = 0;
	mem_free_symbol(current_sym);
    	current_sym = (struct SYMBOL *) 0;
	break;

    case PRS_K_SYM2DEP2:
    	trg_str_size = 64;
	trg_str = malloc(64);
	strcpy(trg_str, current_sym->name);
	trg_str_len = strlen(current_sym->name);
    	double_colon = 0;
	mem_free_symbol(current_sym);
    	current_sym = (struct SYMBOL *) 0;
	break;

    case PRS_K_SYM_DEFINED:
	if (Lookup_Symbol(current_sym->name) != (struct SYMBOL *) 0) {
    	    mem_free_symbol(current_sym);
    	    current_sym = (struct SYMBOL *) 0;
    	    just_did_rule = 0;
    	    break;
	}
    case PRS_K_SYM_APPEND:
	if (tpa->tpa0.tpa$l_param == PRS_K_SYM_APPEND) append = "";
    case PRS_K_SYM_VALUE:
    	if (tpa->tpa0.tpa$l_stringcnt == 0) {
    	    Define_Symbol(MMK_K_SYM_DESCRIP, current_sym->name, "", 0, append);
    	} else {
    	    Resolve_Symbols((tpa->tpa_l_stringbase+
    	    	(((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
    	    	tpa->tpa0.tpa$l_stringcnt, &cp, &len, 2);
/*
**  Change deferred symbol references, ${xxx}, into normal symbol
**  references, $(xxx).
*/
    	    cp1 = cp;
    	    i = len;
    	    while (i > 1) {
    	    	if (*cp1 == '$' && *(cp1+1) == '{') {
    	    	    int j;
    	    	    char *cp2;
    	    	    for (j = i-2, cp2 = cp1+2; j > 0; cp2++, j--) {
    	    	    	if (*cp2 == '}') {
    	    	    	    *(cp1+1) = '(';
    	    	    	    *cp2 = ')';
    	    	    	    i -= (cp2-cp1);
    	    	    	    cp1 = cp2;
    	    	    	    break;
    	    	    	}
    	    	    }
    	    	}
    	    	cp1++; i--;
    	    }
    	    Define_Symbol(MMK_K_SYM_DESCRIP, current_sym->name, cp, len, append);
    	    free(cp);
    	}
    	mem_free_symbol(current_sym);
    	current_sym = (struct SYMBOL *) 0;
    	just_did_rule = 0;
    	break;

    case PRS_K_SYM_DO:
	Resolve_Symbols((tpa->tpa_l_stringbase+
    	    (((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
    	    tpa->tpa0.tpa$l_stringcnt, &cp, &len, 0);

	INIT_DYNDESC(result);
	INIT_SDESC(cmd, len, cp);

	sp_once(&cmd, sym_do_actrtn, &result);

	Define_Symbol(MMK_K_SYM_DESCRIP, current_sym->name,
			result.dsc$a_pointer, result.dsc$w_length);

	str$free1_dx(&result);
	free(cp);
	mem_free_symbol(current_sym);
    	current_sym = (struct SYMBOL *) 0;
    	just_did_rule = 0;
    	break;

    case PRS_K_SYM_EVAL:
	Resolve_Symbols((tpa->tpa_l_stringbase+
    	    (((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
    	    tpa->tpa0.tpa$l_stringcnt, &cp, &len, 0);
	Define_Symbol(MMK_K_SYM_DESCRIP, current_sym->name, cp, len);
	free(cp);
    	just_did_rule = 0;
    	break;

    case PRS_K_DEP_TRGAPP:
    	if (trg_str_len == trg_str_size) {
    	    trg_str_size += 64;
    	    trg_str = realloc(trg_str, trg_str_size);
    	}
	if (case_sensitive) {
	    vl_sb = (int) tpa->tpa_l_stringbase;
	    vl_tp = (int) tpa->tpa0.tpa$l_tokenptr;
	    vl_ub = (int) tpa->tpa_l_upbase;
	    vl_cp = (char *) (vl_sb + vl_tp - vl_ub);

	    *(trg_str+trg_str_len) = *vl_cp;
	} else {
    	    *(trg_str+trg_str_len) = tpa->tpa0.tpa$b_char;
	}
    	trg_str_len++;
    	break;

    case PRS_K_DEP_TRGAPP2:
    	if (trg_str_len > trg_str_size-2) {
    	    trg_str_size += 64;
    	    trg_str = realloc(trg_str, trg_str_size);
    	}
    	*(trg_str+trg_str_len) = ',';
	if (case_sensitive) {
	    vl_sb = (int) tpa->tpa_l_stringbase;
	    vl_tp = (int) tpa->tpa0.tpa$l_tokenptr;
	    vl_ub = (int) tpa->tpa_l_upbase;
	    vl_cp = (char *) (vl_sb - vl_tp - vl_ub);

	    *(trg_str+trg_str_len) = *vl_cp;
	} else {
    	    *(trg_str+trg_str_len+1) = tpa->tpa0.tpa$b_char;
	}
    	trg_str_len += 2;
    	break;

    case PRS_K_DEP_DC:
    	double_colon = 1;
    	break;

    case PRS_K_DEP_RHS: {
    	struct OBJREF *o;
    	struct OBJECT *obj2;

    	src_str = malloc(tpa->tpa0.tpa$l_stringcnt+1);
    	strncpy(src_str, (tpa->tpa_l_stringbase+
    	    	(((char *)tpa->tpa0.tpa$l_stringptr)-tpa->tpa_l_upbase)),
    	    	tpa->tpa0.tpa$l_stringcnt);
    	*(src_str+tpa->tpa0.tpa$l_stringcnt) = '\0';
    	trgque.head = trgque.tail = &trgque;
    	srcque.head = srcque.tail = &srcque;
    	refque.head = refque.tail = &refque;
    	Resolve_Symbols(trg_str, trg_str_len, &cp, &len, 0);
    	Parse_Objects(cp, len, &trgque, 1);
    	free(cp);
    	if (*src_str != '\0') {
    	    Resolve_Symbols(src_str, strlen(src_str), &cp, &len, 0);
    	    Parse_Objects(cp, len, &srcque, 0);
    	    free(cp);
    	}
    	make_objrefs(&refque, &srcque);
    	current_cmdque = (struct QUE *) mem_get_cmd();
    	current_cmdque->head = current_cmdque->tail = current_cmdque;
    	current_deplist = 0;
    	while (queue_remove(trgque.head, &obj)) {
    	    if ((obj2 = Find_Object(obj)) == NULL) {
    	    	Insert_Object(obj);
    	    } else {
    	    	mem_free_object(obj);
    	    	obj = obj2;
    	    }
    	    dep = find_dependency(obj, 0);
    	    if (dep == 0) {
    	    	dep = mem_get_depend();
    	    	dep->target = obj;
    	    	dep->double_colon = double_colon;
    	    	queue_insert(dep, dependencies.blink);
    	    } else {
    	    	if ((double_colon && !dep->double_colon) ||
    	    	    	(!double_colon && dep->double_colon)) {
    	    	    lib$signal(MMK__SDCMIX, 1, obj->name);
    	    	    return MMK__SDCMIX;
    	    	}
    	    	if (double_colon) {
    	    	    struct DEPEND *dep2, *dep3;
    	    	    dep2 = mem_get_depend();
    	    	    dep2->target = obj;
    	    	    dep2->double_colon = 1;
    	    	    while (dep->dc_flink != 0) dep = dep->dc_flink;
    	    	    dep->dc_flink = dep2;
    	    	    dep = dep2;
    	    	}
    	    }
    	    copy_objrefs(&dep->sources, &refque);
    	    dep->deplist_flink = current_deplist;
    	    current_deplist = dep;
    	}
    	while (queue_remove(refque.head, &o)) mem_free_objref(o);
    	free(trg_str);
    	trg_str_len = trg_str_size = 0;
    	free(src_str);
    	just_did_rule = 0;
    	break;
    }

    default:
    	lib$signal(MMK__PRSTBLERR, 0);

    }
    return SS$_NORMAL;

} /* parse_store */

/*
**++
**  ROUTINE:	make_objrefs
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Duplicates a queue of objects to another queue.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	make_objrefs(struct QUE *destq, struct QUE *srcq);
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
static void make_objrefs (struct QUE *destq, struct QUE *srcq) {

    struct OBJECT *obj, *obj2;
    struct OBJREF *tmp;

    while (queue_remove(srcq->head, &obj)) {
    	obj2 = Find_Object(obj);
    	if (obj2 == NULL) {
    	    Insert_Object(obj);
    	} else {
    	    mem_free_object(obj);
    	    obj = obj2;
    	}
        for (tmp = destq->head;
                tmp != (struct OBJREF *) destq && tmp->obj != obj;
                tmp = tmp->flink);
        if (tmp == (struct OBJREF *) destq) {
    	    tmp = mem_get_objref();
    	    tmp->obj = obj;
    	    queue_insert(tmp, destq->tail);
        }
    }

} /* make_objrefs */

/*
**++
**  ROUTINE:	copy_objrefs
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Duplicates a queue of object references to another queue.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	copy_objrefs(struct QUE *destq, struct QUE *srcq);
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
static void copy_objrefs (struct OBJREF *destq, struct QUE *srcq) {

    struct OBJREF *tmp, *obj, *o;

    for (obj = srcq->head; obj != (struct OBJREF *) srcq; obj = obj->flink) {
    	for (o = destq->flink; o != destq; o = o->flink)
    	    if (o->obj == obj->obj) break;
    	if (o == destq) {
    	    tmp = mem_get_objref();
    	    tmp->obj = obj->obj;
    	    queue_insert(tmp, destq->blink);
    	}
    }

} /* copy_objrefs */

/*
**++
**  ROUTINE:	sym_do_actrtn
**
**  FUNCTIONAL DESCRIPTION:
**
**	Call back routine for sp_once to accumulate output
**  for '|='.
**
**  RETURNS:	None.
**
**  PROTOTYPE:
**
**      sym_do_actrtn(struct dsc$descriptor *accumulator,
**			  struct dsc$descriptor *in) {
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static void sym_do_actrtn(void *ctx,
			  struct dsc$descriptor *in) {

    static const struct dsc$descriptor space = SDESC(" ");
    struct dsc$descriptor *result = ctx;

    str$concat(result, &space, in);

} /* sym_do_actrtn */
