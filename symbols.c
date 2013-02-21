/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Symbol substitution routines
**
**  MODULE DESCRIPTION:
**
**  	This module contains routines that deal with the
**  MMK global and local symbol lists.
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
**  CREATION DATE:  21-AUG-1998
**
**  MODIFICATION HISTORY:
**
**  	21-AUG-1992 V1.0    Madison 	Initial coding.
**  	01-SEP-1992 V1.1    Madison 	Comments.
**  	29-SEP-1992 V1.2    Madison 	Separate out command-line symbol defs.
**  	14-OCT-1992 V1.2-1  Madison 	Support use of DCL symbols.
**  	02-JUN-1993 V1.3    Madison 	Support use of $*, $<, etc.
**  	07-JUN-1993 V1.4    Madison 	Add MMS$CHANGED_LIST.
**  	16-SEP-1993 V1.4-1  Madison 	Fix make symbol substitution list.
**  	17-OCT-1993 V1.5    Madison 	Allow $(var:subst-rule) notation.
**  	17-OCT-1993 V1.6    Madison 	Symbol setup for libfile deletion.
**  	28-OCT-1993 V1.6-1  Madison 	Fix redefinition of cmdline symbols.
**  	02-DEC-1993 V1.6-2  Madison 	Allow symbol refs inside symbol refs.
**  	12-DEC-1993 V1.7    Madison 	Support for $(MMS) macro.
**  	02-MAR-1994 V1.7-1  Madison 	Fix non-resolvable specials.
**  	14-APR-1994 V1.7-2  Madison 	Fix MMS$CHANGED_LIST creation.
**  	01-JUL-1994 V1.8    Madison 	CMS support.
**  	22-JUL-1994 V1.9    Madison 	Add MMS$TARGET_FNAME.
**  	17-AUG-1994 V1.9-1  Madison 	Fix Define_Symbol for "MMS" override.
**  	22-AUG-1994 V1.9-2  Madison 	Remove KILL_INTERMEDIATES stuff.
**  	02-DEC-1994 V2.0    Madison 	Add default CMS generation stuff.
**  	21-JUN-1995 V2.1    Madison 	Revamp for MMS parity.
**  	03-OCT-1995 V2.1-1  Madison 	Fix handling of nested symbol refs.
**  	29-MAY-1996 V2.1-2  Madison 	Add MMS$SOURCE_FNAME special symbol.
**  	23-MAR-1997 V2.1-3  Madison 	Fix $(sym:sfx1=sfx) resolution.
**  	27-DEC-1998 V2.2    Madison 	Support version macros; general cleanup.
**      29-JAN-2004 V2.2-1  Madison     Don't define MMS$CMS_LIBRARY as "CMS$LIB".
**	07-APR-2010 V2.3    Sneddon	Added append flag to Define_Symbol plus
**					temporary symbol support in preparation
**					for builtin function support.
**	02-JUL-2012 V2.4    Sneddon	Change to find_char arguments.
**	25-JUL-2012 V3.0    Sneddon	Add builtin function support.
**	28-AUG-2012 V3.0-1  Sneddon	Add ERROR, INFO, WARN, WORDLIST.
**	29-AUG-2012 V3.0-2  Sneddon	Improve WORD and WORDS range checking.
**					 Add FIRSTWORD and LASTWORD. WARNING
**					 alias for WARN.  All builtin file
**					 operations.
**	31-AUG-2012 V3.1    Sneddon	Add support for builtins that handle
**					 their own argument resolution.
**	04-SEP-2012 V3.2    Sneddon	Add OR, AND and IF.
**	07-SEP-2012 V3.3    Sneddon	Add CALL, reorganise temporary symbols.
**	27-SEP-2012 V3.3-2  Sneddon	Add FOREACH.
**	08-OCT-2012 V3.3-3  Sneddon	Add FINDSTRING, FILTER, FILTER-OUT,
**					 STRIP, COLLAPSE.
**	24-OCT-2012 V3.3-4  Sneddon	Add JOIN.
**	25-OCT-2012 V3.3-5  Sneddon	Add ADDPREFIX, ADDSUFFIX.
**	26-OCT-2012 V3.3-6  Sneddon	Add SORT
**	12-NOV-2012 V3.3-7  Sneddon	Add PATSUBST.
**	30-JAN-2013 V3.3-8  Sneddon	Add SUBST.
**	05-FEB-2013 V3.3-9  Sneddon	Final touches to builtin support.
**      20-FEB-2014 V3.3-10 Sneddon     Fix issue #25 related to FILEVERSION.
**					 Fix issue #27 related to built in
**					 functions. Fix issue #29, FINDSTRING.
**                                       Fixed IF argument mask.
**	21-FEB-2014 V3.3-11 Sneddon	Fix calls to Resolve_Symbols by
**					 builtin handlers.  Builtin handlers
**					 now receive call-specific argument
**					 stack.  Fixes issue #31.  Fix symbol
**					 types, issue #33.
**--
*/
#pragma module SYMBOLS "V3.3-11"
#include "mmk.h"
#include "globals.h"
#include <builtins.h>
#include <libvmdef.h>
#include <stdarg.h>
#include <string.h>
#include <strdef.h>
#include <ots$routines.h>
#include <str$routines.h>
#include <ctype.h>
#include <rms.h>
#define ARGMAX (sizeof(int) * 8)
/*
** Builtin function descriptor
*/
    struct FUNCTION {
	char *name;
	int vararg, maxarg, mask;
	int (*handler)(int, struct dsc$descriptor *, char **, int *);
    };

/*
** Tree node for LIB$xxx_TREE
*/
    struct LEAF {
	void *left, *right;
	int reserved;
	struct dsc$descriptor str;
    };
    const static int LEAF_S_LEAFDEF = sizeof(struct LEAF);

/*
** Forward declarations
*/

    struct SYMBOL *Lookup_Symbol(char *);
    void Define_Symbol(SYMTYPE, char *, char *, int, ...);
    int Resolve_Symbols(char *, int, char **, int *, int);
    void Clear_Local_Symbols(void);
    void Create_Local_Symbols(struct DEPEND *, struct OBJREF *, struct QUE *);
    static void apply_subst_rule(char *, char *, char **, int *);
    static void apply_full_subst_rule(char *, char *, char **, int *);
    static char *apply_builtin (char *, char *, int, char **, int *, int *,
				int);
    static int apply_addsuffix(int, struct dsc$descriptor *, char **, int *);
    static int apply_addprefix(int, struct dsc$descriptor *, char **, int *);
    static int apply_and(int, struct dsc$descriptor *, char **, int *);
    static int apply_basename(int, struct dsc$descriptor *, char **, int*);
    static int apply_call(int, struct dsc$descriptor *, char **, int *);
    static int apply_collapse(int, struct dsc$descriptor *, char **, int *);
    static int apply_dir(int, struct dsc$descriptor *, char **, int *);
    static int apply_directory(int, struct dsc$descriptor *, char **, int *);
    static int apply_error(int, struct dsc$descriptor *, char **, int *);
    static int apply_filename(int, struct dsc$descriptor *, char **, int *);
    static int apply_filetype(int, struct dsc$descriptor *, char **, int *);
    static int apply_fileversion(int, struct dsc$descriptor *, char **, int *);
    static int apply_filter(int, struct dsc$descriptor *, char **, int *);
    static int apply_filter_out(int, struct dsc$descriptor *, char **, int *);
    static int apply_findstring(int, struct dsc$descriptor *, char **, int *);
    static int apply_firstword(int, struct dsc$descriptor *, char **, int *);
    static int apply_foreach(int, struct dsc$descriptor *, char **, int *);
    static int apply_if(int, struct dsc$descriptor *, char **, int *);
    static int apply_info(int, struct dsc$descriptor *, char **, int *);
    static int apply_join(int, struct dsc$descriptor *, char **, int *);
    static int apply_lastword(int, struct dsc$descriptor *, char **, int *);
    static int apply_notdir(int, struct dsc$descriptor *, char **, int *);
    static int apply_or(int, struct dsc$descriptor *, char **, int *);
    static int apply_origin(int, struct dsc$descriptor *, char **, int *);
    static int apply_patsubst(int, struct dsc$descriptor *, char **, int *);
    static int apply_sort(int, struct dsc$descriptor *, char **, int *);
    static int apply_sort_cmp(struct dsc$descriptor *, struct LEAF *, void *);
    static int apply_sort_malloc(struct dsc$descriptor *, struct LEAF **, int);
    static int apply_sort_cat(struct LEAF *, char **);
    static int apply_strip(int, struct dsc$descriptor *, char **, int *);
    static int apply_subst(int, struct dsc$descriptor *, char **, int *);
    static int apply_warn(int, struct dsc$descriptor *, char **, int *);
    static int apply_wildcard(int, struct dsc$descriptor *, char **, int *);
    static int apply_word(int, struct dsc$descriptor *, char **, int *);
    static int apply_wordlist(int, struct dsc$descriptor *, char **, int *);
    static int apply_words(int, struct dsc$descriptor *, char **, int *);

/*
** Own storage
*/

    static struct SYMTABLE dcl_symbols;
    static int dcl_symbols_inited = 0;
    static char *WHITESPACE = " \r\n\t\v\f";
    static char SPECIALS[] = "@*<+?%&";
    static char *SPECIAL_VAR[] = {"MMS$TARGET","MMS$TARGET_NAME",
    	    	    	    	  "MMS$SOURCE","MMS$SOURCE_LIST",
    	    	    	    	  "MMS$CHANGED_LIST","MMS$LIB_ELEMENT",
    	    	    	    	  "MMS$CMS_GEN"};
    static char *non_resolvables[] = {"MMS","MMSQUALIFIERS",
    	"MMS$TARGET","MMS$TARGET_NAME","MMS$TARGET_MODULE","MMS$LIB_ELEMENT",
    	"MMS$SOURCE","MMS$SOURCE_LIST","MMS$CHANGED_LIST",
    	"MMS$SOURCE_NAME","MMS$SOURCE_LIST_SPACES","MMS$CHANGED_LIST_SPACES",
    	"MMS$CMS_LIBRARY", "MMS$CMS_ELEMENT", "MMS$CMS_GEN",
    	"MMS$TARGET_FNAME","MMS$SOURCE_FNAME"};
    static struct FUNCTION functions[] = {
	{ "ADDPREFIX",		0, 2, 0x00000000, apply_addprefix,   },
	{ "ADDSUFFIX",		0, 2, 0x00000000, apply_addsuffix,   },
	{ "AND",		1, 1, 0xFFFFFFFF, apply_and,	     },
	{ "BASENAME",		0, 1, 0x00000000, apply_basename,    },
	{ "CALL",		1, 1, 0x00000000, apply_call,	     },
	{ "COLLAPSE",		0, 1, 0x00000000, apply_collapse,    },
	{ "DIR",		0, 1, 0x00000000, apply_dir,	     },
	{ "DIRECTORY",		0, 1, 0x00000000, apply_directory,   },
	{ "ERROR",		1, 1, 0x00000000, apply_error,	     },
	{ "FILENAME",		0, 1, 0x00000000, apply_filename,    },
	{ "FILETYPE",		0, 1, 0x00000000, apply_filetype,    },
	{ "FILEVERSION",	0, 1, 0x00000000, apply_fileversion, },
	{ "FILTER",		0, 2, 0x00000000, apply_filter,      },
	{ "FILTER-OUT",		0, 2, 0x00000000, apply_filter_out,  },
	{ "FINDSTRING",		0, 2, 0x00000000, apply_findstring,  },
	{ "FIRSTWORD",		0, 1, 0x00000000, apply_firstword,   },
	{ "FOREACH",		0, 3, 0x00000004, apply_foreach,     },
	{ "JOIN",		0, 2, 0x00000000, apply_join,	     },
	{ "IF",			1, 2, 0x00000006, apply_if,	     },
	{ "INFO",		1, 1, 0x00000000, apply_info,	     },
	{ "LASTWORD",		0, 1, 0x00000000, apply_lastword,    },
	{ "NOTDIR",		0, 1, 0x00000000, apply_notdir,	     },
	{ "OR",			1, 1, 0xFFFFFFFF, apply_or,	     },
	{ "ORIGIN",		0, 1, 0x00000000, apply_origin,	     },
	{ "PATSUBST",		0, 3, 0x00000000, apply_patsubst,    },
	{ "SORT",		0, 1, 0x00000000, apply_sort,	     },
	{ "STRIP",		0, 1, 0x00000000, apply_strip,	     },
	{ "SUBST",		0, 3, 0x00000000, apply_subst,	     },
	{ "WARN",		1, 1, 0x00000000, apply_warn,	     },
	{ "WARNING",		1, 1, 0x00000000, apply_warn,	     },
	{ "WILDCARD",		0, 1, 0x00000000, apply_wildcard,    },
	{ "WORD",		0, 2, 0x00000000, apply_word,	     },
	{ "WORDLIST",		0, 3, 0x00000000, apply_wordlist,    },
	{ "WORDS",		0, 1, 0x00000000, apply_words,	     }, };

/*
**++
**  ROUTINE:	Lookup_Symbol
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Locates a symbol by name.
**
**  RETURNS:	pointer to struct SYMBOL
**
**  PROTOTYPE:
**
**  	Lookup_Symbol(char *name)
**
**  IMPLICIT INPUTS:	all symbol tables
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**  	non-0: symbol found
**  	    0: symbol not found
**
**  SIDE EFFECTS:   	None.
**
**--
*/
struct SYMBOL *Lookup_Symbol (char *name) {

    struct SYMBOL *sym;
    struct QUE    *symq;
    unsigned char *cp;
    struct dsc$descriptor namdsc, valdsc;
    unsigned int status, hash_value;
    int i;
    static struct SYMTABLE *normal_order[] = {
    	&local_symbols, &cmdline_symbols, &global_symbols,
    	&builtin_symbols, &dcl_symbols};
    struct SYMTABLE *override_order[] = {
    	&local_symbols, &cmdline_symbols,
	&dcl_symbols, &global_symbols, &builtin_symbols};

    if (!dcl_symbols_inited) {
    	for (i = 0; i < MMK_K_SYMTABLE_SIZE; i++) {
    	    INIT_QUEUE(dcl_symbols.symlist[i]);
    	}
    	dcl_symbols_inited = 1;
    }

    hash_value = 0;
    for (cp = (unsigned char *) name, i = 0; *cp != '\0' && i < 4; cp++, i++) {
    	hash_value |= *cp;
    }
    hash_value &= 0xff;

    if (temporary_symbols != 0) {
	symq = &temporary_symbols->symlist[hash_value];
    	for (sym = symq->head; sym != (struct SYMBOL *) symq; sym = sym->flink) {
    	    if (strcmp(name, sym->name) == 0) return sym;
    	}
    }

    for (i = 0; i < sizeof(normal_order)/sizeof(normal_order[0]); i++) {
    	symq = symbol_override ? &override_order[i]->symlist[hash_value]
    	    	    	       : &normal_order[i]->symlist[hash_value];
    	for (sym = symq->head; sym != (struct SYMBOL *) symq; sym = sym->flink) {
    	    if (strcmp(name, sym->name) == 0) return sym;
    	}
    	if ((symbol_override && override_order[i] == &dcl_symbols) ||
    	    	    (!symbol_override && normal_order[i] == &dcl_symbols)) {
    	    INIT_SDESC(namdsc, strlen(name), name);
    	    INIT_DYNDESC(valdsc);
    	    status = lib$get_symbol(&namdsc, &valdsc);
    	    if (OK(status)) {
    	    	sym = mem_get_symbol();
    	    	strcpy(sym->name, name);
		sym->type = MMK_K_SYM_CLI;
    	    	sym->value = malloc(valdsc.dsc$w_length+1);
    	    	memcpy(sym->value, valdsc.dsc$a_pointer, valdsc.dsc$w_length);
    	    	sym->value[valdsc.dsc$w_length] = '\0';
    	    	queue_insert(sym, dcl_symbols.symlist[hash_value].tail);
    	    	str$free1_dx(&valdsc);
    	    	return sym;
    	    }
    	}
    }

    return (struct SYMBOL *) 0;

} /* Lookup_Symbol */

/*
**++
**  ROUTINE:	Define_Symbol
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Creates or re-defines a symbol in the global symbol
**  	table.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Define_Symbol(SYMTYPE symtype, char *name, char *val, int vallen)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	global_symbols
**
**--
*/
void Define_Symbol (SYMTYPE symtype, char *name, char *val, int vallen, ...) {

    struct SYMBOL *sym;
    struct QUE    *symq;
    char upname[MMK_S_SYMBOL+1];
    unsigned char *cp;
    unsigned int hash_value;
    int actualcount, append = 0, i;
    va_list ap;

    va_count(actualcount);
    if (actualcount > 4) {
	va_start(ap, vallen);
	append = va_arg(ap, int);
	va_end(ap);
    }

    strcpy(upname, name);
    upcase(upname);

    hash_value = 0;
    for (cp = (unsigned char *) upname, i = 0; *cp != '\0' && i < 4; cp++, i++) {
        hash_value |= *cp;
    }
    hash_value &= 0xff;

    switch (symtype) {

    case MMK_K_SYM_LOCAL:
    	symq = &local_symbols.symlist[hash_value];
    	break;
    case MMK_K_SYM_DESCRIP:
    	symq = &global_symbols.symlist[hash_value];
    	break;
    case MMK_K_SYM_CMDLINE:
    	symq = &cmdline_symbols.symlist[hash_value];
    	break;
    case MMK_K_SYM_BUILTIN:
    	symq = &builtin_symbols.symlist[hash_value];
    	break;
    case MMK_K_SYM_TEMPORARY:
    	symq = &temporary_symbols->symlist[hash_value];
    	break;
    default:
    	symq = 0;   /* this will cause an ACCVIO - should never happen */
    	break;
    }

    for (sym = symq->head; sym != (struct SYMBOL *) symq; sym = sym->flink) {
    	if (strcmp(upname, sym->name) == 0) break;
    }
    if (sym == (struct SYMBOL *) symq) {
    	sym = mem_get_symbol();
    	strcpy(sym->name, upname);
    	queue_insert(sym, symq->tail);
    } else {
	if (!append) {
    	    free(sym->value);
	    sym->value = 0;
	}
    }
    sym->type = symtype;

    if (vallen < 0) vallen = strlen(val);
    if (sym->value) {
	vallen += strlen(sym->value);
	sym->value = realloc(sym->value, vallen+1);
    } else {
	sym->value = malloc(vallen+1);
	sym->value[0] = '\0';
    }
    strncat(sym->value, val, vallen);

} /* Define_Symbol */

/*
**++
**  ROUTINE:	Resolve_Symbols
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Performs symbol substitution in a string.  Iterates until no
**  further substitutions are performed.
**
**  Symbol references appear as $(symbol-name) in an MMS description line.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Resolve_Symbols(char *in, int inlen, char **out, int *outlen,
**  	    	    	    int dont_resolve_unknowns, ...)
**
**  The output string is allocated by this procedure using malloc
**  and should be freed by the caller when done.
**
**  Optional arguments are used internally by Resolve_Symbol when calling
**  itself.  They are not intended for general use.
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
int Resolve_Symbols (char *in, int inlen, char **out, int *outlen,
    	    	    	int dont_resolve_unknowns) {

    char *cp, *inend, *dp, *pp, *tmp, *val, *colp;
    struct SYMBOL *valsym;
    char var[MMK_S_SYMBOL+1];
    int len, curlen, tmplen, first, did_one = 0, free_val, i;
    int actualcount, resolved_MMS_macro;

    first = 1;
    resolved_MMS_macro = 0;

    do {

    	did_one = 0;
    	tmp = malloc(tmplen = inlen);
    	cp = in;
    	inend = in+inlen;
    	curlen = 0;

    	while (cp < inend) {

    	    colp = 0;
    	    free_val = 0;
	    val = 0;
/*
** Look for the beginning of $(...)
*/
    	    dp = find_char(cp, inend, "$");
    	    if (dp == (char *) 0) {
    	    	len = inend-cp;
    	    } else {
    	    	len = dp-cp;
    	    	if ((dp == inend-1) || ((strchr(SPECIALS, *(dp+1)) == 0)
    	    	    	    	    	    && (*(dp+1) != '('))) {
    	    	    len++;
    	    	    dp = (char *) 0;
    	    	} else  if (dp > inend-3 && *(dp+1) == '(') {
    	    	    len++;
    	    	    dp = (char *) 0;
    	    	}
    	    }

/*
** Copy everything up to the "$(" into the output string, expanding its
** size if necessary.
*/
    	    if (curlen+len > tmplen) {
    	    	tmplen = curlen+len+128;
    	    	tmp = realloc(tmp, tmplen);
    	    }
    	    memcpy(tmp+curlen, cp, len);
    	    curlen += len;

/*
** If we found "$(", locate the closing ")" and extract the symbol name,
** then look it up and stuff the resulting value into the output string.
*/
    	    if (dp == (char *) 0) {
    	    	cp += len;
    	    } else {
    	    	int is_special = 0;
    	    	int builtin_called = 0;
    	    	dp++;
    	    	if (*dp == '(') {
    	    	    dp++;
    	    	    pp = find_char(dp, inend, ")");
/*
**  Check for function calls, then embedded symbol references and lastly,
**  check for $(var:<sfx>=<sfx>).
*/
    	    	    if (pp != (char *) 0) {
			colp = find_char(dp, pp, " :");
			if (colp != 0 && *colp == ' ') {
			    int i;

			    pp = 0;
			    len = min(colp-dp, MMK_S_SYMBOL);
			    strncpy(var, dp, len);
			    var[len] = '\0';
			    upcase(var);
			    builtin_called = 1;
			    ++colp;
			    cp = apply_builtin(var, colp,
					       inend-colp, &val, &len,
					       &resolved_MMS_macro,
					       dont_resolve_unknowns);
			    did_one = (dont_resolve_unknowns == 0);
			    free_val = (val != (char *)0);
			} else {
    	    	    	    colp = find_char(dp, pp, "$");
    	    	    	    if (colp != 0) if (colp < pp-1 && (*(colp+1) == '('
    	    	    	        	|| strchr(SPECIALS, *(colp+1)) != 0)) {
    	    	    	    	tmp[curlen++] = '$';
    	    	    	    	cp += len + 1;
    	    	    	    	dp = colp = pp = 0;
    	    	    	    	continue;
    	    	    	    }
    	    	    	    colp = find_char(dp, pp, ":");
    	    	    	    if (colp != 0) {
    	    	    	    	char *cp;
    	    	    	    	for (cp = colp; isspace(*(cp-1)); cp--);
    	    	    	    	len = min(cp-dp, MMK_S_SYMBOL);
    	    	    	    } else {
    	    	    	    	len = min(pp-dp,MMK_S_SYMBOL);
    	    	    	    }
    	    	    	    strncpy(var, dp, len);
    	    	    	    var[len] = '\0';
    	    	    	    upcase(var);
			}
    	    	    }
    	    	} else {
    	    	    pp = strchr(SPECIALS, *dp);
    	    	    if (pp != 0) {
    	    	    	strcpy(var, SPECIAL_VAR[pp-SPECIALS]);
    	    	    	pp = dp;
    	    	    	is_special = 1;
    	    	    }
    	    	}
		if (!builtin_called) {
    	    	    if (pp != 0) {
    	    	    	valsym = Lookup_Symbol(var);
    	    	    	if (valsym != (struct SYMBOL *) 0) {
    	    	    	    did_one = 1;
    	    	    	    if (strcmp(valsym->name, "MMS") == 0) resolved_MMS_macro = 1;
    	    	    	    if (colp != 0) {
    	    	    	    	apply_subst_rule(valsym->value, colp+1, &val, &len);
    	    	    	    	free_val = 1;
    	    	    	    } else {
    	    	    	    	val = valsym->value;
    	    	    	    	len = strlen(val);
    	    	    	    }
    	    	    	    cp = pp + 1;
    	    	    	} else {
/*
** If dont_resolve_unknowns is set and we didn't find the symbol in the
** symbol table, just copy the symbol reference into the output string.
** Otherwise, the symbol reference resolves to a null string.
**
** When dont_resolve_unknowns is set to 2, we resolve unknowns unless they
** are on the special "non_resolvables" list.
*/
    	    	    	    if (dont_resolve_unknowns == 1) {
    	    	    	    	len = 1;
    	    	    	    	val = is_special ? dp-1 : dp-2;
    	    	    	    	cp = is_special ? dp : dp-1;
    	    	    	    } else {
				if (dont_resolve_unknowns == 2) {
    	    	    	    	    for (i = 0; i < sizeof(non_resolvables)/
    	    	    	    	    	    	sizeof(non_resolvables[0]); i++) {
    	    	    	    	        if (strcmp(var, non_resolvables[i]) == 0)
					    break;
    	    	    	    	    }
    	    	    	    	    if (i < sizeof(non_resolvables)/
    	    	    	    	    	    sizeof(non_resolvables[0])) {
    	    	    	    	        len = 1;
    	    	    	    	    	val = is_special ? dp-1 : dp-2;
    	    	    	    	    	cp = is_special ? dp : dp-1;
    	    	    	    	    } else {
    	    	    	    	    	len = 0;
    	    	    	    	    	val = dp;
    	    	    	    	    	cp = pp + 1;
				    }
    	    	    	    	} else {
    	    	    	    	    len = 0;
    	    	    	    	    val = dp;
    	    	    	    	    cp = pp + 1;
				}
    	    	    	    }
    	    	    	}
    	    	    } else {
    	    	    	len = 1;
    	    	    	val = dp-2;
    	    	    	cp = dp-1;
    	    	    }
		}
    	    	if (curlen+len > tmplen) {
    	    	    tmplen = curlen+len+128;
    	    	    tmp = realloc(tmp, tmplen);
    	    	}
    	    	memcpy(tmp+curlen, val, len);
    	    	curlen += len;
    	    	if (free_val) free(val);
    	    }
    	}

    	if (!first) free(in);
    	first = 0;

    	if (did_one) {
    	    in = tmp;
    	    inlen = curlen;
    	}
    } while (did_one);

    *out = tmp;
    *outlen = curlen;
    return resolved_MMS_macro;

} /* Resolve_Symbols */

/*
**++
**  ROUTINE:	Clear_Local_Symbols
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Deletes all of the symbols in the local symbol table.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Clear_Local_Symbols()
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	local_symbols
**
**--
*/
void Clear_Local_Symbols (void) {

    struct SYMBOL *sym;
    int i;

    for (i = 0; i < MMK_K_SYMTABLE_SIZE; i++) {
    	while (queue_remove(local_symbols.symlist[i].head, &sym)) {
    	    mem_free_symbol(sym);
    	}
    }

} /* Clear_Local_Symbols */

/*
**++
**  ROUTINE:	Create_Local_Symbols
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Given a dependency rule, the local symbols for commands
**  invoked under that dependency are defined.
**
**  RETURNS:	void
**
**  PROTOTYPE:
**
**  	Create_Local_Symbols(struct DEPEND *dependency_rule)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:	None.
**
**  SIDE EFFECTS:   	local_symbols
**
**--
*/
void Create_Local_Symbols (struct DEPEND *dep, struct OBJREF *srcref, struct QUE *chgque) {

    struct OBJREF *obj;
    struct RULE *r;
    char nam[256], *src, *cp;
    int srclen, srcsize, i;

    extract_name(nam, dep->target->name);
    Define_Symbol(MMK_K_SYM_LOCAL, "MMS$TARGET_NAME", nam, -1);
    extract_filename(nam, dep->target->name);
    Define_Symbol(MMK_K_SYM_LOCAL, "MMS$TARGET_FNAME", nam, -1);

    if (dep->target->type == MMK_K_OBJ_LIBMOD) {
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$TARGET", dep->target->libfile->name, -1);
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$TARGET_MODULE", dep->target->name, -1);
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$LIB_ELEMENT", dep->target->name, -1);
    } else {
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$TARGET", dep->target->name, -1);
    }
/*
**  If we didn't get an explicit source reference, try and find the "best"
**  one available.  That's the one where there's a rule for building the
**  target from one of the sources, or if there is no such rule, just the
**  first source in the list.
*/

    if (srcref == 0) {
    	for (obj = dep->sources.flink; obj != &dep->sources; obj = obj->flink) {
    	    if (obj->obj->type == MMK_K_OBJ_LIBMOD) {
    	    	r = find_rule(dep->target->sfx, obj->obj->libfile->sfx);
    	    } else {
    	    	r = find_rule(dep->target->sfx, obj->obj->sfx);
    	    }
    	    if (r) break;
    	}
    	if (obj == &dep->sources) obj = dep->sources.flink;
    } else obj = srcref;

/*
**  We still have an out if there were no sources in the list (in which case
**  we just don't define the sources macros, except for MMS$CMS_GEN).
*/
    	    
    if (obj != &dep->sources) {
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$SOURCE", obj->obj->name, -1);
    	if (obj->obj->type == MMK_K_OBJ_CMSFILE) {
    	    char lib[256];
    	    unsigned int status;
    	    status = cms_parse_name(obj->obj->name, lib, sizeof(lib), 0,
    	    	    	    	nam, sizeof(nam), 0, 0);
    	    if (OK(status)) {
    	    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_ELEMENT", nam, -1);
    	    	if (lib[0] != '\0') Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_LIBRARY", lib, -1);
                else if (strlen(cms$lib) != 7 || !strneql_case_blind(cms$lib, "CMS$LIB", 7))
                    Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_LIBRARY", cms$lib, -1);
    	    }
    	    if (obj->obj->cms_gen[0] != '\0') {
    	    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", obj->obj->cms_gen, -1);
    	    } else if (cms_default_generation[0] != '\0') {
    	    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", cms_default_generation, -1);
    	    } else Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", "1+", 2);
    	} else if (cms_default_generation[0] != '\0') {
    	    Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", cms_default_generation, -1);
    	} else Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", "1+", 2);
    	extract_name(nam, obj->obj->name);
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$SOURCE_NAME", nam, -1);
    	extract_filename(nam, obj->obj->name);
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$SOURCE_FNAME", nam, -1);
    	srcsize = strlen(obj->obj->name)+256;
    	src = malloc(srcsize);
    	strcpy(src, obj->obj->name);
    	srclen = strlen(obj->obj->name);
    	for (obj = obj->flink; obj != &dep->sources; obj = obj->flink) {
    	    i = make_object_name(nam, obj->obj);
    	    if (srclen + i > srcsize-2) {
    	    	srcsize += i + 256;
    	    	src = realloc(src, srcsize);
    	    }
    	    *(src+(srclen++)) = ',';
    	    strcpy(src+srclen, nam);
    	    srclen += strlen(nam);
    	}
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$SOURCE_LIST", src, srclen);
    	for (cp = src; *cp; cp++) if (*cp == ',') *cp = ' ';
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$SOURCE_LIST_SPACES", src, srclen);
    	free(src);
    } else if (cms_default_generation[0] != '\0') { 
    	Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", cms_default_generation, -1);
    } else Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CMS_GEN", "1+", 2);

/*
**  Now build the changed-source list
*/

    srcsize = 256;
    src = malloc(srcsize);
    srclen = 0;
    for (obj = chgque->head; obj != (struct OBJREF *) chgque; obj = obj->flink) {
    	i = make_object_name(nam, obj->obj);
    	if (srclen + i > srcsize-2) {
    	    srcsize += i + 256;
    	    src = realloc(src, srcsize);
    	}
    	if (srclen > 0) *(src+(srclen++)) = ',';
    	strcpy(src+srclen, nam);
    	srclen += strlen(nam);
    }
    src[srclen] = '\0';
    Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CHANGED_LIST", src, srclen);
    for (cp = src; *cp; cp++) if (*cp == ',') *cp = ' ';
    Define_Symbol(MMK_K_SYM_LOCAL, "MMS$CHANGED_LIST_SPACES", src, srclen);
    free(src);


} /* Create_Local_Symbols */

/*
**++
**  ROUTINE:	apply_subst_rule
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
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
static void apply_subst_rule (char *orig, char *rule, char **xval, int *xlen) {

    char lhs[256], rhs[256];
    char *cp, *val, *cp1, *dp;
    int i, len, curlen, add_rhs, lhslen, rhslen;
    
    val = malloc(len = strlen(orig)+256);

    if (*rule == ':') {
    	apply_full_subst_rule(orig, rule+1, xval, xlen);
    	return;
    }
    while (isspace(*rule)) rule++;
    cp1 = lhs;
    for (cp = rule; *cp != '\0' && *cp != '=' && *cp != ')' && !isspace(*cp); cp++) *cp1++ = *cp;
    while (isspace(*cp)) cp++;
    *cp1 = '\0';
    lhslen = cp1-lhs;
    cp1 = rhs;
    if (*cp == '=') {
    	for (cp++; isspace(*cp); cp++);
    	while (*cp != '\0' && *cp != ')' && !isspace(*cp)) *cp1++ = *cp++;
    }
    *cp1 = '\0';
    rhslen = cp1-rhs;

    curlen = 0;
    cp = orig;
    while (*cp != '\0') {
    	add_rhs = 0;
    	for (cp1 = cp; *cp1 != '\0' && *cp1 != ',' && !isspace(*cp1); cp1++);
    	for (dp = cp1-1; dp >= cp && *dp != '.' && *dp != ']' && *dp != '>' && *dp != ':'; dp--);
    	if (dp < cp || *dp != '.') dp = 0;
    	if (dp == 0) {
    	    if (lhslen == 0) {
    	    	dp = cp1;
    	    	add_rhs = 1;
    	    }
    	} else if (cp1-dp == lhslen) {
    	    add_rhs = strneql_case_blind(dp, lhs, lhslen);
    	}
    	i = add_rhs ? rhslen + (dp-cp) : cp1-cp;
    	if ((curlen + i) >= len) {
    	    len = curlen + i + len + 256;
    	    val = realloc(val, len);
    	}
    	if (add_rhs) {
    	    memcpy(val+curlen, cp, dp-cp);
    	    curlen += (dp-cp);
    	    memcpy(val+curlen, rhs, rhslen);
    	    curlen += rhslen;
    	} else {
    	    memcpy(val+curlen, cp, cp1-cp);
    	    curlen += (cp1-cp);
    	}
    	while (isspace(*cp1)) cp1++;
    	if (*cp1 == '\0') break;
    	if (*cp1 == ',') {
    	    *(val+curlen) = ',';
    	    curlen++;
    	    for (cp = cp1+1; isspace(*cp); cp++);
    	} else {
    	    *(val+curlen) = ' ';
    	    curlen++;
    	    cp = cp1;
    	}
    }

    *xval = val;
    *xlen = curlen;

} /* apply_subst_rule */

/*
**++
**  ROUTINE:	apply_full_subst_rule
**
**  FUNCTIONAL DESCRIPTION:
**
**  	tbs
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
static void apply_full_subst_rule (char *orig, char *rule, char **xval, int *xlen) {

    char lhs[256], rhs[256];
    char *cp, *val, *cp1;
    int i, len, curlen, lhslen, rhslen, remain;
    
    val = malloc(len = strlen(orig)+256);

    cp1 = lhs;
    for (cp = rule; *cp != '\0' && *cp != '=' && *cp != ')'; cp++) {
        if (*cp == '\\') { cp++; if (*cp == '\0') break; }
        *cp1++ = *cp;
    }
    *cp1 = '\0';
    lhslen = cp1-lhs;
    cp1 = rhs;
    if (*cp == '=') {
        for (cp++; *cp != '\0' && *cp != ')'; cp++) {
    	    if (*cp == '\\') { cp++; if (*cp == '\0') break; }
    	    *cp1++ = *cp;
        }
    }

    *cp1 = '\0';
    rhslen = cp1-rhs;

    curlen = 0;
    remain = strlen(orig);
    cp = orig;

    while (remain > 0) {

    	if (remain >= lhslen) {
    	    if (strneql_case_blind(cp, lhs, lhslen)) {
    	    	cp1 = rhs;
    	    	i = rhslen;
    	    	cp += lhslen;
    	    	remain -= lhslen;
    	    } else {
    	    	cp1 = cp;
    	    	i = 1;
    	    	cp++;
    	    	remain--;
    	    }

    	} else {
    	    cp1 = cp;
    	    i = remain;
    	    cp += remain;
    	    remain = 0;
    	}

    	if ((curlen+i) >= len) {
    	    len = curlen + i + len + 256;
    	    val = realloc(val, len);
    	}

    	if (i == 1) {
    	    val[curlen++] = *cp1;
    	} else {
    	    memcpy(val+curlen, cp1, i);
    	    curlen += i;
    	}
    }

    *xval = val;
    *xlen = curlen;

} /* apply_full_subst_rule */

/*
**++
**  ROUTINE:	apply_builtin
**
**  FUNCTIONAL DESCRIPTION:
**
**  	This function parses and calls all builtin functions.
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
static char *apply_builtin (char *name, char *in, int inlen,
			    char **out, int *outlen, int *resolved_MMS_macro,
			    int dont_resolve_unknowns) {

    struct dsc$descriptor argv[ARGMAX];
    struct FUNCTION *f = 0;
    char *ap, *cp, *inend;
    int argc, depth, i, status;

    *out = 0;
    *outlen = 0;

    for (i = 0; i < sizeof(functions)/sizeof(functions[0]); i++) {
	if (strcasecmp(functions[i].name, name) == 0) {
	    f = &functions[i];
	    break;
	}
    }
    if (f == 0) lib$signal(MMK__UNRFUN, 1, name);

    argc = 0;
    depth = 0;
    inend = in + inlen;
    ap = cp = in;
    while (cp < inend) {
	cp = find_char(cp, inend, ",)$");
	if (cp == (char *)0) {
	    lib$signal(MMK__UTLBADMAC, 1, f->name);
	    f = 0;
	    cp = inend;
	    break;
	} else {
	    if (*cp == '$') {
		if (cp <= inend-1 && strchr(SPECIALS, *(cp+1))) {
		    cp++; 
		} else if (cp <= inend-3 && *(cp+1) == '(') {
		    depth++;
		    cp += 2;
		} else {
		    cp++;
		}
	    } else if (*cp == ',' || *cp == ')') {
		if (depth == 0) {
		    if (argc >= ARGMAX) {
			lib$signal(MMK__TOOMANYARGS);
		    } else {
		    	argv[argc].dsc$a_pointer = ap;
			argv[argc].dsc$b_class = DSC$K_CLASS_S;
			argv[argc].dsc$b_dtype = DSC$K_DTYPE_T;
		    	argv[argc].dsc$w_length = cp-ap;
			argc++;
			if (*cp++ == ')')
			    break;
			ap = cp;
		    }
		} else {
		    if (*cp++ == ')')
			depth--;
		}
	    }
	}
    }

    if (f != (struct FUNCTION *)0) {
    	if (dont_resolve_unknowns != 0) {
	    *out = cat(*out, "$(", 2, f->name, -1, " ", 1, in, cp-in);
	    *outlen = strlen(*out);
    	} else {
	    /*
	    ** Check the function to make sure we have enough arguments, etc.
	    ** and call the builtin handler.
	    */
	    if (argc < f->maxarg) {
	    	lib$signal(MMK__INSFARGS, 1, f->name);
	    } else if (!f->vararg && argc > f->maxarg) {
	    	lib$signal(MMK__TOOMANYARGS, 1, f->name);
   	    } else {
	    	for (i = 0; i < argc; i++) {
		    if (f->mask & (1 << i)) {
		    	char *tmp;
		    	tmp = cat(0, argv[i].dsc$a_pointer,
				  argv[i].dsc$w_length);
		    	argv[i].dsc$a_pointer = tmp;
		    } else {
	            	char *rptr;
	            	int rlen;
	            	*resolved_MMS_macro |= Resolve_Symbols(
						argv[i].dsc$a_pointer,
						argv[i].dsc$w_length, &rptr,
						&rlen, dont_resolve_unknowns);
		    	argv[i].dsc$a_pointer = rptr;
		    	argv[i].dsc$w_length = (unsigned short)rlen;
	    	    }
	    	}

	    	*resolved_MMS_macro |= (f->handler)(argc, argv, out, outlen);

	        for (i = 0; i < argc; i++) {
	    	    if (argv[i].dsc$a_pointer != 0) {
		    	free(argv[i].dsc$a_pointer);
		    	argv[i].dsc$a_pointer = 0;
		   	argv[i].dsc$w_length = 0;
		    }
	    	}
	    }
	}
    }

    return cp;
} /* apply_builtin */

/*
**++
**  ROUTINE:	apply_addprefix
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in ADDPREFIX function.
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
static int apply_addprefix (int argc, struct dsc$descriptor *argv,
			    char **out, int *outlen) {

    char *cp, *ep, *in, *inend, *prefix = 0;
    int prefixlen = 0;

    *out = 0;
    *outlen = 0;

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while ((cp < inend)
    	&& (strchr(WHITESPACE, *cp) != (char *) 0))
    	cp++;
    if (cp < inend) {
    	prefix = cp;
	prefixlen = inend - prefix;
    }

    in = cp = argv[1].dsc$a_pointer;
    inend = in + argv[1].dsc$w_length;
    while (cp < inend) {
    	while ((cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    cp++;
	if (cp < inend) {
    	    ep = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
    	    *out = cat(*out, prefix, prefixlen, ep, cp-ep, " ");
	}
    }
    if (*out != 0) *outlen = strlen(*out) - 1;

    return 0;
} /* apply_addprefix */

/*
**++
**  ROUTINE:	apply_addsuffix
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in ADDSUFFIX function.
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
static int apply_addsuffix (int argc, struct dsc$descriptor *argv,
			    char **out, int *outlen) {

    char *cp, *ep, *in, *inend, *suffix = 0;
    int suffixlen = 0;

    *out = 0;
    *outlen = 0;

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while ((cp < inend)
    	&& (strchr(WHITESPACE, *cp) != (char *) 0))
    	cp++;
    if (cp < inend) {
    	suffix = cp;
	suffixlen = inend - suffix;
    }

    in = cp = argv[1].dsc$a_pointer;
    inend = in + argv[1].dsc$w_length;
    while (cp < inend) {
    	while ((cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    cp++;
	if (cp < inend) {
    	    ep = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
    	    *out = cat(*out, ep, cp-ep, suffix, suffixlen, " ");
	}
    }
    if (*out != 0) *outlen = strlen(*out) - 1;

    return 0;
} /* apply_addsuffix */

/*
**++
**  ROUTINE:	apply_and
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in AND function.
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
static int apply_and (int argc, struct dsc$descriptor *argv,
		      char **out, int *outlen) {

    int i, len, resolved_MMS_macro;
    char *ep, *sp, *tmp, *tmpend;

    *out = 0;
    *outlen = 0;

    for (i = 0; i < argc; i++) {
	resolved_MMS_macro = Resolve_Symbols(argv[i].dsc$a_pointer,
					     argv[i].dsc$w_length, &tmp,
					     &len, 0);
	if (len != 0) {
	    sp = tmp;
	    tmpend = tmp + len;
	    while ((sp < tmpend)
		&& (strchr(WHITESPACE, *sp) != (char *) 0))
		sp++;
	    if (sp < tmpend) {
		ep = tmpend;
		tmpend = sp;
		while ((--ep >= tmpend)
		    && (strchr(WHITESPACE, *ep) != (char *) 0))
		    ;
		len = (ep - sp) + 1;
		if (len > 0) {
		    *out = cat(*out, sp, len, " ", 1);
		    free(tmp);
		} else {
		    break;
		}
	    } else {
		break;
	    }
	} else {
	    break;
	}
    }

    if (i == argc) {
	if (*out != (char *) 0) *outlen = strlen(*out) - 1;
    } else {
	free(*out);
	*out = 0;
    }

    return resolved_MMS_macro;
} /* apply_and */

/*
**++
**  ROUTINE:	apply_basename
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in BASENAME  function.
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
static int apply_basename (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
	    	*out = cat(*out, nam.nam$l_dev,
			   nam.nam$b_dev + nam.nam$b_dir + nam.nam$b_name,
			   " ", 1);
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_basename */

/*
**++
**  ROUTINE:	apply_call
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in CALL  function.
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
static int apply_call (int argc, struct dsc$descriptor *argv,
		       char **out, int *outlen) {

    struct SYMBOL *sym;
    struct SYMTABLE *symq;
    char *var;
    int i, resolved_MMS_macro = 0;

    *out = 0;
    *outlen = 0;

    var = cat(0, argv[0].dsc$a_pointer, argv[0].dsc$w_length);
    sym = Lookup_Symbol(var);
    if (sym != (struct SYMBOL *) 0) {
	symq = mem_get_symtable();
	symq->next = temporary_symbols;
	temporary_symbols = symq;

	for (i = 0; i < argc; i++) {
	    char *name;
	    name = itoa(i);
	    Define_Symbol(MMK_K_SYM_TEMPORARY, name, argv[i].dsc$a_pointer,
			    argv[i].dsc$w_length);
	    free(name);
	}

	resolved_MMS_macro = Resolve_Symbols(sym->value, strlen(sym->value),
					     out, outlen, 0);

	symq = temporary_symbols;
	temporary_symbols = symq->next;
	mem_free_symtable(symq);
    }

    free(var);

    return resolved_MMS_macro;
} /* apply_call */

/*
**++
**  ROUTINE:	apply_collapse
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in COLLAPSE function.
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
static int apply_collapse (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    char *cp, *ep, *in, *inend;

    *out = 0;
    *outlen = 0;

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
    	    ep = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    *out = cat (*out, ep, cp-ep);
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }
    if (*out != (char *) 0) *outlen = strlen(*out);

    return 0;
} /* apply_collapse */

/*
**++
**  ROUTINE:	apply_dir
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in DIR  function.
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
static int apply_dir (int argc, struct dsc$descriptor *argv,
		      char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
	    	*out = cat(*out, nam.nam$l_dev, nam.nam$b_dev + nam.nam$b_dir,
			   " ", 1);
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_dir */

/*
**++
**  ROUTINE:	apply_directory
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in DIRECTORY  function.
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
static int apply_directory (int argc, struct dsc$descriptor *argv,
			    char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
	    	*out = cat(*out, nam.nam$l_dir, nam.nam$b_dir, " ", 1);
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_directory */

/*
**++
**  ROUTINE:	apply_error
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in ERROR function.
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
static int apply_error (int argc, struct dsc$descriptor *argv,
			char **out, int *outlen) {

    lib$stop(MMK__ERROR, 1, &argv[0]);

    return 0;
} /* apply_error */

/*
**++
**  ROUTINE:	apply_filename
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FILENAME  function.
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
static int apply_filename (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
	    	*out = cat(*out, nam.nam$l_name, nam.nam$b_name, " ", 1);
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_filename */

/*
**++
**  ROUTINE:	apply_filetype
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FILETYPE function.
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
static int apply_filetype (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
	    	*out = cat(*out, nam.nam$l_type, nam.nam$b_type, " ", 1);
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_filetype */

/*
**++
**  ROUTINE:	apply_fileversion
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FILEVERSION function.
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
static int apply_fileversion (int argc, struct dsc$descriptor *argv,
			      char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS], rsa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$l_rsa = rsa;
    nam.nam$b_rss = sizeof(rsa);
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
		if (OK(sys$search(&fab))) {
	    	    *out = cat(*out, nam.nam$l_ver, nam.nam$b_ver, " ", 1);
		} else {
		    *out = cat(*out, ";", 1, " ", 1);
		}
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    if (*out != 0) *outlen = strlen(*out) - 1;

    return 0;
} /* apply_fileversion */

/*
**++
**  ROUTINE:	apply_filter
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FILTER function.
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
static int apply_filter (int argc, struct dsc$descriptor *argv,
			 char **out, int *outlen) {

    struct PATDEF {
	struct PATDEF *flink, *blink;
	struct dsc$descriptor str;
    } patque, *pattern;
    struct dsc$descriptor text;
    char *cp, *in, *inend;

    *out = 0;
    *outlen = 0;
    INIT_QUEUE(patque);
    INIT_SDESC(text, 0, 0);

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
	    pattern = malloc(sizeof(struct PATDEF));
	    memset(pattern, 0, sizeof(struct PATDEF));
	    queue_insert(pattern, &patque);
	    INIT_SDESC(pattern->str, 0, cp);
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    pattern->str.dsc$w_length = cp - pattern->str.dsc$a_pointer;
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }

    in = cp = argv[1].dsc$a_pointer;
    inend = in + argv[1].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
    	    text.dsc$a_pointer = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    text.dsc$w_length = cp - text.dsc$a_pointer;

	    for (pattern = patque.flink; pattern != &patque;
			pattern = pattern->flink) {
		if (str$match_wild(&text, &pattern->str) == STR$_MATCH) {
		    *out = cat(*out, text.dsc$a_pointer, text.dsc$w_length);
		    break;
		}
	    }
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }
    if (*out != (char *) 0) *outlen = strlen(*out) - 1;

    while (queue_remove(patque.flink, &pattern)) {
	free(pattern);
    }

    return 0;
} /* apply_filter */

/*
**++
**  ROUTINE:	apply_filter_out
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FILTER-OUT function.
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
static int apply_filter_out (int argc, struct dsc$descriptor *argv,
			     char **out, int *outlen) {

    struct PATDEF {
	struct PATDEF *flink, *blink;
	struct dsc$descriptor str;
    } patque, *pattern;
    struct dsc$descriptor text;
    char *cp, *in, *inend;

    *out = 0;
    *outlen = 0;
    INIT_QUEUE(patque);
    INIT_SDESC(text, 0, 0);

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
	    pattern = malloc(sizeof(struct PATDEF));
	    memset(pattern, 0, sizeof(struct PATDEF));
	    queue_insert(pattern, &patque);
	    INIT_SDESC(pattern->str, 0, cp);
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    pattern->str.dsc$w_length = cp - pattern->str.dsc$a_pointer;
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }

    in = cp = argv[1].dsc$a_pointer;
    inend = in + argv[1].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
    	    text.dsc$a_pointer = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    text.dsc$w_length = cp - text.dsc$a_pointer;

	    for (pattern = patque.flink; pattern != &patque;
			pattern = pattern->flink) {
		if (str$match_wild(&text, &pattern->str) == STR$_NOMATCH) {
		    *out = cat(*out, text.dsc$a_pointer, text.dsc$w_length);
		    break;
		}
	    }
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }
    if (*out != (char *) 0) *outlen = strlen(*out) - 1;

    while (queue_remove(patque.flink, &pattern)) {
	free(pattern);
    }

    return 0;
} /* apply_filter_out */

/*
**++
**  ROUTINE:	apply_findstring
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FINDSTRING function.
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
static int apply_findstring (int argc, struct dsc$descriptor *argv,
			     char **out, int *outlen) {

    struct dsc$descriptor substr;
    char *cp, *in, *inend;

    *out = 0;
    *outlen = 0;

    INIT_SDESC(substr, 0, 0);
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
    	    substr.dsc$a_pointer = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    substr.dsc$w_length = cp - substr.dsc$a_pointer;
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }

    if ((substr.dsc$w_length > 0) && (str$position(&argv[1], &substr) != 0)) {
	*out = cat(0, substr.dsc$a_pointer, substr.dsc$w_length);
	*outlen = substr.dsc$w_length;
    }

    return 0;
} /* apply_findstring */

/*
**++
**  ROUTINE:	apply_firstword
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FIRSTWORD function.
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
static int apply_firstword (int argc, struct dsc$descriptor *argv,
			    char **out, int *outlen) {

    char *cp, *in, *inend, *sp;

    *out = 0;
    *outlen = 0;

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while ((cp < inend)
	&& (strchr(WHITESPACE, *cp++) != (char *) 0))
	;
    if (cp < inend) {
	sp = cp-1;
	while ((cp < inend)
	    && (strchr(WHITESPACE, *cp++) == (char *) 0))
	    ;
	*outlen = cp-sp;
	*out = cat(0, sp, *outlen);
    }

    return 0;
} /* apply_firstword */

/*
**++
**  ROUTINE:	apply_foreach
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in FOREACH function.
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
static int apply_foreach (int argc, struct dsc$descriptor *argv,
			  char **out, int *outlen) {

    struct SYMTABLE *symq;
    int resolved_MMS_macro = 0, tmplen;
    char *cp, *ep, *in, *inend, *tmp, *var;

    *out = 0;
    *outlen = 0;

    symq = mem_get_symtable();
    symq->next = temporary_symbols;
    temporary_symbols = symq;

    var = cat(0, argv[0].dsc$a_pointer, argv[0].dsc$w_length);
    in = cp = argv[1].dsc$a_pointer;
    inend = in + argv[1].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
    	    ep = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    Define_Symbol(MMK_K_SYM_TEMPORARY, var, ep, cp-ep);
	    resolved_MMS_macro |= Resolve_Symbols(argv[2].dsc$a_pointer,
						  argv[2].dsc$w_length, &tmp,
						  &tmplen, 0);
	    *out = cat (*out, tmp, tmplen, " ");
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }
    if (*out != (char *) 0) *outlen = strlen(*out) - 1;

    symq = temporary_symbols;
    temporary_symbols = symq->next;
    mem_free_symtable(symq);
    free(var);

    return resolved_MMS_macro;
} /* apply_foreach */

/*
**++
**  ROUTINE:	apply_if
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in IF function.
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
static int apply_if (int argc, struct dsc$descriptor *argv,
		     char **out, int *outlen) {

    int i, len, resolved_MMS_macro;
    char *ep, *in, *inend, *sp;

    *out = 0;
    *outlen = 0;

    sp = in = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while ((sp < inend)
	&& (strchr(WHITESPACE, *sp) != (char *) 0))
	sp++;

    ep = inend;
    inend = sp;
    while ((--ep >= inend)
	&& (strchr(WHITESPACE, *ep) != (char *) 0))
	;

    if ((ep - sp) + 1 > 0) {
	resolved_MMS_macro = Resolve_Symbols(argv[1].dsc$a_pointer,
					argv[1].dsc$w_length, out, outlen, 0);
    } else if (argc > 2) {
	resolved_MMS_macro = Resolve_Symbols(argv[2].dsc$a_pointer,
					argv[2].dsc$w_length, out, outlen, 0);
    }

    return resolved_MMS_macro;
} /* apply_if */

/*
**++
**  ROUTINE:	apply_info
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in INFO function.
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
static int apply_info (int argc, struct dsc$descriptor *argv,
		       char **out, int *outlen) {

    int i;

    *out = 0;
    *outlen = 0;

    for (i = 0; i < argc; i++)
	lib$signal(MMK__INFO, 1, &argv[i]);

    return 0;
} /* apply_info */

/*
**++
**  ROUTINE:	apply_join
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in JOIN function.
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
static int apply_join (int argc, struct dsc$descriptor *argv,
		       char **out, int *outlen) {

    char *cp1, *cp2, *in1, *in2, *inend1, *inend2;

    *out = 0;
    *outlen = 0;

    in1 = cp1 = argv[0].dsc$a_pointer;
    inend1 = in1 + argv[0].dsc$w_length;
    in2 = cp2 = argv[1].dsc$a_pointer;
    inend2 = in2 + argv[1].dsc$w_length;
    while ((cp1 < inend1) || (cp2 < inend2)) {
	char *ep1 = 0, *ep2 = 0;
	int elen1 = 0, elen2 = 0;

	if (cp1 < inend1) {
    	    while ((cp1 < inend1)
	    	&& (strchr(WHITESPACE, *cp1) != (char *) 0))
    	    	cp1++;
	    if (cp1 < inend1) {
	    	ep1 = cp1;
    	    	while ((++cp1 < inend1)
    	    	    && (strchr(WHITESPACE, *cp1) == (char *) 0))
    	    	    ;
		elen1 = cp1 - ep1;
	    }
	}

	if (cp2 < inend2) {
    	    while ((cp2 < inend2)
	    	&& (strchr(WHITESPACE, *cp2) != (char *) 0))
    	    	cp2++;
	    if (cp2 < inend2) {
	    	ep2 = cp2;
    	    	while ((++cp2 < inend2)
    	    	    && (strchr(WHITESPACE, *cp2) == (char *) 0))
    	    	    ;
		elen2 = cp2 - ep2;
	    }
	}

	if ((elen1 != 0) || (elen2 != 0)) {
	    *out = cat(*out, ep1, elen1, ep2, elen2, " ");
	}
    }
    if (*out != 0) *outlen = strlen(*out) - 1;

    return 0;
} /* apply_join */

/*
**++
**  ROUTINE:	apply_lastword
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in LASTWORD function.
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
static int apply_lastword (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    char *cp, *ep, *in, *inend;

    *out = 0;
    *outlen = 0;

    inend = argv[0].dsc$a_pointer;
    in = cp = inend + argv[0].dsc$w_length;
    while ((--cp >= inend)
	&& (strchr(WHITESPACE, *cp) != (char *) 0))
	;
    if (cp >= inend) {
	ep = cp;
	while ((--cp >= inend)
	    && (strchr(WHITESPACE, *cp) == (char *) 0))
	    ;
	*outlen = ep-cp;
	*out = cat(0, ++cp, *outlen);
    }

    return 0;
} /* apply_lastword */

/*
**++
**  ROUTINE:	apply_notdir
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in NOTDIR  function.
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
static int apply_notdir (int argc, struct dsc$descriptor *argv,
			 char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
	    	*out = cat(*out, nam.nam$l_name,
			   nam.nam$b_name + nam.nam$b_type, " ", 1);
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_notdir */

/*
**++
**  ROUTINE:	apply_or
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in OR function.
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
static int apply_or (int argc, struct dsc$descriptor *argv,
		     char **out, int *outlen) {

    int i, len, resolved_MMS_macro;
    char *ep, *sp, *outend;

    for (i = 0; i < argc; i++) {
	resolved_MMS_macro = Resolve_Symbols(argv[i].dsc$a_pointer,
					     argv[i].dsc$w_length, out,
					     outlen, 0);
	if (*outlen != 0) {
	    sp = *out;
	    outend = *out + *outlen;
	    while ((sp < outend)
		&& (strchr(WHITESPACE, *sp) != (char *) 0))
		sp++;
	    if (sp < outend) {
		ep = outend;
		outend = sp;
		while ((--ep >= outend)
		    && (strchr(WHITESPACE, *ep) != (char *) 0))
		    ;
		len = (ep - sp) + 1;
		if (len > 0) {
		    char *tmp;
		    tmp = cat(0, sp, len);
		    free(*out);
		    *out = tmp;
		    *outlen = len;
		    break;
		}
	    }
	}
	if (*out != (char *) 0) free(*out);
	*out = 0;
	*outlen = 0;
    }

    return resolved_MMS_macro;
} /* apply_or */

/*
**++
**  ROUTINE:	apply_origin
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in ORIGIN function.
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
static int apply_origin (int argc, struct dsc$descriptor *argv,
			 char **out, int *outlen) {

    static char *ORIGINS[] = { "UNDEFINED", "SPECIAL", "FILE",
	"COMMAND LINE", "SPECIAL", "DEFAULT", "CLI SYMBOL", "TEMPORARY" };

    struct SYMBOL *sym;
    char *var;

    var = malloc(argv[0].dsc$w_length+1);
    memcpy(var, argv[0].dsc$a_pointer, argv[0].dsc$w_length);
    var[argv[0].dsc$w_length] = '\0';

    sym = Lookup_Symbol(var);
    if (sym == (struct SYMBOL *)0) {
	*out = strdup(ORIGINS[0]);
    } else {
	*out = strdup(ORIGINS[sym->type+1]);
    }
    *outlen = strlen(*out);
    free(var);

    return 0;
} /* apply_origin */

/*
**++
**  ROUTINE:	apply_patsubst
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in PATSUBST function.
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
static int apply_patsubst (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    struct FROM {
	struct FROM *flink, *blink;
	char *ptr;
	int len;
    } *froment, fromque = { &fromque, &fromque, 0, 0 };
    char *cp, *in, *inend, *pat, *patend, *pp, *to, *toend, *tp, *tp2;

    *out = 0;
    *outlen = 0;

    pat = argv[0].dsc$a_pointer;
    patend = pat + argv[0].dsc$w_length;

    to = argv[1].dsc$a_pointer;
    toend = to + argv[1].dsc$w_length;

    in = cp = argv[2].dsc$a_pointer;
    inend = in + argv[2].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
	    char star = 0;
	    in = cp;
	    pp = pat;
	    while (cp < inend) {
		if (star) {
		    if (*cp == star) {
			froment = 0;
			star = 0;
			pp++;
		    } else {
			froment->len++;
		    }
		} else if ((*pp == '*') || (*pp == '%')) {
		    froment = malloc(sizeof(struct FROM));
		    froment->ptr = cp;
		    froment->len = 1;
		    queue_insert(froment, fromque.blink);
		    if (*pp++ == '*') {
			if (pp < patend)
			    star = *pp;
			else
			    break;
		    }
		} else if (*cp == *pp) {
		    pp++;
		} else {
		    break;
		}
		if (strchr(WHITESPACE, *(++cp)) != 0) break;
	    }

	    if ((pp == patend)
		&& ((cp == inend) || (strchr(WHITESPACE, *(cp)) != 0))) {
		for (tp = tp2 = to; tp < toend; tp++) {
		    if ((*tp == '%') || (*tp == '*')) {
			if (queue_remove(fromque.flink, &froment)) {
			    if (*tp == '%') froment->len = 1;
			    *out = cat(*out, tp2, tp-tp2, froment->ptr,
				       froment->len);
			    free(froment);
			} else {
			    *out = cat(*out, tp2, tp-tp2);
			}
			tp2 = tp+1;
		    }
		}
		if (tp2 != toend) {
		    *out = cat(*out, tp2, tp-tp2, " ", 1);
		}
	    } else {
		*out = cat(*out, in, cp-in, " ", 1);
	    }

	    while (queue_remove(fromque.flink, &froment)) {
		free(froment);
	    }
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }

    if (*out) *outlen = strlen(*out) - 1;

    return 0;
} /* apply_patsubst */

/*
**++
**  ROUTINE:	apply_sort
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in SORT function.
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
static int apply_sort (int argc, struct dsc$descriptor *argv,
		       char **out, int *outlen) {

    struct dsc$descriptor symbol;
    static int zone = 0;
    char *cp, *ep, *in, *inend;
    int count, duplicates = 0, status;
    void *tree = 0;

    *out = 0;
    *outlen = 0;

    if (zone == 0) {
    	unsigned int algorithm = LIB$K_VM_FIXED;
    	unsigned int flags = LIB$M_VM_GET_FILL0 | LIB$M_VM_EXTEND_AREA;
    	status = lib$create_vm_zone(&zone, &algorithm, &LEAF_S_LEAFDEF,
				    &flags);
    	if (!OK(status)) lib$signal(MMK__NOALLOC, 1, "SORT", status);
    }

    symbol.dsc$b_dtype = DSC$K_DTYPE_T;
    symbol.dsc$b_class = DSC$K_CLASS_S;

    count = 0;
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
	    struct LEAF *node;

	    symbol.dsc$a_pointer = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    symbol.dsc$w_length = cp - symbol.dsc$a_pointer;
	    status = lib$insert_tree(&tree, &symbol, &duplicates,
				     apply_sort_cmp, apply_sort_malloc,
				     &node, zone);
	    if (!OK(status)) lib$signal(status);
	    count++;
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }

    lib$traverse_tree(&tree, apply_sort_cat, out);
    if (*out != 0) *outlen = strlen(*out) - 1;

    lib$reset_vm_zone(&zone);

    return 0;
} /* apply_sort */

/*
**++
**  ROUTINE:	apply_sort_cmp
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Compare routine used by apply_sort to call lib$insert_tree.
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
static int apply_sort_cmp(struct dsc$descriptor *symbol,
			  struct LEAF *node,
			  void *unused) {
    return str$compare(symbol, &node->str);
} /* apply_sort_cmp */

/*
**++
**  ROUTINE:	apply_sort_malloc
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Allocation routine used by apply_sort to call lib$insert_tree.
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
static int apply_sort_malloc(struct dsc$descriptor *symbol,
			     struct LEAF **node,
			     int zone) {
    int status;

    status = lib$get_vm(&LEAF_S_LEAFDEF, node, &zone);
    if (OK(status)) (*node)->str = *symbol;
    return status;
} /* apply_sort_malloc */

/*
**++
**  ROUTINE:	apply_sort_cat
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Routine that traverses a balanced binary tree.  Called by apply_sort
**  for use with lib$traverse_tree.
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
static int apply_sort_cat(struct LEAF *node,
			  char **out) {

    *out = cat(*out, node->str.dsc$a_pointer, node->str.dsc$w_length, " ", 1);
    return 1;
} /* apply_sort_cat */

/*
**++
**  ROUTINE:	apply_strip
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in STRIP function.
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
static int apply_strip (int argc, struct dsc$descriptor *argv,
			char **out, int *outlen) {

    char *cp, *ep, *in, *inend;

    *out = 0;
    *outlen = 0;

    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
    	if (strchr(WHITESPACE, *cp) == (char *) 0) {
    	    ep = cp;
    	    while ((++cp < inend)
    	    	&& (strchr(WHITESPACE, *cp) == (char *) 0))
    	    	;
	    *out = cat (*out, ep, cp-ep, " ");
    	}
    	while ((++cp < inend)
    	    && (strchr(WHITESPACE, *cp) != (char *) 0))
    	    ;
    }
    if (*out != (char *) 0) *outlen = strlen(*out) - 1;

    return 0;
} /* apply_strip */

/*
**++
**  ROUTINE:	apply_subst
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in SUBST function.
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
static int apply_subst (int argc, struct dsc$descriptor *argv,
			char **out, int *outlen) {

    struct dsc$descriptor *from, *in, *to;
    int pos, start = 1;

    *out = 0;
    *outlen = 0;

    from = &argv[0];
    to = &argv[1];
    in = &argv[2];

    while ((pos = str$position(in, from, &start)) != 0) {
	*out = cat(*out, in->dsc$a_pointer+start-1, pos-start,
		   to->dsc$a_pointer, to->dsc$w_length);
	start = pos + from->dsc$w_length;
    }
    if ((pos == 0) && (start <= in->dsc$w_length))
	*out = cat(*out, in->dsc$a_pointer+start-1, in->dsc$w_length-start+1);

    if (*out) *outlen = strlen(*out);

    return 0;
} /* apply_subst */

/*
**++
**  ROUTINE:	apply_warn
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in WARN function.
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
static int apply_warn (int argc, struct dsc$descriptor *argv,
		       char **out, int *outlen) {

    int i;

    *out = 0;
    *outlen = 0;

    for (i = 0; i < argc; i++)
	lib$signal(MMK__WARN, 1, &argv[i]);

    return 0;
} /* apply_warn */

/*
**++
**  ROUTINE:	apply_wildcard
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in WILDCARD  function.
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
static int apply_wildcard (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    char *cp, *in, *inend, *sp;
    char esa[NAM$C_MAXRSS], rsa[NAM$C_MAXRSS];
    struct FAB fab;
    struct NAM nam;

    *out = 0;
    *outlen = 0;
    fab = cc$rms_fab;
    fab.fab$l_nam = &nam;
    nam = cc$rms_nam;
    nam.nam$l_esa = esa;
    nam.nam$b_ess = sizeof(esa);
    nam.nam$l_rsa = rsa;
    nam.nam$b_rss = sizeof(rsa);
    nam.nam$b_nop = NAM$M_SYNCHK;
#ifdef NAM$M_NO_SHORT_UPCASE
    nam.nam$b_nop |= NAM$M_NO_SHORT_UPCASE;
#endif
    in = cp = argv[0].dsc$a_pointer;
    inend = in + argv[0].dsc$w_length;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    fab.fab$l_fna = cp;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	    fab.fab$b_fns = cp - fab.fab$l_fna;
	    if (OK(sys$parse(&fab))) {
		while (OK(sys$search(&fab))) {
	    	    *out = cat(*out, nam.nam$l_name,
			       nam.nam$b_name + nam.nam$b_type, " ", 1);
		}
	    }
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }
    *outlen = strlen(*out) - 1;

    return 0;
} /* apply_wildcard */

/*
**++
**  ROUTINE:	apply_word
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in WORD function.
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
static int apply_word (int argc, struct dsc$descriptor *argv,
		       char **out, int *outlen) {

    char *cp, *ep, *in, *inend;
    int e, n, status;

    *out = 0;
    *outlen = 0;

    status = ots$cvt_tu_l(&argv[0], &n);
    if (OK(status)) {
	in = cp = argv[1].dsc$a_pointer;
	inend = in + argv[1].dsc$w_length;
	ep = 0;
	e = 0;
	while (cp < inend) {
	    if (strchr(WHITESPACE, *cp) == (char *)0) {
		e++;
		ep = cp;
	        while ((++cp < inend)
		    && (strchr(WHITESPACE, *cp) == (char *) 0))
		    ;
	    }
	    if (e == n) break;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) != (char *) 0))
		;
	    ep = 0;
	}
	if (ep != (char *)0) {
	    *outlen = cp-ep;
	    *out = malloc(*outlen);
	    memcpy(*out, ep, *outlen);
	}
    }

    return 0;
} /* apply_word */

/*
**++
**  ROUTINE:	apply_wordlist
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in WORDLIST function.
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
static int apply_wordlist (int argc, struct dsc$descriptor *argv,
			   char **out, int *outlen) {

    int b, e, i, status;
    char *cp, *in, *inend, *sp;

    *out = 0;
    *outlen = 0;

    status = ots$cvt_tu_l(&argv[0], &b);
    if (OK(status)) {
	status = ots$cvt_tu_l(&argv[1], &e);
	if (OK(status)) {
	    in = cp = argv[2].dsc$a_pointer;
	    inend = in + argv[2].dsc$w_length;
	    i = 0;
	    while (cp < inend) {
	    	if (strchr(WHITESPACE, *cp) == (char *)0) {
		    i++;
		    sp = cp;
		    while ((++cp < inend)
			&& (strchr(WHITESPACE, *cp) == (char *) 0))
		    	;
		    if (i >= b && i <= e)
		    	*out = cat(*out, sp, cp-sp, " ", 1);
		    if (i >= e) break;
	    	}
	        while ((++cp < inend)
		    && (strchr(WHITESPACE, *cp) != (char *) 0))
		    ;
	    }
	    if (*out != 0) *outlen = strlen(*out) - 1;
	}
    }

    return 0;
} /* apply_wordlist */

/*
**++
**  ROUTINE:	apply_words
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Handler for built-in WORDS function.
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
static int apply_words (int argc, struct dsc$descriptor *argv,
			char **out, int *outlen) {

    char *cp, *in, *inend;
    int e;

    *out = 0;
    *outlen = 0;

    in = cp = argv[1].dsc$a_pointer;
    inend = in + argv[1].dsc$w_length;
    e = 0;
    while (cp < inend) {
	if (strchr(WHITESPACE, *cp) == (char *)0) {
	    e++;
	    while ((++cp < inend)
		&& (strchr(WHITESPACE, *cp) == (char *) 0))
		;
	}
	while ((++cp < inend)
	    && (strchr(WHITESPACE, *cp) != (char *) 0))
	    ;
    }

    *out = itoa(e);
    *outlen = strlen(*out);

    return 0;
} /* apply_words */
