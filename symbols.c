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
**  Copyright (c) 2012, Endless Software Solutions.
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
**					 alias for WARN.
**--
*/
#pragma module SYMBOLS "V3.0-2"
#include "mmk.h"
#include "globals.h"
#include <stdarg.h>
#include <string.h>
#include <ots$routines.h>
#include <ctype.h>
#define ARGMAX (sizeof(int) * 8)
/*
** Builtin function descriptor
*/
    struct FUNCTION {
	char *name;
	int vararg, maxarg;
	int (*handler)(int, char **, int *);
    };

/*
** Forward declarations
*/
    struct SYMBOL *Lookup_Symbol(char *);
    void Define_Symbol(SYMTYPE, char *, char *, int, ...);
    int Resolve_Symbols(char *, int, char **, int *, int, ...);
    void Clear_Local_Symbols(void);
    static void Clear_Temporary_Symbols(unsigned);
    void Create_Local_Symbols(struct DEPEND *, struct OBJREF *, struct QUE *);
    static void apply_subst_rule(char *, char *, char **, int *);
    static void apply_full_subst_rule(char *, char *, char **, int *);
    static char *apply_builtin (char *, char *, int, char **, int *, int *,
				int *, int);
    static int apply_error(int, char **, int*);
    static int apply_firstword(int, char **, int*);
    static int apply_info(int, char **, int*);
    static int apply_lastword(int, char **, int*);
    static int apply_origin(int, char **, int *);
    static int apply_warn(int, char **, int*);
    static int apply_word(int, char **, int*);
    static int apply_wordlist(int, char **, int*);
    static int apply_words(int, char **, int *);

/*
** Own storage
*/

    static struct SYMTABLE dcl_symbols;
    static int dcl_symbols_inited = 0;
    static struct SYMBOL *temporary_symbols = 0;
    static struct dsc$descriptor argv[ARGMAX];
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
	{ "ERROR",		1, 1, apply_error,	},
	{ "FIRSTWORD",		0, 1, apply_firstword,	},
	{ "INFO",		1, 1, apply_info,	},
	{ "LASTWORD",		0, 1, apply_lastword,	},
	{ "ORIGIN",		0, 1, apply_origin,	},
	{ "WARN",		1, 1, apply_warn,	},
	{ "WARNING",		1, 1, apply_warn,	},
	{ "WORD",		0, 2, apply_word,	},
	{ "WORDLIST",		0, 3, apply_wordlist,	},
	{ "WORDS",		0, 1, apply_words,	}, };

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
    static struct SYMTABLE *override_order[] = {
    	&local_symbols, &cmdline_symbols, &dcl_symbols,
    	&global_symbols, &builtin_symbols};

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

    if (symtype == MMK_K_SYM_TEMPORARY) {
	/*
	** Temporary symbols are handled a bit differently.  As there can only
	** be one named symbol at any given level of $(FOREACH ) invokation,
	** they are actually stored as a singly linked list.
	*/
	if (temporary_symbols && append) {
	    sym = temporary_symbols;
	} else {
	    sym = mem_get_symbol();
	    strcpy(sym->name, upname);
	    if (temporary_symbols) sym->flink = temporary_symbols;
	    temporary_symbols = sym;
        }
    } else {
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
    }

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
**  	    	    	    int dont_resolve_unknowns)
**
**  The output string is allocated by this procedure using malloc
**  and should be freed by the caller when done.
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
    	    	    	int dont_resolve_unknowns, ...) {

    va_list ap;
    char *cp, *inend, *dp, *pp, *tmp, *val, *colp;
    struct SYMBOL *valsym;
    char var[MMK_S_SYMBOL+1];
    int len, curlen, tmplen, first, did_one, free_val, i;
    int actualcount, resolved_MMS_macro, *was_one;

    va_count(actualcount);
    if (actualcount > 5) {
	va_start(ap, dont_resolve_unknowns);
	was_one = va_arg(ap, int *);
	*was_one = 0;
	va_end(ap);
    } else {
	was_one = 0;
    }

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
						&resolved_MMS_macro, &did_one,
						dont_resolve_unknowns);
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
    	    	    	    	    	if (strcmp(var, non_resolvables[i]) == 0) break;
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

	if (did_one && was_one != 0) *was_one = 1;
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
**  ROUTINE:    Clear_Temporary_Symbols
**
**  FUNCTIONAL DESCRIPTION:
**
**      Deletes all of the symbols in the temporary symbol table.
**
**  RETURNS:    void
**
**  PROTOTYPE:
**
**      Clear_Temporary_Symbols(int level)
**
**  IMPLICIT INPUTS:    None.
**
**  IMPLICIT OUTPUTS:   None.
**
**  COMPLETION CODES:   None.
**
**  SIDE EFFECTS:       temporary_symbols
**
**--
*/
static void Clear_Temporary_Symbols (unsigned level) {

    struct SYMBOL *sym;
    unsigned i;

    for (i = 0, sym = temporary_symbols; i < level && sym != NULL; i++, sym = temporary_symbols) {
	temporary_symbols = sym->flink;
	mem_free_symbol(sym);
    }

} /* Clear_Temporary_Symbols */

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
			    int *did_one, int dont_resolve_unknowns) {

    struct FUNCTION *f = 0;
    char *ap, *cp, *inend;
    int argc, depth, i, status;

    for (i = 0; i < sizeof(functions)/sizeof(functions[0]); i++) {
	if (strcmp(functions[i].name, name) == 0) {
	    f = &functions[i];
	    break;
	}
    }
    *out = 0;
    *outlen = 0;
    argc = 0;
    depth = 0;
    inend = in + inlen;
    ap = cp = in;
    while (cp < inend) {
	cp = find_char(ap, inend, ",)$");
	if (cp == (char *)0) {
	    lib$signal(MMK__UTLBADMAC, 2, f->name, 1); // where is line no.?
	    break;
	} else {
	    if (*cp == '$') {
		if (cp <= inend-1 && strchr(SPECIALS, *(cp+1))) {
		    // is a special
		    cp++; 
		} else if (cp <= inend-3 && *(++cp) == '(') {
		    depth++;
		    continue;
		}
	    } else  if (*cp == ',' || *cp == ')') {
		if (depth == 0) {
		    if (argc >= ARGMAX) {
			lib$signal(MMK__TOOMANYARGS);
		    } else {
		    	argv[argc].dsc$a_pointer = ap;
			argv[argc].dsc$b_class = DSC$K_CLASS_S;
			argv[argc].dsc$b_dtype = DSC$K_DTYPE_T;
		    	argv[argc].dsc$w_length = cp-ap;
			argc++;
			if (*cp == ')')
			    break;
			ap = ++cp;
		    }
		} else {
		    if (*cp == ')')
			depth--;
		}
	    }
	}
    }

    if (f != (struct FUNCTION *)0) {
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
		if (0) {  // not supposed to process the argument...
	            // copy the arguments with malloc/memcpy
		} else {
	            char *rptr;
	            int rlen;
	            *resolved_MMS_macro |= Resolve_Symbols(argv[i].dsc$a_pointer,
						argv[i].dsc$w_length, &rptr,
						&rlen, dont_resolve_unknowns);
		    argv[i].dsc$a_pointer = rptr;
		    argv[i].dsc$w_length = (unsigned short)rlen;
	    	}
	    }

	    // if unresolved
	    	// no call....what do we do?
	    // else
	    	*resolved_MMS_macro |= (f->handler)(argc, out, outlen);

	    for (i = 0; i < argc; i++) {
	    	if (argv[i].dsc$a_pointer != 0) {
		    free(argv[i].dsc$a_pointer);
		    argv[i].dsc$a_pointer = 0;
		    argv[i].dsc$w_length = 0;
	    	}
	    }
	}
    } else {
	lib$signal(MMK__UNRFUN, 1, name);
    }

    return ++cp;
} /* apply_builtin */

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
static int apply_error (int argc, char **out, int *outlen) {

    lib$stop(MMK__ERROR, 1, &argv[0]);

    return 0;
} /* apply_error */

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
static int apply_firstword (int argc, char **out, int *outlen) {

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
static int apply_info (int argc, char **out, int *outlen) {

    int i;

    for (i = 0; i < argc; i++)
	lib$signal(MMK__INFO, 1, &argv[i]);

    return 0;
} /* apply_info */

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
static int apply_lastword (int argc, char **out, int *outlen) {

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
static int apply_origin (int argc, char **out, int *outlen) {

    static char *ORIGINS[] = { "UNDEFINED", "SPECIAL", "FILE",
	"COMMAND LINE", "SPECIAL", "DEFAULT", "CLI SYMBOL", "TEMPORARY" };

    struct SYMBOL *sym;
    char *var;
    int type;

    var = malloc(argv[0].dsc$w_length+1);
    memcpy(var, argv[0].dsc$a_pointer, argv[0].dsc$w_length);
    var[argv[0].dsc$w_length] = '\0';

    sym = Lookup_Symbol(var);
    if (sym == (struct SYMBOL *)0) {
	*out = strdup(ORIGINS[0]);
    } else {
	type = (sym->type & ~0x7) + 1;
	*out = strdup(ORIGINS[type]);
    }
    *outlen = strlen(*out);
    free(var);

    return 0;
} /* apply_origin */

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
static int apply_warn (int argc, char **out, int *outlen) {

    int i;

    for (i = 0; i < argc; i++)
	lib$signal(MMK__WARN, 1, &argv[i]);

    return 0;
} /* apply_warn */

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
static int apply_word (int argc, char **out, int *outlen) {

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
static int apply_wordlist (int argc, char **out, int *outlen) {

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
static int apply_words (int argc, char **out, int *outlen) {

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
