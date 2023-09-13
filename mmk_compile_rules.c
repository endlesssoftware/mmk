/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	MMK rules compiler.
**
**  MODULE DESCRIPTION:
**
**  	This program parses the rules in an MMK rules file and converts them
**  into C structures to be compiled and linked into the MMK utility.
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
**  CREATION DATE:  30-APR-1993
**
**  MODIFICATION HISTORY:
**
**  	30-APR-1993 V1.0    Madison 	Initial coding.
**  	17-OCT-1993 V1.1    Madison 	Update for MMK V2.2.
**  	14-JUL-1994 V1.2    Madison 	Update for MMK V3.2.
**  	22-AUG-1994 V1.2-1  Madison 	Update for MMK V3.2-2.
**  	10-JAN-1995 V1.2-2  Madison 	Add put_command (for MMK V3.3-1).
**  	21-JUN-1995 V1.3    Madison 	Changes for MMK V3.4.
**  	22-DEC-1996 V1.4    Madison 	Changes for MMK V3.6.
**  	27-DEC-1998 V1.5    Madison 	General cleanup.
**      03-MAY-2004 V1.6    Madison     Integrate IA64 changes.
**	16-OCT-2008 V1.7    Sneddon	Added new globals.
**	07-SEP-2012 V1.7-1  Sneddon	Added temporary_symbols.
**--
*/
#define MMKC_VERSION 	"V1.7-1"
#define MMKC_COPYRIGHT  "Copyright (c) 2008, Matthew Madison.\n" \
                        "  Copyright (c) 2012, Endless Software Solutions.\n" \
                        "  See LICENSE.TXT in distribution kit for license information."

#pragma module MMK_COMPILE_RULES MMKC_VERSION

    char $$$Copyright[] = MMKC_COPYRIGHT;
    char *Version = MMKC_VERSION, *Copyright = &$$$Copyright[0];

#include "mmk.h"
#include "clidefs.h"

/*
** Forward declarations
*/
    unsigned int main(void);
    unsigned int put_output(void *);
    unsigned int put_command(void *);
    static unsigned int cli_get_value(char *, char *, int);
    static unsigned int cli_present(char *);

/*
** Globals used throughout the program
*/

#pragma nostandard
    GLOBAL struct SYMTABLE  global_symbols;
    GLOBAL struct SYMTABLE  local_symbols;
    GLOBAL struct SYMTABLE  cmdline_symbols;
    GLOBAL struct SYMTABLE  builtin_symbols;
    GLOBAL struct SYMTABLE  *temporary_symbols;
    GLOBAL struct RULE      rules;
    GLOBAL struct RULE	    *default_rule = 0;
    GLOBAL struct DEPEND    dependencies;
    GLOBAL struct DEPEND    dep_internal;
    GLOBAL struct DEPEND    dep_deferred;
    GLOBAL struct SFX       suffixes;
    GLOBAL struct CMD       do_first;
    GLOBAL struct CMD       do_last;
    GLOBAL int  	    verify, do_log, did_an_update, noaction, check_status;
    GLOBAL int		    builtins, case_sensitive, gnu_syntax, mms_syntax;
    GLOBAL int	    	    from_sources, force, ignore, use_cms, skip_intermediates;
    GLOBAL int	    	    override_silent, override_ignore, symbol_override;
    GLOBAL int		    override_builtins, override_case, override_gnu_syntax, override_mms_syntax;
    GLOBAL unsigned int	    exit_status;
    GLOBAL char	    	    cms$lib[256*16];
    GLOBAL char	    	    cms_default_generation[256];
#pragma standard

/*
** External references
*/
#ifndef __VAX
    extern mmk_compile_rules_cld;
#else
    globalref mmk_compile_rules_cld;
#endif
#define MMK_COMPILE_RULES_CLD (&mmk_compile_rules_cld)
    void Generate_Structures(char *, char *); /* module GENSTRUC */

/*
**++
**  ROUTINE:	main
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Main program.  Fetches the command from the command line,
**  uses CLI$ routines to parse it, then starts the build process.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	main
**
**  IMPLICIT INPUTS:	See global definitions at module head.
**
**  IMPLICIT OUTPUTS:	See global definitions at module head.
**
**  COMPLETION CODES:
**
**  	SS$_NORMAL, MMK__ALLOK : normal successful completion.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
unsigned int main (void) {

    DESCRIP cmdstr;
    char Output_File[256], tmp[256];
    $DESCRIPTOR(cmdname, "MMKC ");
    unsigned int status;
    int i;

/*
** Initialize the globals
*/
    temporary_symbols = 0;
    for (i = 0; i < MMK_K_SYMTABLE_SIZE; i++) {
    	INIT_QUEUE(global_symbols.symlist[i]);
    	INIT_QUEUE(local_symbols.symlist[i]);
    	INIT_QUEUE(cmdline_symbols.symlist[i]);
    	INIT_QUEUE(builtin_symbols.symlist[i]);
    }
    INIT_QUEUE(rules);
    INIT_QUEUE(dependencies);
    INIT_QUEUE(dep_internal);
    INIT_QUEUE(dep_deferred);
    INIT_QUEUE(suffixes);
    INIT_QUEUE(do_first);
    INIT_QUEUE(do_last);
    exit_status = SS$_NORMAL;
    ignore = override_silent = override_ignore = symbol_override = 0;
    skip_intermediates = 0;

/*
** Fetch and parse command string
*/
    INIT_DYNDESC(cmdstr);
    status = lib$get_foreign(&cmdstr);
    str$prefix(&cmdstr, &cmdname);
    status = cli$dcl_parse(&cmdstr, MMK_COMPILE_RULES_CLD,
    	    	lib$get_input, lib$get_input);
    if (!OK(status)) return (status | STS$M_INHIB_MSG);

/*
** Get the command parameters and qualifiers
*/
    Output_File[0] = '\0';
    if (cli_present("OUTPUT") == CLI$_PRESENT) {
    	cli_get_value("OUTPUT", Output_File, sizeof(Output_File));
    }

    status = cli_get_value("RULES_FILE", tmp, sizeof(tmp));
    if (!OK(status)) return status | STS$M_INHIB_MSG;

    Read_Description(tmp, "SYS$DISK:[].MMS", 1);
    if (OK(exit_status)) Generate_Structures(tmp, Output_File);

    return exit_status | STS$M_INHIB_MSG;

}

/*
**++
**  ROUTINE:	cli_get_value
**
**  FUNCTIONAL DESCRIPTION:
**
**  	C Interface to CLI$GET_VALUE.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	cli_get_value (char *argname, DESCRIP *arg)
**
** argname: ASCIZ_string, read only, by reference
** arg:	    char_string, write only, by descriptor (dynamic)
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**  	All those from CLI$PRESENT and CLI$GET_VALUE.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int cli_get_value (char *argname, char *arg, int argsize) {

    DESCRIP argnamd, argd;
    unsigned short arglen;
    int status;

    INIT_SDESC(argnamd, strlen(argname), argname);
    INIT_SDESC(argd, argsize-1, arg);
    status = cli$present(&argnamd);
    if ($VMS_STATUS_SUCCESS(status)) {
    	status = cli$get_value(&argnamd, &argd, &arglen);
    	if (OK(status)) *(arg+arglen) = '\0';
    }
    return status;
}

/*
**++
**  ROUTINE:	put_output
**
**  FUNCTIONAL DESCRIPTION:
**
**  	LIB$PUT_OUTPUT replacement that directs things either
**  to SYS$OUTPUT or to the file specified in /OUTPUT.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	put_output(struct dsc$descriptor *dsc)
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
unsigned int put_output (void *dsc) {

    return lib$put_output(dsc);

} /* put_output */

/*
**++
**  ROUTINE:	put_command
**
**  FUNCTIONAL DESCRIPTION:
**
**  	LIB$PUT_OUTPUT replacement that directs things either
**  to SYS$OUTPUT or to the file specified in /OUTPUT (for commands).
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	put_command(struct dsc$descriptor *dsc)
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
unsigned int put_command (void *dsc) {

    return lib$put_output(dsc);

} /* put_output */

/*
**++
**  ROUTINE:	cli_present
**
**  FUNCTIONAL DESCRIPTION:
**                               
**  	C Interface to CLI$PRESENT.
**
**  RETURNS:	cond_value, intword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	cli_present (char *argname)
**
** argname: ASCIZ_string, read only, by reference
**
**  IMPLICIT INPUTS:	None.
**
**  IMPLICIT OUTPUTS:	None.
**
**  COMPLETION CODES:
**
**  	All those from CLI$PRESENT.
**
**  SIDE EFFECTS:   	None.
**
**--
*/
static unsigned int cli_present (char *argname) {

    DESCRIP argnamd;

    INIT_SDESC(argnamd, strlen(argname), argname);
    return cli$present(&argnamd);
}
