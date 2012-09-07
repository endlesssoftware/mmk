/*
** GLOBALS.H
**
**  External reference definitions for MMS globals.  These are defined
**  as globals in MMK.C.
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
*/
#ifndef globals_h__
#define globals_h__
#include "mmk.h"
#pragma nostandard
    EXTERN struct SYMTABLE  global_symbols;
    EXTERN struct SYMTABLE  local_symbols;
    EXTERN struct SYMTABLE  cmdline_symbols;
    EXTERN struct SYMTABLE  builtin_symbols;
    EXTERN struct SYMTABLE  temporary_symbols;
    EXTERN struct RULE      rules;
    EXTERN struct RULE	    *default_rule;
    EXTERN struct DEPEND    dependencies, dep_internal, dep_deferred;
    EXTERN struct SFX       suffixes;
    EXTERN struct CMD       do_first;
    EXTERN struct CMD       do_last;
    EXTERN int  	    verify, do_log, did_an_update, noaction, check_status;
    EXTERN int	    	    force, from_sources, ignore, use_cms, skip_intermediates;
    EXTERN int		    builtins, case_sensitive, gnu_syntax, mms_syntax;
    EXTERN int	    	    override_silent, override_ignore, symbol_override;
    EXTERN int		    override_builtins, override_case, override_gnu_syntax, override_mms_syntax;
    EXTERN unsigned int     exit_status;
    EXTERN char	    	    cms$lib[];
    EXTERN char	    	    cms_default_generation[];
#pragma standard
#endif /* globals_h__ */
