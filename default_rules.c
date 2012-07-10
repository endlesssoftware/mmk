/*
**++
**  FACILITY:	MMK
**
**  ABSTRACT:	Map the default rules.
**
**  MODULE DESCRIPTION:
**
**  	Builds the symbols, rules, and suffixes lists from the
**  default rules compiled using MMK_COMPILE_RULES.
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
**  CREATION DATE:  30-APR-1993
**
**  MODIFICATION HISTORY:
**
**  	30-APR-1993 V1.0    Madison 	Initial coding.
**  	04-JUN-1993 V1.1    Madison 	Default rule support.
**  	21-JUN-1995 V1.2    Madison 	Symbol table revamp.
**  	27-DEC-1998 V1.2-1  Madison 	General cleanup.
**      03-MAY-2004 V1.3    Madison     Integrate IA64 changes.
**--
*/
#pragma module DEFAULT_RULES "V1.3"
#include "mmk.h"
#include "globals.h"
#include "etc_dir:mmk_default_rules.h"

/*
**++
**  ROUTINE:	Map_Default_Rules
**
**  FUNCTIONAL DESCRIPTION:
**
**  	Builds the symbols, rules, and suffixes lists from the
**  default rules compiled using MMK_COMPILE_RULES.
**
**  RETURNS:	cond_value, longword (unsigned), write only, by value
**
**  PROTOTYPE:
**
**  	Map_Default_Rules();
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
void Map_Default_Rules (void) {

    int i, j, next_rule, last_rule;
    char *cp;

    if (SFX_INIT_COUNT > 0) {
    	suffixes.flink = &sfx_init[0];
    	suffixes.blink = &sfx_init[SFX_INIT_COUNT-1];
    	sfx_init[0].flink = (SFX_INIT_COUNT > 1 ? &sfx_init[1] : &suffixes);
    	sfx_init[0].blink = (struct SFX *) &suffixes;
    	sfx_init[SFX_INIT_COUNT-1].flink = &suffixes;
    	sfx_init[SFX_INIT_COUNT-1].blink = 
    	    (SFX_INIT_COUNT > 1 ? &sfx_init[SFX_INIT_COUNT-2] :
    	    	(struct SFX *) &suffixes);
    	for (i = 1; i < SFX_INIT_COUNT-1; i++) {
    	    sfx_init[i].flink = &sfx_init[i+1];
    	    sfx_init[i].blink = &sfx_init[i-1];
    	}
    }

    if (GSYM_INIT_COUNT > 0) {
    	unsigned int hash_value;
    	unsigned char *cp;
    	int i;

    	for (i = 0; i < GSYM_INIT_COUNT; i++) {
    	    hash_value = 0;
    	    for (cp = (unsigned char *) gsym_init[i].name, j = 0;
    	    	    	    *cp != '\0' && j < 4; cp++, j++) {
    	    	hash_value |= *cp;
    	    }
    	    hash_value &= 0xff;
    	    queue_insert(&gsym_init[i],
    	    	    builtin_symbols.symlist[hash_value].tail);
    	}
    }

    if (RULE_INIT_COUNT > 0) {
    	rules.flink = &rule_init[0];
    	rules.blink = &rule_init[RULE_REAL_LAST_IDX];
    	last_rule = i = 0;
    	while (i < RULE_INIT_COUNT) {
    	    if (rule_init[i].next != 0) {
    	    	for (j = i+1; j < RULE_INIT_COUNT; j++) {
    	    	    rule_init[j-1].next = &rule_init[j];
    	    	    rule_init[j-1].parent = &rule_init[i];
    	    	    if (rule_init[j].next == 0) break;
    	    	}
    	    	next_rule = j + 1;
    	    } else {
    	    	next_rule = i + 1;
    	    }
    	    if (i == 0) {
    	    	rule_init[i].flink = (RULE_INIT_COUNT_REAL > 1 ? &rule_init[next_rule] :
    	    	    	(struct RULE *) &rules);
    	    	rule_init[i].blink = (struct RULE *) &rules;
    	    } else if (i == RULE_REAL_LAST_IDX) {
    	    	rule_init[i].flink = &rules;
    	    	rule_init[i].blink = 
    	    	    (RULE_INIT_COUNT_REAL > 1 ? &rule_init[last_rule] :
    	    	    	(struct RULE *) &rules);
    	    } else {
    	    	rule_init[i].flink = &rule_init[next_rule];
    	    	rule_init[i].blink = &rule_init[last_rule];
    	    }

    	    if (rule_init[i].cmdque.flags > 0) {
    	    	struct CMD *cmd_init = rule_init[i].cmdque.flink;
    	    	for (j = 0; j < rule_init[i].cmdque.flags; j++) {
    	    	    if (j == 0) {
    	    	    	cmd_init[j].flink =
    	    	    	    (rule_init[i].cmdque.flags > 1 ? &cmd_init[1] :
    	    	    	    	&rule_init[i].cmdque);
    	    	    	cmd_init[j].blink = &rule_init[i].cmdque;
    	    	    } else if (j == rule_init[i].cmdque.flags-1) {
    	    	    	cmd_init[j].flink = &rule_init[i].cmdque;
    	    	    	cmd_init[j].blink = 
    	    	    	    (rule_init[i].cmdque.flags > 1 ? &cmd_init[j-1] :
    	    	    	    &rule_init[i].cmdque);
    	    	    	rule_init[i].cmdque.blink = &cmd_init[j];
    	    	    } else {
    	    	    	cmd_init[j].flink = &cmd_init[j+1];
    	    	    	cmd_init[j].blink = &cmd_init[j-1];
    	    	    }
    	    	}
    	    }
    	    last_rule = i;
    	    i = next_rule;
    	}
    }

#ifdef DEFAULT_RULE_EXISTS
    default_rule = &default_rule_init;
    if (default_rule_init.cmdque.flags > 0) {
    	struct CMD *cmd_init = default_rule_init.cmdque.flink;
    	for (j = 0; j < default_rule_init.cmdque.flags; j++) {
    	    if (j == 0) {
    	    	cmd_init[j].flink =
    	    	    (default_rule_init.cmdque.flags > 1 ? &cmd_init[1] :
    	    	    	    	&default_rule_init.cmdque);
    	    	cmd_init[j].blink = &default_rule_init.cmdque;
    	    } else if (j == default_rule_init.cmdque.flags-1) {
    	    	cmd_init[j].flink = &default_rule_init.cmdque;
    	    	cmd_init[j].blink = 
    	    	    	(default_rule_init.cmdque.flags > 1 ? &cmd_init[j-1] :
    	    	    	&default_rule_init.cmdque);
    	    	default_rule_init.cmdque.blink = &cmd_init[j];
    	    } else {
    	    	cmd_init[j].flink = &cmd_init[j+1];
    	    	cmd_init[j].blink = &cmd_init[j-1];
    	    }
    	}
    }
#endif /* DEFAULT_RULE_EXISTS */

    return;

} /* Map_Default_Rules */
