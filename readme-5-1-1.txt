MMK V5.1-1 was released by Hunter Goatley on July 13, 2023.

I tried to coordinate with Tim Sneddon, but my emails to Tim have been
unanswered as of that date.

So I forged ahead. MMK V5.1-1 runs on VAX, Alpha, I64, and X86_64.

I only had to make a few changes to get it to work on X86_64, but I
fixed several bugs in the kitting to get all of that to work and be
consistent.

These are the updated and new files that I touched. They have not
been added to github yet, as I'm hoping to coordinate that with Tim.

Files changed from the V5.1 distribution on github:

BUILD_PCSI.MMS - Updated to correct "!=" with "|="
COMPILE.COM - Updated to compile cleanly on VAX V5.1 and Alpha V6.2
DESCRIP.MMS - Numerous updates to add X86_64 support and work around ESS
INSTALLING_VERSION.MMS - Updated to correct "!=" with "|="
KITINSTAL.COM - Added X86_64, fixed MMK_DO_* questions, which use globals
MMK.C - Added a check for __x86_64 to use lib$table_parse
MMK.H - Added prototypes for three str routines if compiling on V6
MMK.X86_64_OPT - New linker options file for X86_64
MMK_COMPILE_RULES.C - Added a check for __x86_64 to use lib$table_parse
MMK_COMPILE_RULES.X86_64_OPT - New linker options file for X86_64
MMK_DEFAULT_RULES_X86_64.MMS - New file for X86_64
MMK___STARTUP.COM - Modified for directory name consistency, add X86_64
PARSE_DESCRIP.C - Added a check for __x86_64 to use lib$table_parse
PARSE_OBJECTS.C - Added a check for __x86_64 to use lib$table_parse
PARSE_TABLES.MAR - Rename EVAX symbol to SIXTYFOUR_BIT
RELEASE_NOTES.SDML - Added note for MMK V5.1-1
STR.C - strdup(), strcasecmp(), and strncasecmp() routines for VMS V6.x
VERSION.H - Bumped version to MMK V5.1-1

Questions? Problems? I can be reached at goathunter@goatley.com

Hunter
