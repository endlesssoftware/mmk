The MMK Make Utility

MMK is a "make" utility for VMS systems.  It is used for building software
systems based on a "description file" (or "makefile") you create that lists
the sources and objects of a system and the dependencies between them.

MMK is similar in functionality to Digital's DEC/Module Management System (MMS),
and understands a syntax in its description files which is mostly compatible
with MMS (at least, older versions of MMS).  It also includes support for
creating inference rules that separate source and target directories, the
syntax for which was borrowed from NMAKE.

MMK runs on VAX/VMS, OpenVMS VAX, OpenVMS AXP, and OpenVMS IA64 systems.
It should build and run on versions of VMS older than V7.0, but is only
being maintained for more recent versions of the operating system.  MMK
has support for the DEC/CMS code management system, but does not require
it.

MMK is written entirely in C (with a little Macro).  Complete source code is
provided.

The following describes the source modules:

INSTALL.TXT               Installation instructions.
LICENSE.TXT               License information.
README.TXT                This file.

BUILD_TARGET.C            Routines for building targets.
CLIDEFS.H                 CLI$ interface definitions.
CMS_INTERFACE.C           DEC/CMS interface routines.
COMPILE.COM               Command procedure for building MMK from sources.
DEFAULT_RULES.C           Setup routines for compiled-in default rules.
DESCRIP.MMS               Description file for building MMK.
FILEIO.C                  File I/O routines.
GENSTRUC.C                Routines for generating structures for built-in rules.
GET_RDT.C                 Routines for getting revision date stamps.
GLOBALS.H                 Include file for MMK globals.
MEM.C                     Memory management routines.
MISC.C                    Miscellaneous support routines.
MMK.ALPHA_OPT             Linker options file for building MMK (AXP).
MMK.C                     MMK main routine.
MMK.H                     Include file for MMK definitions.
MMK.IA64_OPT              Linker options file for building MMK (IA64).
MMK.OPT                   Linker options file for building MMK (VAX).
MMK_CLD.CLD               MMK command language definition.
MMK_COMPILE_RULES.ALPHA_OPT  Linker options file for rules compiler. (AXP)
MMK_COMPILE_RULES.C       Rules compiler main program.
MMK_COMPILE_RULES.I64_OPT Linker options file for rules compiler. (IA64)
MMK_COMPILE_RULES.OPT     Linker options file for rules compiler (VAX).
MMK_COMPILE_RULES_CLD.CLD Command language definition for rules compiler.
MMK_DEFAULT_RULES.MMS     Default rules for compiling into MMK (VAX).
MMK_DEFAULT_RULES_AXP.MMS Default rules for copmiling into MMK (AXP).
MMK_DEFAULT_RULES_I64.MMS Default rules for copmiling into MMK (IA64).
MMK_DOC.SDML              VAX DOCUMENT source for MMK documentation.
MMK_HELP.RNH              RUNOFF source for MMK help file.
MMK_MSG.H                 Include file containing MMK message code definitions.
MMK_MSG.MSG               MMK message definitions.
OBJECTS.C                 Routines for managing the object tree.
PARSE_DESCRIP.C           Routines for parsing description files.
PARSE_OBJECTS.C           Routines for parsing object references.
PARSE_TABLES.MAR          LIB$TPARSE table for parsing description files.
READDESC.C                Routines for reading in description files.
RELEASE_NOTES.SDML        Revision information.
SP_MGR.C                  Subprocess management routines.
SYMBOLS.C                 Routines for managing the symbol tables.


--------------------------------------------------------------------------------
COPYRIGHT NOTICE

Copyright (c) 2008, Matthew Madison.
Copyright (c) 2012, Endless Software Solutions.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.
    * Neither the name of the copyright owner nor the names of any
      other contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
