!++
!   MMK_PCSI.MMS
!
!   Description file for generating PCSI product description files.
!
!   Copyright (c) 2013, Endless Software Solutions.
!
!   All rights reserved.
!
!   Redistribution and use in source and binary forms, with or without
!   modification, are permitted provided that the following conditions
!   are met:
!
!       * Redistributions of source code must retain the above
!         copyright notice, this list of conditions and the following
!         disclaimer.
!       * Redistributions in binary form must reproduce the above
!         copyright notice, this list of conditions and the following
!         disclaimer in the documentation and/or other materials provided
!         with the distribution.
!       * Neither the name of the copyright owner nor the names of any
!         other contributors may be used to endorse or promote products
!         derived from this software without specific prior written
!         permission.
!
!   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
!   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
!   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
!   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
!   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
!   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
!   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
!   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
!   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!   25-FEB-2011 V1.0    Sneddon     Initial coding.
!--
TMP != PIPE TMP="$(MMK_MAJOR_VERSION)" ; IF F$LENGTH(TMP) .EQ 1 THEN TMP="0"+TMP ; WRITE/SYMBOL SYS$OUTPUT TMP
KIT_VERSION = $(COLLAPSE $(TMP)$(MMK_MINOR_VERSION))
GENERATION != WRITE SYS$OUTPUT F$CVTIME(,,"DATE")-"-"-"-"
BASE_SYSTEM = $(KITARCH)VMS

ECHO = WRITE SYS$OUTPUT

BINDIR = MG_BIN:[MMK]
KITDIR = MG_KIT:[MMK]
SRCDIR = MG_SRC:[MMK]

ALWAYS_MAKE : ! Fake target, to force make...

DOCUMENTATION ~= $(ADDPREFIX $(KITDIR),$(WILDCARD $(KITDIR)*.HTML)) -
		 $(ADDPREFIX $(KITDIR),$(WILDCARD $(KITDIR)*.PDF)) -
		 $(ADDPREFIX $(KITDIR),$(WILDCARD $(KITDIR)*.PS)) -
		 $(ADDPREFIX $(KITDIR),$(WILDCARD $(KITDIR)*.TXT))

#=============================================================================
# Construct package
#=============================================================================

PACKAGE :
    PRODUCT PACKAGE MMK/PRODUCER=ESS/BASE_SYSTEM=$(BASE_SYSTEM) -
	/DESTINATION=$(KITDIR)/LOG/TRACE -
	/SOURCE=$(KITDIR)MMK.PCSI$DESC -
	/MATERIAL=(MG_KIT:[MMK],MG_BIN_$(KITARCH):[MMK],MG_SRC:[MMK]) -
	/FORMAT=SEQUENTIAL

#=============================================================================
# Product Description File
#=============================================================================

DESCRIPTION : HEADER,-
		DOCHEADER,-
		  $(DOCUMENTATION),-
		DOCFOOTER,-
		SOURCE,-
	      FOOTER
    @ CONTINUE

HEADER :
    @ $(ECHO) "product ESS $(BASE_SYSTEM) MMK $(MMK_VERSION) full ;"
    @ $(ECHO) "  directory [SYSHLP.MMK] ;"
    @ $(ECHO) "  file [SYSHLP]MMK$(KIT_VERSION).RELEASE_NOTES release notes ;"
    @ $(ECHO) "  file [SYSEXE]MMK.EXE generation $(GENERATION) archive ;"
    @ $(ECHO) "  module [MMK]MMK_CLD.CLD type command module MMK ;"
    @ $(ECHO) "  module [MMK]MMK_HELP.HLP type help module MMK ;"

DOCHEADER :
    @ $(ECHO) "  option DOCUMENTATION default "YES" ;"

$(DOCUMENTATION) : ALWAYS_MAKE
    @ $(ECHO) "    file [SYSHLP.MMK]$(NOTDIR $(MMS$TARGET))"
    @ $(ECHO) "      generation $(GENERATION) ;"

DOCFOOTER :
    @ $(ECHO) "  end option ;"

SOURCE :
    @ $(ECHO) "  option SOURCE default "NO" ;"
    @ $(ECHO) "    file [SYSHLP.MMK]MMK$(KIT_VERSION)_SOURCE.ZIP"
    @ $(ECHO) "      generation $(GENERATION) ;"
    @ $(ECHO) "  end option ;"

FOOTER :
    @ $(ECHO) "end product ;"

#=============================================================================
# Product Text File
#=============================================================================

TEXT :
    @ $(ECHO) "=product ESS $(BASE_SYSTEM) MMK $(MMK_VERSION) full"
    @ $(ECHO) "1 'PRODUCT"
    @ $(ECHO) "=prompt MMK Make Uility"
    @ $(ECHO) "MMK is a ""make"" utility for VMS systems.  It is used for "
    @ $(ECHO) "building software systems based on a ""description file"" (or "
    @ $(ECHO) """makefile"") you create that lists the sources and objects "
    @ $(ECHO) "of a system and the dependencies between them."
    @ $(ECHO) ""
    @ $(ECHO) "MMK is similar in functionality to Digital's DEC/Module "
    @ $(ECHO) "Management System (MMS), and understands a syntax in its "
    @ $(ECHO) "description files which is a superset of that which is "
    @ $(ECHO) "understood by MMS."
    @ $(ECHO) ""
    @ $(ECHO) "MMK runs on VAX/VMS, OpenVMS VAX, OpenVMS AXP, and OpenVMS "
    @ $(ECHO) "Industry Standard 64."
    @ $(ECHO) "1 'NOTICE"
    @ $(ECHO) "=prompt Copyright (c) 2013 Endless Software Solutions."
    @ $(ECHO) "Copyright (c) 2008, Matthew Madison."
    @ $(ECHO) "Copyright (c) 2013, Endless Software Solutions."
    @ $(ECHO) ""
    @ $(ECHO) "All rights reserved."
    @ $(ECHO) ""
    @ $(ECHO) "Redistribution and use in source and binary forms, with or "
    @ $(ECHO) "without modification, are permitted provided that the "
    @ $(ECHO) "following conditions are met:"
    @ $(ECHO) ""
    @ $(ECHO) "    * Redistributions of source code must retain the above"
    @ $(ECHO) "      copyright notice, this list of conditions and the"
    @ $(ECHO) "      following disclaimer."
    @ $(ECHO) "    * Redistributions in binary form must reproduce the above"
    @ $(ECHO) "      copyright notice, this list of conditions and the"
    @ $(ECHO) "      following disclaimer in the documentation and/or other "
    @ $(ECHO) "      materials provided with the distribution."
    @ $(ECHO) "    * Neither the name of the copyright owner nor the names of"
    @ $(ECHO) "      any other contributors may be used to endorse or promote"
    @ $(ECHO) "      products derived from this software without specific"
    @ $(ECHO) "      prior written permission."
    @ $(ECHO) ""
    @ $(ECHO) "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND "
    @ $(ECHO) "CONTRIBUTORS ""AS IS"" AND ANY EXPRESS OR IMPLIED WARRANTIES,"
    @ $(ECHO) "INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF "
    @ $(ECHO) "MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE "
    @ $(ECHO) "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR "
    @ $(ECHO) "CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,"
    @ $(ECHO) "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT "
    @ $(ECHO) "NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; "
    @ $(ECHO) "LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) "
    @ $(ECHO) "HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN "
    @ $(ECHO) "CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR "
    @ $(ECHO) "OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,"
    @ $(ECHO) "EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
    @ $(ECHO) "1 'PRODUCER"
    @ $(ECHO) "=prompt This software is released by Endless Software Solutions."
    @ $(ECHO) "1 SOURCE"
    @ $(ECHO) "=prompt Do you want to install the source code?"
    @ $(ECHO) "MMK is open source software and includes the full source of"
    @ $(ECHO) "the software.  To install a ZIP file containing the source"
    @ $(ECHO) "code, answer YES to this option."
    @ $(ECHO) "1 DOCUMENTATION"
    @ $(ECHO) "=prompt Do you want to install the software documentation?"
    @ $(ECHO) "The full MMK documentation is included in this software kit."
    @ $(ECHO) "It is available in HTML, PDF, PostScript and Text formts.  To"
    @ $(ECHO) "install the documentation, answer YES to this option."
