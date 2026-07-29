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
!   26-MAR-2013 V1.1    Sneddon     Change to put executables in arch-specific
!				     directory. See issue #57.
!--
TMP |= PIPE TMP="$(MMK_MAJOR_VERSION)" ; IF F$LENGTH(TMP) .EQ 1 THEN TMP="0"+TMP ; WRITE/SYMBOL SYS$OUTPUT TMP
KIT_VERSION = $(COLLAPSE $(TMP)$(MMK_MINOR_VERSION))
GENERATION |= WRITE SYS$OUTPUT F$CVTIME(,,"DATE")-"-"-"-"
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
    @ $(ECHO) "  information CHECK_DESTINATION confirm phase before "
    @ $(ECHO) "    with helptext ;"
    @ $(ECHO) "  directory [MMK] ;"
    @ $(ECHO) "  file [MMK]MMK___STARTUP.COM generation $(GENERATION) archive ;"
    @ $(ECHO) "  file [MMK]MMK$(KIT_VERSION).RELEASE_NOTES release notes ;"
    @ $(ECHO) "  execute preconfigure (""WRITE SYS$OUTPUT """"%I, moving release notes to SYS$HELP"""""","
    @ $(ECHO) "    ""COPY/LOG PCSI$SOURCE:[MMK]MMK$(KIT_VERSION).RELEASE_NOTES SYS$COMMON:[SYSHLP]"")"
    @ $(ECHO) "    uses [MMK]MMK$(KIT_VERSION).RELEASE_NOTES ;"
    @ $(ECHO) "  directory [MMK.$(KITARCH)_EXE] ;"
    @ $(ECHO) "  file [MMK.$(KITARCH)_EXE]MMK.EXE generation $(GENERATION) archive ;"
    @ $(ECHO) "  information RELEASE_NOTES phase after with helptext ;"
    @ $(ECHO) "  if (<option COMMAND default YES>) ;"
    @ $(ECHO) "    module [MMK]MMK_CLD.CLD type command module MMK ;"
    @ $(ECHO) "  else ;"
    @ $(ECHO) "    information NO_COMMAND phase after with helptext;"
    @ $(ECHO) "  end if ;"
    @ $(ECHO) "  option HELP default YES ;"
    @ $(ECHO) "    module [MMK]MMK_HELP.HLP type help module MMK ;"
    @ $(ECHO) "  end option ;"

DOCHEADER :
    @ $(ECHO) "  option DOCUMENTATION default YES ;"
    @ $(ECHO) "    directory [MMK.DOC] ;"

$(DOCUMENTATION) : ALWAYS_MAKE
    @ $(ECHO) "    file [MMK.DOC]$(NOTDIR $(MMS$TARGET))"
    @ $(ECHO) "      generation $(GENERATION) ;"

DOCFOOTER :
    @ $(ECHO) "  end option ;"

SOURCE :
    @ $(ECHO) "  option SOURCE default NO ;"
    @ $(ECHO) "    directory [MMK.SRC] ;"
    @ $(ECHO) "    file [MMK.SRC]MMK$(KIT_VERSION)_SOURCE.ZIP"
    @ $(ECHO) "      generation $(GENERATION) ;"
    @ $(ECHO) "  end option ;"

FOOTER :
    @ $(ECHO) "  execute "
    @ $(ECHO) "    postinstall ""@PCSI$SOURCE:[MMK]MMK_PCSI.COM POSTINSTALL"""
    @ $(ECHO) "    uses [MMK]MMK_PCSI.COM ;"
    @ $(ECHO) "  if (<option SYSTEM_STARTUP default YES>) ;
    @ $(ECHO) "    execute postinstall "
    @ $(ECHO) "      ""COPY/LOG PCSI$DESTINATION:[MMK]MMK_STARTUP.COM SYS$COMMON:[SYS$STARTUP]"" ;"
    @ $(ECHO) "  end if ;"
    @ $(ECHO) "  file [MMK]MMK_PCSI.COM ;"
    @ $(ECHO) "  execute install """""
    @ $(ECHO) "    remove ""@PCSI$DESTINATION:[MMK]MMK_PCSI.COM REMOVE"""
    @ $(ECHO) "    uses [MMK]MMK_PCSI.COM ;"
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
    @ $(ECHO) "1 RELEASE_NOTES"
    @ $(ECHO) "=prompt Release notes for MMK $(MMK_VERSION) are available."
    @ $(ECHO) "Release notes for MMK $(MMK_VERSION) are avilable in the "
    @ $(ECHO) "file SYS$HELP:MMK$(KIT_VERSION).RELEASE_NOTES."
    @ $(ECHO) "1 CHECK_DESTINATION"
    @ $(ECHO) "=prompt Confirm installation destination:"
    @ $(ECHO) "************************************************************"
    @ $(ECHO) ""
    @ $(ECHO) " This product, by default will install the MMK product tree"
    @ $(ECHO) " into SYS$SYSDEVICE:[VMS$COMMON.].  If you wish to install"
    @ $(ECHO) " in an alternate location, please answer NO to this"
    @ $(ECHO) " confirmation and reinstall using:"
    @ $(ECHO) ""
    @ $(ECHO) "   PRODUCT INSTALL MMK/DESTINATION=ddcu:[dir]"
    @ $(ECHO) ""
    @ $(ECHO) "************************************************************"
    @ $(ECHO) "1 COMMAND"
    @ $(ECHO) "=prompt Do you want to install the MMK command into DCLTABLES?"
    @ $(ECHO) "The MMK make utility can be installed into DCLTABLES making"
    @ $(ECHO) "it a known command.  This is not a critical features of the"
    @ $(ECHO) "product and it is easy enough to run MMK by simply defining"
    @ $(ECHO) "a symbol (foreign command) to point to the executable.  To "
    @ $(ECHO) "install MMK in the system command tables, answer YES to"
    @ $(ECHO) "this question."
    @ $(ECHO) "1 NO_COMMAND"
    @ $(ECHO) "=prompt MMK was not installed as a command."
    @ $(ECHO) "Because MMK was not installed as a command, you will need "
    @ $(ECHO) "to define a foreign command, like so:"
    @ $(ECHO) " "
    @ $(ECHO) "    $ MMK == ""$ddcu:[MMK.$(KITARCH)_EXE]MMK.EXE"""
    @ $(ECHO) " "
    @ $(ECHO) "1 HELP"
    @ $(ECHO) "=prompt Do you want to add MMK to the system help library?"
    @ $(ECHO) "The MMK make utility installation kit includes an online"
    @ $(ECHO) "help file that can be installed into the system help file."
    @ $(ECHO) "It can then be accessed with the HELP command.  This is"
    @ $(ECHO) "not a critical feature of the product.  To install the MMK"
    @ $(ECHO) "online help file in the system HELP library, answer YES"
    @ $(ECHO) "to this question."
    @ $(ECHO) "1 DOCUMENTATION"
    @ $(ECHO) "=prompt Do you want to install the software documentation?"
    @ $(ECHO) "The full MMK documentation is included in this software kit."
    @ $(ECHO) "It is available in HTML, PDF, PostScript and Text formts.  To"
    @ $(ECHO) "install the documentation, answer YES to this option."
    @ $(ECHO) "1 SOURCE"
    @ $(ECHO) "=prompt Do you want to install the source code?"
    @ $(ECHO) "MMK is open source software and includes the full source of"
    @ $(ECHO) "the software.  To install a ZIP file containing the source"
    @ $(ECHO) "code, answer YES to this option."
    @ $(ECHO) "1 SYSTEM_STARTUP"
    @ $(ECHO) "=prompt Copy system startup procedure to SYS$STARTUP?"
    @ $(ECHO) "As part of the installation process MMK generates a small "
    @ $(ECHO) "system startup procedure that needs to be executed before "
    @ $(ECHO) "using MMK.  To make things more convenient, the install "
    @ $(ECHO) "process can copy this procedure to SYS$STARTUP.  This is"
    @ $(ECHO) "not a critical feature of the product."
