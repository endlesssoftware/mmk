$! [MMK]KITINSTAL.COM
$!
$!  KITINSTAL procedure for installing MMK.
$!
$! Copyright (c) 2013, Endless Software Solutions.
$!
$! All rights reserved.
$!
$! Redistribution and use in source and binary forms, with or without
$! modification, are permitted provided that the following conditions
$! are met:
$!
$!     * Redistributions of source code must retain the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer.
$!     * Redistributions in binary form must reproduce the above
$!       copyright notice, this list of conditions and the following
$!       disclaimer in the documentation and/or other materials provided
$!       with the distribution.
$!     * Neither the name of the copyright owner nor the names of any
$!       other contributors may be used to endorse or promote products
$!       derived from this software without specific prior written
$!       permission.
$!
$! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
$! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
$! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
$! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
$! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
$! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
$! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
$! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
$! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
$! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
$! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
$!
$ on control_y then goto mmk_control_y
$ on warning then goto mmk_fail
$!
$ if (p1 .eqs. "VMI$_INSTALL") then goto mmk_install
$ if (f$element(0,"_",p1) .eqs. "HELP") then goto 'p1'
$ exit VMI$_UNSUPPORTED
$!
$MMK_CONTROL_Y:
$ VMI$CALLBACK CONTROL_Y
$!
$MMK_FAIL:
$ MMK_STATUS == $STATUS
$ EXIT 'MMK_STATUS
$!
$!.
$MMK_INSTALL:
$!
$ MMK_SAY := WRITE SYS$OUTPUT
$ DEFINE BIN_DIR VMI$KWD:
$!
$ IF tmp .GT. 0 .AND. tmp .LT. 1024
$ THEN
$   mmk_arch = "VAX"
$   mmk_system_type = 1
$   mmk_system_name = mmk_arch
$ ELSE
$   mmk_system_type = F$GETSYI ("ARCH_TYPE")
$   mmk_system_name = F$ELEMENT(mmk_system_type, ",", "OTHER,VAX,AXP,I64") - ","
$   mmk_arch = F$EDIT (F$GETSYI ("ARCH_NAME"), "TRIM,UPCASE")
$ ENDIF
$ IF mmk_arch .EQS. "VAX"
$ THEN
$   MMK_REQD_VMSVER = "V6.2"
$   MMK_REQD_VMSVER_OLD = "062"
$   base_saveset   = "B"
$ ENDIF
$ IF mmk_arch .EQS. "ALPHA"
$ THEN
$   MMK_REQD_VMSVER = "V1.5"
$   MMK_REQD_VMSVER_OLD = "015"
$   base_saveset   = "C"
$ ENDIF
$ IF mmk_arch .EQS. "IA64"
$ THEN
$   MMK_REQD_VMSVER = "V8.2"
$   MMK_REQD_VMSVER_OLD = "082"
$   base_saveset   = "D"
$ ENDIF
$ VMI$CALLBACK CHECK_VMS_VERSION MMK_VMSVEROK 'MMK_REQD_VMSVER_OLD'
$ IF .NOT. MMK_VMSVEROK
$ THEN
$   VMI$CALLBACK MESSAGE E VMSVER -
        "This product requires OpenVMS ''mmk_system_name' ''MMK_REQD_VMSVER' to run."
$   EXIT VMI$_FAILURE
$ ENDIF
$ OPEN/READ MMK_T VMI$KWD:MMK_INSTALLING_VERSION.DAT
$ READ MMK_T mmk_installing_version
$ READ MMK_T mmk_kit_version
$ CLOSE MMK_T
$!
$ mmk_say ""
$ mmk_say F$FAO("               MMK Make Utility !AS Installation Procedure",-
		mmk_installing_version)
$ TYPE SYS$INPUT:

        Copyright (c) 2008, Matthew Madison.
        Copyright (c) 2013, Endless Software Solutions.

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

    MMK is a "make" utility for VMS systems.  It is used for building
    software systems based on a "description file" (or "makefile") you
    create that lists the sources and objects of a system and the
    dependencies between them.

    MMK is similar in functionality to Digital's DEC/Module Management
    System (MMS), and understands a syntax in its description files which
    is a superset of that which is understood by MMS.

    MMK runs on VAX/VMS, OpenVMS VAX, OpenVMS AXP, and OpenVMS Industry
    Standard 64.

$!
$ mmk_upgrading = 0
$!
$ mmk_do_command = "YES"
$ mmk_do_help = "YES"
$ mmk_do_doc = "YES"
$ mmk_do_source = "NO"
$ mmk_do_startup = "YES"
$!
$ mmk_pcsi_db = "SYS$SYSTEM:ESS-''mmk_system_name'VMS-MMK-*.PCSI$DATABASE"
$ IF F$SEARCH(mmk_pcsi_db) .NES. ""
$ THEN
$   VMI$CALLBACK MESSAGE E PCSI -
	"This product has already been installed via PRODUCT INSTALL"
$   TYPE SYS$INPUT:

    **** WARNING! ****

    The installation has detected a previous installation via the
    POLYCENTER Software Installation utility (PCSI).  Either remove
    the product via the PRODUCT REMOVE command and restart the
    VMSINSTAL process, or continue to update the software using PCSI
    software kits.

$   EXIT VMI$_FAILURE
$ ENDIF
$!
$ if (f$search("SYS$STARTUP:MMK_STARTUP.COM") .nes. "") then -
$   @SYS$STARTUP:MMK_STARTUP
$ if (f$trnlnm("MMK") .eqs. "")
$ then
$   mmk_def_root = "SYS$COMMON:[MMK.]"
$ else
$   mmk_def_root = f$parse("MMK",,,"DEVICE","NO_CONCEAL") -
                 + f$parse("MMK",,,"DIRECTORY","NO_CONCEAL") -
                 - "[000000]"
$   if (f$search("MMK") .nes. "")
$   then
$     VMI$CALLBACK MESSAGE I INSTALDET -
	"An existing installation has been detected at ''mmk_def_root'"
$     mmk_upgrading = 1
$   endif
$ endif
$ mmk_def_root = mmk_def_root - ".]"
$!
$Ask_MMK_Top:
$ VMI$CALLBACK ASK mmk_root -
	"Where should the MMK root directory be located" -
	'mmk_def_root'
$ IF F$PARSE(mmk_root,"$$NOSUCHDEV$$:[$$NOSUCHDIR$$]",,"DEVICE","SYNTAX_ONLY") .EQS. "$$NOSUCHDEV$$:" .OR. -
     F$PARSE(mmk_root,"$$NOSUCHDEV$$:[$$NOSUCHDIR$$]",,"DIRECTORY","SYNTAX_ONLY") .EQS. "[$$NOSUCHDIR$$]" .OR. -
     F$PARSE(mmk_root,,,,"SYNTAX_ONLY") .EQS. "" .OR. -
     F$LOCATE(">[",mmk_root) .LT. F$LENGTH(mmk_root) .OR. F$LOCATE("]<",mmk_root) .LT. F$LENGTH(mmk_root)
$ THEN
$   TYPE SYS$INPUT:

    Please enter a device and directory specification.

$   GOTO Ask_MMK_Top
$ ENDIF
$!
$ if (mmk_upgrading .and. (mmk_def_root .eqs. mmk_root))
$ then
$   vmi$callback ask mmk_ok -
	"Do you want to upgrade the current installation" -
	"YES" B "@VMI$KWD:VMSINSTAL HELP_UPGRADE"
$   if (mmk_ok) then goto Ask_MMK_Top
$ endif
$!
$ VMI$CALLBACK ASK mmk_do_command -
	"Do you want to install the MMK command into DCLTABLES" -
	'mmk_do_command' B "@VMI$KWD:KITINSTAL HELP_COMMAND"
$!
$ VMI$CALLBACK ASK mmk_do_help -
	"Do you want to add MMK to the system help library" -
	'mmk_do_help' B "@VMI$KWD:KITINSTAL HELP_HELP"
$!
$ VMI$CALLBACK ASK mmk_do_doc -
	"Do you want to install the software documentation" -
	'mmk_do_doc' B "@VMI$KWD:KITINSTAL HELP_DOC"
$!
$ VMI$CALLBACK ASK mmk_do_source -
	"Do you want to install the source code" -
	'mmk_do_source' B "@VMI$KWD:KITINSTAL HELP_SOURCE"
$!
$ VMI$CALLBACK ASK mmk_do_startup -
	"Copy system startup procedure to SYS$STARTUP" -
	'mmk_do_startup' B "@VMI$KWD:KITINSTAL HELP_STARTUP"
$!
$ VMI$CALLBACK MESSAGE I INSTALL "Installing MMK software..."
$!
$ mmk_root = F$PARSE(mmk_root,,,"DEVICE","SYNTAX_ONLY") + -
              "[" + (F$EXTRACT(1,-1,F$PARSE(mmk_root,,,"DIRECTORY","SYNTAX_ONLY")) --
                         "][" - "><" - ">[" - "]<" - "]" - ">")
$ mmk_iroot = F$PARSE(mmk_root+"]",,,"DEVICE","SYNTAX_ONLY,NO_CONCEAL") + -
              "[" + (F$EXTRACT(1,-1,F$PARSE(mmk_root+"]",,,"DIRECTORY","NO_CONCEAL,SYNTAX_ONLY")) --
                         "][" - "><" - ">[" - "]<" - "]" - ">") + "]"
$ mmk_install_device = F$PARSE (mmk_iroot,,,"DEVICE")
$ mmk_install_root = "MMK_DEVICE:"+ F$PARSE (mmk_iroot,,,"DIRECTORY") - "]" + ".]"
$ DEFINE MMK_DEVICE 'mmk_install_device'/TRANSLATION=(CONCEALED,TERMINAL)
$ DEFINE MMK_INSTALL_ROOT 'mmk_install_root'/TRANSLATION=CONCEALED
$ DEFINE MMK_ROOT 'mmk_install_root'/TRANSLATION=CONCEALED
$!
$ IF F$PARSE ("''mmk_root']").eqs."" then -
    VMI$CALLBACK CREATE_DIRECTORY USER 'mmk_root'] -
                "/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:RE,W:E)"
$!
$ IF mmk_do_command THEN -
$    VMI$CALLBACK PROVIDE_DCL_COMMAND MMK_CLD.CLD
$
$ IF mmk_do_help THEN -
$    VMI$CALLBACK PROVIDE_DCL_HELP MMK_HELP.HLP
$!
$ VMI$CALLBACK RESTORE_SAVESET 'base_saveset'
$ VMI$CALLBACK PROVIDE_IMAGE MMK_TMP MMK.EXE 'mmk_root'] K
$!
$ IF mmk_do_doc
$ THEN
$   VMI$CALLBACK MESSAGE I INSTALL_DOC "Installing documentation files..."
$   IF F$PARSE("''mmk_root'.DOC]").eqs."" THEN -
        VMI$CALLBACK CREATE_DIRECTORY USER 'mmk_root'.DOC] -
                "/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:R,W:R)"
$   VMI$CALLBACK PROVIDE_FILE "" MMK_DOC_LIST.DAT "" T
$ ENDIF
$!
$ IF mmk_do_source
$ THEN
$   VMI$CALLBACK MESSAGE I INSTALL_SOURCE "Installing source kit..."
$   IF F$PARSE("''mmk_root'.SRC]").eqs."" THEN -
        VMI$CALLBACK CREATE_DIRECTORY USER 'mmk_root'.SRC] -
                "/OWNER=[1,4]/PROT=(S:RWE,O:RWE,G:R,W:R)"
$   VMI$CALLBACK RESTORE_SAVESET E
$   VMI$CALLBACK PROVIDE_FILE MMK_TMP MMK'mmk_kit_version'_SOURCE.ZIP -
	'mmk_root'.SRC]
$ ENDIF
$!
$ close/nolog sp
$ open/write sp VMI$KWD:MMK_STARTUP.COM
$ write sp "$! MMK Startup Procedure -- generated by VMSINSTAL at ''f$time()'"
$ write sp "$ set noon"
$ write sp "$ definee/system/nolog MMK ''mmk_root']MMK.EXE"
$ write sp "$ exitt 1"
$ close/nolog sp
$!
$ vmi$callback provide_file MMK_TMP MMK_STARTUP.COM 'mmk_root'] C
$!
$ if (mmk_do_startup)
$ then
$    vmi$callback provide_file MMK_TMP MMK_STARTUP.COM 'mmk_root'] C
$    vmi$callback set startup MMK_STARTUP.COM
$ endif
$
$ EXIT VMI$_SUCCESS
$!
$HELP_:
$ type SYS$INPUT:

    The following version of MMK has been detected by the
    installation procedure:

$ mmk/identification
$ type SYS$INPUT:

    To replace this installation with the software in this
    software installation kit, answer YES.  To chose another
    directory to install to, answer NO and enter a different
    path.  To exit this installation completely type ^C.

$ exit VMI$_SUCCESS
$help_command:
$ type sys$input:

    The MMK make utility can be installed into DCLTABLES making
    it a known command.  This is not a critical features of the
    product and it is easy enough to run MMK by simply defining
    a symbol (foreign command) to point to the executable.  To
    install MMK in the system command tables, answer YES to
    this question.

$ exit vmi$_success
$!
$help_help:
$ type sys$input:

    The MMK make utility installation kit includes an online
    help file that can be installed into the system help file.
    It can then be accessed with the HELP command.  This is
    not a critical feature of the product.  To install the MMK
    online help file in the system HELP library, answer YES
    to this question.

$ exit vmi$_success
$!
$help_doc:
$ type sys$input:

    The full MMK documentation is included in this software kit.
    It is available in HTML, PDF, PostScript and Text formts.  To
    install the documentation, answer YES to this option.

$ exit vmi$_success
$!
$help_source:
$ type sys$input:

    MMK is open source software and includes the full source of
    the software.  To install a ZIP file containing the source
    code, answer YES to this option.

$ exit vmi$_success
$!
$help_startup:
$ type sys$input:

    As part of the installation process MMK generates a small
    system startup procedure that needs to be executed before
    using MMK.  To make things more convenient, the install
    process can copy this procedure to SYS$STARTUP.  This is
    not a critical feature of the product.

$ exit vmi$_success
