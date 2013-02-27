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
$ ON CONTROL_Y THEN GOTO MMK_CONTROL_Y
$ ON WARNING THEN GOTO MMK_FAIL
$!
$ DELETE_SYMBOL := DELETE/SYMBOL
$!
$ IF P1 .EQS. "VMI$_INSTALL" THEN GOTO MMK_INSTALL
$ IF P1 .EQS. "VMI$_POSTINSTALL" THEN GOTO MMK_POSTINSTALL
$ EXIT VMI$_UNSUPPORTED
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
$   mmk_system_type = mmk_arch
$   opt = ".OPT"
$   mmk_exe_dir = "EXE"
$ ELSE
$   mmk_system_type = F$GETSYI ("ARCH_NAME")
$   mmk_arch = F$EDIT (mmk_system_type, "TRIM,UPCASE")
$   opt = ".''mmk_arch'_OPT"
$   mmk_exe_dir = "''mmk_arch'_EXE"
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
        "This product requires OpenVMS ''mmk_system_type' ''MMK_REQD_VMSVER' to run."
$   EXIT VMI$_FAILURE
$ ENDIF
$ OPEN/READ MMK_T VMI$KWD:MMK_INSTALLING_VERSION.DAT
$ READ MMK_T mmk___tmp
$ CLOSE MMK_T
$ IF F$LOCATE (":", mx___tmp) .LT. F$LENGTH (mx___tmp)
$ THEN
$   mx_installing_version = F$EDIT (F$ELEMENT (1, ":", mx___tmp), "TRIM,COMPRESS")
$ ELSE
$   mx_installing_version = mx___tmp
$ ENDIF
$! Pull off architecture name
$ mx_installing_version = f$element(1," ", mx_installing_version)
