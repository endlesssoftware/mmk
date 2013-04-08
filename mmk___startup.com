$ ! Procedure:	MMK___STARTUP.COM
$ __vfy = "VFY_''f$parse(f$environment("procedure"),,,"name")'"
$ if (f$type('__vfy') .eqs. "") then __vfy = 0
$ __vfy_saved = f$verify(&__vfy)
$
$err: subroutine
$ set noon
$ severity = f$edit(P1,"COLLAPSE,UPCASE")
$ identification = f$edit(P2,"COLLAPSE,UPCASE")
$ text = f$edit(P3,"TRIM")
$ continuation = (f$edit(P4,"COLLAPSE,UPCASE") .nes. "")
$ say = "write sys$output"
$ percent = "%"
$ if (continuation) then percent = "-"
$ if ((severity .eqs. "") .or. (identification .eqs. "") .or. (text .eqs. ""))
$ then say "%''facility'-F-SPOTTHEERR, <''severity'><''identification'><''text'>"
$ else say "''percent'''facility'-''severity'-''identification', ''text'"
$ endif
$ exitt 1
$ endsubroutine
$
$ procedure = f$element(0,";",f$environment("PROCEDURE"))
$ procedure_name = f$parse(procedure,,,"NAME")
$ facility = procedure_name
$ location = f$parse(procedure,,,"DEVICE","NO_CONCEAL") -
		+ f$parse(procedure,,,"DIRECTORY","NO_CONCEAL") -
		- "][" - ".]" - "]"
$
$ set noon
$ set symbol/scope=(nolocal)
$ on warning then goto bail_out
$ on control_y then goto bail_out
$
$ if ((f$getsyi("HW_MODEL") .gt. 0) .and. (f$getsyi("HW_MODEL") .lt. 1024))
$ then _arch_type = 1
$ else _arch_type = f$getsyi("ARCH_TYPE")
$ endif
$ _arch_name = f$element(_arch_type,",","OTHER,VAX,AXP,I64") - ","
$ _vax = (_arch_type .eq. 1)
$ _axp = (_arch_type .eq. 2)
$ _i64 = (_arch_type .eq. 3)
$ _other = (.not. (_vax .or. _axp .or. _i64))
$
$ dns = "define/nolog/system"
$ say = "write sys$output"
$ err = "call___ err"
$ saysym = "write/symbol sys$output"
$
$start:
$ dns/translation=concealed	MMK_ROOT	'location'.]
$ dns				MMK_EXE_AXP	MMK_ROOT:[EXE_AXP]
$ dns				MMK_EXE_I64	MMK_ROOT:[EXE_I64]
$ dns				MMK_EXE_VAX	MMK_ROOT:[EXE_VAX]
$ dns				MMK_EXE		MMK_EXE_'_arch_name':
$
$ dns				MMK		MMK_EXE:MMK.EXE
$
$ if (f$parse(mmk_root+".SRC]") .nes. "") then -
$ dns				MMK_SRC		MMK_ROOT:[SRC]
$ if (f$parse(mmk_root+".DOC]") .nes. "") then -
$ dns				MMK_DOC		MMK_ROOT:[DOC]
$
$bail_out:
$ exitt 1.or.(0*f$verify(__vfy_saved))
$ !+==========================================================================
$ !
$ !  FACILITY:   MMK
$ !
$ !  ABSTRACT:   MMK system startup procedure.
$ !
$ !  AUTHOR:         Tim Sneddon
$ !
$ !  Copyright (c) 2013, Endless Software Solutions.
$ !
$ !  All rights reserved.
$ !
$ !  Redistribution and use in source and binary forms, with or without
$ !  modification, are permitted provided that the following conditions
$ !  are met:
$ !
$ !      * Redistributions of source code must retain the above
$ !        copyright notice, this list of conditions and the following
$ !        disclaimer.
$ !      * Redistributions in binary form must reproduce the above
$ !        copyright notice, this list of conditions and the following
$ !        disclaimer in the documentation and/or other materials provided
$ !        with the distribution.
$ !      * Neither the name of the copyright owner nor the names of any
$ !        other contributors may be used to endorse or promote products
$ !        derived from this software without specific prior written
$ !        permission.
$ !
$ !  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
$ !  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
$ !  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
$ !  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
$ !  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
$ !  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
$ !  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
$ !  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
$ !  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
$ !  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
$ !  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
$ !
$ !  CREATION DATE:  07-APR-2013
$ !
$ !  MODIFICATION HISTORY:
$ !
$ !      07-APR-2013 V1.0    Sneddon     Initial coding.
$ !-==========================================================================
