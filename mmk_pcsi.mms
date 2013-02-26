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
PRODUCER = $(MG_PRODUCER)
PRODUCT  = MMK
VERSION  = $(MMK_VERSION)

ECHO = WRITE SYS$OUTPUT
.IF MMSALPHA
ARCH = AXPVMS
.ELSIF MMSIA64
ARCH = I64VMS
.ELSIF MMSVAX
ARCH = VAXVMS
.ENDIF
!DAYTIME != SHOW DAYTIME
!DATE = $(WORD 1,$(DAYTIME))
GEN = 20130226

BINDIR = MG_BIN:[MMK]
KITDIR = MG_KIT:[MMK]

A = $(FOREACH FILE,$(1),  file $(NOTDIR $(FILE)) generation $(GEN) archive ;)

DESCRIPTION :
    @ $(ECHO) "product $(PRODUCER) $(ARCH) $(PRODUCT) $(VERSION) full ;"
    @ $(ECHO) "$(WILDCARD $(BINDIR)*.EXE)"
    @ $(ECHO) "$(CALL A,$(WILDCARD $(BINDIR)*.EXE))"
    @ $(ECHO) "end product ;"
    @ CONTINUE

TEXT :
    @ CONTINUE
