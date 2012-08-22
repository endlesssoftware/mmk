! MMK_DEFAULT_RULES_AXP.MMS
!
!   COPYRIGHT � 1993, 1997  MADGOAT SOFTWARE.  ALL RIGHTS RESERVED.
!   COPYRIGHT � 2012 ENDLESS SOFTWARE SOLUTIONS.  ALL RIGHTS RESERVED.
!
!  Default build rules for use with MMK.  (for OpenVMS AXP)
!
!  Modification history:
!
!   23-DEC-1992	V1.0	Madison	    Initial coding.
!   17-OCT-1993	V1.1	Madison	    Delete intermediate libfiles.
!   11-APR-1994	V1.2	Madison     Make rules more like MMS's.
!   05-JUL-1994	V2.0	Madison	    Add CMS support.
!   16-JUL-1994	V2.1	Madison	    Update for V3.2.
!   22-AUG-1994	V2.1-1	Madison	    Eliminate DELETE_SOURCE checks.
!   14-OCT-1994	V2.2	Madison	    Add CXX support.
!   28-DEC-1994	V2.3	Madison	    Make IF commands silent.
!   20-JUN-1997	V2.3-1	Madison	    Add .MAR.MLB inference rule.
!   22-AUG-2012 V2.4    Sneddon	    Add GENCAT support.
!

!
! This symbol can be used to distinguish MMK from DEC's DEC/MMS product
! using .IFDEF directives.
!
__MATTS_MMS__ = __MATTS_MMS__
__MMK__ = __MMK__
__MMK_V32__ = 1
!
! These symbols can be used to distinguish an AXP-based build from a
! VAX-based build.
!
__ALPHA__ = 1
__AXP__   = 1

EXE = .EXE
OLB = .OLB
OBJ = .OBJ
OPT = .OPT
L32 = .L32

.SUFFIXES :     ! clear the suffix list first
.SUFFIXES : $(EXE) $(OLB) $(OBJ) .TLB .HLB .MLB $(L32) .CAT .C .CXX .BAS .B32 .BLI .FOR -
    	    .COB .COR .DBL .RPG .SCN .PLI .PEN .PAS .MAC .MAR .M64 .MSG .MSGX .CLD -
    	    .R32 .REQ .TXT .H .MEM .HLP .RNH .RNO .MMS .DAT .OPT .SDML .COM -
    	    .C~ .CXX~ .BAS~ .B32~ .BLI~ .FOR~ .COB~ .COR~ .DBL~ .RPG~ .SCN~ -
    	    .PLI~ .PAS~ .MAC~ .MAR~ .M64~ .MSG~ .CLD~ .R32~ .REQ~ .TXT~ -
    	    .H~ .HLP~ .RNH~ .RNO~ .MMS~ .DAT~ .OPT~ .SDML~ .COM~

LINK	    = LINK
LINKFLAGS   = /EXEC=$(MMS$TARGET)

$(OBJ)$(OLB) :
    @ IF F$SEARCH("$(MMS$TARGET)") .EQS. "" THEN $(LIBR)/CREATE $(MMS$TARGET)
    $(LIBR)$(LIBRFLAGS) $(MMS$TARGET) $(MMS$SOURCE)

.TXT.TLB :
    @ IF F$SEARCH("$(MMS$TARGET)") .EQS. "" THEN $(LIBR)/CREATE/TEXT $(MMS$TARGET)
    $(LIBR)$(LIBRFLAGS) $(MMS$TARGET) $(MMS$SOURCE)/MODULE=$(MMS$TARGET_MODULE)

.HLP.HLB :
    @ IF F$SEARCH("$(MMS$TARGET)") .EQS. "" THEN $(LIBR)/CREATE/HELP $(MMS$TARGET)
    $(LIBR)$(LIBRFLAGS) $(MMS$TARGET) $(MMS$SOURCE)

.MAC.MLB :
    @ IF F$SEARCH("$(MMS$TARGET)") .EQS. "" THEN $(LIBR)/CREATE/MACRO $(MMS$TARGET)
    $(LIBR)$(LIBRFLAGS) $(MMS$TARGET) $(MMS$SOURCE)

.MAR.MLB :
    @ IF F$SEARCH("$(MMS$TARGET)") .EQS. "" THEN $(LIBR)/CREATE/MACRO $(MMS$TARGET)
    $(LIBR)$(LIBRFLAGS) $(MMS$TARGET) $(MMS$SOURCE)

LIBR	    = LIBRARY
LIBRFLAGS   = /REPLACE


.BAS$(OBJ) :
    $(BASIC)$(BASFLAGS) $(MMS$SOURCE)
BASIC	    = BASIC
BASFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.BLI$(OBJ) :
    $(BLISS)$(BFLAGS) $(MMS$SOURCE)
.B32$(OBJ) :
    $(BLISS)$(BFLAGS) $(MMS$SOURCE)
BFLAGS	    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.C$(OBJ) :
    $(CC)$(CFLAGS) $(MMS$SOURCE)
CC  	    = CC
CFLAGS	    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.COB$(OBJ) :
    $(COBOL)$(COBFLAGS) $(MMS$SOURCE)
COBOL	    = COBOL
COBFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.COR$(OBJ) :
    $(CORAL)$(CORFLAGS) $(MMS$SOURCE)
CORAL	    = CORAL
CORFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.CXX$(OBJ) :
    $(CXX)$(CXXFLAGS) $(MMS$SOURCE)
CXX 	    = CXX
CXXFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.DBL$(OBJ) :
    $(DIBOL)$(DBLFLAGS) $(MMS$SOURCE)
DIBOL	    = DIBOL
DBLFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.CLD$(OBJ) :
    $(SETCMD)$(SETCMDFLAGS) $(MMS$SOURCE)
SETCMD	    = SET COMMAND
SETCMDFLAGS = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.FOR$(OBJ) :
    $(FORT)$(FFLAGS) $(MMS$SOURCE)
FORT   	    = FORTRAN
FFLAGS 	    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.MAR$(OBJ) :
    $(MACRO)$(MFLAGS) $(MMS$SOURCE)
MACRO	    = MACRO/MIGRATION
MFLAGS	    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)

.M64$(OBJ) :
    $(TASM)$(TASMFLAGS) $(MMS$SOURCE)
TASM	    = MACRO
TASMFLAGS   = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.MSG$(OBJ) : 
    $(MESSAGE)$(MSGFLAGS) $(MMS$SOURCE)
MESSAGE     = MESSAGE
MSGFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.PAS$(OBJ) :
    $(PASCAL)$(PFLAGS) $(MMS$SOURCE)
.PAS.PEN   :
    $(PASCAL)$(PENVFLAGS) $(MMS$SOURCE)
PASCAL	    = PASCAL
PFLAGS	    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)
PENVFLAGS   = /ENVIRONMENT=$(MMS$TARGET_NAME).ENV/NOLIST


.PLI$(OBJ) :
    $(PLI)$(PLIFLAGS) $(MMS$SOURCE)
PLI 	    = PLI
PLIFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.REQ$(L32) :
    $(BLISS)/LIBR=$(MMS$TARGET_NAME)$(L32)$(BLIBFLAGS) $(MMS$SOURCE)
.R32$(L32) :
    $(BLISS)/LIBR=$(MMS$TARGET_NAME)$(L32)$(BLIBFLAGS) $(MMS$SOURCE)
BLISS	    = BLISS
BLIBFLAGS   = /NOLIST


.RPG$(OBJ) :
    $(RPG)$(RPGFLAGS) $(MMS$SOURCE)
RPG 	    = RPG
RPGFLAGS    = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.RNH.HLP :
    $(RUNOFF)$(RFLAGS) $(MMS$SOURCE)
.RNO.MEM :
    $(RUNOFF)$(RFLAGS) $(MMS$SOURCE)
RUNOFF	    = RUNOFF
RFLAGS	    = /OUTPUT=$(MMS$TARGET)


.SCN$(OBJ) :
    $(SCAN)$(SCANFLAGS) $(MMS$SOURCE)
SCAN	    = SCAN
SCANFLAGS   = /NOLIST/OBJECT=$(MMS$TARGET_NAME)$(OBJ)


.MSGX.CAT :
    $(GENCAT)$(GENCATFLAGS) $(MMS$SOURCE) $(MMS$TARGET)
GENCAT	    = GENCAT
GENCATFLAGS =


CMS 	    = CMS
CMSCOMMENT  = ""
CMSFLAGS    = /GENERATION=$(MMS$CMS_GEN)

.B32~.B32 :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).B32 $(CMSFLAGS) $(CMSCOMMENT)

.BAS~.BAS :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).BAS $(CMSFLAGS) $(CMSCOMMENT)

.BLI~.BLI :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).BLI $(CMSFLAGS) $(CMSCOMMENT)

.C~.C :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).C $(CMSFLAGS) $(CMSCOMMENT)

.CLD~.CLD :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).CLD $(CMSFLAGS) $(CMSCOMMENT)

.COB~.COB :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).COB $(CMSFLAGS) $(CMSCOMMENT)

.COR~.COR :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).COR $(CMSFLAGS) $(CMSCOMMENT)

.COM~.COM :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).COM $(CMSFLAGS) $(CMSCOMMENT)

.CXX~.CXX :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).CXX $(CMSFLAGS) $(CMSCOMMENT)

.DAT~.DAT :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).DAT $(CMSFLAGS) $(CMSCOMMENT)

.DBL~.DBL :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).DBL $(CMSFLAGS) $(CMSCOMMENT)

.FOR~.FOR :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).FOR $(CMSFLAGS) $(CMSCOMMENT)

.H~.H :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).H $(CMSFLAGS) $(CMSCOMMENT)

.HLP~.HLP :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).HLP $(CMSFLAGS) $(CMSCOMMENT)

.MAC~.MAC :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).MAC $(CMSFLAGS) $(CMSCOMMENT)

.MAR~.MAR :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).MAR $(CMSFLAGS) $(CMSCOMMENT)

.M64~.M64 :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).M64 $(CMSFLAGS) $(CMSCOMMENT)

.MMS~.MMS :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).MMS $(CMSFLAGS) $(CMSCOMMENT)

.MSG~.MSG :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).MSG $(CMSFLAGS) $(CMSCOMMENT)

.MSGX~.MSGX :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).MSGX $(CMSFLAGS) $(CMSCOMMENT)

.OPT~.OPT :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).OPT $(CMSFLAGS) $(CMSCOMMENT)

.PAS~.PAS :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).PAS $(CMSFLAGS) $(CMSCOMMENT)

.PLI~.PLI :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).PLI $(CMSFLAGS) $(CMSCOMMENT)

.R32~.R32 :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).R32 $(CMSFLAGS) $(CMSCOMMENT)

.REQ~.REQ :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).REQ $(CMSFLAGS) $(CMSCOMMENT)

.RNH~.RNH :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).RNH $(CMSFLAGS) $(CMSCOMMENT)

.RNO~.RNO :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).RNO $(CMSFLAGS) $(CMSCOMMENT)

.SCN~.SCN :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).SCN $(CMSFLAGS) $(CMSCOMMENT)

.SDML~.SDML :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).SDML $(CMSFLAGS) $(CMSCOMMENT)

.TXT~.TXT :
	@ IF "$(MMS$CMS_LIBRARY)" .NES. "" THEN DEFINE/USER CMS$LIB $(MMS$CMS_LIBRARY)
	$(CMS) FETCH $(MMS$CMS_ELEMENT) /OUTPUT=$(MMS$TARGET_NAME).TXT $(CMSFLAGS) $(CMSCOMMENT)
