/*
** MMK.H
**
**  Main #include file for the MMK Make utilty.
**
**  Copyright (c) 2008, Matthew Madison.
**  Copyright (c) 2014, Endless Software Solutions.
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
**  MODIFICATION HISTORY:
**
**  27-Aug-1992	    Madison 	Initial commenting.
**  02-Apr-1993	    Madison 	Add new flag to CMD structure.
**  22-Oct-1993	    Madison 	Better support for deleting intermediates.
**  01-JUL-1994	    Madison 	Add CMS support.
**  14-JUL-1994	    Madison 	Add prefixed inference rule support.
**  21-JUN-1995	    Madison 	Beefed up symbol support.
**  03-OCT-1995	    Madison 	Let symbol name be up to 39 chars.
**  22-DEC-1996	    Madison 	Add support for rules with must-execute commands.
**  27-DEC-1998	    Madison 	Add prototypes.
**  02-JUN-2009	    Sneddon 	Add type to SYMBOL as well as MMK_K_SYM_CLI
**				and MMK_K_SYM_DEFAULT.
**  03-JUN-2009	    Sneddon	Added MMK_K_SYM_TEMPORARY and SDESC.
**  07-APR-2010	    Sneddon 	Updated definition of Define_Symbol.
**  17-DEC-2010     Sneddon	Added cat function.
**  10-FEB-2011	    Sneddon	Added itoa function.
**  12-APR-2011	    Sneddon     Added trim function.
**  02-JUN-2012	    Sneddon	Update find_char definition. Remove MMK_S_MODULE.
**  12-JUL-2012	    Sneddon	Add sp_once.
**  13-JUL-2012	    Sneddon	Fix length for SDESC.
**  14-JUL-2012	    Sneddon	Adjust testing for MMK_S_DCL depending on VMS
**				 version.  Is based on WRK_C_INPBUFSIZ in
**				 [DCL]DCLDEF.SDL (VMS source).
**  07-FEB-2013	    Sneddon	Tweaked Resolve_Symbols definition.
**  09-JUN-2014	    Sneddon	Add length argument to find_suffix.
**  12-JUN-2014	    Sneddon	Add create_suffix and set_mmssuffixes.
*/
#ifndef mmk_h__
#define mmk_h__
/*
** Generic stuff first
*/
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
/*
**  Status codes
*/
#include "mmk_msg.h"

/*
** Some private type names that I use sometimes
*/
#include <descrip.h>
#include <rms.h>
#include <stsdef.h>
#include <ssdef.h>
#include <lib$routines.h>
#include <str$routines.h>
#include <starlet.h>
#include <builtins.h>
#define EXTERN	    globalref
#define EXTERNAL    globalref
#define GLOBAL	    globaldef

typedef unsigned long	LONG;
typedef unsigned short	WORD;
typedef	unsigned char	BYTE;
typedef void 	    	*POINTER;
typedef struct { LONG long1, long2; } TIME;
typedef struct dsc$descriptor DESCRIP;
typedef struct { WORD bufsiz, itmcod; POINTER bufadr, retlen; } ITMLST;

/*
** Handy macros
*/
#define OK(s) $VMS_STATUS_SUCCESS(s)
#define INIT_DYNDSCPTR(str) {str->dsc$w_length = 0; str->dsc$a_pointer = (void *) 0;\
    	    str->dsc$b_class = DSC$K_CLASS_D; str->dsc$b_dtype = DSC$K_DTYPE_T;}
#define INIT_DYNDESC(str) {str.dsc$w_length = 0; str.dsc$a_pointer = (void *) 0;\
    	    str.dsc$b_class = DSC$K_CLASS_D; str.dsc$b_dtype = DSC$K_DTYPE_T;}
#define INIT_SDESC(str,len,ptr) {str.dsc$w_length=(len);str.dsc$a_pointer=(void *)(ptr);\
    	    str.dsc$b_class=DSC$K_CLASS_S; str.dsc$b_dtype=DSC$K_DTYPE_T;}
#define ITMLST_INIT(itm,c,s,a,r) {itm.bufsiz=(s); itm.itmcod=(c);\
    	    itm.bufadr=(POINTER)(a); itm.retlen=(POINTER)(r);}
#define SDESC(s) {sizeof(s)-1,DSC$K_DTYPE_T,DSC$K_CLASS_S,s}

#ifdef min
#undef min
#endif
#define min(a, b) (((a) < (b)) ? (a) : (b))

#ifdef max
#undef max
#endif
#define max(a, b) (((a) > (b)) ? (a) : (b))

#define queue_empty(addr) ((((struct QUE *)(addr))->head) == (((struct QUE *)(addr))->tail))
#if defined(__ALPHA) || defined(__ia64__)
#define queue_insert(item,pred) __PAL_INSQUEL((void *)(pred),(void *)(item))
#define queue_remove(entry,addr) (((struct QUE *)(entry))->head == \
   (struct QUE *) (entry) ? 0:(__PAL_REMQUEL((void *)(entry),(void *)(addr)),1))
#else
#define queue_insert(item,pred) _INSQUE(item,pred)
#define queue_remove(entry,addr) (((struct QUE *)entry)->head == entry ? 0 :\
    	    	    	    (_REMQUE(entry,addr),1))
#endif

/*
** Structure definitions.  The following size definitions are just
** for the sizes of names for things, not for the structures themselves.
*/
#define MMK_S_SYMBOL	237	/* 237 to allow for file names + trailing null */
#if defined(__ALPHA) || defined(__ia64__)
#define MMK_S_SFX       237     /* 235 + leading dot + trailing null */
#define MMK_S_FILE      4096    /* 4095 is RMS limit, + trailing null */
#if __VMS_VER >= 70320022	/* OpenVMS Alpha V7.3-2 and higher has... */
#define MMK_S_DCL	4097	/*  4096 is DCL command line + trailing null */
#else				/* OpenVMS Alpha V7.3-1 and lower has... */
#define MMK_S_DCL	256	/* 255 is DCL command line + trailing null */
#endif
#define MMK_S_MAXRSS	NAML$C_MAXRSS
#else
#define MMK_S_SFX       41      /* 39 + leading dot + trailing null */
#define MMK_S_FILE      256     /* 255 is RMS limit, + trailing null */
#define MMK_S_DCL	256	/* 255 is DCL command line + trailing null */
#define MMK_S_MAXRSS	NAML$C_MAXRSS
#endif

/*
** Generic absolute queue header
*/

    struct QUE {
    	void *head, *tail;
    };

#define INIT_QUEUE(_q) {struct QUE *__q = (struct QUE *)&(_q);\
    	    	    	__q->head = __q->tail = __q;}

/*
** Symbol definition.  We don't get a whole lot of symbols defined,
** so we just store them in a regular linked list.  The value string
** is dynamically allocated.
*/

    enum Symbol_Types {
    	MMK_K_SYM_BUILTIN, MMK_K_SYM_DESCRIP,
    	MMK_K_SYM_CMDLINE, MMK_K_SYM_LOCAL,
	MMK_K_SYM_DEFAULT, MMK_K_SYM_CLI,
	MMK_K_SYM_TEMPORARY
    };
    typedef enum Symbol_Types SYMTYPE;

    struct SYMBOL {
    	struct SYMBOL *flink, *blink;
	SYMTYPE type;
    	char name[MMK_S_SYMBOL];
    	char *value;
    };

#define MMK_K_SYMTABLE_SIZE 	256
    struct SYMTABLE {
	struct SYMTABLE *next;
    	struct QUE symlist[MMK_K_SYMTABLE_SIZE];
    };

/*
** Suffix defintion.  Linked list of suffixes is derived from .SUFFIX
** directive.
*/

    struct SFX {
    	struct SFX *flink, *blink;
    	char value[MMK_S_SFX];
    };

/*
** Command line definition.  The command string is dynamically allocated.
*/

#define CMD_M_NOECHO	    1
#define CMD_M_IGNERR	    2
#define CMD_M_FORCED_FIRST  4
#define CMD_M_FORCED_LAST   8

    struct CMD {
    	struct CMD *flink, *blink;
    	unsigned int flags;
    	char *cmd;
    };

/*
** Default build rule definition.  Lookup is based on source suffix and
** target suffix.  Provides the commands used to build the target from
** the source, in the absence of any commands provided by a dependency
** rule.
*/

    struct RULE {
    	struct RULE *flink, *blink;
	struct RULE *next, *parent;
    	int srcpfxlen, trgpfxlen;
    	char src[MMK_S_SFX];
    	char trg[MMK_S_SFX];
	char srcpfx[MMK_S_FILE];
	char trgpfx[MMK_S_FILE];
    	struct CMD cmdque;
    };

/*
** Object definition.  An object can be a file, a module from a library,
** a library (which is just a file, really), or just a generic target
** name.  One data structure handles all types.
*/

#define MMK_K_OBJ_FILE	    1
#define MMK_K_OBJ_LIBMOD    2
#define MMK_K_OBJ_LIB	    3
#define MMK_K_OBJ_GENERIC   4
#define MMK_K_OBJ_CMSFILE   5

    struct OBJECT {
    	struct OBJECT *flink, *blink;
    	unsigned int reserved;  /* add nothing above this point! */
    	int type;
    	int build_in_progress;
    	TIME rdt;
    	struct QUE libmodque;	  /* Back pointer(s) to library module(s) */
    	int have_rdt;
    	int libfile_built;  	  /* OK to delete a library module's intermediate */
    	struct OBJECT *libfile;   /* for LIBMOD type, pointer to LIB object */
    	struct OBJECT *libmodobj; /* ref to LIBMOD object for use during build */
    	struct OBJECT *fileobj;   /* for LIBMOD type, pointer to file object */
    	char sfx[MMK_S_SFX];      /* not used for LIBMOD type */
    	char name[MMK_S_FILE];
    	char cms_gen[MMK_S_SYMBOL]; /* used only for CMSFILE type */
    };

/*
** Object reference definition.  Used in source lists in dependency rules
** to reference an actual object.
*/

    struct OBJREF {
    	struct OBJREF *flink, *blink;
    	struct OBJECT *obj;
    };

/*
** Dependency rule definition.  A single object can depend on several sources.
** When dependencies with multiple targets are given in a description file,
** each target gets its own dependency rule, and the common sources are
** duplicated into each rule.  Command lists are shared in the multiple-target
** case, though.
*/

    struct DEPEND {
    	struct DEPEND *flink, *blink;
    	struct DEPEND *dc_flink;
    	struct DEPEND *deplist_flink;
    	struct OBJECT *target;
    	struct OBJREF sources;
    	struct CMD    *cmdqptr;
    	int double_colon;
    };

/*
**  Prototypes
*/

/*
**  MEM
*/
    struct CMD * mem_get_cmd(void);
    void mem_free_cmd(struct CMD *c);
    struct DEPEND * mem_get_depend(void);
    void mem_free_depend(struct DEPEND *dep);
    struct SYMBOL * mem_get_symbol(void);
    void mem_free_symbol(struct SYMBOL *sym);
    struct RULE * mem_get_rule(void);
    void mem_free_rule(struct RULE *r);
    struct OBJECT * mem_get_object(void);
    void mem_free_object(struct OBJECT *obj);
    struct OBJREF * mem_get_objref(void);
    void mem_free_objref(struct OBJREF *obj);
    struct SFX * mem_get_sfx(void);
    void mem_free_sfx(struct SFX *s);
    struct SYMTABLE * mem_get_symtable(void);
    void mem_free_symtable(struct SYMTABLE *symtable);
/*
**  FILEIO
*/
#ifndef FILEIO_MODULE_BUILD
typedef POINTER FILEHANDLE;
#endif
    unsigned int file_create(char *, FILEHANDLE *, char *);
    unsigned int file_create_share(char *, FILEHANDLE *, char *);
    unsigned int file_open(char *, FILEHANDLE *, char *, char *, int *);
    unsigned int file_find(char *, char *, char *, unsigned char *);
    unsigned int file_exists(char *, char *);
    unsigned int file_close(FILEHANDLE);
    unsigned int file_dclose(FILEHANDLE);
    unsigned int file_read(FILEHANDLE, char *, int, int *);
    unsigned int file_write(FILEHANDLE, char *, int);
    unsigned int file_getpos(FILEHANDLE, unsigned short[3]);
    unsigned int file_setpos(FILEHANDLE, unsigned short[3]);
    unsigned int file_get_rdt(char *, TIME *);
    unsigned int file_get_filespec(FILEHANDLE, char *, int);
/*
**  SP_MGR
*/
#ifndef SP_MGR_MODULE_BUILD
typedef POINTER SPHANDLE;
#endif
    unsigned int sp_open(SPHANDLE *ctxpp, void *inicmd, unsigned int (*rcvast)(void *), void *rcvastprm);
    unsigned int sp_close(SPHANDLE *ctxpp);
    unsigned int sp_send(SPHANDLE *ctxpp, void *cmdstr);
    unsigned int sp_receive(SPHANDLE *ctxpp, void *rcvstr, int *rcvlen);
    void sp_once (void *cmd, void (*actrtn)(void *, struct dsc$descriptor *), void *param);
    unsigned int sp_show_subprocess(SPHANDLE ctx);
/*
**  GET_RDT
*/
    unsigned int lbr_get_rdt(char *lib, char *mod, TIME *rdt);
    void lbr_flush(void);
    unsigned int get_rdt(struct OBJECT *obj);
/*
**  SYMBOLS
*/
    struct SYMBOL * Lookup_Symbol(char *name);
    void Define_Symbol(SYMTYPE symtype, char *name, char *val, int vallen, ...);
    int Resolve_Symbols(char *in, int inlen, char * *out, int *outlen, int dont_resolve_unknowns);
    void Clear_Local_Symbols(void);
    void Create_Local_Symbols(struct DEPEND *dep, struct OBJREF *srcref, struct QUE *chgque);
    void set_mmssuffixes(void);
/*
**  OBJECTS
*/
    struct OBJECT * Find_Object(struct OBJECT *template);
    void Insert_Object(struct OBJECT *obj);
/*
**  PARSE_OBJECTS
*/
    void Parse_Objects(char *line, int linelen, struct QUE *objque, int is_target);
/*
**  PARSE_DESCRIP
*/
    void parse_descrip(char *xline, int xlinelen, FILEHANDLE *newu, int *newmaxl,
    	    	    	int current_line, char *current_file);
/*
**  READDESC
*/
    void Read_Description(char *fspec, char *defspec, int rules_file);
/*
**  MISC
*/
    void Build_Suffix_List(char *line, int linelen);
    char * itoa(int);
    char * cat(char *dest, ...);
    char * trim(char *s);
    char * find_char(char *base, char *end, char *charset);
    void upcase(char *str);
    int extract_name(char *dest, char *src);
    int extract_prefix(char *dest, char *src);
    int extract_filetype(char *dest, char *src);
    int extract_filename(char *dest, char *src);
    int extract_nametype(char *dest, char *src);
    int prefix_match(char *pfx, char *fspec);
    struct SFX * find_suffix(char *name, int len);
    int create_suffix(char *name, int len, struct SFX *pos);
    struct RULE * find_rule(char *trg, char *src);
    struct RULE * find_rule_with_prefixes(struct OBJECT *trg, struct OBJECT *src);
    struct RULE * scan_rule_list(struct RULE *base, char *target_name, int generalize);
    int make_object_name(char *name, struct OBJECT *obj);
    int logical_present(char *lognam);
    int get_logical(char *lognam, char *buf, int bufsize);
    int strneql_case_blind(char *s1, char *s2, int len);
    void set_ctrlt_ast(unsigned int (*routine)(void *), void *arg);
    void clear_ctrlt_ast(void);
    unsigned int find_image_symbol(char *image, char *symbol, void *symval);
/*
**  CMS_INTERFACE
*/
    unsigned int cms_init(void);
    unsigned int cms_get_rdt(char *, char *, TIME *);
    unsigned int cms_fetch_file(char *, char *);
    unsigned int cms_parse_name(char *, char *, int, int *, char *, int, int *, int);

/*
**  Main module (MMK)
*/
    unsigned int put_output(void *);
    unsigned int put_command(void *);
/*
**  BUILD_TARGET
*/
    void Build_Target(char *name);
    struct DEPEND *find_dependency(struct OBJECT *obj, int fakeit);
    void close_subprocess(void);

#endif /* mmk_h__ */
