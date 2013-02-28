$ IF p1 .EQS. ""
$ THEN doc_dir = "MG_KIT:[MMK]"
$ ELSE doc_dir = p1
$ ENDIF
$ IF p2 .EQS. ""
$ THEN outfile = "MG_KIT:[]MMK_DOC_LIST.DAT"
$ ELSE outfile = p2
$ ENDIF
$ create 'outfile
$ close/nolog mmk_doc_list
$ open/append mmk_doc_list 'outfile
$ write mmk_doc_list "!"
$ write mmk_doc_list "! MMK documentation files."
$ write mmk_doc_list "!"
$ call make_list "''DOC_DIR'MMK*.PS"
$ call make_list "''DOC_DIR'MMK*.PDF"
$ call make_list "''DOC_DIR'MMK*.TXT"
$ call make_list "''DOC_DIR'MMK*.HTML"
$ close/nolog mmk_doc_list
$ write sys$output "''outfile' created"
$ exit
$ MAKE_LIST: SUBROUTINE
$  _Loop:
$	file = f$search(p1)
$	if file.eqs."" then exit
$	name = f$parse(file,"","","NAME")+f$parse(file,"","","TYPE")
$	write mmk_doc_list f$fao("MMK_TMP !32AS VMI$ROOT:[SYSHLP.MMK]", name)
$	goto _loop
$ ENDSUBROUTINE
