case $2 in
  *.mli)
    ocamlfind ppx_tools/rewriter -ppx $1 -intf $2;;
  *.ml)
    ocamlfind ppx_tools/rewriter -ppx $1 -impl $2;;
  *)
    false;;
esac
