#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"jenga"
  [ oasis_lib "jenga_lib"
  ; oasis_lib "tenacious_lib"
  ; file "META" ~section:"lib"
  ; oasis_exe "jenga" ~dest:"jenga"
  ]
