(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  10.    export
  91.    import
  103.   functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature TokSig =
sig
    datatype tok =
        KEY_abstype
      | KEY_and
      | KEY_andalso
      | KEY_as
      | KEY_case
      | KEY_datatype
      | KEY_do
      | KEY_else
      | KEY_end
      | KEY_eqtype
      | KEY_exception
      | KEY_fn
      | KEY_fun
      | KEY_functor
      | KEY_handle
      | KEY_if
      | KEY_in
      | KEY_include
      | KEY_infix
      | KEY_infixr
      | KEY_let
      | KEY_local
      | KEY_nonfix
      | KEY_of
      | KEY_op
      | KEY_open
      | KEY_orelse
      | KEY_raise
      | KEY_rec
      | KEY_sharing
      | KEY_sig
      | KEY_signature
      | KEY_struct
      | KEY_structure
      | KEY_then
      | KEY_type
      | KEY_val
      | KEY_where
      | KEY_while
      | KEY_with
      | KEY_withtype
      | SYM_COLON
      | SYM_COLON_GT
      | SYM_COMMA
      | SYM_DOT
      | SYM_DOTS
      | SYM_EQ
      | SYM_FUNC_ARROW
      | SYM_HASH
      | SYM_HASH_LS
      | SYM_LB
      | SYM_LP
      | SYM_LS
      | SYM_RB
      | SYM_RP
      | SYM_RS
      | SYM_SEMI
      | SYM_STAR
      | SYM_TYPE_ARROW
      | SYM_UNDERSCORE
      | SYM_VBAR
      | TOK'CharLit     of char
      | TOK'Err         of string
      | TOK'Ident       of string
      | TOK'Number      of int
      | TOK'StringLit   of string
      | TOK'Symid       of string
      | TOK'TyVar       of string
      | TOK'Unknown     of char
      | TOK'Word        of word

    val sofChar : char -> string
    val sofStringLit : string -> string
    val sofTok : tok -> string
end;

(*----------------------------------------------------------------------
INDEX: import
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
    val stringOfInt     : int -> string
    val sofChar         : char -> string
    val sofStringLit    : string -> string
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor TOK(structure Prel : PrelSig_Import) : TokSig =
struct

  open Prel

  datatype tok =
      TOK'Ident of string
    | TOK'Symid of string (* why distinguish from Ident ? -- because
                           module level identifiers (strid,sigid,funid)
                           may not be symbolic *)
    | TOK'TyVar of string
    | TOK'Number of int
    | TOK'Word of word
    | TOK'CharLit of char
    | TOK'StringLit of string
    | TOK'Unknown of char
    | TOK'Err of string
      (* keywords *)
    | KEY_abstype
    | KEY_and
    | KEY_andalso
    | KEY_as
    | KEY_case
    | KEY_do
    | KEY_datatype
    | KEY_else
    | KEY_end
    | KEY_exception
    | KEY_fn
    | KEY_fun
    | KEY_handle
    | KEY_if
    | KEY_in
    | KEY_infix
    | KEY_infixr
    | KEY_let
    | KEY_local
    | KEY_nonfix
    | KEY_of
    | KEY_op
    | KEY_open
    | KEY_orelse
    | KEY_raise
    | KEY_rec
    | KEY_then
    | KEY_type
    | KEY_val
    | KEY_with
    | KEY_withtype
    | KEY_while
    (* 1-char symbols *)
    | SYM_LP
    | SYM_RP
    | SYM_LS
    | SYM_RS
    | SYM_LB
    | SYM_RB
    | SYM_COMMA
    | SYM_COLON
    | SYM_SEMI
    | SYM_UNDERSCORE
    | SYM_VBAR
    | SYM_EQ
    | SYM_HASH
    | SYM_DOT
    | SYM_STAR
    (* multi-char symbols *)
    | SYM_DOTS
    | SYM_FUNC_ARROW
    | SYM_TYPE_ARROW
    | SYM_HASH_LS
    (* module level keywords/symbol *)
    | KEY_eqtype
    | KEY_functor
    | KEY_include
    | KEY_sharing
    | KEY_sig
    | KEY_signature
    | KEY_struct
    | KEY_structure
    | KEY_where
    | SYM_COLON_GT

  fun sofTok tok =
      case tok of
          TOK'Ident s   => s^"(*ID*)"
        | TOK'Symid s   => s^"(*SYMID*)"
        | TOK'TyVar s   => "'" ^ s
        | TOK'Number n  => stringOfInt n
        | TOK'Word w    => Word.toString w
        | TOK'CharLit c => "#\"" ^ sofChar c ^ "\""
        | TOK'StringLit s => sofStringLit s
        | TOK'Unknown c => "Unknown:'"^implode[c]^"'"
        | TOK'Err s     => "Error: "^s
        (* keywords *)
        | KEY_abstype   => "abstype"
        | KEY_and       => "and"
        | KEY_andalso   => "andalso"
        | KEY_as        => "as"
        | KEY_case      => "case"
        | KEY_do        => "do"
        | KEY_datatype  => "datatype"
        | KEY_else      => "else"
        | KEY_end       => "end"
        | KEY_exception => "exception"
        | KEY_fn        => "fn"
        | KEY_fun       => "fun"
        | KEY_handle    => "handle"
        | KEY_if        => "if"
        | KEY_in        => "in"
        | KEY_infix     => "infix"
        | KEY_infixr    => "infixr"
        | KEY_let       => "let"
        | KEY_local     => "local"
        | KEY_nonfix    => "nonfix"
        | KEY_of        => "of"
        | KEY_op        => "op"
        | KEY_open      => "open"
        | KEY_orelse    => "orelse"
        | KEY_raise     => "raise"
        | KEY_rec       => "rec"
        | KEY_then      => "then"
        | KEY_type      => "type"
        | KEY_val       => "val"
        | KEY_with      => "with"
        | KEY_withtype  => "withtype"
        | KEY_while     => "while"
        (* 1 char symbols *)
        | SYM_LP        => "("
        | SYM_RP        => ")"
        | SYM_LS        => "["
        | SYM_RS        => "]"
        | SYM_LB        => "{"
        | SYM_RB        => "}"
        | SYM_COMMA     => ","
        | SYM_COLON     => ":"
        | SYM_SEMI      => ";"
        | SYM_UNDERSCORE=> "_"
        | SYM_VBAR      => "|"
        | SYM_EQ        => "="
        | SYM_HASH      => "#"
        | SYM_DOT       => "."
        | SYM_STAR      => "*"
        (* multi char symbols *)
        | SYM_DOTS      => "..."
        | SYM_FUNC_ARROW=> "=>"
        | SYM_TYPE_ARROW=> "->"
        | SYM_HASH_LS   => "#["
        (* module level keywords/symbol *)
        | KEY_eqtype    => "eqtype"
        | KEY_functor   => "functor"
        | KEY_include   => "include"
        | KEY_sharing   => "sharing"
        | KEY_sig       => "sig"
        | KEY_signature => "signature"
        | KEY_struct    => "struct"
        | KEY_structure => "structure"
        | KEY_where     => "where"
        | SYM_COLON_GT  => ":>"


end
