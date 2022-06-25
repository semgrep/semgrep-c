(**
   Boilerplate to be used as a template when mapping the c CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_false_ (env : env) (tok : CST.false_) =
  (* false *) token env tok

let map_anon_choice_BANG_67174d6 (env : env) (x : CST.anon_choice_BANG_67174d6) =
  (match x with
  | `BANG tok -> (* "!" *) token env tok
  | `TILDE tok -> (* "~" *) token env tok
  | `DASH tok -> (* "-" *) token env tok
  | `PLUS tok -> (* "+" *) token env tok
  )

let map_imm_tok_pat_36637e2 (env : env) (tok : CST.imm_tok_pat_36637e2) =
  (* pattern "[^\\n']" *) token env tok

let map_pat_c3ea183 (env : env) (tok : CST.pat_c3ea183) =
  (* pattern #[ 	]*define *) token env tok

let map_primitive_type (env : env) (tok : CST.primitive_type) =
  (* primitive_type *) token env tok

let map_type_qualifier (env : env) (x : CST.type_qualifier) =
  (match x with
  | `Const tok -> (* "const" *) token env tok
  | `Vola tok -> (* "volatile" *) token env tok
  | `Rest tok -> (* "restrict" *) token env tok
  | `X__Atomic tok -> (* "_Atomic" *) token env tok
  )

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env) (tok : CST.imm_tok_prec_p1_pat_c7f65b4) =
  (* pattern "[^\\\\\"\\n]+" *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_]\w* *) token env tok

let map_true_ (env : env) (tok : CST.true_) =
  (* true *) token env tok

let map_pat_56631e5 (env : env) (tok : CST.pat_56631e5) =
  (* pattern #[ 	]*else *) token env tok

let map_pat_3df6e71 (env : env) (tok : CST.pat_3df6e71) =
  (* pattern #[ 	]*if *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_pat_c46d1b2 (env : env) (tok : CST.pat_c46d1b2) =
  (* pattern #[ 	]*endif *) token env tok

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) =
  (match x with
  | `Extern tok -> (* "extern" *) token env tok
  | `Static tok -> (* "static" *) token env tok
  | `Auto tok -> (* "auto" *) token env tok
  | `Regi tok -> (* "register" *) token env tok
  | `Inline tok -> (* "inline" *) token env tok
  )

let map_ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  (match x with
  | `X___cdecl tok -> (* "__cdecl" *) token env tok
  | `X___clrc tok -> (* "__clrcall" *) token env tok
  | `X___stdc tok -> (* "__stdcall" *) token env tok
  | `X___fast tok -> (* "__fastcall" *) token env tok
  | `X___this tok -> (* "__thiscall" *) token env tok
  | `X___vect tok -> (* "__vectorcall" *) token env tok
  )

let map_pat_9d92f6a (env : env) (tok : CST.pat_9d92f6a) =
  (* pattern #[ 	]*ifndef *) token env tok

let map_pat_ca8830e (env : env) (tok : CST.pat_ca8830e) =
  (* pattern #[ 	]*include *) token env tok

let map_ms_unaligned_ptr_modifier (env : env) (x : CST.ms_unaligned_ptr_modifier) =
  (match x with
  | `X__unal tok -> (* "_unaligned" *) token env tok
  | `X___unal tok -> (* "__unaligned" *) token env tok
  )

let map_system_lib_string (env : env) (tok : CST.system_lib_string) =
  (* system_lib_string *) token env tok

let map_pat_bfeb4bb (env : env) (tok : CST.pat_bfeb4bb) =
  (* pattern #[ 	]*elif *) token env tok

let map_number_literal (env : env) (tok : CST.number_literal) =
  (* number_literal *) token env tok

let map_pat_25b90ba (env : env) (tok : CST.pat_25b90ba) =
  (* pattern #[ 	]*ifdef *) token env tok

let map_preproc_directive (env : env) (tok : CST.preproc_directive) =
  (* pattern #[ \t]*[a-zA-Z]\w* *) token env tok

let map_preproc_arg (env : env) (tok : CST.preproc_arg) =
  (* preproc_arg *) token env tok

let map_anon_choice_DASHDASH_d11def2 (env : env) (x : CST.anon_choice_DASHDASH_d11def2) =
  (match x with
  | `DASHDASH tok -> (* "--" *) token env tok
  | `PLUSPLUS tok -> (* "++" *) token env tok
  )

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 =
    (match v1 with
    | `LDQUOT tok -> (* "L\"" *) token env tok
    | `UDQUOT_c163aae tok -> (* "u\"" *) token env tok
    | `UDQUOT_df3447d tok -> (* "U\"" *) token env tok
    | `U8DQUOT tok -> (* "u8\"" *) token env tok
    | `DQUOT tok -> (* "\"" *) token env tok
    )
  in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Imm_tok_prec_p1_pat_c7f65b4 x ->
          map_imm_tok_prec_p1_pat_c7f65b4 env x
      | `Esc_seq tok -> (* escape_sequence *) token env tok
      )
    ) v2
  in
  let v3 = (* "\"" *) token env v3 in
  todo env (v1, v2, v3)

let map_char_literal (env : env) ((v1, v2, v3) : CST.char_literal) =
  let v1 =
    (match v1 with
    | `LSQUOT tok -> (* "L'" *) token env tok
    | `USQUOT_d861d39 tok -> (* "u'" *) token env tok
    | `USQUOT_2701bdc tok -> (* "U'" *) token env tok
    | `U8SQUOT tok -> (* "u8'" *) token env tok
    | `SQUOT tok -> (* "'" *) token env tok
    )
  in
  let v2 =
    (match v2 with
    | `Esc_seq tok -> (* escape_sequence *) token env tok
    | `Imm_tok_pat_36637e2 x -> map_imm_tok_pat_36637e2 env x
    )
  in
  let v3 = (* "'" *) token env v3 in
  todo env (v1, v2, v3)

let map_anon_choice_pat_25b90ba_4a37f8c (env : env) (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  (match x with
  | `Pat_25b90ba x -> map_pat_25b90ba env x
  | `Pat_9d92f6a x -> map_pat_9d92f6a env x
  )

let map_ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  (match x with
  | `Ms_unal_ptr_modi x -> map_ms_unaligned_ptr_modifier env x
  | `Ms_rest_modi tok -> (* "__restrict" *) token env tok
  | `Ms_unsi_ptr_modi tok -> (* "__uptr" *) token env tok
  | `Ms_signed_ptr_modi tok -> (* "__sptr" *) token env tok
  )

let map_preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = (* pattern #[ \t]*[a-zA-Z]\w* *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> (* preproc_arg *) token env tok
    | None -> todo env ())
  in
  let v3 = (* "\n" *) token env v3 in
  todo env (v1, v2, v3)

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) =
  let v1 = (* "." *) token env v1 in
  let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
  todo env (v1, v2)

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  (match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) ->
      let v1 = (* "defined" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* pattern [a-zA-Z_]\w* *) token env v3 in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Defi_id (v1, v2) ->
      let v1 = (* "defined" *) token env v1 in
      let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
      todo env (v1, v2)
  )

let map_anon_choice_type_id_d3c4b5f (env : env) (x : CST.anon_choice_type_id_d3c4b5f) =
  (match x with
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  | `DOTDOTDOT tok -> (* "..." *) token env tok
  )

let map_ms_declspec_modifier (env : env) ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = (* "__declspec" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* pattern [a-zA-Z_]\w* *) token env v3 in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> (* preproc_arg *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "\n" *) token env v4 in
  todo env (v1, v2, v3, v4)

let rec map_preproc_argument_list (env : env) ((v1, v2, v3) : CST.preproc_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_preproc_expression env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_preproc_expression env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_preproc_binary_expression (env : env) (x : CST.preproc_binary_expression) =
  (match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_GT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_LT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) ->
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_preproc_call_expression (env : env) ((v1, v2) : CST.preproc_call_expression) =
  let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
  let v2 = map_preproc_argument_list env v2 in
  todo env (v1, v2)

and map_preproc_expression (env : env) (x : CST.preproc_expression) =
  (match x with
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  | `Prep_call_exp x -> map_preproc_call_expression env x
  | `Num_lit tok -> (* number_literal *) token env tok
  | `Char_lit x -> map_char_literal env x
  | `Prep_defi x -> map_preproc_defined env x
  | `Prep_un_exp (v1, v2) ->
      let v1 = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_preproc_expression env v2 in
      todo env (v1, v2)
  | `Prep_bin_exp x -> map_preproc_binary_expression env x
  | `Prep_paren_exp (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  )

let map_preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_type_id_d3c4b5f env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_id_d3c4b5f env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

let map_preproc_function_def (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
  let v3 = map_preproc_params env v3 in
  let v4 =
    (match v4 with
    | Some tok -> (* preproc_arg *) token env tok
    | None -> todo env ())
  in
  let v5 = (* "\n" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

let rec map_abstract_declarator (env : env) (x : CST.abstract_declarator) =
  (match x with
  | `Abst_poin_decl (v1, v2, v3) ->
      let v1 = (* "*" *) token env v1 in
      let v2 = List.map (map_type_qualifier env) v2 in
      let v3 =
        (match v3 with
        | Some x -> map_abstract_declarator env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Abst_func_decl (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> map_abstract_declarator env x
        | None -> todo env ())
      in
      let v2 = map_parameter_list env v2 in
      todo env (v1, v2)
  | `Abst_array_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_abstract_declarator env x
        | None -> todo env ())
      in
      let v2 = (* "[" *) token env v2 in
      let v3 = List.map (map_type_qualifier env) v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_exp_508611b env x
        | None -> todo env ())
      in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Abst_paren_decl (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_abstract_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_anon_choice_exp_508611b (env : env) (x : CST.anon_choice_exp_508611b) =
  (match x with
  | `Exp x -> map_expression env x
  | `STAR tok -> (* "*" *) token env tok
  )

and map_anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) =
  (match x with
  | `Exp x -> map_expression env x
  | `Comma_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_anon_choice_exp_55b4dba env v3 in
      todo env (v1, v2, v3)
  )

and map_anon_choice_init_pair_1a6981e (env : env) (x : CST.anon_choice_init_pair_1a6981e) =
  (match x with
  | `Init_pair (v1, v2, v3) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Subs_desi x -> map_subscript_designator env x
          | `Field_desi x -> map_field_designator env x
          )
        ) v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Exp x -> map_expression env x
        | `Init_list x -> map_initializer_list env x
        )
      in
      todo env (v1, v2, v3)
  | `Exp x -> map_expression env x
  | `Init_list x -> map_initializer_list env x
  )

and map_anon_choice_param_decl_bdc8cc9 (env : env) (x : CST.anon_choice_param_decl_bdc8cc9) =
  (match x with
  | `Param_decl (v1, v2) ->
      let v1 = map_declaration_specifiers env v1 in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Decl x -> map_declarator env x
            | `Abst_decl x -> map_abstract_declarator env x
            )
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `DOTDOTDOT tok -> (* "..." *) token env tok
  )

and map_anon_choice_prep_else_in_field_decl_list_97ea65e (env : env) (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) =
  (match x with
  | `Prep_else_in_field_decl_list (v1, v2) ->
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        List.map (map_field_declaration_list_item env) v2
      in
      todo env (v1, v2)
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        List.map (map_field_declaration_list_item env) v4
      in
      let v5 =
        (match v5 with
        | Some x ->
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_anon_choice_stor_class_spec_5764fed (env : env) (x : CST.anon_choice_stor_class_spec_5764fed) =
  (match x with
  | `Stor_class_spec x -> map_storage_class_specifier env x
  | `Type_qual x -> map_type_qualifier env x
  | `Attr_spec x -> map_attribute_specifier env x
  | `Ms_decl_modi x -> map_ms_declspec_modifier env x
  )

and map_anon_choice_type_id_opt_field_decl_list_9aebd83 (env : env) (x : CST.anon_choice_type_id_opt_field_decl_list_9aebd83) =
  (match x with
  | `Id_opt_field_decl_list (v1, v2) ->
      let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_field_declaration_list env x
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Field_decl_list x -> map_field_declaration_list env x
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_expression env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_assignment_left_expression (env : env) (x : CST.assignment_left_expression) =
  (match x with
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  | `Call_exp x -> map_call_expression env x
  | `Field_exp x -> map_field_expression env x
  | `Poin_exp x -> map_pointer_expression env x
  | `Subs_exp x -> map_subscript_expression env x
  | `Paren_exp x -> map_parenthesized_expression env x
  )

and map_attribute_specifier (env : env) ((v1, v2, v3, v4) : CST.attribute_specifier) =
  let v1 = (* "__attribute__" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_argument_list env v3 in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_argument_list env v2 in
  todo env (v1, v2)

and map_declaration_specifiers (env : env) ((v1, v2, v3) : CST.declaration_specifiers) =
  let v1 =
    List.map (map_anon_choice_stor_class_spec_5764fed env) v1
  in
  let v2 = map_type_specifier env v2 in
  let v3 =
    List.map (map_anon_choice_stor_class_spec_5764fed env) v3
  in
  todo env (v1, v2, v3)

and map_declarator (env : env) (x : CST.declarator) =
  (match x with
  | `Poin_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_ms_based_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "*" *) token env v2 in
      let v3 = List.map (map_ms_pointer_modifier env) v3 in
      let v4 = List.map (map_type_qualifier env) v4 in
      let v5 = map_declarator env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Func_decl (v1, v2, v3) ->
      let v1 = map_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 = List.map (map_attribute_specifier env) v3 in
      todo env (v1, v2, v3)
  | `Array_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = List.map (map_type_qualifier env) v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_exp_508611b env x
        | None -> todo env ())
      in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Paren_decl (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  )

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) =
  let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_enumerator env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_enumerator env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Cond_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Assign_exp (v1, v2, v3) ->
      let v1 = map_assignment_left_expression env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> (* "=" *) token env tok
        | `STAREQ tok -> (* "*=" *) token env tok
        | `SLASHEQ tok -> (* "/=" *) token env tok
        | `PERCEQ tok -> (* "%=" *) token env tok
        | `PLUSEQ tok -> (* "+=" *) token env tok
        | `DASHEQ tok -> (* "-=" *) token env tok
        | `LTLTEQ tok -> (* "<<=" *) token env tok
        | `GTGTEQ tok -> (* ">>=" *) token env tok
        | `AMPEQ tok -> (* "&=" *) token env tok
        | `HATEQ tok -> (* "^=" *) token env tok
        | `BAREQ tok -> (* "|=" *) token env tok
        )
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Bin_exp x -> map_binary_expression env x
  | `Un_exp (v1, v2) ->
      let v1 = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Update_exp x -> map_update_expression env x
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_descriptor env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Poin_exp x -> map_pointer_expression env x
  | `Sizeof_exp (v1, v2) ->
      let v1 = (* "sizeof" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> map_expression env x
        | `LPAR_type_desc_RPAR (v1, v2, v3) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = map_type_descriptor env v2 in
            let v3 = (* ")" *) token env v3 in
            todo env (v1, v2, v3)
        )
      in
      todo env (v1, v2)
  | `Subs_exp x -> map_subscript_expression env x
  | `Call_exp x -> map_call_expression env x
  | `Field_exp x -> map_field_expression env x
  | `Comp_lit_exp (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_descriptor env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_initializer_list env v4 in
      todo env (v1, v2, v3, v4)
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  | `Num_lit tok -> (* number_literal *) token env tok
  | `Str_lit x -> map_string_literal env x
  | `True tok -> (* true *) token env tok
  | `False tok -> (* false *) token env tok
  | `Null tok -> (* "NULL" *) token env tok
  | `Conc_str (v1, v2) ->
      let v1 = map_string_literal env v1 in
      let v2 = List.map (map_string_literal env) v2 in
      todo env (v1, v2)
  | `Char_lit x -> map_char_literal env x
  | `Paren_exp x -> map_parenthesized_expression env x
  )

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    List.map (map_field_declaration_list_item env) v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_field_declaration_list_item (env : env) (x : CST.field_declaration_list_item) =
  (match x with
  | `Field_decl (v1, v2, v3, v4) ->
      let v1 = map_declaration_specifiers env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) ->
            let v1 = map_field_declarator env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_field_declarator env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_bitfield_clause env x
        | None -> todo env ())
      in
      let v4 = (* ";" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Prep_def x -> map_preproc_def env x
  | `Prep_func_def x -> map_preproc_function_def env x
  | `Prep_call x -> map_preproc_call env x
  | `Prep_if_in_field_decl_list (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        List.map (map_field_declaration_list_item env) v4
      in
      let v5 =
        (match v5 with
        | Some x ->
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
        | None -> todo env ())
      in
      let v6 = map_pat_c46d1b2 env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Prep_ifdef_in_field_decl_list (v1, v2, v3, v4, v5) ->
      let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
      let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
      let v3 =
        List.map (map_field_declaration_list_item env) v3
      in
      let v4 =
        (match v4 with
        | Some x ->
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
        | None -> todo env ())
      in
      let v5 = map_pat_c46d1b2 env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_field_declarator (env : env) (x : CST.field_declarator) =
  (match x with
  | `Poin_field_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_ms_based_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "*" *) token env v2 in
      let v3 = List.map (map_ms_pointer_modifier env) v3 in
      let v4 = List.map (map_type_qualifier env) v4 in
      let v5 = map_field_declarator env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Func_field_decl (v1, v2) ->
      let v1 = map_field_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      todo env (v1, v2)
  | `Array_field_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_field_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = List.map (map_type_qualifier env) v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_exp_508611b env x
        | None -> todo env ())
      in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Paren_field_decl (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_field_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  )

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> (* "." *) token env tok
    | `DASHGT tok -> (* "->" *) token env tok
    )
  in
  let v3 = (* pattern [a-zA-Z_]\w* *) token env v3 in
  todo env (v1, v2, v3)

and map_initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_init_pair_1a6981e env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_init_pair_1a6981e env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = (* "__based" *) token env v1 in
  let v2 = map_argument_list env v2 in
  todo env (v1, v2)

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = map_anon_choice_param_decl_bdc8cc9 env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_decl_bdc8cc9 env v2 in
            todo env (v1, v2)
          ) v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_exp_55b4dba env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) =
  let v1 =
    (match v1 with
    | `STAR tok -> (* "*" *) token env tok
    | `AMP tok -> (* "&" *) token env tok
    )
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_subscript_designator (env : env) ((v1, v2, v3) : CST.subscript_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "]" *) token env v3 in
  todo env (v1, v2, v3)

and map_subscript_expression (env : env) ((v1, v2, v3, v4) : CST.subscript_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) =
  let v1 = List.map (map_type_qualifier env) v1 in
  let v2 = map_type_specifier env v2 in
  let v3 = List.map (map_type_qualifier env) v3 in
  let v4 =
    (match v4 with
    | Some x -> map_abstract_declarator env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_type_specifier (env : env) (x : CST.type_specifier) =
  (match x with
  | `Struct_spec (v1, v2, v3) ->
      let v1 = (* "struct" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_ms_declspec_modifier env x
        | None -> todo env ())
      in
      let v3 =
        map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v3
      in
      todo env (v1, v2, v3)
  | `Union_spec (v1, v2, v3) ->
      let v1 = (* "union" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_ms_declspec_modifier env x
        | None -> todo env ())
      in
      let v3 =
        map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v3
      in
      todo env (v1, v2, v3)
  | `Enum_spec (v1, v2) ->
      let v1 = (* "enum" *) token env v1 in
      let v2 =
        (match v2 with
        | `Id_opt_enum_list (v1, v2) ->
            let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
            let v2 =
              (match v2 with
              | Some x -> map_enumerator_list env x
              | None -> todo env ())
            in
            todo env (v1, v2)
        | `Enum_list x -> map_enumerator_list env x
        )
      in
      todo env (v1, v2)
  | `Macro_type_spec (v1, v2, v3, v4) ->
      let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_descriptor env v3 in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Sized_type_spec (v1, v2) ->
      let v1 =
        List.map (fun x ->
          (match x with
          | `Signed tok -> (* "signed" *) token env tok
          | `Unsi tok -> (* "unsigned" *) token env tok
          | `Long tok -> (* "long" *) token env tok
          | `Short tok -> (* "short" *) token env tok
          )
        ) v1
      in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
            | `Prim_type tok -> (* primitive_type *) token env tok
            )
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Prim_type tok -> (* primitive_type *) token env tok
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Choice_DASHDASH_exp (v1, v2) ->
      let v1 = map_anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Exp_choice_DASHDASH (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DASHDASH_d11def2 env v2 in
      todo env (v1, v2)
  )

let map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 =
    (match v1 with
    | Some x -> map_anon_choice_exp_55b4dba env x
    | None -> todo env ())
  in
  let v2 = (* ";" *) token env v2 in
  todo env (v1, v2)

let rec map_type_declarator (env : env) (x : CST.type_declarator) =
  (match x with
  | `Poin_type_decl (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_ms_based_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "*" *) token env v2 in
      let v3 = List.map (map_ms_pointer_modifier env) v3 in
      let v4 = List.map (map_type_qualifier env) v4 in
      let v5 = map_type_declarator env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Func_type_decl (v1, v2) ->
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      todo env (v1, v2)
  | `Array_type_decl (v1, v2, v3, v4, v5) ->
      let v1 = map_type_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = List.map (map_type_qualifier env) v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_exp_508611b env x
        | None -> todo env ())
      in
      let v5 = (* "]" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Paren_type_decl (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
  )

let map_anon_choice_decl_f8b0ff3 (env : env) (x : CST.anon_choice_decl_f8b0ff3) =
  (match x with
  | `Decl x -> map_declarator env x
  | `Init_decl (v1, v2, v3) ->
      let v1 = map_declarator env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Init_list x -> map_initializer_list env x
        | `Exp x -> map_expression env x
        )
      in
      todo env (v1, v2, v3)
  )

let map_type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition) =
  let v1 = (* "typedef" *) token env v1 in
  let v2 = List.map (map_type_qualifier env) v2 in
  let v3 = map_type_specifier env v3 in
  let v4 = map_type_declarator env v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_declarator env v2 in
      todo env (v1, v2)
    ) v5
  in
  let v6 = (* ";" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let map_declaration (env : env) ((v1, v2, v3, v4) : CST.declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_decl_f8b0ff3 env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

let rec map_anon_choice_prep_else_8b52b0f (env : env) (x : CST.anon_choice_prep_else_8b52b0f) =
  (match x with
  | `Prep_else (v1, v2) ->
      let v1 = map_pat_56631e5 env v1 in
      let v2 = map_translation_unit env v2 in
      todo env (v1, v2)
  | `Prep_elif (v1, v2, v3, v4, v5) ->
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = map_translation_unit env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_anon_choice_prep_else_8b52b0f env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  )

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_translation_unit env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_translation_unit env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_function_definition (env : env) ((v1, v2, v3, v4) : CST.function_definition) =
  let v1 =
    (match v1 with
    | Some x -> map_ms_call_modifier env x
    | None -> todo env ())
  in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_declarator env v3 in
  let v4 = map_compound_statement env v4 in
  todo env (v1, v2, v3, v4)

and map_non_case_statement (env : env) (x : CST.non_case_statement) =
  (match x with
  | `Labe_stmt (v1, v2, v3) ->
      let v1 = (* pattern [a-zA-Z_]\w* *) token env v1 in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Comp_stmt x -> map_compound_statement env x
  | `Exp_stmt x -> map_expression_statement env x
  | `If_stmt (v1, v2, v3, v4) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = (* "else" *) token env v1 in
            let v2 = map_statement env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4)
  | `Switch_stmt (v1, v2, v3) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_compound_statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | `Decl x -> map_declaration env x
        | `Opt_choice_exp_SEMI x -> map_expression_statement env x
        )
      in
      let v4 =
        (match v4 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v5 = (* ";" *) token env v5 in
      let v6 =
        (match v6 with
        | Some x -> map_anon_choice_exp_55b4dba env x
        | None -> todo env ())
      in
      let v7 = (* ")" *) token env v7 in
      let v8 = map_statement env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_anon_choice_exp_55b4dba env x
        | None -> todo env ())
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2) ->
      let v1 = (* "break" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Cont_stmt (v1, v2) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Goto_stmt (v1, v2, v3) ->
      let v1 = (* "goto" *) token env v1 in
      let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Case_stmt (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | `Case_exp (v1, v2) ->
            let v1 = (* "case" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | `Defa tok -> (* "default" *) token env tok
        )
      in
      let v2 = (* ":" *) token env v2 in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Choice_labe_stmt x -> map_non_case_statement env x
          | `Decl x -> map_declaration env x
          | `Type_defi x -> map_type_definition env x
          )
        ) v3
      in
      todo env (v1, v2, v3)
  | `Choice_labe_stmt x -> map_non_case_statement env x
  )

and map_top_level_item (env : env) (x : CST.top_level_item) =
  (match x with
  | `Func_defi x -> map_function_definition env x
  | `Link_spec (v1, v2, v3) ->
      let v1 = (* "extern" *) token env v1 in
      let v2 = map_string_literal env v2 in
      let v3 =
        (match v3 with
        | `Func_defi x -> map_function_definition env x
        | `Decl x -> map_declaration env x
        | `Decl_list x -> map_declaration_list env x
        )
      in
      todo env (v1, v2, v3)
  | `Decl x -> map_declaration env x
  | `Choice_case_stmt x -> map_statement env x
  | `Type_defi x -> map_type_definition env x
  | `Empty_decl (v1, v2) ->
      let v1 = map_type_specifier env v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Prep_if (v1, v2, v3, v4, v5, v6) ->
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = map_translation_unit env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_anon_choice_prep_else_8b52b0f env x
        | None -> todo env ())
      in
      let v6 = map_pat_c46d1b2 env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Prep_ifdef (v1, v2, v3, v4, v5) ->
      let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
      let v2 = (* pattern [a-zA-Z_]\w* *) token env v2 in
      let v3 = map_translation_unit env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_anon_choice_prep_else_8b52b0f env x
        | None -> todo env ())
      in
      let v5 = map_pat_c46d1b2 env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Prep_incl (v1, v2, v3) ->
      let v1 = map_pat_ca8830e env v1 in
      let v2 =
        (match v2 with
        | `Str_lit x -> map_string_literal env x
        | `System_lib_str tok ->
            (* system_lib_string *) token env tok
        | `Id tok -> (* pattern [a-zA-Z_]\w* *) token env tok
        | `Prep_call_exp x -> map_preproc_call_expression env x
        )
      in
      let v3 = (* "\n" *) token env v3 in
      todo env (v1, v2, v3)
  | `Prep_def x -> map_preproc_def env x
  | `Prep_func_def x -> map_preproc_function_def env x
  | `Prep_call x -> map_preproc_call env x
  )

and map_translation_unit (env : env) (xs : CST.translation_unit) =
  List.map (map_top_level_item env) xs
