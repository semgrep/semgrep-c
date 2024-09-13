(**
   Boilerplate to be used as a template when mapping the c CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_imm_tok_lpar (env : env) (tok : CST.imm_tok_lpar) =
  (* "(" *) token env tok

let map_preproc_arg (env : env) (tok : CST.preproc_arg) =
  (* preproc_arg *) token env tok

let map_storage_class_specifier (env : env) (x : CST.storage_class_specifier) =
  (match x with
  | `Extern tok -> R.Case ("Extern",
      (* "extern" *) token env tok
    )
  | `Static tok -> R.Case ("Static",
      (* "static" *) token env tok
    )
  | `Auto tok -> R.Case ("Auto",
      (* "auto" *) token env tok
    )
  | `Regi tok -> R.Case ("Regi",
      (* "register" *) token env tok
    )
  | `Inline tok -> R.Case ("Inline",
      (* "inline" *) token env tok
    )
  | `X___inline tok -> R.Case ("X___inline",
      (* "__inline" *) token env tok
    )
  | `X___inline__ tok -> R.Case ("X___inline__",
      (* "__inline__" *) token env tok
    )
  | `X___forc tok -> R.Case ("X___forc",
      (* "__forceinline" *) token env tok
    )
  | `Thread_local tok -> R.Case ("Thread_local",
      (* "thread_local" *) token env tok
    )
  | `X___thread tok -> R.Case ("X___thread",
      (* "__thread" *) token env tok
    )
  )

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_false_ (env : env) (tok : CST.false_) =
  (* false *) token env tok

let map_true_ (env : env) (tok : CST.true_) =
  (* true *) token env tok

let map_pat_bfeb4bb (env : env) (tok : CST.pat_bfeb4bb) =
  (* pattern #[ 	]*elif *) token env tok

let map_preproc_directive (env : env) (tok : CST.preproc_directive) =
  (* pattern #[ \t]*[a-zA-Z0-9]\w* *) token env tok

let map_anon_choice_BANG_67174d6 (env : env) (x : CST.anon_choice_BANG_67174d6) =
  (match x with
  | `BANG tok -> R.Case ("BANG",
      (* "!" *) token env tok
    )
  | `TILDE tok -> R.Case ("TILDE",
      (* "~" *) token env tok
    )
  | `DASH tok -> R.Case ("DASH",
      (* "-" *) token env tok
    )
  | `PLUS tok -> R.Case ("PLUS",
      (* "+" *) token env tok
    )
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok

let map_null (env : env) (x : CST.null) =
  (match x with
  | `NULL tok -> R.Case ("NULL",
      (* "NULL" *) token env tok
    )
  | `Null tok -> R.Case ("Null",
      (* "nullptr" *) token env tok
    )
  )

let map_imm_tok_pat_36637e2 (env : env) (tok : CST.imm_tok_pat_36637e2) =
  (* pattern "[^\\n']" *) token env tok

let map_break_statement (env : env) ((v1, v2) : CST.break_statement) =
  let v1 = (* "break" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_continue_statement (env : env) ((v1, v2) : CST.continue_statement) =
  let v1 = (* "continue" *) token env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

let map_pat_25b90ba (env : env) (tok : CST.pat_25b90ba) =
  (* pattern #[ 	]*ifdef *) token env tok

let map_imm_tok_pat_509ec78 (env : env) (tok : CST.imm_tok_pat_509ec78) =
  (* pattern \r?\n *) token env tok

let map_primitive_type (env : env) (tok : CST.primitive_type) =
  (* primitive_type *) token env tok

let map_ms_unaligned_ptr_modifier (env : env) (x : CST.ms_unaligned_ptr_modifier) =
  (match x with
  | `X__unal tok -> R.Case ("X__unal",
      (* "_unaligned" *) token env tok
    )
  | `X___unal tok -> R.Case ("X___unal",
      (* "__unaligned" *) token env tok
    )
  )

let map_pat_ca8830e (env : env) (tok : CST.pat_ca8830e) =
  (* pattern #[ 	]*include *) token env tok

let map_type_qualifier (env : env) (x : CST.type_qualifier) =
  (match x with
  | `Const tok -> R.Case ("Const",
      (* "const" *) token env tok
    )
  | `Cons tok -> R.Case ("Cons",
      (* "constexpr" *) token env tok
    )
  | `Vola tok -> R.Case ("Vola",
      (* "volatile" *) token env tok
    )
  | `Rest tok -> R.Case ("Rest",
      (* "restrict" *) token env tok
    )
  | `X___rest__ tok -> R.Case ("X___rest__",
      (* "__restrict__" *) token env tok
    )
  | `X___exte__ tok -> R.Case ("X___exte__",
      (* "__extension__" *) token env tok
    )
  | `X__Atomic tok -> R.Case ("X__Atomic",
      (* "_Atomic" *) token env tok
    )
  | `X__Nore tok -> R.Case ("X__Nore",
      (* "_Noreturn" *) token env tok
    )
  | `Nore tok -> R.Case ("Nore",
      (* "noreturn" *) token env tok
    )
  )

let map_gnu_asm_qualifier (env : env) (x : CST.gnu_asm_qualifier) =
  (match x with
  | `Vola tok -> R.Case ("Vola",
      (* "volatile" *) token env tok
    )
  | `Inline tok -> R.Case ("Inline",
      (* "inline" *) token env tok
    )
  | `Goto tok -> R.Case ("Goto",
      (* "goto" *) token env tok
    )
  )

let map_imm_tok_prec_p1_pat_c7f65b4 (env : env) (tok : CST.imm_tok_prec_p1_pat_c7f65b4) =
  (* pattern "[^\\\\\"\\n]+" *) token env tok

let map_pat_c46d1b2 (env : env) (tok : CST.pat_c46d1b2) =
  (* pattern #[ 	]*endif *) token env tok

let map_pat_a6d4183 (env : env) (tok : CST.pat_a6d4183) =
  (* pattern #[ 	]*elifndef *) token env tok

let map_anon_choice_signed_a0bfc19 (env : env) (x : CST.anon_choice_signed_a0bfc19) =
  (match x with
  | `Signed tok -> R.Case ("Signed",
      (* "signed" *) token env tok
    )
  | `Unsi tok -> R.Case ("Unsi",
      (* "unsigned" *) token env tok
    )
  | `Long tok -> R.Case ("Long",
      (* "long" *) token env tok
    )
  | `Short tok -> R.Case ("Short",
      (* "short" *) token env tok
    )
  )

let map_variadic_parameter (env : env) (v1 : CST.variadic_parameter) =
  (* "..." *) token env v1

let map_number_literal (env : env) (tok : CST.number_literal) =
  (* number_literal *) token env tok

let map_system_lib_string (env : env) (tok : CST.system_lib_string) =
  (* system_lib_string *) token env tok

let map_pat_3df6e71 (env : env) (tok : CST.pat_3df6e71) =
  (* pattern #[ 	]*if *) token env tok

let map_pat_56631e5 (env : env) (tok : CST.pat_56631e5) =
  (* pattern #[ 	]*else *) token env tok

let map_pat_c3ea183 (env : env) (tok : CST.pat_c3ea183) =
  (* pattern #[ 	]*define *) token env tok

let map_pat_0307ca2 (env : env) (tok : CST.pat_0307ca2) =
  (* pattern #[ 	]*elifdef *) token env tok

let map_ms_call_modifier (env : env) (x : CST.ms_call_modifier) =
  (match x with
  | `X___cdecl tok -> R.Case ("X___cdecl",
      (* "__cdecl" *) token env tok
    )
  | `X___clrc tok -> R.Case ("X___clrc",
      (* "__clrcall" *) token env tok
    )
  | `X___stdc tok -> R.Case ("X___stdc",
      (* "__stdcall" *) token env tok
    )
  | `X___fast tok -> R.Case ("X___fast",
      (* "__fastcall" *) token env tok
    )
  | `X___this tok -> R.Case ("X___this",
      (* "__thiscall" *) token env tok
    )
  | `X___vect tok -> R.Case ("X___vect",
      (* "__vectorcall" *) token env tok
    )
  )

let map_pat_9d92f6a (env : env) (tok : CST.pat_9d92f6a) =
  (* pattern #[ 	]*ifndef *) token env tok

let map_anon_choice_DASHDASH_d11def2 (env : env) (x : CST.anon_choice_DASHDASH_d11def2) =
  (match x with
  | `DASHDASH tok -> R.Case ("DASHDASH",
      (* "--" *) token env tok
    )
  | `PLUSPLUS tok -> R.Case ("PLUSPLUS",
      (* "++" *) token env tok
    )
  )

let map_char_literal (env : env) ((v1, v2, v3) : CST.char_literal) =
  let v1 =
    (match v1 with
    | `LSQUOT tok -> R.Case ("LSQUOT",
        (* "L'" *) token env tok
      )
    | `USQUOT_d861d39 tok -> R.Case ("USQUOT_d861d39",
        (* "u'" *) token env tok
      )
    | `USQUOT_2701bdc tok -> R.Case ("USQUOT_2701bdc",
        (* "U'" *) token env tok
      )
    | `U8SQUOT tok -> R.Case ("U8SQUOT",
        (* "u8'" *) token env tok
      )
    | `SQUOT tok -> R.Case ("SQUOT",
        (* "'" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      | `Imm_tok_pat_36637e2 x -> R.Case ("Imm_tok_pat_36637e2",
          map_imm_tok_pat_36637e2 env x
        )
      )
    ) v2)
  in
  let v3 = (* "'" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_call (env : env) ((v1, v2, v3) : CST.preproc_call) =
  let v1 = (* pattern #[ \t]*[a-zA-Z0-9]\w* *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_imm_tok_pat_509ec78 env v3 in
  R.Tuple [v1; v2; v3]

let map_string_literal (env : env) ((v1, v2, v3) : CST.string_literal) =
  let v1 =
    (match v1 with
    | `LDQUOT tok -> R.Case ("LDQUOT",
        (* "L\"" *) token env tok
      )
    | `UDQUOT_c163aae tok -> R.Case ("UDQUOT_c163aae",
        (* "u\"" *) token env tok
      )
    | `UDQUOT_df3447d tok -> R.Case ("UDQUOT_df3447d",
        (* "U\"" *) token env tok
      )
    | `U8DQUOT tok -> R.Case ("U8DQUOT",
        (* "u8\"" *) token env tok
      )
    | `DQUOT tok -> R.Case ("DQUOT",
        (* "\"" *) token env tok
      )
    )
  in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Imm_tok_prec_p1_pat_c7f65b4 x -> R.Case ("Imm_tok_prec_p1_pat_c7f65b4",
          map_imm_tok_prec_p1_pat_c7f65b4 env x
        )
      | `Esc_seq tok -> R.Case ("Esc_seq",
          (* escape_sequence *) token env tok
        )
      )
    ) v2)
  in
  let v3 = (* "\"" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_pat_0307ca2_dbf6a9d (env : env) (x : CST.anon_choice_pat_0307ca2_dbf6a9d) =
  (match x with
  | `Pat_0307ca2 x -> R.Case ("Pat_0307ca2",
      map_pat_0307ca2 env x
    )
  | `Pat_a6d4183 x -> R.Case ("Pat_a6d4183",
      map_pat_a6d4183 env x
    )
  )

let map_ms_pointer_modifier (env : env) (x : CST.ms_pointer_modifier) =
  (match x with
  | `Ms_unal_ptr_modi x -> R.Case ("Ms_unal_ptr_modi",
      map_ms_unaligned_ptr_modifier env x
    )
  | `Ms_rest_modi tok -> R.Case ("Ms_rest_modi",
      (* "__restrict" *) token env tok
    )
  | `Ms_unsi_ptr_modi tok -> R.Case ("Ms_unsi_ptr_modi",
      (* "__uptr" *) token env tok
    )
  | `Ms_signed_ptr_modi tok -> R.Case ("Ms_signed_ptr_modi",
      (* "__sptr" *) token env tok
    )
  )

let map_anon_choice_pat_25b90ba_4a37f8c (env : env) (x : CST.anon_choice_pat_25b90ba_4a37f8c) =
  (match x with
  | `Pat_25b90ba x -> R.Case ("Pat_25b90ba",
      map_pat_25b90ba env x
    )
  | `Pat_9d92f6a x -> R.Case ("Pat_9d92f6a",
      map_pat_9d92f6a env x
    )
  )

let map_preproc_def (env : env) ((v1, v2, v3, v4) : CST.preproc_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = map_imm_tok_pat_509ec78 env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_ms_declspec_modifier (env : env) ((v1, v2, v3, v4) : CST.ms_declspec_modifier) =
  let v1 = (* "__declspec" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_type_id_fe6e1ce (env : env) (x : CST.anon_choice_type_id_fe6e1ce) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Vari_param x -> R.Case ("Vari_param",
      map_variadic_parameter env x
    )
  )

let map_goto_statement (env : env) ((v1, v2, v3) : CST.goto_statement) =
  let v1 = (* "goto" *) token env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_field_designator (env : env) ((v1, v2) : CST.field_designator) =
  let v1 = (* "." *) token env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  R.Tuple [v1; v2]

let map_gnu_asm_goto_list (env : env) ((v1, v2) : CST.gnu_asm_goto_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 =
              (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
            in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_anon_choice_type_id_1a79fc3 (env : env) (x : CST.anon_choice_type_id_1a79fc3) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  )

let map_preproc_defined (env : env) (x : CST.preproc_defined) =
  (match x with
  | `Defi_LPAR_id_RPAR (v1, v2, v3, v4) -> R.Case ("Defi_LPAR_id_RPAR",
      let v1 = (* "defined" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Defi_id (v1, v2) -> R.Case ("Defi_id",
      let v1 = (* "defined" *) token env v1 in
      let v2 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      R.Tuple [v1; v2]
    )
  )

let map_anon_choice_type_id_d3c4b5f (env : env) (x : CST.anon_choice_type_id_d3c4b5f) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  )

let map_gnu_asm_clobber_list (env : env) ((v1, v2) : CST.gnu_asm_clobber_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_string_literal env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_string_literal env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_concatenated_string (env : env) ((v1, v2, v3) : CST.concatenated_string) =
  let v1 =
    (match v1 with
    | `Id tok -> R.Case ("Id",
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    )
  in
  let v2 = map_string_literal env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Str_lit x -> R.Case ("Str_lit",
          map_string_literal env x
        )
      | `Id tok -> R.Case ("Id",
          (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
        )
      )
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_gnu_asm_output_operand (env : env) ((v1, v2, v3, v4, v5) : CST.gnu_asm_output_operand) =
  let v1 =
    (match v1 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "[" *) token env v1 in
        let v2 =
          (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v2 = map_string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v4
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_old_style_parameter_list (env : env) ((v1, v2, v3) : CST.old_style_parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_type_id_fe6e1ce env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_id_fe6e1ce env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_sized_type_specifier (env : env) (x : CST.sized_type_specifier) =
  (match x with
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (v1, v2, v3) -> R.Case ("Rep_choice_signed_opt_choice_id_rep1_choice_signed",
      let v1 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_type_id_1a79fc3 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Rep1_choice_signed_opt_choice_id_rep_choice_signed (v1, v2, v3) -> R.Case ("Rep1_choice_signed_opt_choice_id_rep_choice_signed",
      let v1 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v1)
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_choice_type_id_1a79fc3 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        R.List (List.map (map_anon_choice_signed_a0bfc19 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_preproc_argument_list (env : env) ((v1, v2, v3) : CST.preproc_argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_preproc_expression env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_preproc_expression env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_preproc_binary_expression (env : env) (x : CST.preproc_binary_expression) =
  (match x with
  | `Prep_exp_PLUS_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_PLUS_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_DASH_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_DASH_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_STAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_STAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_SLASH_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_SLASH_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_PERC_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_PERC_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BARBAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BARBAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_AMPAMP_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_AMPAMP_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BAR_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BAR_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_HAT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_HAT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_AMP_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_AMP_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_EQEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_EQEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_BANGEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_BANGEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_GT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_GT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_GTEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_GTEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_LTEQ_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_LTEQ_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_LT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_LT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_LTLT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_LTLT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Prep_exp_GTGT_prep_exp (v1, v2, v3) -> R.Case ("Prep_exp_GTGT_prep_exp",
      let v1 = map_preproc_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_preproc_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_preproc_call_expression (env : env) ((v1, v2) : CST.preproc_call_expression) =
  let v1 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = map_preproc_argument_list env v2 in
  R.Tuple [v1; v2]

and map_preproc_expression (env : env) (x : CST.preproc_expression) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Prep_call_exp x -> R.Case ("Prep_call_exp",
      map_preproc_call_expression env x
    )
  | `Num_lit tok -> R.Case ("Num_lit",
      (* number_literal *) token env tok
    )
  | `Char_lit x -> R.Case ("Char_lit",
      map_char_literal env x
    )
  | `Prep_defi x -> R.Case ("Prep_defi",
      map_preproc_defined env x
    )
  | `Prep_un_exp (v1, v2) -> R.Case ("Prep_un_exp",
      let v1 = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_preproc_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Prep_bin_exp x -> R.Case ("Prep_bin_exp",
      map_preproc_binary_expression env x
    )
  | `Prep_paren_exp (v1, v2, v3) -> R.Case ("Prep_paren_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

let map_preproc_params (env : env) ((v1, v2, v3) : CST.preproc_params) =
  let v1 = map_imm_tok_lpar env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_type_id_d3c4b5f env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_type_id_d3c4b5f env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_gnu_asm_output_operand_list (env : env) ((v1, v2) : CST.gnu_asm_output_operand_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_gnu_asm_output_operand env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_gnu_asm_output_operand env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_preproc_include (env : env) ((v1, v2, v3) : CST.preproc_include) =
  let v1 = map_pat_ca8830e env v1 in
  let v2 =
    (match v2 with
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `System_lib_str tok -> R.Case ("System_lib_str",
        (* system_lib_string *) token env tok
      )
    | `Id tok -> R.Case ("Id",
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
      )
    | `Prep_call_exp x -> R.Case ("Prep_call_exp",
        map_preproc_call_expression env x
      )
    )
  in
  let v3 = map_imm_tok_pat_509ec78 env v3 in
  R.Tuple [v1; v2; v3]

let map_preproc_function_def (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_function_def) =
  let v1 = map_pat_c3ea183 env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = map_preproc_params env v3 in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* preproc_arg *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = map_imm_tok_pat_509ec78 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let rec map_abstract_declarator (env : env) (x : CST.abstract_declarator) =
  (match x with
  | `Abst_poin_decl (v1, v2, v3) -> R.Case ("Abst_poin_decl",
      let v1 = (* "*" *) token env v1 in
      let v2 = R.List (List.map (map_type_qualifier env) v2) in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_abstract_declarator env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Abst_func_decl (v1, v2) -> R.Case ("Abst_func_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_abstract_declarator env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_parameter_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Abst_array_decl (v1, v2, v3, v4, v5) -> R.Case ("Abst_array_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_abstract_declarator env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "[" *) token env v2 in
      let v3 = R.List (List.map (map_type_qualifier env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_508611b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Abst_paren_decl (v1, v2, v3) -> R.Case ("Abst_paren_decl",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_abstract_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_decl_opt_gnu_asm_exp_2c80446 (env : env) (x : CST.anon_choice_decl_opt_gnu_asm_exp_2c80446) =
  (match x with
  | `Decl_opt_gnu_asm_exp (v1, v2) -> R.Case ("Decl_opt_gnu_asm_exp",
      let v1 = map_declarator env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_gnu_asm_expression env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Init_decl (v1, v2, v3) -> R.Case ("Init_decl",
      let v1 = map_declarator env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Init_list x -> R.Case ("Init_list",
            map_initializer_list env x
          )
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_exp_3078596 (env : env) (x : CST.anon_choice_exp_3078596) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  )

and map_anon_choice_exp_508611b (env : env) (x : CST.anon_choice_exp_508611b) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  )

and map_anon_choice_exp_55b4dba (env : env) (x : CST.anon_choice_exp_55b4dba) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Comma_exp (v1, v2, v3) -> R.Case ("Comma_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "," *) token env v2 in
      let v3 = map_anon_choice_exp_55b4dba env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_init_pair_1a6981e (env : env) (x : CST.anon_choice_init_pair_1a6981e) =
  (match x with
  | `Init_pair x -> R.Case ("Init_pair",
      map_initializer_pair env x
    )
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Init_list x -> R.Case ("Init_list",
      map_initializer_list env x
    )
  )

and map_anon_choice_opt___exte___exp_2bc8eaa (env : env) (x : CST.anon_choice_opt___exte___exp_2bc8eaa) =
  (match x with
  | `Opt___exte___exp (v1, v2) -> R.Case ("Opt___exte___exp",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "__extension__" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  )

and map_anon_choice_param_decl_4ac2852 (env : env) (x : CST.anon_choice_param_decl_4ac2852) =
  (match x with
  | `Param_decl (v1, v2) -> R.Case ("Param_decl",
      let v1 = map_declaration_specifiers env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            (match x with
            | `Decl x -> R.Case ("Decl",
                map_declarator env x
              )
            | `Abst_decl x -> R.Case ("Abst_decl",
                map_abstract_declarator env x
              )
            )
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Vari_param x -> R.Case ("Vari_param",
      map_variadic_parameter env x
    )
  )

and map_anon_choice_prep_else_8b52b0f (env : env) (x : CST.anon_choice_prep_else_8b52b0f) =
  (match x with
  | `Prep_else (v1, v2) -> R.Case ("Prep_else",
      let v1 = map_pat_56631e5 env v1 in
      let v2 = R.List (List.map (map_block_item env) v2) in
      R.Tuple [v1; v2]
    )
  | `Prep_elif (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = R.List (List.map (map_block_item env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_8b52b0f env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_enum_list_8258275 (env : env) (x : CST.anon_choice_prep_else_in_enum_list_8258275) =
  (match x with
  | `Prep_else_in_enum_list (v1, v2) -> R.Case ("Prep_else_in_enum_list",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_list",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_8258275 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 (env : env) (x : CST.anon_choice_prep_else_in_enum_list_no_comma_04fd5a5) =
  (match x with
  | `Prep_else_in_enum_list_no_comma (v1, v2) -> R.Case ("Prep_else_in_enum_list_no_comma",
      let v1 = map_pat_56631e5 env v1 in
      let v2 = R.List (List.map (map_enumerator env) v2) in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_enum_list_no_comma (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_enum_list_no_comma",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 = R.List (List.map (map_enumerator env) v4) in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_prep_else_in_field_decl_list_97ea65e (env : env) (x : CST.anon_choice_prep_else_in_field_decl_list_97ea65e) =
  (match x with
  | `Prep_else_in_field_decl_list (v1, v2) -> R.Case ("Prep_else_in_field_decl_list",
      let v1 = map_pat_56631e5 env v1 in
      let v2 =
        R.List (List.map (map_field_declaration_list_item env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Prep_elif_in_field_decl_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_elif_in_field_decl_list",
      let v1 = map_pat_bfeb4bb env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_field_declaration_list_item env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_anon_choice_type_id_opt_field_decl_list_9aebd83 (env : env) (x : CST.anon_choice_type_id_opt_field_decl_list_9aebd83) =
  (match x with
  | `Id_opt_field_decl_list (v1, v2) -> R.Case ("Id_opt_field_decl_list",
      let v1 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_field_declaration_list env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Field_decl_list x -> R.Case ("Field_decl_list",
      map_field_declaration_list env x
    )
  )

and map_argument_list (env : env) ((v1, v2, v3) : CST.argument_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_opt___exte___exp_2bc8eaa env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_opt___exte___exp_2bc8eaa env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_assignment_left_expression (env : env) (x : CST.assignment_left_expression) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Field_exp x -> R.Case ("Field_exp",
      map_field_expression env x
    )
  | `Poin_exp x -> R.Case ("Poin_exp",
      map_pointer_expression env x
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  )

and map_attribute (env : env) ((v1, v2, v3) : CST.attribute) =
  let v1 =
    (match v1 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 =
          (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
        in
        let v2 = (* "::" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_attribute_declaration (env : env) ((v1, v2, v3, v4) : CST.attribute_declaration) =
  let v1 = (* "[[" *) token env v1 in
  let v2 = map_attribute env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 = (* "]]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_attribute_specifier (env : env) ((v1, v2, v3, v4) : CST.attribute_specifier) =
  let v1 = (* "__attribute__" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_argument_list env v3 in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_attributed_statement (env : env) ((v1, v2) : CST.attributed_statement) =
  let v1 =
    R.List (List.map (map_attribute_declaration env) v1)
  in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_PLUS_exp (v1, v2, v3) -> R.Case ("Exp_PLUS_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_DASH_exp (v1, v2, v3) -> R.Case ("Exp_DASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_STAR_exp (v1, v2, v3) -> R.Case ("Exp_STAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_SLASH_exp (v1, v2, v3) -> R.Case ("Exp_SLASH_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_PERC_exp (v1, v2, v3) -> R.Case ("Exp_PERC_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BARBAR_exp (v1, v2, v3) -> R.Case ("Exp_BARBAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMPAMP_exp (v1, v2, v3) -> R.Case ("Exp_AMPAMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BAR_exp (v1, v2, v3) -> R.Case ("Exp_BAR_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_HAT_exp (v1, v2, v3) -> R.Case ("Exp_HAT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_AMP_exp (v1, v2, v3) -> R.Case ("Exp_AMP_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_EQEQ_exp (v1, v2, v3) -> R.Case ("Exp_EQEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_BANGEQ_exp (v1, v2, v3) -> R.Case ("Exp_BANGEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GT_exp (v1, v2, v3) -> R.Case ("Exp_GT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTEQ_exp (v1, v2, v3) -> R.Case ("Exp_GTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTEQ_exp (v1, v2, v3) -> R.Case ("Exp_LTEQ_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LT_exp (v1, v2, v3) -> R.Case ("Exp_LT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_LTLT_exp (v1, v2, v3) -> R.Case ("Exp_LTLT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_GTGT_exp (v1, v2, v3) -> R.Case ("Exp_GTGT_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_bitfield_clause (env : env) ((v1, v2) : CST.bitfield_clause) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_block_item (env : env) (x : CST.block_item) =
  (match x with
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Old_style_func_defi x -> R.Case ("Old_style_func_defi",
      map_old_style_function_definition env x
    )
  | `Link_spec x -> R.Case ("Link_spec",
      map_linkage_specification env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Choice_case_stmt x -> R.Case ("Choice_case_stmt",
      map_statement env x
    )
  | `Attr_stmt x -> R.Case ("Attr_stmt",
      map_attributed_statement env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Empty_decl x -> R.Case ("Empty_decl",
      map_empty_declaration env x
    )
  | `Prep_if x -> R.Case ("Prep_if",
      map_preproc_if env x
    )
  | `Prep_ifdef x -> R.Case ("Prep_ifdef",
      map_preproc_ifdef env x
    )
  | `Prep_incl x -> R.Case ("Prep_incl",
      map_preproc_include env x
    )
  | `Prep_def x -> R.Case ("Prep_def",
      map_preproc_def env x
    )
  | `Prep_func_def x -> R.Case ("Prep_func_def",
      map_preproc_function_def env x
    )
  | `Prep_call x -> R.Case ("Prep_call",
      map_preproc_call env x
    )
  )

and map_call_expression (env : env) ((v1, v2) : CST.call_expression) =
  let v1 = map_expression env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_case_statement (env : env) ((v1, v2, v3) : CST.case_statement) =
  let v1 =
    (match v1 with
    | `Case_exp (v1, v2) -> R.Case ("Case_exp",
        let v1 = (* "case" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      )
    | `Defa tok -> R.Case ("Defa",
        (* "default" *) token env tok
      )
    )
  in
  let v2 = (* ":" *) token env v2 in
  let v3 =
    R.List (List.map (fun x ->
      (match x with
      | `Choice_attr_stmt x -> R.Case ("Choice_attr_stmt",
          map_non_case_statement env x
        )
      | `Decl x -> R.Case ("Decl",
          map_declaration env x
        )
      | `Type_defi x -> R.Case ("Type_defi",
          map_type_definition env x
        )
      )
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_block_item env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration (env : env) ((v1, v2, v3) : CST.declaration) =
  let v1 = map_declaration_specifiers env v1 in
  let v2 = map_declaration_declarator env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_declarator (env : env) ((v1, v2) : CST.declaration_declarator) =
  let v1 =
    map_anon_choice_decl_opt_gnu_asm_exp_2c80446 env v1
  in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        map_anon_choice_decl_opt_gnu_asm_exp_2c80446 env v2
      in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_declaration_list (env : env) ((v1, v2, v3) : CST.declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_block_item env) v2) in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_declaration_modifiers (env : env) (x : CST.declaration_modifiers) =
  (match x with
  | `Stor_class_spec x -> R.Case ("Stor_class_spec",
      map_storage_class_specifier env x
    )
  | `Type_qual x -> R.Case ("Type_qual",
      map_type_qualifier env x
    )
  | `Attr_spec x -> R.Case ("Attr_spec",
      map_attribute_specifier env x
    )
  | `Attr_decl x -> R.Case ("Attr_decl",
      map_attribute_declaration env x
    )
  | `Ms_decl_modi x -> R.Case ("Ms_decl_modi",
      map_ms_declspec_modifier env x
    )
  )

and map_declaration_specifiers (env : env) ((v1, v2, v3) : CST.declaration_specifiers) =
  let v1 =
    R.List (List.map (map_declaration_modifiers env) v1)
  in
  let v2 = map_type_specifier env v2 in
  let v3 =
    R.List (List.map (map_declaration_modifiers env) v3)
  in
  R.Tuple [v1; v2; v3]

and map_declarator (env : env) (x : CST.declarator) =
  (match x with
  | `Attr_decl (v1, v2) -> R.Case ("Attr_decl",
      let v1 = map_declarator env v1 in
      let v2 =
        R.List (List.map (map_attribute_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Poin_decl (v1, v2, v3, v4, v5) -> R.Case ("Poin_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_ms_based_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "*" *) token env v2 in
      let v3 =
        R.List (List.map (map_ms_pointer_modifier env) v3)
      in
      let v4 = R.List (List.map (map_type_qualifier env) v4) in
      let v5 = map_declarator env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_decl (v1, v2, v3, v4) -> R.Case ("Func_decl",
      let v1 = map_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_gnu_asm_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        R.List (List.map (map_attribute_specifier env) v4)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Array_decl (v1, v2, v3, v4, v5) -> R.Case ("Array_decl",
      let v1 = map_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = R.List (List.map (map_type_qualifier env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_508611b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Paren_decl (v1, v2, v3) -> R.Case ("Paren_decl",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  )

and map_do_statement (env : env) ((v1, v2, v3, v4, v5) : CST.do_statement) =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_statement env v2 in
  let v3 = (* "while" *) token env v3 in
  let v4 = map_parenthesized_expression env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_else_clause (env : env) ((v1, v2) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 = map_statement env v2 in
  R.Tuple [v1; v2]

and map_empty_declaration (env : env) ((v1, v2) : CST.empty_declaration) =
  let v1 = map_type_specifier env v1 in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_enumerator (env : env) ((v1, v2) : CST.enumerator) =
  let v1 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.enumerator_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      (match x with
      | `Enum_COMMA (v1, v2) -> R.Case ("Enum_COMMA",
          let v1 = map_enumerator env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        )
      | `Prep_if_in_enum_list x -> R.Case ("Prep_if_in_enum_list",
          map_preproc_if_in_enumerator_list env x
        )
      | `Prep_ifdef_in_enum_list x -> R.Case ("Prep_ifdef_in_enum_list",
          map_preproc_ifdef_in_enumerator_list env x
        )
      | `Prep_call_COMMA (v1, v2) -> R.Case ("Prep_call_COMMA",
          let v1 = map_preproc_call env v1 in
          let v2 = (* "," *) token env v2 in
          R.Tuple [v1; v2]
        )
      )
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some v1 -> R.Option (Some (
        (match v1 with
        | `Enum x -> R.Case ("Enum",
            map_enumerator env x
          )
        | `Prep_if_in_enum_list_no_comma x -> R.Case ("Prep_if_in_enum_list_no_comma",
            map_preproc_if_in_enumerator_list_no_comma env x
          )
        | `Prep_ifdef_in_enum_list_no_comma x -> R.Case ("Prep_ifdef_in_enum_list_no_comma",
            map_preproc_ifdef_in_enumerator_list_no_comma env x
          )
        | `Prep_call x -> R.Case ("Prep_call",
            map_preproc_call env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Exp_not_bin x -> R.Case ("Exp_not_bin",
      map_expression_not_binary env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  )

and map_expression_not_binary (env : env) (x : CST.expression_not_binary) =
  (match x with
  | `Cond_exp (v1, v2, v3, v4, v5) -> R.Case ("Cond_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "?" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_expression env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_expression env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Assign_exp (v1, v2, v3) -> R.Case ("Assign_exp",
      let v1 = map_assignment_left_expression env v1 in
      let v2 =
        (match v2 with
        | `EQ tok -> R.Case ("EQ",
            (* "=" *) token env tok
          )
        | `STAREQ tok -> R.Case ("STAREQ",
            (* "*=" *) token env tok
          )
        | `SLASHEQ tok -> R.Case ("SLASHEQ",
            (* "/=" *) token env tok
          )
        | `PERCEQ tok -> R.Case ("PERCEQ",
            (* "%=" *) token env tok
          )
        | `PLUSEQ tok -> R.Case ("PLUSEQ",
            (* "+=" *) token env tok
          )
        | `DASHEQ tok -> R.Case ("DASHEQ",
            (* "-=" *) token env tok
          )
        | `LTLTEQ tok -> R.Case ("LTLTEQ",
            (* "<<=" *) token env tok
          )
        | `GTGTEQ tok -> R.Case ("GTGTEQ",
            (* ">>=" *) token env tok
          )
        | `AMPEQ tok -> R.Case ("AMPEQ",
            (* "&=" *) token env tok
          )
        | `HATEQ tok -> R.Case ("HATEQ",
            (* "^=" *) token env tok
          )
        | `BAREQ tok -> R.Case ("BAREQ",
            (* "|=" *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 = map_anon_choice_BANG_67174d6 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Update_exp x -> R.Case ("Update_exp",
      map_update_expression env x
    )
  | `Cast_exp (v1, v2, v3, v4) -> R.Case ("Cast_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_descriptor env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_expression env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Poin_exp x -> R.Case ("Poin_exp",
      map_pointer_expression env x
    )
  | `Sizeof_exp (v1, v2) -> R.Case ("Sizeof_exp",
      let v1 = (* "sizeof" *) token env v1 in
      let v2 =
        (match v2 with
        | `Exp x -> R.Case ("Exp",
            map_expression env x
          )
        | `LPAR_type_desc_RPAR (v1, v2, v3) -> R.Case ("LPAR_type_desc_RPAR",
            let v1 = (* "(" *) token env v1 in
            let v2 = map_type_descriptor env v2 in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Alig_exp (v1, v2, v3, v4) -> R.Case ("Alig_exp",
      let v1 =
        (match v1 with
        | `X___alig__ tok -> R.Case ("X___alig__",
            (* "__alignof__" *) token env tok
          )
        | `X___alig tok -> R.Case ("X___alig",
            (* "__alignof" *) token env tok
          )
        | `X__alig tok -> R.Case ("X__alig",
            (* "_alignof" *) token env tok
          )
        | `Alig tok -> R.Case ("Alig",
            (* "alignof" *) token env tok
          )
        | `X__Alig tok -> R.Case ("X__Alig",
            (* "_Alignof" *) token env tok
          )
        )
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_descriptor env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Offs_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Offs_exp",
      let v1 = (* "offsetof" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_descriptor env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v5
      in
      let v6 = (* ")" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Gene_exp (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> R.Case ("Gene_exp",
      let v1 = (* "_Generic" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "," *) token env v4 in
      let v5 = map_type_descriptor env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_expression env v7 in
      let v8 =
        R.List (List.map (fun (v1, v2, v3, v4) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_descriptor env v2 in
          let v3 = (* ":" *) token env v3 in
          let v4 = map_expression env v4 in
          R.Tuple [v1; v2; v3; v4]
        ) v8)
      in
      let v9 = (* ")" *) token env v9 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]
    )
  | `Subs_exp x -> R.Case ("Subs_exp",
      map_subscript_expression env x
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Field_exp x -> R.Case ("Field_exp",
      map_field_expression env x
    )
  | `Comp_lit_exp (v1, v2, v3, v4) -> R.Case ("Comp_lit_exp",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_descriptor env v2 in
      let v3 = (* ")" *) token env v3 in
      let v4 = map_initializer_list env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Num_lit tok -> R.Case ("Num_lit",
      (* number_literal *) token env tok
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `True tok -> R.Case ("True",
      (* true *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* false *) token env tok
    )
  | `Null x -> R.Case ("Null",
      map_null env x
    )
  | `Conc_str x -> R.Case ("Conc_str",
      map_concatenated_string env x
    )
  | `Char_lit x -> R.Case ("Char_lit",
      map_char_literal env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Gnu_asm_exp x -> R.Case ("Gnu_asm_exp",
      map_gnu_asm_expression env x
    )
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_field_declaration_declarator (env : env) ((v1, v2, v3) : CST.field_declaration_declarator) =
  let v1 = map_field_declarator env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_bitfield_clause env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    R.List (List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_field_declarator env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_bitfield_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_field_declaration_list (env : env) ((v1, v2, v3) : CST.field_declaration_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    R.List (List.map (map_field_declaration_list_item env) v2)
  in
  let v3 = (* "}" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_field_declaration_list_item (env : env) (x : CST.field_declaration_list_item) =
  (match x with
  | `Field_decl (v1, v2, v3, v4) -> R.Case ("Field_decl",
      let v1 = map_declaration_specifiers env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_field_declaration_declarator env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Prep_def x -> R.Case ("Prep_def",
      map_preproc_def env x
    )
  | `Prep_func_def x -> R.Case ("Prep_func_def",
      map_preproc_function_def env x
    )
  | `Prep_call x -> R.Case ("Prep_call",
      map_preproc_call env x
    )
  | `Prep_if_in_field_decl_list (v1, v2, v3, v4, v5, v6) -> R.Case ("Prep_if_in_field_decl_list",
      let v1 = map_pat_3df6e71 env v1 in
      let v2 = map_preproc_expression env v2 in
      let v3 = (* "\n" *) token env v3 in
      let v4 =
        R.List (List.map (map_field_declaration_list_item env) v4)
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
          ))
        | None -> R.Option None)
      in
      let v6 = map_pat_c46d1b2 env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Prep_ifdef_in_field_decl_list (v1, v2, v3, v4, v5) -> R.Case ("Prep_ifdef_in_field_decl_list",
      let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
      let v2 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
      in
      let v3 =
        R.List (List.map (map_field_declaration_list_item env) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            (match x with
            | `Choice_prep_else_in_field_decl_list x -> R.Case ("Choice_prep_else_in_field_decl_list",
                map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
              )
            | `Prep_elif_in_field_decl_list x -> R.Case ("Prep_elif_in_field_decl_list",
                map_preproc_elifdef_in_field_declaration_list env x
              )
            )
          ))
        | None -> R.Option None)
      in
      let v5 = map_pat_c46d1b2 env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_field_declarator (env : env) (x : CST.field_declarator) =
  (match x with
  | `Attr_field_decl (v1, v2) -> R.Case ("Attr_field_decl",
      let v1 = map_field_declarator env v1 in
      let v2 =
        R.List (List.map (map_attribute_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Poin_field_decl (v1, v2, v3, v4, v5) -> R.Case ("Poin_field_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_ms_based_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "*" *) token env v2 in
      let v3 =
        R.List (List.map (map_ms_pointer_modifier env) v3)
      in
      let v4 = R.List (List.map (map_type_qualifier env) v4) in
      let v5 = map_field_declarator env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_field_decl (v1, v2) -> R.Case ("Func_field_decl",
      let v1 = map_field_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Array_field_decl (v1, v2, v3, v4, v5) -> R.Case ("Array_field_decl",
      let v1 = map_field_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = R.List (List.map (map_type_qualifier env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_508611b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Paren_field_decl (v1, v2, v3) -> R.Case ("Paren_field_decl",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_field_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  )

and map_field_expression (env : env) ((v1, v2, v3) : CST.field_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `DOT tok -> R.Case ("DOT",
        (* "." *) token env tok
      )
    | `DASHGT tok -> R.Case ("DASHGT",
        (* "->" *) token env tok
      )
    )
  in
  let v3 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v3
  in
  R.Tuple [v1; v2; v3]

and map_for_statement (env : env) ((v1, v2, v3, v4, v5) : CST.for_statement) =
  let v1 = (* "for" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_for_statement_body env v3 in
  let v4 = (* ")" *) token env v4 in
  let v5 = map_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_for_statement_body (env : env) ((v1, v2, v3, v4) : CST.for_statement_body) =
  let v1 =
    (match v1 with
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Opt_choice_exp_SEMI x -> R.Case ("Opt_choice_exp_SEMI",
        map_expression_statement env x
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_function_definition (env : env) ((v1, v2, v3, v4) : CST.function_definition) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_ms_call_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_declarator env v3 in
  let v4 = map_compound_statement env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_gnu_asm_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.gnu_asm_expression) =
  let v1 =
    (match v1 with
    | `Asm tok -> R.Case ("Asm",
        (* "asm" *) token env tok
      )
    | `X___asm__ tok -> R.Case ("X___asm__",
        (* "__asm__" *) token env tok
      )
    )
  in
  let v2 = R.List (List.map (map_gnu_asm_qualifier env) v2) in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (match v4 with
    | `Str_lit x -> R.Case ("Str_lit",
        map_string_literal env x
      )
    | `Conc_str x -> R.Case ("Conc_str",
        map_concatenated_string env x
      )
    )
  in
  let v5 =
    (match v5 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_gnu_asm_output_operand_list env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_gnu_asm_input_operand_list env v1 in
              let v2 =
                (match v2 with
                | Some (v1, v2) -> R.Option (Some (
                    let v1 = map_gnu_asm_clobber_list env v1 in
                    let v2 =
                      (match v2 with
                      | Some x -> R.Option (Some (
                          map_gnu_asm_goto_list env x
                        ))
                      | None -> R.Option None)
                    in
                    R.Tuple [v1; v2]
                  ))
                | None -> R.Option None)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_gnu_asm_input_operand (env : env) ((v1, v2, v3, v4, v5) : CST.gnu_asm_input_operand) =
  let v1 =
    (match v1 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "[" *) token env v1 in
        let v2 =
          (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
        in
        let v3 = (* "]" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v2 = map_string_literal env v2 in
  let v3 = (* "(" *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_gnu_asm_input_operand_list (env : env) ((v1, v2) : CST.gnu_asm_input_operand_list) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_gnu_asm_input_operand env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_gnu_asm_input_operand env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_if_statement (env : env) ((v1, v2, v3, v4) : CST.if_statement) =
  let v1 = (* "if" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_else_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_initializer_list (env : env) ((v1, v2, v3, v4) : CST.initializer_list) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_init_pair_1a6981e env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_init_pair_1a6981e env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_initializer_pair (env : env) (x : CST.initializer_pair) =
  (match x with
  | `Rep1_choice_subs_desi_EQ_choice_exp (v1, v2, v3) -> R.Case ("Rep1_choice_subs_desi_EQ_choice_exp",
      let v1 =
        R.List (List.map (fun x ->
          (match x with
          | `Subs_desi x -> R.Case ("Subs_desi",
              map_subscript_designator env x
            )
          | `Field_desi x -> R.Case ("Field_desi",
              map_field_designator env x
            )
          | `Subs_range_desi x -> R.Case ("Subs_range_desi",
              map_subscript_range_designator env x
            )
          )
        ) v1)
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_anon_choice_exp_3078596 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id_COLON_choice_exp (v1, v2, v3) -> R.Case ("Id_COLON_choice_exp",
      let v1 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 = (* ":" *) token env v2 in
      let v3 = map_anon_choice_exp_3078596 env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_labeled_statement (env : env) ((v1, v2, v3) : CST.labeled_statement) =
  let v1 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
  in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_linkage_specification (env : env) ((v1, v2, v3) : CST.linkage_specification) =
  let v1 = (* "extern" *) token env v1 in
  let v2 = map_string_literal env v2 in
  let v3 =
    (match v3 with
    | `Func_defi x -> R.Case ("Func_defi",
        map_function_definition env x
      )
    | `Decl x -> R.Case ("Decl",
        map_declaration env x
      )
    | `Decl_list x -> R.Case ("Decl_list",
        map_declaration_list env x
      )
    )
  in
  R.Tuple [v1; v2; v3]

and map_ms_based_modifier (env : env) ((v1, v2) : CST.ms_based_modifier) =
  let v1 = (* "__based" *) token env v1 in
  let v2 = map_argument_list env v2 in
  R.Tuple [v1; v2]

and map_non_case_statement (env : env) (x : CST.non_case_statement) =
  (match x with
  | `Attr_stmt x -> R.Case ("Attr_stmt",
      map_attributed_statement env x
    )
  | `Labe_stmt x -> R.Case ("Labe_stmt",
      map_labeled_statement env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Exp_stmt x -> R.Case ("Exp_stmt",
      map_expression_statement env x
    )
  | `If_stmt x -> R.Case ("If_stmt",
      map_if_statement env x
    )
  | `Switch_stmt x -> R.Case ("Switch_stmt",
      map_switch_statement env x
    )
  | `Do_stmt x -> R.Case ("Do_stmt",
      map_do_statement env x
    )
  | `While_stmt x -> R.Case ("While_stmt",
      map_while_statement env x
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `Ret_stmt x -> R.Case ("Ret_stmt",
      map_return_statement env x
    )
  | `Brk_stmt x -> R.Case ("Brk_stmt",
      map_break_statement env x
    )
  | `Cont_stmt x -> R.Case ("Cont_stmt",
      map_continue_statement env x
    )
  | `Goto_stmt x -> R.Case ("Goto_stmt",
      map_goto_statement env x
    )
  | `Seh_try_stmt (v1, v2, v3) -> R.Case ("Seh_try_stmt",
      let v1 = (* "__try" *) token env v1 in
      let v2 = map_compound_statement env v2 in
      let v3 =
        (match v3 with
        | `Seh_except_clause x -> R.Case ("Seh_except_clause",
            map_seh_except_clause env x
          )
        | `Seh_fina_clause x -> R.Case ("Seh_fina_clause",
            map_seh_finally_clause env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Seh_leave_stmt (v1, v2) -> R.Case ("Seh_leave_stmt",
      let v1 = (* "__leave" *) token env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_old_style_function_declarator (env : env) ((v1, v2) : CST.old_style_function_declarator) =
  let v1 = map_declarator env v1 in
  let v2 = map_old_style_parameter_list env v2 in
  R.Tuple [v1; v2]

and map_old_style_function_definition (env : env) ((v1, v2, v3, v4, v5) : CST.old_style_function_definition) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_ms_call_modifier env x
      ))
    | None -> R.Option None)
  in
  let v2 = map_declaration_specifiers env v2 in
  let v3 = map_old_style_function_declarator env v3 in
  let v4 = R.List (List.map (map_declaration env) v4) in
  let v5 = map_compound_statement env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_parameter_list (env : env) ((v1, v2, v3) : CST.parameter_list) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_param_decl_4ac2852 env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_anon_choice_param_decl_4ac2852 env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_exp_55b4dba env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_pointer_expression (env : env) ((v1, v2) : CST.pointer_expression) =
  let v1 =
    (match v1 with
    | `STAR tok -> R.Case ("STAR",
        (* "*" *) token env tok
      )
    | `AMP tok -> R.Case ("AMP",
        (* "&" *) token env tok
      )
    )
  in
  let v2 = map_expression env v2 in
  R.Tuple [v1; v2]

and map_preproc_elifdef (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_block_item env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_8b52b0f env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_elifdef_in_enumerator_list (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef_in_enumerator_list) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_enumerator env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_list_8258275 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_elifdef_in_enumerator_list_no_comma (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef_in_enumerator_list_no_comma) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_enumerator env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_elifdef_in_field_declaration_list (env : env) ((v1, v2, v3, v4) : CST.preproc_elifdef_in_field_declaration_list) =
  let v1 = map_anon_choice_pat_0307ca2_dbf6a9d env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (map_field_declaration_list_item env) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_field_decl_list_97ea65e env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_preproc_if (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 = R.List (List.map (map_block_item env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_8b52b0f env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_enumerator_list (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enumerator_list) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_enumerator env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_list_8258275 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_if_in_enumerator_list_no_comma (env : env) ((v1, v2, v3, v4, v5, v6) : CST.preproc_if_in_enumerator_list_no_comma) =
  let v1 = map_pat_3df6e71 env v1 in
  let v2 = map_preproc_expression env v2 in
  let v3 = (* "\n" *) token env v3 in
  let v4 = R.List (List.map (map_enumerator env) v4) in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
      ))
    | None -> R.Option None)
  in
  let v6 = map_pat_c46d1b2 env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_preproc_ifdef (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_block_item env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_prep_else x -> R.Case ("Choice_prep_else",
            map_anon_choice_prep_else_8b52b0f env x
          )
        | `Prep_elif x -> R.Case ("Prep_elif",
            map_preproc_elifdef env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_preproc_ifdef_in_enumerator_list (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_enumerator_list) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_enumerator env v1 in
      let v2 = (* "," *) token env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_prep_else_in_enum_list x -> R.Case ("Choice_prep_else_in_enum_list",
            map_anon_choice_prep_else_in_enum_list_8258275 env x
          )
        | `Prep_elif_in_enum_list x -> R.Case ("Prep_elif_in_enum_list",
            map_preproc_elifdef_in_enumerator_list env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_preproc_ifdef_in_enumerator_list_no_comma (env : env) ((v1, v2, v3, v4, v5) : CST.preproc_ifdef_in_enumerator_list_no_comma) =
  let v1 = map_anon_choice_pat_25b90ba_4a37f8c env v1 in
  let v2 =
    (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v2
  in
  let v3 = R.List (List.map (map_enumerator env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        (match x with
        | `Choice_prep_else_in_enum_list_no_comma x -> R.Case ("Choice_prep_else_in_enum_list_no_comma",
            map_anon_choice_prep_else_in_enum_list_no_comma_04fd5a5 env x
          )
        | `Prep_elif_in_enum_list_no_comma x -> R.Case ("Prep_elif_in_enum_list_no_comma",
            map_preproc_elifdef_in_enumerator_list_no_comma env x
          )
        )
      ))
    | None -> R.Option None)
  in
  let v5 = map_pat_c46d1b2 env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_return_statement (env : env) ((v1, v2, v3) : CST.return_statement) =
  let v1 = (* "return" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_choice_exp_55b4dba env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_seh_except_clause (env : env) ((v1, v2, v3) : CST.seh_except_clause) =
  let v1 = (* "__except" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_seh_finally_clause (env : env) ((v1, v2) : CST.seh_finally_clause) =
  let v1 = (* "__finally" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  R.Tuple [v1; v2]

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Case_stmt x -> R.Case ("Case_stmt",
      map_case_statement env x
    )
  | `Choice_attr_stmt x -> R.Case ("Choice_attr_stmt",
      map_non_case_statement env x
    )
  )

and map_subscript_designator (env : env) ((v1, v2, v3) : CST.subscript_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "]" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_subscript_expression (env : env) ((v1, v2, v3, v4) : CST.subscript_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "[" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_subscript_range_designator (env : env) ((v1, v2, v3, v4, v5) : CST.subscript_range_designator) =
  let v1 = (* "[" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "..." *) token env v3 in
  let v4 = map_expression env v4 in
  let v5 = (* "]" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_switch_statement (env : env) ((v1, v2, v3) : CST.switch_statement) =
  let v1 = (* "switch" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_compound_statement env v3 in
  R.Tuple [v1; v2; v3]

and map_type_declarator (env : env) (x : CST.type_declarator) =
  (match x with
  | `Attr_type_decl (v1, v2) -> R.Case ("Attr_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 =
        R.List (List.map (map_attribute_declaration env) v2)
      in
      R.Tuple [v1; v2]
    )
  | `Poin_type_decl (v1, v2, v3, v4, v5) -> R.Case ("Poin_type_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_ms_based_modifier env x
          ))
        | None -> R.Option None)
      in
      let v2 = (* "*" *) token env v2 in
      let v3 =
        R.List (List.map (map_ms_pointer_modifier env) v3)
      in
      let v4 = R.List (List.map (map_type_qualifier env) v4) in
      let v5 = map_type_declarator env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Func_type_decl (v1, v2) -> R.Case ("Func_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 = map_parameter_list env v2 in
      R.Tuple [v1; v2]
    )
  | `Array_type_decl (v1, v2, v3, v4, v5) -> R.Case ("Array_type_decl",
      let v1 = map_type_declarator env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = R.List (List.map (map_type_qualifier env) v3) in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_choice_exp_508611b env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Paren_type_decl (v1, v2, v3) -> R.Case ("Paren_type_decl",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_type_declarator env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  | `Choice_signed x -> R.Case ("Choice_signed",
      map_anon_choice_signed_a0bfc19 env x
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  )

and map_type_definition (env : env) ((v1, v2, v3, v4, v5, v6) : CST.type_definition) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "__extension__" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "typedef" *) token env v2 in
  let v3 = map_type_definition_type env v3 in
  let v4 = map_type_definition_declarators env v4 in
  let v5 =
    R.List (List.map (map_attribute_specifier env) v5)
  in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_type_definition_declarators (env : env) ((v1, v2) : CST.type_definition_declarators) =
  let v1 = map_type_declarator env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_declarator env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_type_definition_type (env : env) ((v1, v2, v3) : CST.type_definition_type) =
  let v1 = R.List (List.map (map_type_qualifier env) v1) in
  let v2 = map_type_specifier env v2 in
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
  R.Tuple [v1; v2; v3]

and map_type_descriptor (env : env) ((v1, v2, v3, v4) : CST.type_descriptor) =
  let v1 = R.List (List.map (map_type_qualifier env) v1) in
  let v2 = map_type_specifier env v2 in
  let v3 = R.List (List.map (map_type_qualifier env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_abstract_declarator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_type_specifier (env : env) (x : CST.type_specifier) =
  (match x with
  | `Struct_spec (v1, v2, v3, v4, v5) -> R.Case ("Struct_spec",
      let v1 = (* "struct" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_ms_declspec_modifier env x
          ))
        | None -> R.Option None)
      in
      let v4 =
        map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v4
      in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Union_spec (v1, v2, v3, v4) -> R.Case ("Union_spec",
      let v1 = (* "union" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_ms_declspec_modifier env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        map_anon_choice_type_id_opt_field_decl_list_9aebd83 env v3
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Enum_spec (v1, v2, v3) -> R.Case ("Enum_spec",
      let v1 = (* "enum" *) token env v1 in
      let v2 =
        (match v2 with
        | `Id_opt_COLON_prim_type_opt_enum_list (v1, v2, v3) -> R.Case ("Id_opt_COLON_prim_type_opt_enum_list",
            let v1 =
              (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
            in
            let v2 =
              (match v2 with
              | Some (v1, v2) -> R.Option (Some (
                  let v1 = (* ":" *) token env v1 in
                  let v2 = (* primitive_type *) token env v2 in
                  R.Tuple [v1; v2]
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_enumerator_list env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Enum_list x -> R.Case ("Enum_list",
            map_enumerator_list env x
          )
        )
      in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_attribute_specifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_type_spec (v1, v2, v3, v4) -> R.Case ("Macro_type_spec",
      let v1 =
        (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env v1
      in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_descriptor env v3 in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Sized_type_spec x -> R.Case ("Sized_type_spec",
      map_sized_type_specifier env x
    )
  | `Prim_type tok -> R.Case ("Prim_type",
      (* primitive_type *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* pattern (\p{XID_Start}|\$|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\$|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})* *) token env tok
    )
  )

and map_update_expression (env : env) (x : CST.update_expression) =
  (match x with
  | `Choice_DASHDASH_exp (v1, v2) -> R.Case ("Choice_DASHDASH_exp",
      let v1 = map_anon_choice_DASHDASH_d11def2 env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Exp_choice_DASHDASH (v1, v2) -> R.Case ("Exp_choice_DASHDASH",
      let v1 = map_expression env v1 in
      let v2 = map_anon_choice_DASHDASH_d11def2 env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_while_statement (env : env) ((v1, v2, v3) : CST.while_statement) =
  let v1 = (* "while" *) token env v1 in
  let v2 = map_parenthesized_expression env v2 in
  let v3 = map_statement env v3 in
  R.Tuple [v1; v2; v3]

let map_top_level_statement (env : env) (x : CST.top_level_statement) =
  (match x with
  | `Case_stmt x -> R.Case ("Case_stmt",
      map_case_statement env x
    )
  | `Attr_stmt x -> R.Case ("Attr_stmt",
      map_attributed_statement env x
    )
  | `Labe_stmt x -> R.Case ("Labe_stmt",
      map_labeled_statement env x
    )
  | `Comp_stmt x -> R.Case ("Comp_stmt",
      map_compound_statement env x
    )
  | `Top_level_exp_stmt (v1, v2) -> R.Case ("Top_level_exp_stmt",
      let v1 = map_expression_not_binary env v1 in
      let v2 = (* ";" *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `If_stmt x -> R.Case ("If_stmt",
      map_if_statement env x
    )
  | `Switch_stmt x -> R.Case ("Switch_stmt",
      map_switch_statement env x
    )
  | `Do_stmt x -> R.Case ("Do_stmt",
      map_do_statement env x
    )
  | `While_stmt x -> R.Case ("While_stmt",
      map_while_statement env x
    )
  | `For_stmt x -> R.Case ("For_stmt",
      map_for_statement env x
    )
  | `Ret_stmt x -> R.Case ("Ret_stmt",
      map_return_statement env x
    )
  | `Brk_stmt x -> R.Case ("Brk_stmt",
      map_break_statement env x
    )
  | `Cont_stmt x -> R.Case ("Cont_stmt",
      map_continue_statement env x
    )
  | `Goto_stmt x -> R.Case ("Goto_stmt",
      map_goto_statement env x
    )
  )

let map_top_level_item (env : env) (x : CST.top_level_item) =
  (match x with
  | `Func_defi x -> R.Case ("Func_defi",
      map_function_definition env x
    )
  | `Old_style_func_defi x -> R.Case ("Old_style_func_defi",
      map_old_style_function_definition env x
    )
  | `Link_spec x -> R.Case ("Link_spec",
      map_linkage_specification env x
    )
  | `Decl x -> R.Case ("Decl",
      map_declaration env x
    )
  | `Choice_case_stmt x -> R.Case ("Choice_case_stmt",
      map_top_level_statement env x
    )
  | `Attr_stmt x -> R.Case ("Attr_stmt",
      map_attributed_statement env x
    )
  | `Type_defi x -> R.Case ("Type_defi",
      map_type_definition env x
    )
  | `Empty_decl x -> R.Case ("Empty_decl",
      map_empty_declaration env x
    )
  | `Prep_if x -> R.Case ("Prep_if",
      map_preproc_if env x
    )
  | `Prep_ifdef x -> R.Case ("Prep_ifdef",
      map_preproc_ifdef env x
    )
  | `Prep_incl x -> R.Case ("Prep_incl",
      map_preproc_include env x
    )
  | `Prep_def x -> R.Case ("Prep_def",
      map_preproc_def env x
    )
  | `Prep_func_def x -> R.Case ("Prep_func_def",
      map_preproc_function_def env x
    )
  | `Prep_call x -> R.Case ("Prep_call",
      map_preproc_call env x
    )
  )

let map_translation_unit (env : env) (xs : CST.translation_unit) =
  R.List (List.map (map_top_level_item env) xs)

let map_comment (env : env) (tok : CST.comment) =
  (* comment *) token env tok

let dump_tree root =
  map_translation_unit () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Comment (_loc, x) -> ("comment", "comment", map_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
