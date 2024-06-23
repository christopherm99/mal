open Llvm

exception Error of string

module StringMap = Map.Make (String)

type context = {
  llvm_context : llcontext;
  llvm_module : llmodule;
  llvm_builder : llbuilder;
  scope : (llvalue * lltype) StringMap.t; (* lexical scoping *)
  func : (string * llvalue) option; (* current enclosing function *)
}

let unsafe_unpack = function
  | Some x -> x
  | None -> raise (Invalid_argument "failed to unpack")

let create_ctx () =
  let llctx = global_context () in
  {
    llvm_context = llctx;
    llvm_module = create_module llctx "module name";
    llvm_builder = builder llctx;
    scope = StringMap.empty;
    (* TODO: what's a good default size? *)
    func = None;
  }

let rec codegen_expr ctx = function
  | Syntax.Number n -> const_int (i64_type ctx.llvm_context) n
  | Syntax.Variable name -> (
      match StringMap.find_opt name ctx.scope with
      | Some (v, _) -> v
      | None -> raise (Error (Printf.sprintf "unknown variable name: %s" name)))
  | Syntax.Binary (op, lhs, rhs) -> (
      let lhs_val = codegen_expr ctx lhs in
      let rhs_val = codegen_expr ctx rhs in
      match op with
      | Syntax.Plus -> build_add lhs_val rhs_val "addtmp" ctx.llvm_builder
      | Syntax.Times -> build_mul lhs_val rhs_val "multmp" ctx.llvm_builder
      | Syntax.Minus -> build_sub lhs_val rhs_val "subtmp" ctx.llvm_builder)
  | Syntax.Call (name, args) ->
      let fn, ft =
        match StringMap.find_opt name ctx.scope with
        | Some x -> x
        | None ->
            print_string "scope dump: ";
            StringMap.iter (fun key _ -> Printf.printf "%s " key) ctx.scope;
            print_string "\n";
            raise
              (Error (Printf.sprintf "function named '%s' is not defined" name))
      in
      Printf.printf "building call. %s\n%!"
        (string_of_lltype (function_type (i64_type ctx.llvm_context) [||]));
      build_call ft fn
        (Array.of_list (List.map (codegen_expr ctx) args))
        "calltmp" ctx.llvm_builder

and codegen_statement ctx = function
  | Syntax.Let (name, expr) ->
      (* TODO: should this be a real variable? *)
      {
        ctx with
        scope =
          StringMap.add name
            (codegen_expr ctx expr, i64_type ctx.llvm_context)
            ctx.scope;
      }
  | Syntax.If (cond, then_, else_) -> (
      match ctx.func with
      | Some (_, func) ->
          (* TODO: support non-ints *)
          let comp =
            build_icmp Icmp.Ne (codegen_expr ctx cond)
              (const_int (i64_type ctx.llvm_context) 0)
              "ifcond" ctx.llvm_builder
          in
          let if_bb = insertion_block ctx.llvm_builder in
          let then_bb = append_block ctx.llvm_context "then" func in
          let end_bb = append_block ctx.llvm_context "end" func in
          position_at_end then_bb ctx.llvm_builder;
          ignore @@ codegen ~ctx then_;
          (match block_terminator (insertion_block ctx.llvm_builder) with
          | Some _ -> ()
          | None -> ignore @@ build_br end_bb ctx.llvm_builder);
          (match else_ with
          | Some else_ ->
              let then_end_bb = insertion_block ctx.llvm_builder in
              let else_bb = insert_block ctx.llvm_context "else" end_bb in
              position_at_end else_bb ctx.llvm_builder;
              ignore @@ codegen ~ctx else_;
              (match
                 ( block_terminator then_end_bb,
                   block_terminator (insertion_block ctx.llvm_builder) )
               with
              | Some _, Some _ -> delete_block end_bb
              | _, None -> ignore @@ build_br end_bb ctx.llvm_builder
              | _ -> ());
              position_at_end if_bb ctx.llvm_builder;
              ignore @@ build_cond_br comp then_bb else_bb ctx.llvm_builder
          | None ->
              position_at_end if_bb ctx.llvm_builder;
              ignore @@ build_cond_br comp then_bb end_bb ctx.llvm_builder);
          position_at_end end_bb ctx.llvm_builder;
          (* this seems wrong, but doesn't cause a crash? *)
          ctx
      | None -> raise (Error "if statement must be inside a function"))
  | Syntax.Return expr -> (
      match ctx.func with
      | Some _ ->
          let ret = codegen_expr ctx expr in
          ignore @@ build_ret ret ctx.llvm_builder;
          ctx
      | None -> raise (Error "return must be inside a function"))
  | Syntax.Function (proto, body) -> (
      let (Syntax.Prototype (name, args)) = proto in
      let ft =
        function_type
          (i64_type ctx.llvm_context)
          (Array.make (List.length args) (i64_type ctx.llvm_context))
      in
      let fn =
        match lookup_function name ctx.llvm_module with
        | None -> declare_function name ft ctx.llvm_module
        | Some fn ->
            (* if f already has a body, we can't reuse this name *)
            if Array.length (basic_blocks fn) == 0 then ()
            else
              raise (Error (Printf.sprintf "redefinition of function: %s" name));
            (* ensure prototypes match *)
            if Array.length (params fn) == List.length args then ()
            else
              raise
                (Error
                   (Printf.sprintf
                      "definition of function %s does not match forward \
                       declaration"
                      name));
            fn
      in
      let fnscope =
        List.mapi
          (fun i arg_name ->
            let p = (params fn).(i) in
            set_value_name arg_name p;
            (arg_name, (p, i64_type ctx.llvm_context)))
          args
      in
      let ctx = { ctx with scope = StringMap.add name (fn, ft) ctx.scope } in
      let fnctx =
        {
          ctx with
          scope = StringMap.add_seq (fnscope |> List.to_seq) ctx.scope;
          func = Some (name, fn);
        }
      in
      let bb = append_block fnctx.llvm_context "entry" fn in
      position_at_end bb fnctx.llvm_builder;
      ignore @@ codegen ~ctx:fnctx body;
      Llvm.dump_module ctx.llvm_module;
      try
        Llvm_analysis.assert_valid_function fn;
        ctx
      with e ->
        Llvm.dump_module ctx.llvm_module;
        delete_function fn;
        (* FIXME: bug with deleting forward declarations *)
        raise e)

and codegen ?(ctx = create_ctx ()) = function
  | h :: t ->
      let ctx = codegen_statement ctx h in
      codegen ~ctx t
  | [] -> ctx
