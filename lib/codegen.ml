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
      | Syntax.Times -> build_mul lhs_val rhs_val "multmp" ctx.llvm_builder)
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

let rec codegen_statement ctx = function
  | Syntax.Let (_, _) -> raise (Error "let: unimplemented")
  | Syntax.Return _ -> raise (Error "return must be inside a function")
  | Syntax.Function (proto, body) -> (
      let (Syntax.Prototype (name, args)) = proto in
      let ft =
        function_type
          (i64_type ctx.llvm_context)
          (Array.make (List.length args) (i64_type ctx.llvm_context))
      in
      let f =
        match lookup_function name ctx.llvm_module with
        | None -> declare_function name ft ctx.llvm_module
        | Some f ->
            (* if f already has a body, we can't reuse this name *)
            if Array.length (basic_blocks f) == 0 then ()
            else
              raise (Error (Printf.sprintf "redefinition of function: %s" name));
            (* ensure prototypes match *)
            if Array.length (params f) == List.length args then ()
            else
              raise
                (Error
                   (Printf.sprintf
                      "definition of function %s does not match forward \
                       declaration"
                      name));
            f
      in
      let fnscope =
        List.mapi
          (fun i arg_name ->
            let p = (params f).(i) in
            set_value_name arg_name p;
            (arg_name, (p, i64_type ctx.llvm_context)))
          args
      in
      let fnctx =
        {
          ctx with
          scope = StringMap.add_seq (fnscope |> List.to_seq) ctx.scope;
          func = Some (name, f);
        }
      in
      let name, fn = unsafe_unpack fnctx.func in
      let bb = append_block fnctx.llvm_context "entry" fn in
      position_at_end bb fnctx.llvm_builder;
      let rec codegen_func ctx = function
        | Syntax.Return e :: _ ->
            let ret = codegen_expr ctx e in
            ignore @@ build_ret ret ctx.llvm_builder;
            Llvm_analysis.assert_valid_function fn
        | h :: t ->
            let ctx = codegen_statement ctx h in
            codegen_func ctx t
        | [] -> raise (Error "no return in function")
      in
      try
        codegen_func fnctx body;
        { ctx with scope = StringMap.add name (fn, ft) ctx.scope }
      with e ->
        delete_function fn;
        (* FIXME: bug with deleting forward declarations *)
        raise e)

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

let rec codegen ?(ctx = create_ctx ()) = function
  | h :: t ->
      let ctx = codegen_statement ctx h in
      codegen ~ctx t
  | [] -> ctx
