open Llvm

exception Error of string
let context = global_context ()
let the_module = create_module context "the module"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let i64_type = i64_type context

let rec codegen_expr = function
  | Syntax.Number n -> const_int i64_type n
  | Syntax.Variable name ->
      (try Hashtbl.find named_values name with
        | Not_found -> raise (Error (Printf.sprintf "unknown variable name: %s" name)))
  | Syntax.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs in
      let rhs_val = codegen_expr rhs in
      begin
        match op with
        | Syntax.Plus -> build_add lhs_val rhs_val "addtmp" builder
        | Syntax.Times -> build_mul lhs_val rhs_val "multmp" builder
      end

let codegen_proto = function
  | Syntax.Prototype (name, args) ->
      let ints = Array.make (List.length args) i64_type in
      let ft = function_type i64_type ints in
      let f =
        match lookup_function name the_module with
        | None -> declare_function name ft the_module
        | Some f ->
            (* if f already has a body, we can't reuse this name *)
            if Array.length (basic_blocks f) == 0 then () else
              raise (Error (Printf.sprintf "redefinition of function: %s" name));
            (* ensure prototypes match *)
            if Array.length (params f) == List.length args then () else
              raise (Error (Printf.sprintf "redefinition of function %s with mismatched args" name));
            f
      in
      let ps = params f in
      (* set argument names *)
      List.iteri (fun i arg_name ->
        let p = ps.(i) in
        set_value_name arg_name p;
        Hashtbl.add named_values arg_name p;
      ) args;
      f

let codegen_statement = function
  | Syntax.Let (_, _) -> raise (Error "let: unimplemented")
  | Syntax.Return _ -> raise (Error "return must be inside a function")
  | Syntax.Function (proto, body) ->
      Hashtbl.clear named_values; (* TODO: global scoping! *)
      let the_function = codegen_proto proto in
      let bb = append_block context "entry" the_function in
      position_at_end bb builder;
      try
        let ret_val =
          match body with
          | [Syntax.Return e] -> codegen_expr e
          | _ -> raise (Error "functions only support single return statements (for now!)")
        in
        let _ = build_ret ret_val builder in
        Llvm_analysis.assert_valid_function the_function;
        the_function
      with e ->
        delete_function the_function; (* FIXME: bug with deleting forward declarations *)
        raise e

