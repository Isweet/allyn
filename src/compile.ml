open Core
open Llvm

open Syntax

type llsymbols = (String.t, llvalue) Hashtbl.t

type llenv =
  { context : llcontext
  ; modul   : llmodule
  ; builder : llbuilder
  ; symbols : llsymbols
  }

let rec compile_expr (env : llenv) (e : Syntax.expr) : llvalue Option.t =
  let double_type = double_type env.context in
  let open Option.Monad_infix in
  match e.base with
  | Syntax.Number n        ->
    Option.return (const_float double_type n.value)
  | Syntax.Variable var    ->
    Hashtbl.find env.symbols var.value
  | Syntax.ABinOp abinop   ->
    compile_expr env abinop.lhs >>= fun lhs ->
    compile_expr env abinop.rhs >>= fun rhs ->
    Option.return
      (match abinop.op with
       | Arith.Bin.Op.Add      -> build_fadd lhs rhs "add" env.builder
       | Arith.Bin.Op.Mult     -> build_fmul lhs rhs "mul" env.builder
       | Arith.Bin.Op.Subtract -> build_fsub lhs rhs "sub" env.builder)
  | Syntax.ABinRel abinrel ->
    compile_expr env abinrel.lhs >>= fun lhs ->
    compile_expr env abinrel.rhs >>= fun rhs ->
    Option.return
      (match abinrel.op with
       | Arith.Bin.Rel.Equal    ->
         let i = build_fcmp Fcmp.Ueq lhs rhs "eq" env.builder in
         build_uitofp i double_type "(float) eq" env.builder
       | Arith.Bin.Rel.LessThan ->
         let i = build_fcmp Fcmp.Ult lhs rhs "lt" env.builder in
         build_uitofp i double_type "(float) lt" env.builder)
  | Syntax.Call call       ->
    lookup_function call.value env.modul >>= fun f ->
    let params = params f in
    if Array.length params = Array.length call.args then
      Option.all (List.map ~f:(compile_expr env) (Array.to_list call.args)) >>= fun args_l ->
      let args = List.to_array args_l in
      Option.return (build_call f args "call" env.builder)
    else
      None

let compile_proto (env : llenv) (p : Syntax.proto) : llvalue Option.t =
  let double_type = double_type env.context in
  let args_t = Array.create ~len:(Array.length p.base.params) double_type in
  let ret_t = double_type in
  let f_t = function_type ret_t args_t in
  let open Option.Monad_infix in
  begin
    match lookup_function p.base.name env.modul with
    | None   ->
      Option.return (declare_function p.base.name f_t env.modul)
    | Some f ->
      if Array.length (basic_blocks f) = 0 then
        if Array.length (params f) = Array.length p.base.params then
          Option.return f
        else
          None
      else
        None
  end >>= fun f ->
  Array.iteri
    ~f:(fun i a ->
        let n = p.base.params.(i) in
        set_value_name n a;
        ignore(Hashtbl.add env.symbols ~key:n ~data:a))
    (params f);
  Option.return f

let compile_func (env : llenv) (f : Syntax.func) : llvalue Option.t =
  Hashtbl.clear env.symbols;
  let open Option.Monad_infix in
  compile_proto env f.base.header >>= fun header ->
  let entry_bb = append_block env.context "entry" header in
  position_at_end entry_bb env.builder;
  match compile_expr env f.base.body with
  | None   ->
    delete_function header;
    None
  | Some e ->
    ignore(build_ret e env.builder);
    Llvm_analysis.assert_valid_function header;
    Option.return header

let compile_prog (env : llenv) (prog : Syntax.prog) : (llvalue List.t) Option.t =
  Option.all (List.map
                ~f:(fun t ->
                    match t with
                    | Expression e ->
                      let anonymous = { loc  = e.value.loc
                                      ; base = { header = { loc  = e.value.loc
                                                          ; base = { name = ""
                                                                   ; params = [| |]
                                                                   }
                                                          }
                                               ; body   = e.value }
                                      }
                      in
                      compile_func env anonymous
                    | Extern p     -> compile_proto env p.value
                    | Definition f -> compile_func env f.value)
                prog)
