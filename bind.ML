

(* LEX *)
val trace_lex = ref false;

(* EMBED *)
val optimize_multi_app = ref true;
val optimize_multi_fn = ref true;
val optimize_prim_app = ref true;

(* CPS / COMPILE *)
val right_to_left_tuples = ref false;
val right_to_left_args = ref true;

(* CPS *)
val lift_lets = ref false;
val bind_all_prim_app = ref false;

(* COMPILE *)
val delay_select = ref true;
val new_pat_comp = ref true;
val opt_last_constr = ref true;

(* EVAL3 *)
val print_lang = ref false;
val print_embed = ref false;
val print_cps = ref false;
val print_code = ref false;

(* HERE *)
val run_compiler = ref true;

(* MACHINE *)
val display_counts = ref false;

(* RUN *)
val quiet = ref false;


structure Prel = PREL();
structure Sort = SORT();

structure Assoc = ASSOC(structure Prel = Prel)

structure QLayout = QLAYOUT(structure Prel = Prel)

structure Misclay = MISCLAY(structure Prel = Prel
							structure QLayout = QLayout)

structure ImpHash = IMP_HASH(structure Prel = Prel
							 structure Misclay = Misclay)

structure Atom = ATOM(structure Prel = Prel)

structure Ccode = CCODE(structure Prel = Prel
						structure Misclay = Misclay)

val wrapCol = 100

structure Value = VALUE(val prefixNML = prefixNML
						structure Prel = Prel
						(*structure Sort = Sort
						structure Assoc = Assoc*)
						structure Atom = Atom
						structure Misclay = Misclay
						structure Ccode = Ccode
						structure ImpHash = ImpHash
						val wrapCol = wrapCol)

structure Basis = BASIS(structure Prel = Prel
						structure Assoc = Assoc
						structure Misclay = Misclay
						structure Atom = Atom
						structure Value = Value)

structure Builtin = BUILTIN(val prefixNML = prefixNML
							structure Prel = Prel
							structure Atom = Atom
							structure Value = Value
							structure Basis = Basis)

structure Pos = POS(structure Prel = Prel)

structure Lang = LANG(structure Prel = Prel
					  structure Pos = Pos
					  structure Atom = Atom)


structure Interpreter = INTERPRETER(val prefixNML = prefixNML
									val display_counts = display_counts
									structure Prel = Prel
									structure Sort = Sort
									structure Assoc = Assoc
									structure Pos = Pos
									structure Atom = Atom
									structure Lang = Lang
									structure Value = Value
									structure Basis = Basis)


structure Embed = EMBED(structure Prel = Prel
						val optimize_multi_app = optimize_multi_app
						val optimize_multi_fn = optimize_multi_fn
						val optimize_prim_app = optimize_prim_app
						structure Sort = Sort
						structure Assoc = Assoc
						structure Pos = Pos
						structure Atom = Atom
						structure Lang = Lang
						structure Value = Value
						structure Basis = Basis)


structure Tok = TOK(structure Prel = Prel)

structure Lex = LEX(structure Prel = Prel
					val trace_lex = trace_lex
					structure Pos = Pos
					structure Tok = Tok)

structure Par1 = PAR1(structure Lex = Lex)
structure Par2 = PAR2(structure Lex = Lex)
structure Par3 = PAR3(structure Lex = Lex)

structure Par = Par3


structure Parser = PARSER(val prefixNML = prefixNML
						  structure Prel = Prel
						  structure Assoc = Assoc
						  structure Tok = Tok
						  structure Par = Par
						  structure Atom = Atom
						  structure Lang = Lang)

structure Pretty = PRETTY(structure Prel = Prel
						  structure Misclay = Misclay
						  structure Tok = Tok
						  structure Atom = Atom
						  structure Lang = Lang
						  structure Parser = Parser) (* stupid *)


structure Cps = CPS(structure Prel = Prel
					val right_to_left_tuples = right_to_left_tuples
					val right_to_left_args = right_to_left_args
					val bind_all_prim_app = bind_all_prim_app
					val lift_lets = lift_lets
					structure Atom = Atom
					structure Embed = Embed
					structure Misclay = Misclay
					structure Value = Value
					structure Pretty = Pretty)

structure Machine = MACHINE(val prefixNML = prefixNML
							val wrapCol = wrapCol
							val display_counts = display_counts
							structure Prel = Prel
							structure Assoc = Assoc
							structure Pos = Pos
							structure Atom = Atom
							structure Embed = Embed
							structure Value = Value
							structure Cps = Cps
							structure Misclay = Misclay
							structure Pretty = Pretty
							structure Ccode = Ccode)

structure Compile = COMPILE3(val prefixNML = prefixNML
							 structure Prel = Prel
							 val right_to_left_tuples = right_to_left_tuples
							 val right_to_left_args = right_to_left_args
							 val delay_select = delay_select
							 val new_pat_comp = new_pat_comp
							 val opt_last_constr = opt_last_constr
							 structure Assoc = Assoc
							 structure Misclay = Misclay
							 structure Atom = Atom
							 structure Value = Value
							 structure Embed = Embed
							 structure Cps = Cps
							  structure Machine = Machine)

structure Eval3 = EVAL3(structure Prel = Prel
						val print_lang = print_lang
						val print_embed = print_embed
						val print_cps = print_cps
						val print_code = print_code
						structure Assoc = Assoc
						structure Pos = Pos
						structure Atom = Atom
						structure Value = Value
						structure Basis = Basis
						structure Embed = Embed
						structure Cps = Cps
						structure Compile = Compile
						structure Machine = Machine
						val prefixNML = prefixNML
						val wrapCol = wrapCol
						structure Misclay = Misclay
						structure Pretty = Pretty)

					   
(* select evaluator... *)
structure Eval = 
struct
  open Interpreter
  fun eval'dec env dec = (if !run_compiler then Eval3.eval'dec else Interpreter.eval'dec) env dec
end

structure Program = PROGRAM(structure Prel = Prel
							structure Lang = Lang
							structure Value = Value
							structure Basis = Basis
							structure Eval = Eval)

structure Run =
	RUN(val prefixNML = prefixNML
		val quiet = quiet
		val wrapCol = wrapCol
		structure Prel = Prel
		structure QLayout = QLayout
		structure Tok = Tok
		structure Pos = Pos
		structure Lex = Lex
		structure Par = Par
		structure Atom = Atom
		structure Parser = Parser
		structure Pretty = Pretty
		structure Value = Value
		structure Basis = Basis
		structure Builtin = Builtin
		structure Program = Program);


fun tc() =
	(Run.Nexec "datatype 'a option = Some of 'a | None";
	 Run.Nuse "predefined/nml_NonPrim.ML";
	 Run.Nexec "open NonPrim";
	 Run.Nuse "EXPECT.ML");
