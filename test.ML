
datatype testRes = PASS | FAIL | NO_EXPECT | NO_TESTFILE | ERR of string

val string'testRes = (fn PASS			=> "pass"
					   | FAIL			=> "FAIL"
					   | NO_EXPECT		=> "NOX "
					   | NO_TESTFILE	=> "NOTF"
					   | ERR s			=> ("<"^s^">"))

fun sofTestLine1 maxTestNameLength test res1 =
	let open Prel
		fun justify s = s ^ spaces (maxTestNameLength - size s)
	in old_implode [justify test," : ",
					string'testRes res1]
	end

fun sofTestLine2 maxTestNameLength test res1 res2 =
	let open Prel
		fun justify s = s ^ spaces (maxTestNameLength - size s)
	in old_implode [justify test," : ",
					string'testRes res1, " -- ",
					string'testRes res2]
	end

(*
fun echoAsWeGo__runTestsWith (tester1,tester2) tests =
	let open Prel
		val maxTestNameLength = maximum 0 (map size tests)
		fun loop [] = ()
		  | loop (test::tests) =
			let val res1 = tester1 test
				val res2 = tester2 test
			in (echo (sofTestLine maxTestNameLength test res1 res2); 
				loop tests)
			end
	in loop tests
	end
*)

fun runTestsWith1 tester1 tests =
	let val q = !quiet
		val () = (quiet := true)
		open Prel
		val maxTestNameLength = maximum 0 (map size tests)
		fun loop [] = []
		  | loop (test::tests) =
			let val res1 = tester1 test
			in ((sofTestLine1 maxTestNameLength test res1) ::
				loop tests)
			end
	in (map echo (loop tests); quiet := q)
	end

fun runTestsWith2 (tester1,tester2) tests =
	let val q = !quiet
		val () = (quiet := true)
		open Prel
		val maxTestNameLength = maximum 0 (map size tests)
		fun loop [] = []
		  | loop (test::tests) =
			let val res1 = tester1 test
				val res2 = tester2 test
			in ((sofTestLine2 maxTestNameLength test res1 res2) ::
				loop tests)
			end
	in (map echo (loop tests); quiet := q)
	end


val testRes'boolOpt = 
	fn None => NO_EXPECT
	 | Some bool => if bool then PASS else FAIL


structure Expect = struct val expect = ref None : bool option ref end
fun EXPECT Fbool = (Expect.expect := Some (Fbool()))

fun tryOpenIn path = 
	let open TextIO
		val instream = openIn path
	in (closeIn instream; None)
	end
	handle IO.Io {function,name,...} => Some {problem= (function^":"^name)}

fun canOpenIn path = case (tryOpenIn path) of None => true | Some _ => false

fun runTestNJ path =
	if not (canOpenIn path) then NO_TESTFILE else
	let val U as () = (Expect.expect := None)
		val U as () = use path
	in testRes'boolOpt (!Expect.expect)
	end
		handle IO.Io _ => NO_TESTFILE
			 | Prel.Abort s => ERR s
			 | _ => ERR "some-other-exception"


fun mapOption f (Some x) = Some (f x) 
  | mapOption f None = None

local open Prel Atom Value in
fun derefOption'value (who:string) (v:value) : value option = 
	let val (con',vOpt) = con'value (who^": expected con") v
	in
		if (sof'con con' = "None") then
			case vOpt of
				None => None
			  | Some _ => abort (who^"None: unexpected value")
		else if (sof'con con' = "Some") then
			case vOpt of
				None => abort (who^"Some: expected value")
			  | Some v => Some v
		else
			abort (who^": expected None/Some")
	end
end

fun getExpect basis : bool option =
	let
		open Atom Basis Value
		val (value,_) = look'longvid "" (env'basis basis) (LONG ([STRID (ID "Expect")],VID (ID "expect")))
	in
		mapOption (bool'value "getExpect(bool)")
				  (derefOption'value "getExpect(option)"
									 (derefAddr'value "getExpect(!)" value))
	end


fun runTestNML testFile =
	(Run.resetInitialBasis();
(*
	 Run.Nuse (preludeDir^"option.ML");
	 Run.Nuse "nml_NonPrim.ML";
	 Run.Nexec "open NonPrim";
	 Run.Nexec "structure Expect = struct val expect = ref None : bool option ref end";
	 Run.Nexec "fun EXPECT bool = (Expect.expect := Some bool)";
 *)
	 tc();
	 Run.Nuse testFile;
	 let val basis = Run.getTheBasis()
	 in testRes'boolOpt (getExpect basis)
	 end)
	handle IO.Io _ => NO_TESTFILE
		 | Prel.Abort s => ERR s
		 (*| x => ERR (ExnName.exnName x ^ ":" ^ ExnName.exnMessage x)*)
		 | _ => ERR "some-other-exception"



val runTestsNJ = runTestsWith1 runTestNJ
val runTests1 = runTestsWith1 runTestNML

val runTests = runTestsWith2 (runTestNJ,runTestNML)


(*
val tests = 
	[
	 "regs/,missing.ML",
	 "regs/,garbage.ML",
	 "regs/,failure.ML",
	 "regs/,nox.ML",
	 "regs/ids1.ML",
	 "regs/ids2.ML",
	 "regs/ids3.ML",
	 "regs/ids4.ML",
	 "regs/data-rep.ML",
	 "regs/basis-types.ML",
	 (*"regs/basis-types2.ML",*) (* nml allows something NJ rejects *)
	 "regs/append-example.ML",
	 "regs/gen-exception.ML",
	 "regs/val-named-ref.ML",
	 "regs/test-while.ML",
	 "regs/eval-order-func-arg.ML",
	 "regs/eval-order-arg-func.ML", (* non-standard order *)
	 "regs/local-scope.ML",
	 "regs/raise-order.ML",
	 "regs/assign-order.ML"
	 ]
*)

val tnames =
	[
	 "ids1",
	 "ids2",
	 "ids3",
	 "ids4",
	 "data-rep",
	 "basis-types",
	 (*"basis-types2",*) (* nml allows something NJ rejects *)
	 "append-example",
	 "gen-exception",
	 "gen-exception1",
	 "val-named-ref",
	 "test-while",
	 "eval-order-func-arg",
	 "eval-order-arg-func", (* non-standard order *)
	 "local-scope",
	 "raise-order",
	 "assign-order"
	 ,"ref-equality"
	 ,"excon1"
	 ]


val tests = 
	[
	 "regs/,missing.ML",
	 "regs/,garbage.ML",
	 "regs/,failure.ML",
	 "regs/,nox.ML"]
	@ map (fn s => "regs/"^s^".ML") tnames


fun exportTestFile tname =
	let val path = "regs/"^tname^".ML"
		val x_path = ",C/"^tname^".C"
		val () = Run.Nexec "structure Expect = struct val F = ref (fn () => false) end"
		val () = Run.Nexec "fun EXPECT F = (Expect.F := F)"
		val () = Run.Nuse path
	in Run.Nexport x_path "fn _ => print (if (!Expect.F)() then \"pass\n\" else \"FAIL\n\")"
	end

fun exportTests() = (tc(); map exportTestFile tnames)
