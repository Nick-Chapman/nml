(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  10.    export
  22.    imports
  166.   functor
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature EvalSig =
sig
	type dec
	type env
	type exval
	val eval'dec : env -> dec -> (env,exval) either
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/Prel.ML *)
signature PrelSig_Import =
sig
	val echo			: string -> unit
	val enumerateFrom	: int -> 'a list -> ('a * int) list
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ../prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
	type ('a,'b) assoc
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PosSig POS.ML *)
signature PosSig_Import =
sig
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    type vid;
    type 'a long;
    type lab;
    type scon;
end;
(*IMPORTDONE:*)

(*IMPORTSIG: ValueSig VALUE.ML *)
signature ValueSig_Import =
sig
	type vid
	type lab
	type scon
	type value
	type exval
	type layout
	val value'getTuple	: string -> value * int -> value
end;
(*IMPORTDONE:*)

(*IMPORTSIG: BasisSig BASIS.ML *)
signature BasisSig_Import =
sig
	type 'a long
	type vid
	type value
	type env
		 
	type ('a,'b) assoc
	type layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: EmbedSig EMBED.ML *)
signature EmbedSig_Import =
sig
  type value
  type vid
  type scon
  type uid
    type match;
    type dec;
  type env
  type Basis_env
  type Lang_dec
  val embed'dec     : Basis_env -> Lang_dec -> dec * env * {bound: uid list}
  val unembed'dec   : dec -> Lang_dec 
  val reify'env		: env -> (uid * value) list -> Basis_env
end;
(*IMPORTDONE:*)

(*IMPORTSIG: CpsSig CPS.ML *)
signature CpsSig_Import =
sig
  type value
  type vid
  type uid
  type scon
    type cps;
    type atom;
    type bind;
    type afunc;
    type cmatch;
    type amatch;
  type Embed_dec
  val cpsTransform : Embed_dec * {bound:uid list} -> cps
  type layout
  val lay'cps       : cps -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: CompileSig COMPILE3.ML *)
signature CompileSig_Import =
sig
  type cps
  type code
  val generateCode : cps -> code
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MachineSig MACHINE.ML *)
signature MachineSig_Import =
sig
  type value
  type exval
  type vid
  type uid
  type scon
  type cps
    type code;
    type instruction;
  type layout
  val lay'code : code -> layout						  
  val eval'code	: code -> (value,exval) either
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val Word                      : string -> layout
  val squash_to                 : int -> layout -> layout
  val prefixed_output_layout    : string -> layout -> unit
  val //``                      : layout * layout -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: PrettySig PRETTY.ML *)
signature PrettySig_Import =
	sig
		type layout
		type scon
		type dec
		val lay'dec_noIM : dec -> layout
	end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor EVAL3
		(val prefixNML : string
		 val print_lang : bool ref
		 val print_embed : bool ref
		 val print_cps : bool ref
		 val print_code : bool ref
		 structure Prel : PrelSig_Import
		 structure Assoc : AssocSig_Import
		 structure Pos : PosSig_Import
		 structure Atom : AtomSig_Import
		 structure Value : ValueSig_Import
		 structure Basis : BasisSig_Import
		 structure Embed : EmbedSig_Import
		 structure Cps : CpsSig_Import
		 structure Compile : CompileSig_Import
		 structure Machine : MachineSig_Import

		 structure Misclay : MisclaySig_Import
		 structure Pretty : PrettySig_Import
		 val wrapCol : int

		 sharing type Assoc.assoc = Basis.assoc
		 sharing type Atom.lab = Value.lab
		 sharing type Atom.scon = Embed.scon = Value.scon
		 sharing type Atom.long = Basis.long
		 sharing type Atom.vid = Value.vid = Basis.vid = Embed.vid = Cps.vid
		 sharing type Embed.uid = Cps.uid
		 sharing type Value.value = Basis.value = Embed.value = Machine.value
		 sharing type Value.exval = Machine.exval

		 sharing type Basis.env = Embed.Basis_env
		 sharing type Embed.dec = Cps.Embed_dec
		 sharing type Cps.cps = Compile.cps
		 sharing type Compile.code = Machine.code

		 sharing type Misclay.layout = Pretty.layout = Machine.layout = Cps.layout
		 sharing type Pretty.dec = Embed.Lang_dec

		 ) : EvalSig =
struct

(*  val U = (Machine.SLOC,Machine.FLOC,Machine.LOC'Null)*)

  open Prel Misclay
  infix //``

  type env = Embed.Basis_env
  type dec = Embed.Lang_dec
  type exval = Value.exval

  fun echo'lay lay x =
	  (prefixed_output_layout prefixNML (squash_to wrapCol (lay x));
	   echo "")

  fun echo'lay_NO_SQUASH lay x =
	  (prefixed_output_layout prefixNML (lay x);
	   echo "")

  fun lay'dec dec = Pretty.lay'dec_noIM dec
  fun lay'embed_dec dec = lay'dec (Embed.unembed'dec dec)

  fun lay'cps cps = Cps.lay'cps cps


  fun layTag s lay x = Word s //`` (lay x)

  val echoDec = echo'lay (layTag "LANG..." lay'dec)
  val echoEmbedDec = echo'lay (layTag "EMBED... " lay'embed_dec)
  val echoCps = echo'lay (layTag "CPS... " lay'cps)
  val echoCode = echo'lay (*_NO_SQUASH*) (layTag "CODE... " Machine.lay'code)


  val eval'dec : (env -> dec -> (env,exval) either) =
	  fn basis_env =>
		 fn lang_dec =>
			let 
				val () = if !print_lang then echoDec lang_dec else ()
				val (embed_dec,embed_env,{bound=xs}) = Embed.embed'dec basis_env lang_dec
				val () = if !print_embed then echoEmbedDec embed_dec else ()
				val cps = Cps.cpsTransform (embed_dec,{bound=xs})
				val () = if !print_cps then echoCps cps else ()
				val code = Compile.generateCode cps
				val () = if !print_code then echoCode code else ()
			in case (Machine.eval'code code)
				of That exval => That exval
				 | This v => let val xvs = (case xs
											 of [] => []
											  | [x] => [(x,v)]
											  | _ => map (fn (x,n) => (x, Value.value'getTuple "eval'dec" (v,n)))
														 (enumerateFrom 0 xs))
							 in This (Embed.reify'env embed_env xvs)
							 end
			end

end (* functor *)
