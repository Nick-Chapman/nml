(*------------------------------------------------------------------------------
 CONTENTS-START-LINE: HERE=2 SEP=1
  42.    export
  53.    imports
  259.   functor
  312.   newLabel
  322.   countApps
  342.   canDup'code
  353.   g_code - `goto' optimization wrapper for code
  399.   use g_code as code in rest of file...
  405.   SharingCode
  415.   raise Match/Bind
  422.   seq = code->code
  438.   seq - util
  453.   compileSequence (always L->R)
  464.   compileTupleSequence (order determined by: right_to_left_tuples)
  473.   env - compilation env, binds uid -> location (r)
  490.   Delayed tuple building
  523.   eq - stack_location
  536.   obi - object info (at compile time)
  553.   obi - matchers
  623.   obi - destructors
  700.   mkOi / forceObi
  733.   b_code - branching code, result of compiling pattern matching
  789.   closeB'rd - for use in let binding.
  799.   m_code (mc) -- code for a cmatch
  836.   m_seq (ms) -- code for a amatch
  856.   compilation schemes : Cps(+Fv) -> Code
  876.   specialized b_code matchers/deconstructors
  890.   NEW pat compilation
  918.   NEW amatch/cmatch compilation
  960.   PrimApp
  982.   compile_saveFrame
  999.   B
  1025.  C, Aret, CM, AM
  1098.  Ad, A, At, AMt, Ft
  1168.  fix-up schemes
 CONTENTS-END-LINE:
------------------------------------------------------------------------------*)

(*----------------------------------------------------------------------
INDEX: export
----------------------------------------------------------------------*)

signature CompileSig =
sig
  type cps
  type code
  val generateCode : cps -> code
end;

(*----------------------------------------------------------------------
INDEX: imports
----------------------------------------------------------------------*)

(*IMPORTSIG: PrelSig ../prelude/PREL.ML *)
signature PrelSig_Import =
sig
    val echo            : string -> unit
    val abort           : string -> 'a
    val filter          : ('a -> bool) -> 'a list -> 'a list
    val fold            : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val stringOfInt     : int -> string
    val exists          : ('a -> bool) -> 'a list -> bool
    val enumerateFrom   : int -> 'a list -> ('a * int) list
    val copy            : int -> 'a -> 'a list
    val upto            : int * int -> int list
    val curry           : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val unzip           : ('a * 'b) list -> 'a list * 'b list
    val split           : ('a -> bool) -> 'a list -> 'a list * 'a list
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AssocSig ../prelude/ASSOC.ML *)
signature AssocSig_Import =
sig
    type ('a,'b) assoc
    val assoc'empty     : ('a,'b) assoc
    val assoc'single    : 'a * 'b -> ('a,'b) assoc
    val assoc'plus      : ('a,'b) assoc -> ('a,'b) assoc -> ('a,'b) assoc
    val find'assoc      : ('a,'b) assoc -> ('a -> bool) -> 'b option
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MisclaySig ../prelude/MISCLAY.ML *)
signature MisclaySig_Import =
sig
  type layout
  val ++                        : layout * layout -> layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: AtomSig ATOM.ML *)
signature AtomSig_Import =
sig
    type vid;
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
    type exname
    type value
    type exval
    type layout
    type builtin
    val arity'builtin   : builtin -> int
    type con

    val eq'con          : con -> con -> bool
    val siblings'con    : con -> con list
    datatype prim
      = PRIM'Builtin    of builtin
      | PRIM'Con0       of con
      | PRIM'Ref
    val value'exval     : exval -> value
    val exval'Match     : exval
    val exval'Bind      : exval
    val eq'scon         : scon -> scon -> bool
    val eq'exname       : exname -> exname -> bool
end;
(*IMPORTDONE:*)

(*IMPORTSIG: EmbedSig EMBED.ML *)
signature EmbedSig_Import =
sig
  type exname
  type value
  type prim
  type vid
  type scon
  type con
  type uid
  val eq'uid        : uid -> uid -> bool
  val sof'uid       : uid -> string
  datatype pat
    = PAT'Wild
    | PAT'V         of uid
    | PAT'Layered   of uid * pat
    | PAT'Ref       of pat
    | PAT'C         of con * pat option
    | PAT'E         of exname * pat option
    | PAT'G         of uid * pat option
    | PAT'Scon      of scon
    | PAT'Tuple     of pat list
    type func;
    type match;
    type dec;
  type env
end;
(*IMPORTDONE:*)

(*IMPORTSIG: CpsSig CPS.ML *)
signature CpsSig_Import =
sig
  type value
  type prim
  type vid
  type uid
  type scon
  type pat
  datatype cps
    = CPS'Return    of atom
    | CPS'Raise     of atom
    | CPS'Let       of bind * cps
    | CPS'Case      of atom * cmatch
    | CPS'App       of atom * atom list
    | CPS'Handle    of cps * {save:uid list} * cmatch
    | CPS'LetC      of {save:uid list} * pat * cps * cps
  and atom
    = ATOM'Value    of value
    | ATOM'Var      of uid
    | ATOM'NewEx    of vid
    | ATOM'Tuple    of atom list
    | ATOM'PrimApp  of prim * atom
    | ATOM'Fn       of afunc
    | ATOM'Let      of bind * atom
    | ATOM'Case     of atom * amatch
  and bind
    = BIND'Val      of pat * atom
    | BIND'ValRec   of (uid * afunc) list
  and afunc = AFUNC'Def of {save:uid list} * uid list * cps
  and cmatch
    = CMATCH'Alt    of cmatch * cmatch
    | CMATCH'Rule   of pat * cps
    | CMATCH'Atom   of amatch
  and amatch
    = AMATCH'Alt    of amatch * amatch
    | AMATCH'Rule   of pat * atom
  type layout
end;
(*IMPORTDONE:*)

(*IMPORTSIG: MachineSig MACHINE.ML *)
signature MachineSig_Import =
sig
  type value
  type builtin
  type exname
  type vid
  type uid
  type con
  type scon
  type pat
  type atom
  type cps
  type cmatch
  type afunc
  datatype info
    = INFO'Uid      of uid
    | INFO'Pat      of pat
    | INFO'Atom     of atom
    | INFO'String   of string
  datatype q = SLOC of int * info
  datatype r
    = LOC'Stack             of q
    | LOC'Frame             of int * info
    | LOC'Arg               of int * info
    | LOC'Cret
    | LOC'Xret
    | LOC'Value             of value
  datatype im
    = IM'MatchScon          of r * scon
    | IM'MatchC             of r * con
    | IM'MatchE             of r * exname
    | IM'MatchG             of r * r
  datatype label = LABEL of int
  datatype code
    = CODE'Seq              of instruction * code
    | CODE'Return           of r
    | CODE'TailCall         of r * r list
    | CODE'Raise            of r
    | CODE'If               of im * code * code
    | CODE'Where            of code * label * code
    | CODE'Goto             of label
  and instruction
    = INS'NewTuple          of q * {tuple_size:int}
    | INS'NewException      of q * vid
    | INS'NewFn             of q * {frame_size:int, num_args: int} * code * {name: string, info: afunc}
    | INS'NewRef            of q * r
    | INS'NewCon            of q * con * r
    | INS'Copy              of q * r
    | INS'DeCon             of q * r
    | INS'DeExcon           of q * r
    | INS'DeRef             of q * r
    | INS'DeTuple           of q * r * int
    | INS'BuiltinApp        of q * builtin * r list
    | INS'PushContinuation  of {frame_size:int} * code * {info: cps * uid list}
    | INS'PushHandler       of {frame_size:int} * code * {info: cmatch * uid list}
    | INS'SetTupleElement   of r * int * r
    | INS'SetFrameElement   of r * int * r
    | INS'SetContFrameElem  of int * r
    | INS'SetXcontFrameElem of int * r
  type layout
end;
(*IMPORTDONE:*)

(*----------------------------------------------------------------------
INDEX: functor
----------------------------------------------------------------------*)

functor COMPILE3
        (val prefixNML : string
         structure Prel : PrelSig_Import

         val right_to_left_tuples : bool ref
         val right_to_left_args : bool ref
         val delay_select : bool ref
         val new_pat_comp : bool ref
         val opt_last_constr : bool ref

         structure Assoc : AssocSig_Import
         structure Misclay : MisclaySig_Import
         structure Atom : AtomSig_Import
         structure Value : ValueSig_Import
         structure Embed : EmbedSig_Import
         structure Cps : CpsSig_Import
         structure Machine : MachineSig_Import
         sharing type Atom.scon = Value.scon = Embed.scon = Cps.scon = Machine.scon
         sharing type Atom.vid = Embed.vid = Cps.vid = Value.vid = Machine.vid
         sharing type Embed.uid = Cps.uid = Machine.uid
         sharing type Value.con = Embed.con = Machine.con
         sharing type Embed.pat = Cps.pat = Machine.pat
         sharing type Value.value = Embed.value = Cps.value = Machine.value
         sharing type Value.builtin = Machine.builtin
         sharing type Value.prim = Embed.prim = Cps.prim
         sharing type Value.exname = Embed.exname = Machine.exname
         sharing type Cps.atom = Machine.atom
         sharing type Cps.cps = Machine.cps
         sharing type Cps.cmatch = Machine.cmatch
         sharing type Cps.afunc = Machine.afunc
         sharing type Cps.layout = Misclay.layout
        ) : CompileSig =
struct

  open Prel Assoc Atom Value Embed Cps Machine Misclay

  fun nth _ [] = abort "nth/[]"
    | nth n (x::xs) = if (n=0) then x else nth (n-1) xs (* interesting for opt: xs only used in one arm! *)

  fun replace_nth _ _ [] = abort "nth/[]"
    | replace_nth n y (x::xs) = if (n=0) then y :: xs else x :: replace_nth (n-1) y xs

  (* force import as datatype -- avoid eq-type prob *)
  (*val U = (ID,VID)*)

  val value'Bind = value'exval (exval'Bind)
  val value'Match = value'exval (exval'Match)

  infix $ $$;
  infix &&

(*----------------------------------------------------------------------
INDEX: newLabel
----------------------------------------------------------------------*)

  local val U = ref 1
  in fun newLabel () = let val n = !U
                       in U := 1+n; LABEL n
                       end
  end

(*----------------------------------------------------------------------
INDEX: countApps
----------------------------------------------------------------------*)

  fun inc (r as ref x) = (r:=x+1)

  local val U = ref 0
  in fun countApps prefix F =
         let val () = inc U
             val name = prefix ^ "_" ^ stringOfInt (!U)
             val N = ref 1
         in fn x => let val () = echo (prefixNML ^ "**App: " ^ name ^ " --> #" ^ stringOfInt (!N))
                        val () = inc N
                    in F x
                    end
         end
  end

  fun countApps prefix F = F (* no counting *)

(*----------------------------------------------------------------------
INDEX: canDup'code
----------------------------------------------------------------------*)

  val canDup'code =
      fn CODE'Goto _        => true
       | CODE'TailCall _    => true
       | CODE'Raise _       => true
       | CODE'Return _      => true
       | _                  => false

(*----------------------------------------------------------------------
INDEX: g_code - `goto' optimization wrapper for code
----------------------------------------------------------------------*)

  type labs = label list
  fun restrict'labs xs x = let val (yes,no) = split (curry op = x) xs
                           in (no, length yes)
                           end

  type labmap = (label -> code)
  fun lookup'labmap M lab = M lab
  fun extend'labmap M (lab,code) = fn lab' => if (lab=lab') then code else M lab'
  val empty'labmap = fn lab' => abort "empty'labmap"

  abstype g_code = G of labs * (labmap -> code)
  with val mkG = G
       fun deG (G x) = x
  end

  fun code'Raise x              = mkG ([],fn _ => CODE'Raise x)
  fun code'TailCall x           = mkG ([],fn _ => CODE'TailCall x)
  fun code'Return x             = mkG ([],fn _ => CODE'Return x)
  fun code'Seq (instruction,g)  = case (deG g) of (xs,F) => mkG (xs,fn m => CODE'Seq (instruction, F m))
  fun code'If (im,g1,g2)        = case (deG g1,deG g2) of ((xs1,F1),(xs2,F2)) => mkG (xs1@xs2,fn m => CODE'If (im, F1 m, F2 m))
  fun code'Goto lab             = mkG ([lab],fn m => lookup'labmap m lab)

  fun code'Where (g1,lab,g2)    =
      case (deG g1,deG g2)
       of ((xs1,F1),(xs2,F2)) =>
          case (restrict'labs xs1 lab)
           of (xs1,N) =>
              mkG (xs1@xs2,
                   case N of
                       0 => (echo "***code'Where:N=0";
                             fn m => CODE'Where (F1 (extend'labmap m (lab,CODE'Goto lab)), lab,
                                                 F2 m)) (* generate the unreached code *)
                     | 1 => (fn m => F1 (extend'labmap m (lab, F2 m)))
                     | _ => (fn m => let val c2 = F2 m
                                     in if canDup'code c2
                                        then F1 (extend'labmap m (lab, c2))
                                        else CODE'Where (F1 (extend'labmap m (lab,CODE'Goto lab)), lab, c2)
                                     end))

  val closeCode : (g_code -> code) =
      fn g => case (deG g) of ([],F) => F empty'labmap | _ => abort "closeCode"

(*----------------------------------------------------------------------
INDEX: use g_code as code in rest of file...
----------------------------------------------------------------------*)

  type code = g_code

(*----------------------------------------------------------------------
INDEX: SharingCode
----------------------------------------------------------------------*)

  val SharingCode : (code -> code) -> (code -> code) =
      fn F => fn code2 => let val lab = newLabel()
                              val code1 = F (code'Goto lab)
                          in code'Where (code1,lab,code2)
                          end

(*----------------------------------------------------------------------
INDEX: raise Match/Bind
----------------------------------------------------------------------*)

  val code'RaiseMatch = code'Raise (LOC'Value value'Match)
  val code'RaiseBind = code'Raise (LOC'Value value'Bind)

(*----------------------------------------------------------------------
INDEX: seq = code->code
----------------------------------------------------------------------*)

  abstype seq = SEQ of code -> code
  with val mkSeq = SEQ
       fun deSeq (SEQ F) = F
  end

  fun op $$ (seq, code) = deSeq seq (code)
  fun op $ (seq1,seq2) = mkSeq (fn code => deSeq seq1 (deSeq seq2 code))

  val seq'Empty                 = mkSeq (fn code => code)
  fun seq'I instruction         = mkSeq (fn next => code'Seq (instruction, next))
  fun seq'Match (im,{fail})     = mkSeq (fn next => code'If (im, next, fail))

(*----------------------------------------------------------------------
INDEX: seq - util
----------------------------------------------------------------------*)

  local val uref = ref 0
  in fun newQ info = let val u = !uref in (uref:=1+u; SLOC (u,info)) end
  end

  val seq'concat = fold (curry op $) seq'Empty
  val seq'Is = seq'concat o map seq'I

  fun seq'tuple (q,rs) = if length rs < 2 then abort "seq'tuple" else
      seq'I (INS'NewTuple (q, {tuple_size = length rs}))
      $ seq'Is (map (fn (r,n) => INS'SetTupleElement (LOC'Stack q,n,r)) (enumerateFrom 0 rs))

(*----------------------------------------------------------------------
INDEX: compileSequence (always L->R)
----------------------------------------------------------------------*)

  fun compileSequence (X: ('a -> seq * 'b)) : ('a list -> seq * 'b list) =
      fn atoms =>
         case (fold (fn (seq1,rs) => fn atom => let val (seq2,r) = X atom
                                                in (seq1 $ seq2, r::rs)
                                                end) (seq'Empty,[]) atoms)
          of (seq,rs) => (seq,rev rs)

(*----------------------------------------------------------------------
INDEX: compileTupleSequence (order determined by: right_to_left_tuples)
----------------------------------------------------------------------*)

  fun compileTupleSequence X atoms =
      if (!right_to_left_tuples)
      then case (compileSequence X (rev atoms)) of (seq,rs) => (seq, rev rs)
      else compileSequence X atoms

(*----------------------------------------------------------------------
INDEX: env - compilation env, binds uid -> location (r)
----------------------------------------------------------------------*)

  local open Assoc Atom
  in datatype env = ENV of (uid,r) assoc
     val env'empty = ENV assoc'empty
     fun env'plus (ENV a1, ENV a2) = ENV (assoc'plus a1 a2)
     val env'plusList = fold (curry env'plus) env'empty
     fun look'env who (ENV a) k = case (find'assoc a (eq'uid k)) of Some v => v
                                                                   | None => abort("look'env("^who^")"^sof'uid k)
     fun env'single kv = ENV (assoc'single kv)
  end

  infix ++
  val op ++ = env'plus

(*----------------------------------------------------------------------
INDEX: Delayed tuple building
----------------------------------------------------------------------*)

  datatype r_delayed
    = DELAYED'R         of r
    | DELAYED'Tuple     of r_delayed list * {info:atom}


  val rec forceDelayed : r_delayed -> (seq * r) =
       fn DELAYED'R r => (seq'Empty,r)
        | DELAYED'Tuple (rds,
                         {info=atom}) => let val (seq,rs) = compileSequence forceDelayed rds
                                             val q = newQ (INFO'Atom atom)
                                         in (seq $ seq'tuple (q,rs), LOC'Stack q)
                                         end

  val forceDelayedN : (int * r_delayed) -> (seq * r list) =
      fn (N,rd) =>
      case rd
       of DELAYED'Tuple (rds,_) => if (length rds <> N) then abort "forceDelayedN" else
                                     let val (seqs,rs) = unzip (map forceDelayed rds)
                                     in (seq'concat seqs,rs)
                                     end
        | _ =>
          let val (seq,r) = forceDelayed rd
              val (seqs,rs) = unzip (map (fn n => let val q = newQ (INFO'String ("BuiltinApp-Arg-"^stringOfInt n))
                                                      val seq = seq'I (INS'DeTuple (q,r,n))
                                                  in (seq,LOC'Stack q)
                                                  end) (upto (0,N-1)))
          in (seq $ seq'concat seqs,rs)
          end

(*----------------------------------------------------------------------
INDEX: eq - stack_location
----------------------------------------------------------------------*)

  (* Need to test equality of locations for optimized compilation of pattern matching of generated exceptions
  - it is known these can only be in the frame or stack - I think! *)

  fun eq'stack_location (SLOC (n1,_)) (SLOC (n2,_)) = (n1=n2)

  fun eq'location_inFrameOrStack (LOC'Stack q1) (LOC'Stack q2) = eq'stack_location q1 q2
    | eq'location_inFrameOrStack (LOC'Frame (n1,_)) (LOC'Frame (n2,_)) = (n1=n2)
    | eq'location_inFrameOrStack _ _ = abort "eq'location_inFrameOrStack"

(*----------------------------------------------------------------------
INDEX: obi - object info (at compile time)
----------------------------------------------------------------------*)

  datatype obi
    = OBI'Location      of r
    | OBI'Scon          of r * scon
    | OBI'NotScon       of r * scon list
    | OBI'C             of r * con * obi option
    | OBI'NotC          of r * con list
    | OBI'E             of r * exname * obi option
    | OBI'G             of r * r * obi option
    | OBI'NotEG         of r * exname list * r list
    | OBI'Ref           of r * obi
    | OBI'Tuple         of r * obi option list
    | OBI'DelTup        of obi list * {info:atom}

(*----------------------------------------------------------------------
INDEX: obi - matchers
----------------------------------------------------------------------*)

  datatype sma = SMA'Yes of obi | SMA'No | SMA'Maybe of im * obi * obi (* pos/neg obi *)

  type matcher = obi -> sma


  fun obi'MatchScon scon : matcher =
      fn obi =>
         case obi
          of OBI'Scon (_,x) => if (eq'scon scon x)
                               then SMA'Yes obi
                               else SMA'No
           | OBI'NotScon (r,xs) => if (exists (eq'scon scon) xs)
                                   then SMA'No
                                   else SMA'Maybe (IM'MatchScon (r,scon),
                                                   OBI'Scon (r,scon),
                                                   OBI'NotScon (r,scon::xs))
           | OBI'Location r     => SMA'Maybe (IM'MatchScon (r,scon),
                                              OBI'Scon (r,scon),
                                              OBI'NotScon (r,[scon]))
           | _                  => abort "obi'MatchScon"



  fun isLastConstr con xs =
      if (!opt_last_constr)
      then case (filter (fn sib => not (exists (eq'con sib) xs)) (siblings'con con))
           of [] => abort "isLastConstr/0"
            | [last_con] => let val () = if (eq'con con last_con) then () else abort "isLastConstr/1-wrong"
                            in true
                            end
            | _ => false
      else false

  fun obi'MatchC con : matcher =
      fn obi =>
         case obi
          of OBI'C (_,x,_)      => if (eq'con con x)
                                   then SMA'Yes obi
                                   else SMA'No
           | OBI'NotC (r,xs)    => if (exists (eq'con con) xs)
                                   then SMA'No
                                   else (if (isLastConstr con xs)
                                         then SMA'Yes (OBI'C (r,con,None)) (* neg->pos *)
                                         else SMA'Maybe (IM'MatchC (r,con),
                                                         OBI'C (r,con,None),
                                                         OBI'NotC (r,con::xs)))
           | OBI'Location r     => if (isLastConstr con [])
                                   then SMA'Yes (OBI'C (r,con,None)) (* neg->pos *)
                                   else SMA'Maybe (IM'MatchC (r,con),
                                                   OBI'C (r,con,None),
                                                   OBI'NotC (r,[con]))
           | _                  => abort "obi'MatchC"


  fun obi'MatchE exname : matcher =
      fn obi =>
         case obi
          of OBI'E (_,x,_)      => if (eq'exname exname x)
                                   then SMA'Yes obi
                                   else SMA'No
           | OBI'G _            => SMA'No
           | OBI'NotEG (r,es,gs)=> if (exists (eq'exname exname) es)
                                   then SMA'No
                                   else SMA'Maybe (IM'MatchE (r,exname),
                                                   OBI'E (r,exname,None),
                                                   OBI'NotEG (r,exname::es,gs))
           | OBI'Location r     => SMA'Maybe (IM'MatchE (r,exname),
                                              OBI'E (r,exname,None),
                                              OBI'NotEG (r,[exname],[]))
           | _                  => abort "obi'MatchE"


  fun obi'MatchG r2 : matcher =
      fn obi =>
         case obi
          of OBI'G (r,x,_)      => if (eq'location_inFrameOrStack r2 x)
                                   then SMA'Yes obi
                                   else SMA'No (* think this is true! *)
           | OBI'E _            => SMA'No
           | OBI'NotEG (r,es,gs)=> if (exists (eq'location_inFrameOrStack r2) gs)
                                   then SMA'No
                                   else SMA'Maybe (IM'MatchG (r,r2),
                                                   OBI'G (r,r2,None),
                                                   OBI'NotEG (r,es,r2::gs))
           | OBI'Location r     => SMA'Maybe (IM'MatchG (r,r2),
                                              OBI'G (r,r2,None),
                                              OBI'NotEG (r,[],[r2]))
           | _                  => abort "obi'MatchE"

(*----------------------------------------------------------------------
INDEX: obi - destructors
----------------------------------------------------------------------*)

  type destructor = obi -> seq * obi * {Replace: obi -> obi}

  fun obi'DeCon pat : destructor =
      fn OBI'C (r,x,Some obi)       => (seq'Empty,
                                        obi,
                                        {Replace = fn obi => OBI'C (r,x,Some obi)})
       | OBI'C (r,x,None)           => let val q = newQ (INFO'Pat pat)
                                       in (seq'I (INS'DeCon (q,r)),
                                           OBI'Location (LOC'Stack q),
                                           {Replace = fn obi => OBI'C (r,x,Some obi)})
                                       end
       | _                          => abort "obi'DeCon"


  fun obi'DeExcon pat : destructor =
      fn OBI'E (r,x,Some obi)       => (seq'Empty,
                                        obi,
                                        {Replace = fn obi => OBI'E (r,x,Some obi)})
       | OBI'E (r,x,None)           => let val q = newQ (INFO'Pat pat)
                                       in (seq'I (INS'DeExcon (q,r)),
                                           OBI'Location (LOC'Stack q),
                                           {Replace = fn obi => OBI'E (r,x,Some obi)})
                                       end
       | OBI'G (r,x,Some obi)       => (seq'Empty,
                                        obi,
                                        {Replace = fn obi => OBI'G (r,x,Some obi)})
       | OBI'G (r,x,None)           => let val q = newQ (INFO'Pat pat)
                                       in (seq'I (INS'DeExcon (q,r)),
                                           OBI'Location (LOC'Stack q),
                                           {Replace = fn obi => OBI'G (r,x,Some obi)})
                                       end
       | _                          => abort "obi'DeExcon"


  fun obi'DeRef pat : destructor =
      fn OBI'Ref (r,obi) => (seq'Empty,
                             obi,
                             {Replace = fn obi => OBI'Ref (r,obi)})
       | OBI'Location r => let val q = newQ (INFO'Pat pat)
                           in (seq'I (INS'DeRef (q,r)),
                               OBI'Location (LOC'Stack q),
                               {Replace = fn obi => OBI'Ref (r,obi)})
                           end
       | _ => abort "obi'DeRef"


  fun obi'DeTuple W n pat : destructor =
       fn OBI'Location r => let val q = newQ (INFO'Pat pat)
                                val sub_r = LOC'Stack q
                                val sub_obi = OBI'Location sub_r
                                val obis = copy W None
                            in (seq'I (INS'DeTuple (q,r,n)),
                                sub_obi,
                                {Replace = fn sub_obi => OBI'Tuple (r,replace_nth n (Some sub_obi) obis)})
                            end
        | OBI'Tuple (r,oobis) => (case (nth n oobis) of
                                      None => let val q = newQ (INFO'Pat pat)
                                                  val sub_r = LOC'Stack q
                                                  val sub_obi = OBI'Location sub_r
                                              in (seq'I (INS'DeTuple (q,r,n)),
                                                  sub_obi,
                                                  {Replace = fn sub_obi => OBI'Tuple (r,replace_nth n (Some sub_obi) oobis)})
                                              end
                                    | Some sub_obi => (seq'Empty,
                                                       sub_obi,
                                                       {Replace = fn sub_obi => OBI'Tuple (r,replace_nth n (Some sub_obi) oobis)}))
        | OBI'DelTup (obis,info) => let val sub_obi = nth n obis
                                    in (seq'Empty,
                                        sub_obi,
                                        {Replace = fn sub_obi => OBI'DelTup (replace_nth n sub_obi obis, info)})
                                    end
        | _ => abort "obi'DeTuple"

(*----------------------------------------------------------------------
INDEX: mkOi / forceObi
----------------------------------------------------------------------*)

  fun mkObi rd =
      case rd
       of DELAYED'Tuple (rd,info)   => OBI'DelTup (map mkObi rd, info)
        | DELAYED'R r               => OBI'Location r


  val rec forceObi : obi -> (seq * (obi * r)) =
      fn obi =>
         case obi
          of OBI'DelTup (obis,
                         {info=atom}) => let val (seq,pairs) = compileSequence forceObi obis
                                             val (obis,rs) = unzip pairs
                                             val q = newQ (INFO'Atom atom)
                                             val r = LOC'Stack q
                                             val obi = OBI'Tuple (r,map Some obis)
                                             val pair = (obi,r)
                                         in (seq $ seq'tuple (q,rs), pair)
                                         end
           | OBI'Tuple (r,_)    => (seq'Empty,(obi,r))
           | OBI'Location r     => (seq'Empty,(obi,r))
           | OBI'Scon (r,_)     => (seq'Empty,(obi,r))
           | OBI'NotScon (r,_)  => (seq'Empty,(obi,r))
           | OBI'C (r,_,_)      => (seq'Empty,(obi,r))
           | OBI'NotC (r,_)     => (seq'Empty,(obi,r))
           | OBI'E (r,_,_)      => (seq'Empty,(obi,r))
           | OBI'G (r,_,_)      => (seq'Empty,(obi,r))
           | OBI'NotEG (r,_,_)   => (seq'Empty,(obi,r))
           | OBI'Ref (r,_)      => (seq'Empty,(obi,r))

(*----------------------------------------------------------------------
Index: b_code - branching code, result of compiling pattern matching
----------------------------------------------------------------------*)

  datatype sf = SF'Succ of seq * env | SF'Fail of code

  abstype b_code = B of {fail: obi -> code}
                        * {succ: seq * obi * env -> sf}
                          * obi * env -> sf
  with val mkB = B
       fun deB (B x) = x
  end

  val closeB : (b_code * {fail: obi -> code} * obi -> sf) =
      fn (b,fail,obi) =>
         deB b (fail,
                {succ = fn (seq,_,env) => SF'Succ (seq,env)},
                obi, env'empty)

  val b'Empty : b_code =
      mkB (fn ({fail=_},{succ},obi,env) => succ (seq'Empty,obi,env))

  val op && : (b_code * b_code -> b_code) =
      fn (b1,b2) =>
         mkB (fn (fail,{succ},obi,env) =>
                 deB b1 (fail,{succ = fn (seq1,obi,env) =>
                                         deB b2 (fail, {succ = fn (seq2,obi,env) =>
                                                                  succ (seq1 $ seq2, obi, env)},
                                                 obi,env)},
                         obi,env))

  val b'Bind : (uid -> b_code) =
      fn x =>
         mkB (fn ({fail=_},{succ},obi,env) => let val (seq1,(obi,r)) = forceObi obi
                                              in succ (seq1, obi, env ++ env'single (x,r))
                                              end)

  val b'Match : (matcher -> b_code) =
      fn StaticMatch =>
         mkB (fn ({fail},{succ},obi,env) =>
                 case (StaticMatch obi)
                  of SMA'Yes obi => succ (seq'Empty, obi, env)
                   | SMA'No => SF'Fail (fail obi)
                   | SMA'Maybe (im,posObi,negObi) => succ (seq'Match (im,{fail = fail (negObi)}), posObi, env))


  val b'Destruct : destructor -> b_code -> b_code =
      fn StaticDestruct =>
         fn bsub =>
            mkB (fn ({fail},{succ},obi,env) =>
                    let val (seq1,obi,{Replace}) = StaticDestruct(obi)
                    in deB bsub ({fail = fn obi => fail (Replace obi)},
                                 {succ = fn (seq2,obi,env) => succ (seq1 $ seq2,Replace obi, env)},
                                 obi,env)
                    end)

(*----------------------------------------------------------------------
Index: closeB'rd - for use in let binding.
----------------------------------------------------------------------*)

  val closeB'rd : (b_code * {fail: code} * r_delayed -> (seq * env)) =
      fn (b,{fail},rd) =>
         case (closeB (b, {fail = fn _ => fail}, mkObi rd))
          of SF'Succ (seq,env) => (seq,env)
           | SF'Fail _ => abort "closeB'rd /Fail" (* ok, as not possible to unconditionally fail *)

(*----------------------------------------------------------------------
INDEX: m_code (mc) -- code for a cmatch
----------------------------------------------------------------------*)

  abstype m_code = T of ({fail: obi -> code}
                         * obi) -> code
  with val (mkMC,deMC) = (T,fn T x => x) end


  val mc'rule : (b_code * (env -> code) -> m_code) =
      fn (b,RHS) =>
         mkMC (fn (fail,obi) =>
                  case (closeB (b,fail,obi))
                   of SF'Fail code => code (* doesn't mean match is redundant - just not reachable on this branch *)
                    | SF'Succ (seq,env) => seq $$ RHS env) (* match is only redundant, if RHS is never called *)



  val mc'fatbar : (m_code * m_code -> m_code) =
      fn (mc1,mc2) =>
         if not (!new_pat_comp)
         then
             (* share fail continution - not possible when we properly build/pass obi.. *)
             mkMC (fn (fail,obi) =>
                      SharingCode (fn code2 =>
                                      deMC mc1 ({fail = fn obi => code2},
                                                obi)
                                      ) (deMC mc2 (fail,obi)))
         else
             mkMC (fn (fail,obi) => deMC mc1 ({fail = countApps "mc'fatbar" (fn obi => deMC mc2 (fail,obi))},
                                              obi))


  val closeMC'rd : (m_code * {fail: code} * r_delayed -> code) =
      fn (mc,{fail},rd) =>
         deMC mc ({fail = fn _ => fail}, mkObi rd)

(*----------------------------------------------------------------------
INDEX: m_seq (ms) -- code for a amatch
----------------------------------------------------------------------*)

  abstype m_seq = T of (code -> m_code)
  with val (mkMS,deMS) = (T,fn T x => x) end


  val ms'rule : (b_code * (env -> seq) -> m_seq) =
      fn (b,F) =>
         mkMS (fn next => mc'rule (b,fn env => F env $$ next))

  val ms'fatbar : (m_seq * m_seq -> m_seq) =
      fn (ms1,ms2) =>
         mkMS (fn next => mc'fatbar (deMS ms1 next, deMS ms2 next))

  val closeMS'rd : (m_seq * {fail: code} * r_delayed -> seq) =
      fn (ms,fail,rd) =>
         mkSeq (SharingCode (fn next => closeMC'rd (deMS ms next,fail,rd)))

(*----------------------------------------------------------------------
INDEX: compilation schemes : Cps(+Fv) -> Code
----------------------------------------------------------------------*)

  type Pd       = (env * pat                            -> b_code)

  type CM       = (env * cmatch                         -> m_code)
  type AM       = (env * amatch                         -> m_code)
  type AMt      = (env * amatch * q                     -> m_seq)

  type C        = (env * cps                            -> code)
  type Aret     = (env * atom                           -> code)
  type Ad       = (env * atom                           -> seq * r_delayed)
  type A        = (env * atom                           -> seq * r)
  type At       = (env * atom * q                       -> seq)
  type Ft       = (env * {name:string} * afunc * q      -> seq * seq)
  type B        = (env * bind                           -> env * seq)       (* env extended *)

  type schemes = {B:B,C:C,Aret:Aret,Ad:Ad,A:A,At:At,Ft:Ft}

(*----------------------------------------------------------------------
INDEX: specialized b_code matchers/deconstructors
----------------------------------------------------------------------*)

  fun b'MatchC x        = b'Match (obi'MatchC x)
  fun b'MatchE x        = b'Match (obi'MatchE x)
  fun b'MatchG x        = b'Match (obi'MatchG x)
  fun b'MatchScon x     = b'Match (obi'MatchScon x)

  fun b'DeCon pat       = b'Destruct (obi'DeCon pat)
  fun b'DeExcon pat     = b'Destruct (obi'DeExcon pat)
  fun b'DeRef pat       = b'Destruct (obi'DeRef pat)
  fun b'DeTuple W n pat = b'Destruct (obi'DeTuple W n pat)

(*----------------------------------------------------------------------
INDEX: NEW pat compilation
----------------------------------------------------------------------*)

  fun compile'Pd {Pd:Pd} : (env * pat -> b_code) =
      fn (env,pat) => (* just to find generated exceptions *)
         case pat
          of PAT'Wild                       => b'Empty
           | PAT'V x                        => b'Bind x
           | PAT'Layered (x,pat)            => b'Bind x && Pd (env,pat)
           | PAT'Ref pat                    => b'DeRef pat (Pd (env,pat))
           | PAT'C (x,None)                 => b'MatchC x
           | PAT'C (x,Some PAT'Wild)        => b'MatchC x
           | PAT'C (x,Some pat)             => b'MatchC x && b'DeCon pat (Pd (env,pat))
           | PAT'E (exname,None)            => b'MatchE exname
           | PAT'E (exname,Some PAT'Wild)   => b'MatchE exname
           | PAT'E (exname,Some pat)        => b'MatchE exname && b'DeExcon pat (Pd (env,pat))
           | PAT'G (x,None)                 => b'MatchG (look'env "compile'pat" env x)
           | PAT'G (x,Some PAT'Wild)        => b'MatchG (look'env "compile'pat" env x)
           | PAT'G (x,Some pat)             => b'MatchG (look'env "compile'pat" env x) && b'DeExcon pat (Pd (env,pat))
           | PAT'Scon scon                  => b'MatchScon scon
           | PAT'Tuple pats                 =>
             fold (curry op &&) b'Empty
                  (map (fn (pat,n) => if (!delay_select andalso (case pat of PAT'Wild => true | _ => false))
                                      then b'Empty
                                      else b'DeTuple (length pats) n pat (Pd (env,pat)))
                       (enumerateFrom 0 pats))

(*----------------------------------------------------------------------
INDEX: NEW amatch/cmatch compilation
----------------------------------------------------------------------*)

  fun compile'CM {Pd:Pd,CM:CM,AM:AM,C:C} : (env * cmatch -> m_code) =
      fn (env,cmatch) =>
         case cmatch
          of CMATCH'Alt (cm1,cm2)   =>  let val mc1 = CM (env,cm1)
                                            val mc2 = CM (env,cm2)
                                        in mc'fatbar (mc1,mc2)
                                        end
           | CMATCH'Rule (pat,cps)  => let val b1 = Pd (env,pat)
                                           val e_code2 = countApps "A" (fn env1 => C (env ++ env1,cps))
                                       in mc'rule (b1,e_code2)
                                       end
           | CMATCH'Atom am         => AM (env,am)


  fun compile'AM {Pd:Pd,AM:AM,Aret:Aret} : (env * amatch -> m_code) =
      fn (env,amatch) =>
         case amatch
          of AMATCH'Alt (am1,am2)   => let val mc1 = AM (env,am1)
                                           val mc2 = AM (env,am2)
                                       in mc'fatbar (mc1,mc2)
                                       end
           | AMATCH'Rule (pat,atom) => let val b1 = Pd (env,pat)
                                           val e_code2 = countApps "B" (fn env1 => Aret (env ++ env1,atom))
                                       in mc'rule (b1,e_code2)
                                       end

  fun compile'AMt {Pd:Pd,AMt:AMt,At:At} : (env * amatch * q -> m_seq) =
      fn (env,amatch,q) =>
         case amatch
          of AMATCH'Alt (amatch1,amatch2) => let val ms1 = AMt (env,amatch1,q)
                                                 val ms2 = AMt (env,amatch2,q)
                                             in ms'fatbar (ms1,ms2)
                                             end
           | AMATCH'Rule (pat,atom) => let val b1 = Pd (env,pat)
                                           val e_seq2 = countApps "C" (fn env1 => At (env ++ env1,atom,q))
                                       in ms'rule (b1,e_seq2)
                                       end

(*----------------------------------------------------------------------
INDEX: PrimApp
----------------------------------------------------------------------*)

  fun compile_primApp {A:A,Ad:Ad} : (env * prim * atom * q -> seq) =
      fn (env,prim,atom,q) =>
         case (prim)
          of PRIM'Builtin bi   => if (arity'builtin bi = 1)
                                  then let val (seq,r) = A (env,atom)
                                       in seq $ seq'I (INS'BuiltinApp (q,bi,[r]))
                                       end
                                  else let val (seq1,rd) = Ad (env,atom)
                                           val (seq2,rs) = forceDelayedN (arity'builtin bi, rd)
                                       in seq1 $ seq2 $ seq'I (INS'BuiltinApp (q,bi,rs))
                                       end
           | PRIM'Con0 c        => let val (seq,r) = A (env,atom)
                                   in seq $ seq'I (INS'NewCon (q,c,r))
                                   end
           | PRIM'Ref           => let val (seq,r) = A (env,atom)
                                   in seq $ seq'I (INS'NewRef (q,r))
                                   end

(*----------------------------------------------------------------------
INDEX: compile_saveFrame
----------------------------------------------------------------------*)

  fun compile_saveFrame (SetFrameInstr: int * r -> instruction) : (env * uid list -> seq * env) =
      fn (env,save) =>
         let val save_slots = enumerateFrom 0 save
             val envNew = env'plusList (map (fn (x,n) => env'single (x,LOC'Frame (n,INFO'Uid x))) save_slots)
             val fill =
                 fold (fn seqAcc =>
                          fn (x,n) =>
                             let val r = look'env "compile_saveFrame" env x
                             in seqAcc $ seq'I (SetFrameInstr (n,r))
                             end) seq'Empty save_slots
         in (fill,envNew)
         end

(*----------------------------------------------------------------------
INDEX: B
----------------------------------------------------------------------*)

  fun compile_LetRecBinds {Ft} : (env * (uid * afunc) list -> env * seq) =
      fn (env,binds) =>
         let val trips = map (fn (x,f) => (x,f,newQ (INFO'Uid x))) binds
             val env = env ++ env'plusList (map (fn (x,_,q) => env'single (x,LOC'Stack q)) trips)
             val (seqA,seqB) =
                 fold (fn (seqA1,seqB1) =>
                          fn (x,f,q) => let val (seqA2,seqB2) = Ft (env,{name=sof'uid x},f,q)
                                        in (seqA1 $ seqA2, seqB1 $ seqB2)
                                        end) (seq'Empty,seq'Empty) trips
         in (env, seqA $ seqB)
         end

  fun compile'B {Pd:Pd} ({A,Ad,B,C,Aret,At,Ft}:schemes) : (env * bind -> env * seq) =
      fn (env,bind) =>
         case bind of
             BIND'Val (pat,atom) => let val (seq1,rd) = Ad (env,atom)
                                        val b_code = Pd (env,pat)
                                        val (seq2,env1) = closeB'rd (b_code,{fail=code'RaiseBind},rd)
                                    in  (env ++ env1, seq1 $ seq2)
                                    end
           | BIND'ValRec binds => compile_LetRecBinds {Ft=Ft} (env,binds)

(*----------------------------------------------------------------------
INDEX: C, Aret, CM, AM
----------------------------------------------------------------------*)

  fun CopyIfArg r =
      case r
       of LOC'Arg _ => let val q = newQ (INFO'String ("CopyIfArg"))
                       in (seq'I (INS'Copy (q,r)), LOC'Stack q)
                       end
        | _ => (seq'Empty, r)

  fun compile'C {Pd:Pd,CM:CM} ({A,Ad,B,C,Aret,At,Ft}:schemes) : (env * cps -> code) =
      fn (env,cps) =>
         case cps
          of CPS'Return atom                => Aret (env,atom)
           | CPS'Raise atom                 => let val (seq,r) = A (env,atom)
                                               in seq $$ code'Raise r
                                               end
           | CPS'App (func,args)            => let val (seq,r,rs) =
                                                       if (!right_to_left_args)
                                                       then let val (seq1,rs) = compileSequence (curry A env) (rev args)
                                                                val (seq2,r) = A (env,func)
                                                            in (seq1 $ seq2, r, rev rs)
                                                            end
                                                       else let val (seq2,r) = A (env,func)
                                                                val (seq1,rs) = compileSequence (curry A env) args
                                                            in (seq1 $ seq2, r, rs)
                                                            end
                                                   val (seqCopy1,r) = CopyIfArg r
                                                   val (seqCopy2,rs) = compileSequence CopyIfArg rs
                                               in seq $ seqCopy1 $ seqCopy2 $$ code'TailCall (r,rs)
                                               end
           | CPS'Let (bind,cps)             => let val (env,seq1) = B (env,bind)
                                                   val code2 = C (env,cps)
                                               in seq1 $$ code2
                                               end
           | CPS'LetC ({save},pat,cps1,cps2) =>
             let
                 val (fill,envNew) = compile_saveFrame INS'SetContFrameElem (env,save)
                 val code1 = C (env,cps1)
                 val b_code = Pd (envNew,pat)
                 val (seqPat,env1) = closeB'rd (b_code,{fail=code'RaiseBind},DELAYED'R LOC'Cret)
                 val envNew = envNew ++ env1
                 val code2 = C (envNew,cps2)
                 val push = seq'I (INS'PushContinuation ({frame_size=length save},
                                                         closeCode (seqPat $$ code2),
                                                         {info=(cps1,save)}))
             in push $ fill $$ code1
             end
           | CPS'Handle (cps,{save},cmatch) =>
             let
                 val (fill,envNew) = compile_saveFrame INS'SetXcontFrameElem (env,save)
                 val code = C (env,cps)
                 val m_code = CM (envNew,cmatch)
                 val codeHandle = closeMC'rd (m_code,{fail=code'Raise LOC'Xret},DELAYED'R (LOC'Xret))
                 val push = seq'I (INS'PushHandler ({frame_size = length save},
                                                    closeCode codeHandle,
                                                    {info=(cmatch,save)}))
             in push $ fill $$ code
             end
           | CPS'Case (atom,cmatch)         => let val (seq1,rd) = Ad (env,atom)
                                                   val m_code = CM (env,cmatch)
                                                   val code2 = closeMC'rd (m_code,{fail=code'RaiseMatch},rd)
                                               in seq1 $$ code2
                                               end

  fun compile'Aret {AM:AM} ({Ad,A,At,B,C,Aret,Ft}:schemes) : (env * atom -> code) =
      fn (env,atom) =>
         case atom
          of ATOM'Case (atom,amatch)    => let val (seq1,rd) = Ad (env,atom)
                                               val m_code = AM (env,amatch)
                                               val code2 = closeMC'rd (m_code,{fail=code'RaiseMatch},rd)
                                           in seq1 $$ code2
                                           end
           | ATOM'Let (bind,atom)       => let val (env,seq1) = B (env,bind)
                                               val code2 = Aret (env,atom)
                                           in seq1 $$ code2
                                           end
           | _ => let val (seq,r) = A (env,atom)
                  in seq $$ code'Return r
                  end

(*----------------------------------------------------------------------
INDEX: Ad, A, At, AMt, Ft
----------------------------------------------------------------------*)

  fun compile'Ad ({Ad,A,At,B,C,Aret,Ft}:schemes) : (env * atom -> seq * r_delayed) =
      fn (env,atom) =>
         case atom of
             ATOM'Tuple atoms       => let val (seq,rs) = compileTupleSequence (curry Ad env) atoms
                                       in (seq,DELAYED'Tuple (rs,{info=atom}))
                                       end
           | ATOM'Let (bind,atom)   => let val (env,seq1) = B (env,bind)
                                           val (seq2,rd) = Ad (env,atom)
                                       in (seq1 $ seq2, rd)
                                       end
           | _ => let val (seq,r) = A (env,atom)
                  in (seq,DELAYED'R r)
                  end

  fun compile'A ({Ad,A,At,B,C,Aret,Ft}:schemes) : (env * atom -> seq * r) =
      fn (env,atom) =>
         case atom
          of ATOM'Value v           => (seq'Empty, LOC'Value v)
           | ATOM'Var x             => (seq'Empty, look'env "compile_A" env x)
           | ATOM'Let (bind,atom)   => let val (env,seq1) = B (env,bind)
                                           val (seq2,r) = A (env,atom)
                                       in (seq1 $ seq2, r)
                                       end
           | _ => let val q = newQ (INFO'Atom atom)
                      val seq = At (env,atom,q)
                  in (seq, LOC'Stack q)
                  end

  fun compile'At {AMt:AMt} ({Ft,Ad,A,At,B,C,Aret}:schemes) : (env * atom * q -> seq) =
      fn (env,atom,q) =>
         case atom
          of ATOM'Value v               => seq'I (INS'Copy (q,LOC'Value v))
           | ATOM'NewEx x               => seq'I (INS'NewException (q,x))
           | ATOM'Var x                 => let val r = look'env "compile_At" env x
                                           in seq'I (INS'Copy (q,r))
                                           end
           | ATOM'Tuple atoms           => let val (seq,rs) = compileTupleSequence (curry A env) atoms
                                           in seq $ seq'tuple (q,rs)
                                           end
           | ATOM'PrimApp (prim,atom)   => compile_primApp {A=A,Ad=Ad} (env,prim,atom,q)
           | ATOM'Fn func               => let val (seqA,seqB) = Ft (env,{name="anon"},func,q)
                                           in seqA $ seqB
                                           end
           | ATOM'Case (atom,amatch)    => let val (seq1,rd) = Ad (env,atom)
                                               val m_seq = AMt (env,amatch,q)
                                               val seq2 = closeMS'rd (m_seq,{fail=code'RaiseMatch},rd)
                                           in seq1 $ seq2
                                           end
           | ATOM'Let (bind,atom)       => let val (env,seq1) = B (env,bind)
                                               val seq2 = At (env,atom,q)
                                           in seq1 $ seq2
                                           end

  fun compile'Ft ({B,C,Aret,Ad,A,At,Ft}:schemes) : (env * {name:string} * afunc * q -> seq * seq) =
      fn (env,{name},afunc as AFUNC'Def({save},args,cps),q) =>
         let fun SetFrameInstr (n,r) = INS'SetFrameElement (LOC'Stack q, n, r)
             val (seqB,envFrame) = compile_saveFrame SetFrameInstr (env,save)
             val envArgs = env'plusList (map (fn (x,n) => env'single (x,LOC'Arg (n, INFO'Uid x))) (enumerateFrom 0 args))
             val envNew = envFrame ++ envArgs
             val code = C (envNew,cps)
             val sizes = {frame_size = length save, num_args = length args}
             val info = {name=name, info=afunc}
             val seqA = seq'I (INS'NewFn (q,sizes,closeCode code,info))
         in (seqA,seqB)
         end

(*----------------------------------------------------------------------
INDEX: fix-up schemes
----------------------------------------------------------------------*)

  local
      fun Pd x = compile'Pd {Pd=Pd} x
      fun makeSchemes () = {B=B,C=C,Ad=Ad,Aret=Aret,A=A,At=At,Ft=Ft}
      and Ad    x = compile'Ad      (makeSchemes()) x
      and Aret  x = compile'Aret    {AM=AM} (makeSchemes()) x
      and A     x = compile'A       (makeSchemes()) x
      and At    x = compile'At      {AMt=AMt} (makeSchemes()) x
      and Ft    x = compile'Ft      (makeSchemes()) x
      and B     x = compile'B       {Pd=Pd} (makeSchemes()) x
      and C     x = compile'C       {Pd=Pd,CM=CM}(makeSchemes()) x

      and AMt  x = compile'AMt {Pd=Pd,AMt=AMt,At=At} x
      and AM   x = compile'AM {Pd=Pd,AM=AM,Aret=Aret} x
      and CM   x = compile'CM {Pd=Pd,AM=AM,CM=CM,C=C} x

  in val schemes = makeSchemes()
  end

  fun generateCode cps =
      let val {B,C,Ad,Aret,A,At,Ft}:schemes = schemes
      in closeCode (C (env'empty,cps))
      end

  (* reinstate real Machine.code type,, *)
  type code = Machine.code

end (* functor *)
