
(*----------------------------------------------------------------------
INDEX:  TODO - avoid tupling / untupling
----------------------------------------------------------------------*)
(*

(* this is merging two diff optimization ideas... hmm, maybe can seperate*)
  datatype location_structure
    = LOCS'Tuple    of location_structure list
    | LOCS'Loc      of r
    | LOCS'DeCon    of r
    | LOCS'DeExcon  of r
    | LOCS'DeTuple  of r * int

  type rs = location_structure
		   
(* New compilation schemes *)
			
	S : rs -> seq * r (* do the Decons / build the tuple implied by the rs location_structure *)

(* note: more important to avoid un-nesc tuple build (as this allocates) 
as opposed to avoiding un-nesc de-cons *)

	Ps: env * pat * rs -> seq * env (* compile pat to seq, with matching-value spread over rs *)
								  
	As: env * atom -> seq * rs (* compile atom to seq, (delaying tuple builds), so value of atom is spread over es *)

(* Is the following a good idea / sensible / possible ? - what exactly do we avoid?*)
- (* NO, I cant see what benefit these are - despite them being written on my paper notes !!! *)
(* perhaps it helps: let val pat = case ...
or case (case ...) of
*)

	type qs = stack_location_structure

    Pexpect : env * pat -> qs * seq * env (* compile pattern to seq, requested matching-value to be spead over qs *)

    Ats : env * atom * qs -> seq (* compile atom, speading value over targer stack_location_structure (qs) *)


*)
