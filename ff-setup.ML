
structure Prel = PREL();
structure Sort = SORT();
structure QLayout = QLAYOUT(structure Prel = Prel)
structure Misclay = MISCLAY(structure Prel = Prel
							structure QLayout = QLayout)
structure ImpHash = IMP_HASH(structure Prel = Prel
							 structure Misclay = Misclay)


open Prel;
open Sort;
open Misclay;
open ImpHash;
