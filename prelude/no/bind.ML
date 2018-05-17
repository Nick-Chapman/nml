
structure Prel = PREL();
structure Assoc = ASSOC(structure Prel = Prel)
structure QLayout = QLAYOUT(structure Prel = Prel)
structure Misclay = MISCLAY(structure Prel = Prel
							structure QLayout = QLayout)
structure ImpHash = IMP_HASH(structure Prel = Prel
							 structure Misclay = Misclay)
