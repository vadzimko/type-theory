type tree =
      Var      of string
	| Apply    of tree   * tree
	| Abstract of string * tree;;