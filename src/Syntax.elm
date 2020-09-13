type TermLit = VarLit Char
             | AppLit TermLit TermLit
             | LamLit Char TermLit

type TermVal = VarVal Char Int 
             | AppVal TermVal TermVal (Set.Set Char)
             | LamVal Char TermVal (Set.Set Char)
