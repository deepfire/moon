showTypeable _ TrType = showChar '*'
showTypeable _ rep
  | isListTyCon tc, [ty] <- tys =
    showChar '[' . shows ty . showChar ']'
  | isTupleTyCon tc =
    showChar '(' . showArgs (showChar ',') tys . showChar ')'
  where (tc, tys) = splitApps rep
showTypeable _ TrTyCon {trTyCon = tycon, trKindVars = []}
  = showTyCon tycon
showTypeable p TrTyCon {trTyCon = tycon, trKindVars = args}
  = showParen (p > 9) $
    showTyCon tycon .
    showChar ' ' .
    showArgs (showChar ' ') args
showTypeable p TrFun {trFunArg = x, trFunRes = r}
  = showParen (p > 8) $
    showsPrec 9 x . showString " -> " . showsPrec 8 r
showTypeable p TrApp {trAppFun = f, trAppArg = x}
  = showParen (p > 9) $
    showsPrec 8 f .
    showChar ' ' .
    showsPrec 10 x
