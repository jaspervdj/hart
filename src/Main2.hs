{-#
LANGUAGE
GADTs
#-}
{-#
LANGUAGE
PolyKinds #-} module Main
where { import Language.Haskell.Exts; main
:: IO (); main = do { content <- readFile fName; case parseModule
content of { ParseOk mod@(Module _ _ _ _ decls) -> do {
let { onelined = prettyPrintStyleMode
style defaultMode{layout =
PPNoLayout} mod};
putStrLn
onelined};
_ -> error "failed"}}; fName = "Main.hs"}
