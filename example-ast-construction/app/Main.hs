{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where


import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Syntax
import Language.Rust.Pretty.Internal

-- import Data.Text.Prettyprint.Doc (Doc, layoutPretty, LayoutOptions(..), PageWidth(..), pretty)
import Data.Text.Prettyprint.Doc.Render.String (renderShowS)
import Language.Rust.Pretty (prettyAnnotated', pretty, pretty')


-- | Common types to make tests more straightforward
i32, f64, usize :: Ty ()
i32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()
f64 = PathTy Nothing (Path False [PathSegment "f64" Nothing ()] ()) ()
usize = PathTy Nothing (Path False [PathSegment "usize" Nothing ()] ()) ()

monitorTrait :: Item ()
monitorTrait = Trait [] PublicV (mkIdent "MonitorTriggers") False Normal (Generics [] [] (WhereClause [] ()) ()) []
                      [ MethodT [] (mkIdent "heat_on")
                          (Generics [] [] (WhereClause [] ()) ())
                          (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()
                                            , Arg (Just (IdentP (ByValue Immutable) (mkIdent "arg0") Nothing ())) f64 ()]
                                              Nothing
                                              False ())) 
                          Nothing
                          ()
                      , MethodT [] (mkIdent "heat_off")
                          (Generics [] [] (WhereClause [] ()) ())
                          (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()
                                            , Arg (Just (IdentP (ByValue Immutable) (mkIdent "arg0") Nothing ())) f64 ()]
                                              Nothing
                                              False ())) 
                          Nothing
                          ()
                      , MethodT [] (mkIdent "speed_warning")
                          (Generics [] [] (WhereClause [] ()) ())
                          (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()
                                            , Arg (Just (IdentP (ByValue Immutable) (mkIdent "arg0") Nothing ())) f64 ()]
                                              Nothing
                                              False ())) 
                          Nothing
                          ()
                      ]
                      ()


main :: IO ()
main = do
    print (printItem monitorTrait)