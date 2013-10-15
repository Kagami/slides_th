{-# LANGUAGE TemplateHaskell #-}

module TH1 where

import Debug.Trace (trace)
import Language.Haskell.TH

compile :: ExpQ -> ExpQ
compile exp = do
    exp' <- exp
    let input = trace ("\n"++show exp'++"\n") exp'
    litE $ IntegerL $ compile' input

compile' :: Exp -> Integer
compile' (InfixE (Just (LitE (IntegerL a))) op (Just (LitE (IntegerL b)))) =
    name2op op a b
compile' (InfixE (Just (LitE (IntegerL a))) op (Just b)) =
    name2op op a (compile' b)
compile' (InfixE (Just a) op (Just (LitE (IntegerL b)))) =
    name2op op (compile' a) b
compile' (InfixE (Just a) op (Just b)) =
    name2op op (compile' a) (compile' b)
compile' _ =
    error "Not implemented"

name2op :: (Num a, Integral a) => Exp -> (a -> a -> a)
name2op (VarE name)
    | name == '(+) = (+)
    | name == '(-) = (-)
    | name == '(*) = (*)
    | name == '(/) = div
name2op _ = error "Not implemented"
