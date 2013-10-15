{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module TH2 where

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)

deriveShow :: Name -> DecsQ
deriveShow name = do
    info <- reify name
    case info of
        TyConI (DataD _ _ _ [RecC _ fields] _) ->
            [d| instance Show $(conT name) where
                    show rec = $(showRec fields)
            |]
        _ ->
            error "Not implemented"
  where
    showRec :: [VarStrictType] -> ExpQ
    showRec fields =
        let rec = show name ++ ":\n\t"
        in [| rec ++ intercalate "\n\t" $(listE $ map showField fields) |]

    showField :: VarStrictType -> ExpQ
    showField (fieldName, _, _) =
        let recName = mkName "rec"
            field = show fieldName ++ ": "
        in [| field ++ show $(appE (varE fieldName) (varE recName)) |]
