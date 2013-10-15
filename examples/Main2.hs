{-# LANGUAGE TemplateHaskell #-}

import TH2

data Test = Test
    { fieldA :: Int
    , fieldB :: Bool
    }

$(deriveShow ''Test)

main :: IO ()
main = print $ Test 1 False
