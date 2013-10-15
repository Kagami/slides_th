{-# LANGUAGE TemplateHaskell #-}

import TH1

a = $(compile [| 7 * 3 |])
b = $(compile [| 7 * 9 + 3 |])
c = $(compile [| 5 * 8 + 6 * 3 |])

main :: IO ()
main = return ()
