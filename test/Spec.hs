{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

import Test.Hspec
import Lib (run)
import Data.FileEmbed (embedStringFile)
import Text.RawString.QQ (r)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))
import Data.Function ((&))

thueMorse :: Int -> Text
thueMorse 0 = "0"
thueMorse n = thueMorse (n-1) <> T.map (\x -> if x == '0' then '1' else '0') (thueMorse (n-1))

main :: IO ()
main = hspec $
    describe "Real world examples test" $ do
        it "Hello, world!" $ do
            let helloWorld = "Hello, world!"
            run "Hello, world!" `shouldBe` helloWorld
            run "/ world! world!/Hello,/ world! world! world!" `shouldBe` helloWorld
            run "/foo/Hello, world!//bar/foo/bar" `shouldBe` helloWorld
        it "99 bottles of beer" $
            run $(embedStringFile "test/beer.1.in") `shouldBe` $(embedStringFile "test/beer.out")
        it "Thue-Morse sequence" $
            run [r|/*/\/.\\0\/,\\,0,\\,1\/\/.\\1\/,\\,1,\\,0\/\/,\\,\/.\//********/.//.0|] `shouldBe` thueMorse 8
        it "Fibonacci sequence" $
            run $(embedStringFile "test/fibonacci.in") `shouldBe` (map (`T.replicate` "*") [1, 1, 2, 3, 5, 8, 13, 21, 34, 55] & T.intercalate "/")
        it "Quine" $
            let quine = $(embedStringFile "test/quine.txt")
            in run quine `shouldBe` quine
