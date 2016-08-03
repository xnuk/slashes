{-# LANGUAGE OverloadedStrings #-}

module Lib (run) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Monoid ((<>))

run :: Text -> Text
run "" = ""
run code =
    let (pre, code' ) = token code
        (pat, code'') = token code'
        (rep, next  ) = token code''
    in pre <> case () of
        _ | code' == "\0" -> ""
          | code'' == "\0" -> error "non-terminated pattern"
          | next == "\0" -> error "non-terminated replacement"
          | pat == "" -> T.cycle rep
          | pat == rep -> run next
          | otherwise -> let xs = iterate (T.replace pat rep) next
                         in run . fst . head . dropWhile (uncurry (/=)) $ zip xs (tail xs)

token :: Text -> (Text, Text)
token code =
    let (pre, next) = T.break (`elem` ['\\', '/']) code
        nextTail = T.tail next
    in case () of
        _ | next == "" -> (pre, "\0")
          | nextTail == "" -> (pre, "")
          | otherwise -> case T.head next of
              '\\' -> let (a, b) = token (T.tail nextTail) in (pre <> T.singleton (T.head nextTail) <> a, b)
              '/'  -> (pre, nextTail)
              _    -> undefined
              -- State?
