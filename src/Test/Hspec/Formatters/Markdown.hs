{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Formatters.Markdown (
  markdownFormatter
  ) where

import Data.Maybe (fromMaybe)
import Text.Printf
import Test.Hspec.Formatters

h :: Int -> String -> String
h n text = replicate n '#' ++ " " ++ text ++ " " ++ replicate n '#' 

item :: String -> String
item text = "* " ++ text

markdownFormatter :: Formatter
markdownFormatter = silent
  { headerFormatter = writeLine ""
  , exampleGroupStarted = \_ nesting name -> do
      writeLine $ h (length nesting+1) name
      newParagraph
  , exampleGroupDone = newParagraph
  , exampleSucceeded = \(_, requirement) -> writeLine (item requirement)
  , exampleFailed = \(_, requirement) _ -> do
      writeLine $ item (requirement ++ " (failed)")
  , examplePending = \(_, requirement) reason -> writeLine $ item (requirement ++ " (pending, " ++ fromMaybe "No reason given" reason ++ ")")
  , footerFormatter = markdownSummary
  }

markdownSummary :: FormatM ()
markdownSummary = do
  writeLine (h 1 "Test Summary")

  noTotal   <- getTotalCount
  noFail    <- getFailCount
  noPending <- getPendingCount
  writeLine (item (plural noTotal "test"))
  writeLine (item (plural noFail "failure"))
  writeLine (item (plural noPending "test" ++ " pending"))

  time <- getRealTime
  writeLine (item (printf "finished in %1.4f seconds" time))

plural :: Int -> String -> String
plural n text
  | n==0      = "no " ++ text ++ "s"
  | n==1      = show n ++ " " ++ text
  | otherwise = show n ++ " " ++ text ++ "s"
