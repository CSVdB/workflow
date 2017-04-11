module Workflow where

import Introduction

import Workflow.Types
import Workflow.OptParse
-- import qualified Data.Text.IO as T

workflow :: IO ()
workflow = do
	(disp, sett) <- getInstructions
	-- Use this dispatch and these settings
	-- Use T.readFile to read files
	putStrLn "hi"

waiting :: IO ()
waiting = putStrLn "hi"
-- This function should make a list of all "waiting" tasks,
-- sort them chronologically, format them into a nice string and print the string.
