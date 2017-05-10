{-# LANGUAGE RecordWildCards #-}

module Workflow where

import Import
import Workflow.Next
import Workflow.OptParse
import Workflow.Waiting

workflow :: IO ()
workflow = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchWaiting WaitingArgsDispatch {..}) =
    waiting dspWworkDir dspWaitingShouldPrint
execute (DispatchNext NextArgsDispatch {..}) =
    next dspProjectDir dspNextShouldPrint
