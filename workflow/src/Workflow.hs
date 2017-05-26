{-# LANGUAGE RecordWildCards #-}

module Workflow where

import Import
import Workflow.Next
import Workflow.OptParse
import Workflow.Reminders
import Workflow.Waiting

workflow :: IO ()
workflow = do
    (disp, sett) <- getInstructions
    execute disp sett

execute :: Dispatch -> Settings -> IO ()
execute (DispatchWaiting WaitingArgsDispatch {..}) =
    waiting dspWorkDir dspWaitingShouldPrint
execute (DispatchNext NextArgsDispatch {..}) =
    next dspProjectDir dspProjectFiles dspNextShouldPrint
execute (DispatchRem (RemArgsDispatch WaitingArgsDispatch {..} maxDays fromAddress mailTemplate)) =
    reminders maxDays dspWorkDir fromAddress dspWaitingShouldPrint mailTemplate
