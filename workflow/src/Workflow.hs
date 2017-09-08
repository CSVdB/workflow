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
execute (DispatchWaiting DispatchWaitingArgs {..}) =
    waiting dspWorkDir dspWaitingShouldPrint
execute (DispatchNext DispatchNextArgs {..}) =
    next dspProjectDir dspProjectFiles dspNextShouldPrint
execute (DispatchRem (DispatchRemArgs DispatchWaitingArgs {..} maxDays fromAddress mailTemplate)) =
    reminders $
    RemSets maxDays dspWorkDir fromAddress dspWaitingShouldPrint mailTemplate
