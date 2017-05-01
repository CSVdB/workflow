{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Workflow.Next where

import Import
import Workflow.OptParse

next :: Path Abs Dir -> ShouldPrint -> Settings -> IO ()
next _ _ _ = undefined

