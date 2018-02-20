module EarlyAnnPlugin (plugin) where

import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = prepareWeights
  }

prepareWeights :: [CommandLineOption] -> ModSummary -> HsParsedModule
                   -> Hsc HsParsedModule
prepareWeights _ _ = return
