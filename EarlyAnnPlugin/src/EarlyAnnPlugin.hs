module EarlyAnnPlugin (plugin) where

import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = parsedResultAction
  }

parsedResultAction :: [CommandLineOption] -> ModSummary -> HsParsedModule
                   -> Hsc HsParsedModule
parsedResultAction _ _ = return . id
