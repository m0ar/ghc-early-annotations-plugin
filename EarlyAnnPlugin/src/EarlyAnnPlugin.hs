module EarlyAnnPlugin (plugin) where

import GhcPlugins
import HsDecls (HsDecl(..), AnnDecl(..), AnnProvenance(..))
import HsSyn (hsmodDecls)
import HsExpr (HsExpr(..))
import HsExtension (IdP(..), GhcPs)

-- | A compiler plugin must export a value of this type. We take an "identity"
-- plugin ('defaultPlugin'), and replace the 'parsedResultAction' field with our
-- implementation: 'prepareAnnotations'.
plugin :: Plugin
plugin = defaultPlugin {
    parsedResultAction = prepareAnnotations
  }

-- | Extracts binders with payload from 'ANN' pragmas through 'HsParsedModule'
-- and adds them to the typechecking global environment 'TcGblEnv', for use
-- later in the compilation pipeline.
--
-- Note: if TcGblEnv is deemed unfit for this job, we need to find another way
-- to stash the 'ANN' binders and associated payload.
prepareAnnotations :: [CommandLineOption] -> ModSummary -> HsParsedModule
                   -> Hsc HsParsedModule
prepareAnnotations _ _ = return

type StrippedAnnD = (AnnProvenance RdrName, HsExpr GhcPs)

-- | Traverses the top level declarations in the module, finds annotations
-- and returns the annotated binding together with the payload expression.
findAnnDecls :: HsParsedModule -> [StrippedAnnD]
findAnnDecls hpm = let L _ hsModule = hpm_module hpm in
  let lHsDecls = hsmodDecls hsModule in stripAndFilter lHsDecls

-- | Strips location wrappers and collects content from ANN declarations from
-- the top level declarations; we do not traverse deeper into the AST.
--
-- HsDecl has a constructor 'AnnD (AnnDecl name)', where
-- 'AnnDecl name = HsAnnotation (AnnProvenance name) (Located (HsExpr name))'
stripAndFilter :: [Located (HsDecl GhcPs)] -> [StrippedAnnD]
stripAndFilter = foldr unwrapAnnD []
  where 
    unwrapAnnD :: Located (HsDecl GhcPs) -> [StrippedAnnD] -> [StrippedAnnD]
    unwrapAnnD (L _ (AnnD (HsAnnotation _ annProv lHsExpr))) annDecls =
      let L _ hsExpr = lHsExpr in (annProv, hsExpr):annDecls
    unwrapAnnD _ annDecls = annDecls
    -- ^ Not interested in anything else, so we skip any other declaration
