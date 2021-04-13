-- | This module defines all basic Template Haskell functions we use in the rest
-- of this folder, to generate code that deals with all possible known
-- backends.
--
-- Those are all "normal" Haskell functions in the @Q@ monad: they deal with
-- values that represent Haskell code. Those functions are used, in other
-- modules, within Template Haskell splices.
--
-- For more information about Template Haskell:
--   * this tutorial is an incredible resource: https://markkarpov.com/tutorial/th.html
--   * the definition of the AST is fairly readable:
--     * Exp: https://hackage.haskell.org/package/template-haskell-2.16.0.0/docs/Language-Haskell-TH.html#t:Exp
--     * Dec: https://hackage.haskell.org/package/template-haskell-2.16.0.0/docs/Language-Haskell-TH.html#t:Dec

module Hasura.SQL.TH
  ( backendConstructors
  , forEachBackend
  , getBackendTagName
  , getBackendValueName
  , backendList
  , backendCase
  , backendData
  , mkDispatch
  ) where

import           Hasura.Prelude

import           Language.Haskell.TH

import           Hasura.SQL.Backend


-- | Inspects the 'BackendType' to produce a list of its constructors in the 'Q'
-- monad.
backendConstructors :: Q [Name]
backendConstructors = do
  -- It is safe to assume that we know the "shape" of 'BackendType' here. It is
  -- an instance of `Enum`, and therefore was already checked by the compiler:
  -- we have the guarantee it only has nullary normal constructors. Furthermore,
  -- a fail here would only result in a compilation error, not a runtime one.
  TyConI (DataD _ _ _ _ cons _) <- reify ''BackendType
  pure [name | NormalC name _ <- cons]

-- | Associates a value in the 'Q' monad to each backend @Name@.
forEachBackend :: (Name -> Q a) -> Q [a]
forEachBackend f = traverse f =<< backendConstructors

-- | Associates to a backend the Name of its corresponding tag.
getBackendTagName :: Name -> Name
getBackendTagName backend = mkName $ nameBase backend ++ "Tag"

-- | Associates to a backend the Name of its corresponding 'AnyBackend' constructor.
getBackendValueName :: Name -> Name
getBackendValueName backend = mkName $ nameBase backend ++ "Value"

-- | Creates a list of values by associating an expression to each backend.
backendList :: (Name -> Q Exp) -> Q Exp
backendList f = ListE <$> forEachBackend f

-- | Creates a case expression with a match for each backend. It is not possible
-- do directly expand a @Q [Match]@, which is a body of a case, hence the need
-- to instead generate the full @Q Exp@.
backendCase
  :: Q Exp           -- ^ the expresion on which we do a case switch
  -> (Name -> Q Pat) -- ^ the match pattern for a given backend
  -> (Name -> Q Exp) -- ^ the match body for a given backend
  -> Maybe (Q Exp)   -- ^ the default case, if any
  -> Q Exp
backendCase caseExp toPat toBody defaultCase = do
  cexp    <- caseExp
  matches <- forEachBackend \b -> do
    pat  <- toPat  b
    body <- toBody b
    pure $ Match pat (NormalB body) []
  allMatches <- case defaultCase of
    Nothing -> pure matches
    Just e  -> do
      defaultBody <- NormalB <$> e
      pure $ matches ++ [Match WildP defaultBody []]
  pure $ CaseE cexp allMatches

-- | Creates a data type in which there's one constructor per backend.  While
-- this only returns one declaration, it nonetheless returns a @[Dec]@ as it's
-- what the $() splice interpolation syntax expects.
backendData
  :: Name            -- ^ the name of the type
  -> [TyVarBndr]     -- ^ type variables of the type if any
  -> (Name -> Q Con) -- ^ the constructor for a given backend
  -> Q [Dec]
backendData name tVars mkCon = do
  constructors <- forEachBackend mkCon
  pure [DataD [] name tVars Nothing constructors []]

-- | Generates a case expression that applies a function @f@ to each possible value
-- of an 'AnyBackend' @e@:
--
--   case e of
--     FooValue x -> f x
--     BarValue x -> f x
--
-- This function needs to be in a separate file from 'AnyBackend', so that it
-- can be used in splices of another module.
mkDispatch
  :: Name  -- ^ the name of the function to dispatch
  -> Name  -- ^ the name of the 'AnyBackend' value
  -> Q Exp
mkDispatch func value = do
  let fE = pure $ VarE func
      vE = pure $ VarE value
  backendCase [| $vE |]
    -- the pattern for a backend
    (\b -> pure $ ConP (getBackendValueName b) [VarP $ mkName "x"])
    -- the body for a backend
    (const [| $fE x |])
    -- no default case
    Nothing
