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
  , getBackendValue
  , getBackendTypeValue
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


type BackendConstructor = NonEmpty Name

-- | Inspects the 'BackendType' to produce a list of its constructors in the 'Q' monad. Each
-- constructor is represented as a list of names, to include the arguments, if any.
-- This assumes that the arguments themselves don't have arguments.
backendConstructors :: Q [BackendConstructor]
backendConstructors = do
  TyConI (DataD _ _ _ _ cons _) <- reify ''BackendType
  concat <$> for cons \con -> do
    -- We pattern match in the monad to rely on 'fail'.
    NormalC name args <- pure con
    argsConstructors <- for args \(_, arg) -> do
      ConT argName <- pure arg
      TyConI (DataD _ _ _ _ argCons _) <- reify argName
      pure [argCon | NormalC argCon _ <- argCons]
    pure $ map (name :|) $ sequenceA argsConstructors

-- | Associates a value in the 'Q' monad to each backend @Name@.
forEachBackend :: (BackendConstructor -> Q a) -> Q [a]
forEachBackend f = traverse f =<< backendConstructors

-- | Associates to a backend the promoted type level value.
--   getBackendValue ["Postgres", "Vanilla"] = [| Postgres Vanilla |]
getBackendValue :: BackendConstructor -> Exp
getBackendValue backend = foldl1 AppE $ ConE <$> backend

-- | Associates to a backend the promoted type level value.
--   getBackendValue ["Postgres", "Vanilla"] = [t| ('Postgres 'Vanilla) |]
getBackendTypeValue :: BackendConstructor -> Type
getBackendTypeValue backend = ParensT $ foldl1 AppT $ PromotedT <$> backend

-- | Associates to a backend the Name of its corresponding tag.
getBackendTagName :: BackendConstructor -> Name
getBackendTagName backend = mkName $ concatMap nameBase backend ++ "Tag"

-- | Associates to a backend the Name of its corresponding 'AnyBackend' constructor.
getBackendValueName :: BackendConstructor -> Name
getBackendValueName backend = mkName $ concatMap nameBase backend ++ "Value"

-- | Creates a list of values by associating an expression to each backend.
backendList :: (BackendConstructor -> Q Exp) -> Q Exp
backendList f = ListE <$> forEachBackend f

-- | Creates a case expression with a match for each backend. It is not possible
-- do directly expand a @Q [Match]@, which is a body of a case, hence the need
-- to instead generate the full @Q Exp@.
backendCase
  :: Q Exp                         -- ^ the expresion on which we do a case switch
  -> (BackendConstructor -> Q Pat) -- ^ the match pattern for a given backend
  -> (BackendConstructor -> Q Exp) -- ^ the match body for a given backend
  -> Maybe (Q Exp)                 -- ^ the default case, if any
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

-- | Creates a data type in which there's one constructor per backend. While
-- this only returns one declaration, it nonetheless returns a @[Dec]@ as it's
-- what the $() splice interpolation syntax expects.
backendData
  :: Name                          -- ^ the name of the type
  -> [TyVarBndr]                   -- ^ type variables of the type if any
  -> (BackendConstructor -> Q Con) -- ^ the constructor for a given backend
  -> [Name]                        -- ^ classes to derive using the stock strategy
  -> Q [Dec]
backendData name tVars mkCon derivs = do
  constructors <- forEachBackend mkCon
  pure [DataD [] name tVars Nothing constructors [DerivClause (Just StockStrategy) $ map ConT derivs]]

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
