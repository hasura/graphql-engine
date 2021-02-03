module Hasura.SQL.Backend where

import           Hasura.Prelude

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax


data BackendType = Postgres -- | MySQL | MSSQL
  deriving (Show, Bounded, Enum)
  -- TODO: introduce a "None" backend for parts of the code that
  -- should never touch an actual backend, such as introspection
  -- queries.


-- | This TH function generates a function to do a dynamic dispatch of a value
-- based on its backend.
--
-- For instance, consider the following:
--
--   $(mkBackendDispatch (mkName "withSchema") ''BackendSchema $ \b -> [t| SourceInfo $b |])
--
-- This generates the following function:
--
--   withSchema
--     :: (Backend b, BackendSchema 'Postgres, BackendSchema 'MSSQL, ...)
--     => SourceInfo b
--     -> (forall b'. BackendSchema b' => SourceInfo b' -> a)
--     -> a
--   withSchema x f
--     | Just y <- cast x :: Maybe (SourceInfo 'Postgres) = f y
--     | Just y <- cast x :: Maybe (SourceInfo 'MSSQL)    = f y
--     ...
--     | otherwise = error "impossible"
--
-- This thus allows to dispatch a `SourceInfo b` to a function that requires a
-- `BackendSchema` constraint on said b, even if that constraint was not present
-- when the value was created by simply enumerating all possible instances.
--
-- This allows us to extend the behaviour of each backend without having to list
-- each typeclass in the SourceCache, keeping the code separate, at the cost of
-- a few casts. Using Template Haskell also means we don't have to maintain any
-- list of backends in the code.
mkBackendDispatch
  :: Name               -- name of the function
  -> Name               -- name of the typeclass
  -> (Q Type -> Q Type) -- type of the argument
  -> Q [Dec]
mkBackendDispatch funcName className mkType = do
  onError <- [| error "The impossible happened: no matching backend was found." |]
  let varA  = VarT $ mkName "a"
      varB  = VarT $ mkName "b"
      varB' = VarT $ mkName "b'"
  bType  <- mkType $ pure varB
  b'Type <- mkType $ pure varB'
  let mkConstraint = AppT (ConT className)
      mkFunc t1 t2 = AppT (AppT ArrowT t1) t2
      backendTypes = [PromotedT $ mkName (show b) | (b :: BackendType) <- [minBound .. maxBound]]
      baseConstraint = AppT (ConT $ mkName "Backend") varB
      backendConstraints = [mkConstraint b | b <- backendTypes]
      funcType = ForallT [] (baseConstraint : backendConstraints) $ foldr1 mkFunc
        [ bType
        , ForallT [PlainTV $ mkName "b'"] [mkConstraint varB'] $ mkFunc b'Type varA
        , varA
        ]
      defaultCase = ( NormalG $ VarE 'otherwise
                    , onError
                    )
  funcGuards <- for backendTypes \b -> do
    argType <- mkType $ pure b
    pure ( PatG [BindS (ConP justName [VarP $ mkName "y"]) (SigE (AppE (VarE $ mkName "cast") (VarE $ mkName "x")) (AppT (ConT ''Maybe) argType))]
         , AppE (VarE $ mkName "f") (VarE $ mkName "y")
         )
  pure [ SigD funcName funcType
       , FunD funcName [Clause [VarP $ mkName "x", VarP $ mkName "f"] (GuardedB $ funcGuards <> [defaultCase]) []]
       ]
