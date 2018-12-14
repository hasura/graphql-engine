module Hasura.GraphQL.Validate.InputValue
  ( validateInputValue
  , jsonParser
  , valueParser
  , constValueParser
  ) where

import           Data.Scientific                 (fromFloatDigits)
import           Hasura.Prelude
import           Hasura.Server.Utils             (duplicates)

import           Data.Has

import qualified Data.Aeson                      as J
import qualified Data.HashMap.Strict             as Map
import qualified Data.HashMap.Strict.InsOrd      as OMap
import qualified Data.Vector                     as V
import qualified Language.GraphQL.Draft.Syntax   as G

import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Value

newtype P a = P { unP :: Maybe (Either AnnGValue a)}

pNull :: (Monad m) => m (P a)
pNull = return $ P Nothing

pVal :: (Monad m) => a -> m (P a)
pVal = return . P . Just . Right

resolveVar
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m)
  => G.Variable -> m AnnGValue
resolveVar var = do
  varVals <- _vcVarVals <$> ask
  -- TODO typecheck
  onNothing (Map.lookup var varVals) $
    throwVE $ "no such variable defined in the operation: "
    <> showName (G.unVariable var)
  where
    typeCheck expectedTy actualTy = case (expectedTy, actualTy) of
      -- named types
      (G.TypeNamed _ eTy, G.TypeNamed _ aTy) -> eTy == aTy
      -- list types
      (G.TypeList _ eTy, G.TypeList _ aTy) -> typeCheck (G.unListType eTy) (G.unListType aTy)
      (_, _) -> False

pVar
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m)
  => G.Variable -> m (P a)
pVar var = do
  annInpVal <- resolveVar var
  return . P . Just . Left $ annInpVal

data InputValueParser a m
  = InputValueParser
  { getScalar :: a -> m (P J.Value)
  , getList   :: a -> m (P [a])
  , getObject :: a -> m (P [(G.Name, a)])
  , getEnum   :: a -> m (P G.EnumValue)
  }

jsonParser :: (MonadError QErr m) => InputValueParser J.Value m
jsonParser =
  InputValueParser jScalar jList jObject jEnum
  where
    jEnum (J.String t) = pVal $ G.EnumValue $ G.Name t
    jEnum J.Null       = pNull
    jEnum _            = throwVE "expecting a JSON string for Enum"

    jList (J.Array l) = pVal $ V.toList l
    jList J.Null      = pNull
    jList v           = pVal [v]

    jObject (J.Object m) = pVal [(G.Name t, v) | (t, v) <- Map.toList m]
    jObject J.Null       = pNull
    jObject _            = throwVE "expecting a JSON object"

    jScalar J.Null = pNull
    jScalar v      = pVal v

toJValue :: (MonadError QErr m) => G.Value -> m J.Value
toJValue = \case
  G.VVariable _                   ->
    throwVE "variables are not allowed in scalars"
  G.VInt i                        -> return $ J.toJSON i
  G.VFloat f                      -> return $ J.toJSON f
  G.VString (G.StringValue t)     -> return $ J.toJSON t
  G.VBoolean b                    -> return $ J.toJSON b
  G.VNull                         -> return J.Null
  G.VEnum (G.EnumValue n)         -> return $ J.toJSON n
  G.VList (G.ListValueG vals)     ->
    J.toJSON <$> mapM toJValue vals
  G.VObject (G.ObjectValueG objs) ->
    J.toJSON . Map.fromList <$> mapM toTup objs
  where
    toTup (G.ObjectFieldG f v) = (f,) <$> toJValue v

valueParser
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m)
  => InputValueParser G.Value m
valueParser =
  InputValueParser pScalar pList pObject pEnum
  where
    pEnum (G.VVariable var) = pVar var
    pEnum (G.VEnum e)       = pVal e
    pEnum G.VNull           = pNull
    pEnum _                 = throwVE "expecting an enum"

    pList (G.VVariable var) = pVar var
    pList (G.VList lv)      = pVal $ G.unListValue lv
    pList G.VNull           = pNull
    pList v                 = pVal [v]

    pObject (G.VVariable var)    = pVar var
    pObject (G.VObject ov)       = pVal
      [(G._ofName oFld, G._ofValue oFld) | oFld <- G.unObjectValue ov]
    pObject G.VNull = pNull
    pObject _                    = throwVE "expecting an object"

    -- scalar json
    pScalar (G.VVariable var) = pVar var
    pScalar G.VNull           = pNull
    pScalar (G.VInt v)        = pVal $ J.Number $ fromIntegral v
    pScalar (G.VFloat v)      = pVal $ J.Number $ fromFloatDigits v
    pScalar (G.VBoolean b)    = pVal $ J.Bool b
    pScalar (G.VString sv)    = pVal $ J.String $ G.unStringValue sv
    pScalar (G.VEnum _)       = throwVE "unexpected enum for a scalar"
    pScalar v                 = pVal =<< toJValue v

toJValueC :: G.ValueConst -> J.Value
toJValueC = \case
  G.VCInt i                        -> J.toJSON i
  G.VCFloat f                      -> J.toJSON f
  G.VCString (G.StringValue t)     -> J.toJSON t
  G.VCBoolean b                    -> J.toJSON b
  G.VCNull                         -> J.Null
  G.VCEnum (G.EnumValue n)         -> J.toJSON n
  G.VCList (G.ListValueG vals)     ->
    J.toJSON $ map toJValueC vals
  G.VCObject (G.ObjectValueG objs) ->
    J.toJSON . Map.fromList $ map toTup objs
  where
    toTup (G.ObjectFieldG f v) = (f, toJValueC v)

constValueParser :: (MonadError QErr m) => InputValueParser G.ValueConst m
constValueParser =
  InputValueParser pScalar pList pObject pEnum
  where
    pEnum (G.VCEnum e) = pVal e
    pEnum G.VCNull     = pNull
    pEnum _            = throwVE "expecting an enum"

    pList (G.VCList lv) = pVal $ G.unListValue lv
    pList G.VCNull      = pNull
    pList v             = pVal [v]

    pObject (G.VCObject ov)       = pVal
      [(G._ofName oFld, G._ofValue oFld) | oFld <- G.unObjectValue ov]
    pObject G.VCNull = pNull
    pObject _                    = throwVE "expecting an object"

    -- scalar json
    pScalar G.VCNull        = pNull
    pScalar (G.VCInt v)     = pVal $ J.Number $ fromIntegral v
    pScalar (G.VCFloat v)   = pVal $ J.Number $ fromFloatDigits v
    pScalar (G.VCBoolean b) = pVal $ J.Bool b
    pScalar (G.VCString sv) = pVal $ J.String $ G.unStringValue sv
    pScalar (G.VCEnum _)    = throwVE "unexpected enum for a scalar"
    pScalar v               = pVal $ toJValueC v

validateObject
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m
     )
  => InputValueParser a m
  -> InpObjTyInfo -> [(G.Name, a)] -> m AnnGObject
validateObject valParser tyInfo flds = do

  -- check duplicates
  unless (null dups) $
    throwVE $ "when parsing a value of type: " <> showNamedTy (_iotiName tyInfo)
    <> ", the following fields are duplicated: "
    <> showNames dups

  -- check fields with not null types
  forM_ (Map.toList $ _iotiFields tyInfo) $
    \(fldName, inpValInfo) -> do
      let ty = _iviType inpValInfo
          isNotNull = G.isNotNull ty
          fldPresent = fldName `elem` inpFldNames
      when (not fldPresent && isNotNull) $ throwVE $
        "field " <> G.unName fldName <> " of type " <> G.showGT ty
        <> " is required, but not found"

  fmap OMap.fromList $ forM flds $ \(fldName, fldVal) ->
    withPathK (G.unName fldName) $ do
      fldTy <- getInpFieldInfo tyInfo fldName
      convFldVal <- validateInputValue valParser fldTy fldVal
      return (fldName, convFldVal)

  where
    inpFldNames = map fst flds
    dups = duplicates inpFldNames

validateNamedTypeVal
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => InputValueParser a m
  -> G.NamedType -> a -> m AnnGValue
validateNamedTypeVal inpValParser nt val = do
  tyInfo <- getTyInfo nt
  case tyInfo of
    -- this should never happen
    TIObj _ ->
      throw500 $ "unexpected object type info for: "
      <> showNamedTy nt
    TIInpObj ioti ->
      withParsed (getObject inpValParser) val $
      fmap (AGObject nt) . mapM (validateObject inpValParser ioti)
    TIEnum eti ->
      withParsed (getEnum inpValParser) val $
      fmap (AGEnum nt) . mapM (validateEnum eti)
    TIScalar (ScalarTyInfo _ pgColTy _) ->
      withParsed (getScalar inpValParser) val $
      fmap (AGScalar pgColTy) . mapM (validateScalar pgColTy)
  where
    validateEnum enumTyInfo enumVal  =
      if Map.member enumVal (_etiValues enumTyInfo)
      then return enumVal
      else throwVE $ "unexpected value " <>
           showName (G.unEnumValue enumVal) <>
           " for enum: " <> showNamedTy nt
    validateScalar pgColTy =
      runAesonParser (parsePGValue pgColTy)

validateList
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> G.ListType
  -> a
  -> m AnnGValue
validateList inpValParser listTy val =
  withParsed (getList inpValParser) val $ \lM -> do
    let baseTy = G.unListType listTy
    AGArray listTy <$>
      mapM (indexedMapM (validateInputValue inpValParser baseTy)) lM

-- validateNonNull
--   :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
--   => InputValueParser a m
--   -> G.NonNullType
--   -> a
--   -> m AnnGValue
-- validateNonNull inpValParser nonNullTy val = do
--   parsedVal <- case nonNullTy of
--     G.NonNullTypeNamed nt -> validateNamedTypeVal inpValParser nt val
--     G.NonNullTypeList lt  -> validateList inpValParser lt val
--   when (hasNullVal parsedVal) $
--     throwVE $ "unexpected null value for type: " <> G.showGT (G.TypeNonNull nonNullTy)
--   return parsedVal

validateInputValue
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> G.GType
  -> a
  -> m AnnGValue
validateInputValue inpValParser ty val =
  case ty of
    G.TypeNamed _ nt -> validateNamedTypeVal inpValParser nt val
    G.TypeList _ lt  -> validateList inpValParser lt val
    --G.TypeNonNull nnt -> validateNonNull inpValParser nnt val

withParsed
  :: (Monad m)
  => (val -> m (P specificVal))
  -> val
  -> (Maybe specificVal -> m AnnGValue)
  -> m AnnGValue
withParsed valParser val fn = do
  parsedVal <- valParser val
  case unP parsedVal of
    Nothing            -> fn Nothing
    Just (Right a)     -> fn $ Just a
    Just (Left annVal) -> return annVal
