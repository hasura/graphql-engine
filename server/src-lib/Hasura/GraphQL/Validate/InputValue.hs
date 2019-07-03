module Hasura.GraphQL.Validate.InputValue
  ( validateInputValue
  , jsonParser
  , valueParser
  , constValueParser
  , pPrintValueC
  ) where

import           Data.Scientific                 (fromFloatDigits)
import           Hasura.Prelude
import           Hasura.Server.Utils             (duplicates)

import           Data.Has

import qualified Data.Aeson                      as J
import qualified Data.HashMap.Strict             as Map
import qualified Data.HashMap.Strict.InsOrd      as OMap
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Language.GraphQL.Draft.Syntax   as G

import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Value

newtype P a = P { unP :: Maybe (Either (G.Variable, AnnVarVal) a)}

pNull :: (Monad m) => m (P a)
pNull = return $ P Nothing

pVal :: (Monad m) => a -> m (P a)
pVal = return . P . Just . Right

resolveVar
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m
     )
  => G.Variable -> m AnnVarVal
resolveVar var = do
  varVals <- _vcVarVals <$> ask
  onNothing (Map.lookup var varVals) $
    throwVE $ "no such variable defined in the operation: "
    <> showName (G.unVariable var)

pVar
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m)
  => G.Variable -> m (P a)
pVar var = do
  annInpVal <- resolveVar var
  return . P . Just $ Left (var, annInpVal)

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
     , MonadReader ValidationCtx m
     )
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
    pObject G.VNull              = pNull
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

pPrintValueC :: G.ValueConst -> Text
pPrintValueC = \case
  G.VCInt i                        -> T.pack $ show i
  G.VCFloat f                      -> T.pack $ show f
  G.VCString (G.StringValue t)     -> T.pack $ show t
  G.VCBoolean b                    -> bool "false" "true"  b
  G.VCNull                         -> "null"
  G.VCEnum (G.EnumValue n)         -> G.unName n
  G.VCList (G.ListValueG vals)     -> withSquareBraces $ T.intercalate ", " $
                                      map pPrintValueC vals
  G.VCObject (G.ObjectValueG objs) -> withCurlyBraces $ T.intercalate ", " $
                                      map ppObjFld objs
  where
    ppObjFld (G.ObjectFieldG f v) = G.unName f <> ": " <> pPrintValueC v
    withSquareBraces t = "[" <> t <> "]"
    withCurlyBraces t = "{" <> t <> "}"


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
    J.toJSON . OMap.fromList $ map toTup objs
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
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> InpObjTyInfo -> [(G.Name, a)] -> m AnnGObject
validateObject valParser tyInfo flds = do

  -- check duplicates
  unless (null dups) $
    throwVE $ "when parsing a value of type: " <> showNamedTy (_iotiName tyInfo)
    <> ", the following fields are duplicated: "
    <> showNames dups

  -- make default values object
  defValObj <- fmap (OMap.fromList . catMaybes) $
    forM (Map.toList $ _iotiFields tyInfo) $
    \(fldName, inpValInfo) -> do
      let ty = _iviType inpValInfo
          tyAnn = _iviPGTyAnn inpValInfo
          isNotNull = G.isNotNull ty
          defValM = _iviDefVal inpValInfo
          hasDefVal = isJust defValM
          fldPresent = fldName `elem` inpFldNames

      when (not fldPresent && isNotNull && not hasDefVal) $
        throwVE $ "field " <> G.unName fldName <> " of type "
                  <> G.showGT ty <> " is required, but not found"

      convDefValM <-
        validateInputValue constValueParser ty tyAnn `mapM` defValM
      return $ (fldName,) <$> convDefValM

  -- compute input values object
  inpValObj <- fmap OMap.fromList $ forM flds $ \(fldName, fldVal) ->
    withPathK (G.unName fldName) $ do
      fldInfo <- getInpFieldInfo tyInfo fldName
      convFldVal <- validateInputValue valParser (_iviType fldInfo)
                    (_iviPGTyAnn fldInfo) fldVal
      return (fldName, convFldVal)

  return $ inpValObj `OMap.union` defValObj

  where
    inpFldNames = map fst flds
    dups = duplicates inpFldNames

validateNamedTypeVal
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m
     )
  => InputValueParser a m
  -> (G.Nullability, G.NamedType) -> Maybe PGColTyAnn -> a -> m AnnInpVal
validateNamedTypeVal inpValParser (nullability, nt) tyAnn val = do
  tyInfo <- getTyInfo nt
  case tyInfo of
    -- this should never happen
    TIObj _ ->
      throwUnexpTypeErr "object"
    -- this should never happen
    TIIFace _ ->
      throwUnexpTypeErr "interface"
    -- this should never happen
    TIUnion _ ->
      throwUnexpTypeErr "union"
    TIInpObj ioti ->
      withParsed gType tyAnn (getObject inpValParser) val $
      fmap (AGObject nt) . mapM (validateObject inpValParser ioti)
    TIEnum eti ->
      withParsed gType tyAnn (getEnum inpValParser) val $
      fmap (AGEnum nt) . mapM (validateEnum eti)
    TIScalar (ScalarTyInfo _ t colTyM _) -> do
      colTy <- onNothing colTyM $ throwUnexpNoPGTyErr t
      withParsed gType tyAnn (getScalar inpValParser) val $
        fmap (AGPGVal colTy) . mapM (validateScalar colTy)
  where
    throwUnexpTypeErr ty = throw500 $ "unexpected " <> ty <> " type info for: "
      <> showNamedTy nt
    throwUnexpNoPGTyErr ty = throw500 $
      "PGColType not found for scalar type " <> G.unName ty
    validateEnum enumTyInfo enumVal  =
      if Map.member enumVal (_etiValues enumTyInfo)
      then return enumVal
      else throwVE $ "unexpected value " <>
           showName (G.unEnumValue enumVal) <>
           " for enum: " <> showNamedTy nt
    validateScalar pgColTy =
      runAesonParser (parsePGValue pgColTy)
    gType = G.TypeNamed nullability nt

validateList
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> (G.Nullability, G.ListType)
  -> Maybe PGColTyAnn
  -> a
  -> m AnnInpVal
validateList inpValParser (nullability, listTy) pgTyAnn val =
  withParsed ty pgTyAnn (getList inpValParser) val $ \lM -> do
    let bTy = G.unListType listTy
    AGArray listTy <$>
      mapM (indexedMapM (validateInputValue inpValParser bTy pgTyAnn)) lM
  where
    ty = G.TypeList nullability listTy

validateInputValue
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> G.GType
  -> Maybe PGColTyAnn
  -> a
  -> m AnnInpVal
validateInputValue inpValParser ty Nothing val =
  case ty of
    G.TypeNamed nullability nt ->
      validateNamedTypeVal inpValParser (nullability, nt) Nothing val
    G.TypeList nullability lt  ->
      validateList inpValParser (nullability, lt) Nothing val
validateInputValue inpValParser ty (Just pgTyAnn) val =
  case (pgTyAnn,ty) of
    (PTCol colTy,_) ->
      withParsed ty (Just pgTyAnn) (getScalar inpValParser) val $
      fmap (AGPGVal colTy) . mapM (validatePGVal colTy)
    (PTArr pgElemTyAnn,G.TypeList nullability lt) ->
      validateList inpValParser (nullability, lt) (Just pgElemTyAnn) val
    _ ->
      throw500 $ "Invalid Postgres column type annotation " <> T.pack (show pgTyAnn)
      <> " for GraphQL type " <> T.pack (show ty)
  where validatePGVal pct = runAesonParser (parsePGValue pct)

withParsed
  :: (MonadError QErr m, Has TypeMap r, MonadReader r m)
  => G.GType
  -> Maybe PGColTyAnn
  -> (val -> m (P specificVal))
  -> val
  -> (Maybe specificVal -> m AnnGValue)
  -> m AnnInpVal
withParsed expectedTy tyAnn valParser val fn = do
  parsedVal <- valParser val
  case unP parsedVal of
    Nothing              ->
      if G.isNullable expectedTy
      then AnnInpVal expectedTy Nothing <$> fn Nothing
      else throwVE $ "null value found for non-nullable type: "
           <> G.showGT expectedTy
    Just (Right v)       -> AnnInpVal expectedTy Nothing <$> fn (Just v)
    Just (Left (var, varVal)) -> do
      let varTxt = G.unName $ G.unVariable var
          AnnVarVal ty defM inpValM = varVal
      withPathKeys ["$","variableValues", G.unName $ G.unVariable var] $ do
        unless (isTypeAllowed expectedTy ty) $
          throwVE $ "variable " <> varTxt
          <> " of type " <> G.showGT ty
          <> " is used in position expecting " <> G.showGT expectedTy
        annValM <- do
          annInpValM <- mapM (validateInputValue jsonParser ty tyAnn) inpValM
          case annInpValM of
            Nothing -> withPathK "defaultValue" $
                       mapM (validateInputValue constValueParser ty tyAnn) defM
            Just a  -> return $ Just a
        v <- onNothing annValM $ throwVE $
             "expecting a value for non-null type: " <> G.showGT ty
             <> " in variableValues"
        return $ v { _aivVariable = Just var }
  where
    -- is the type 'ofType' allowed at a position of type 'atType'
    -- Examples:
    -- . a! is allowed at a
    -- . [a!]! is allowed at [a]
    -- . but 'a' is not allowed at 'a!'
    isTypeAllowed ofType atType =
      case (ofType, atType) of
        (G.TypeNamed ofTyN ofNt, G.TypeNamed atTyN atNt) ->
          checkNullability ofTyN atTyN && (ofNt == atNt)
        (G.TypeList ofTyN ofLt, G.TypeList atTyN atLt)  ->
          checkNullability ofTyN atTyN &&
          isTypeAllowed (G.unListType ofLt) (G.unListType atLt)
        _ -> False

    -- only when 'atType' is non nullable and 'ofType' is nullable,
    -- this check fails
    checkNullability (G.Nullability ofNullable) (G.Nullability atNullable) =
      case (ofNullable, atNullable) of
        (True, _)      -> True
        (False, False) -> True
        (False, True)  -> False
