{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.GraphQL.Validate.InputValue
  ( validateInputValue
  , jsonParser
  , valueParser
  , constValueParser
  ) where

import           Data.Scientific                 (fromFloatDigits)
import           Hasura.Prelude

import           Data.Has

import qualified Data.Aeson                      as J
import qualified Data.HashMap.Strict             as Map
import qualified Data.Vector                     as V
import qualified Language.GraphQL.Draft.Syntax   as G

import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Context
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Value

newtype P a = P { unP :: Maybe (Either (G.Variable, AnnInpVal) a)}

pNull :: (Monad m) => m (P a)
pNull = return $ P Nothing

pVal :: (Monad m) => a -> m (P a)
pVal = return . P . Just . Right

resolveVar
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m)
  => G.Variable -> m AnnInpVal
resolveVar var = do
  varVals <- _vcVarVals <$> ask
  -- TODO typecheck
  onNothing (Map.lookup var varVals) $
    throwVE $ "no such variable defined in the operation: "
    <> showName (G.unVariable var)
  where
    -- typeCheck expectedTy actualTy = case (expectedTy, actualTy) of
    --   -- named types
    --   (G.TypeNamed eTy, G.TypeNamed aTy) -> eTy == aTy
    --   -- non null type can be expected for a null type
    --   (G.TypeNamed eTy, G.TypeNonNull (G.NonNullTypeNamed aTy)) -> eTy == aTy

    --   -- list types
    --   (G.TypeList eTy, G.TypeList aTy) ->
    --     typeCheck (G.unListType eTy) (G.unListType aTy)
    --   (G.TypeList eTy, G.TypeNonNull (G.NonNullTypeList aTy)) ->
    --     typeCheck (G.unListType eTy) (G.unListType aTy)

    --   -- non null types
    --   (G.TypeNonNull (G.NonNullTypeList eTy), G.TypeNonNull (G.NonNullTypeList aTy)) ->
    --     typeCheck (G.unListType eTy) (G.unListType aTy)
    --   (G.TypeNonNull (G.NonNullTypeNamed eTy), G.TypeNonNull (G.NonNullTypeNamed aTy)) ->
    --     eTy == aTy
    --   (_, _) -> False

pVar
  :: ( MonadError QErr m
     , MonadReader ValidationCtx m)
  => G.Variable -> m (P a)
pVar var = do
  annInpVal <- resolveVar var
  return . P . Just . Left $ (var, annInpVal)

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
    pScalar (G.VObject _)     = throwVE "unexpected object for a scalar"
    pScalar (G.VList _)       = throwVE "unexpected array for a scalar"

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
    pScalar (G.VCObject _)  = throwVE "unexpected object for a scalar"
    pScalar (G.VCList _)    = throwVE "unexpected array for a scalar"

validateObject
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m
     )
  => InputValueParser a m
  -> InpObjTyInfo -> [(G.Name, a)] -> m AnnGObject
validateObject valParser tyInfo flds = do

  fldMap <- fmap (Map.map snd) $ onLeft (mkMapWith fst flds) $ \dups ->
    throwVE $ "when parsing a value of type: " <> showNamedTy (_iotiName tyInfo)
    <> ", the following fields are duplicated: "
    <> showNames dups

  annFldsM <- forM (Map.toList $ _iotiFields tyInfo) $
    \(fldName, inpValInfo) -> do
      let fldValM = Map.lookup fldName fldMap
          ty = _iviType inpValInfo
          isNotNull = not $ G.isNullable ty
      when (isNothing fldValM && isNotNull) $ throwVE $
        "field " <> G.unName fldName <> " of type " <> G.showGT ty
        <> " is required, but not found"
      forM fldValM $ \fldVal ->
        withPathK (G.unName fldName) $ do
          fldTy <- getInpFieldInfo tyInfo fldName
          convFldVal <- validateInputValue valParser fldTy fldVal
          return (fldName, convFldVal)

  return $ Map.fromList $ catMaybes annFldsM

validateNamedTypeVal
  :: ( MonadReader r m, Has TypeMap r
     , MonadError QErr m)
  => InputValueParser a m
  -> (G.Nullability, G.NamedType) -> a -> m AnnInpVal
validateNamedTypeVal inpValParser (nullability, nt) val = do
  tyInfo <- getTyInfo nt
  case tyInfo of
    -- this should never happen
    TIObj _ ->
      throw500 $ "unexpected object type info for: "
      <> showNamedTy nt
    TIInpObj ioti ->
      withParsed gType (getObject inpValParser) val $
      fmap (AGObject nt) . mapM (validateObject inpValParser ioti)
    TIEnum eti ->
      withParsed gType (getEnum inpValParser) val $
      fmap (AGEnum nt) . mapM (validateEnum eti)
    TIScalar (ScalarTyInfo _ pgColTy) ->
      withParsed gType (getScalar inpValParser) val $
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

    gType = G.TypeNamed nullability nt

validateList
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> (G.Nullability, G.ListType)
  -> a
  -> m AnnInpVal
validateList inpValParser (nullability, listTy) val =
  withParsed ty (getList inpValParser) val $ \lM -> do
    let baseTy = G.unListType listTy
    AGArray listTy <$>
      mapM (indexedMapM (validateInputValue inpValParser baseTy)) lM
  where
    ty = G.TypeList nullability listTy

validateInputValue
  :: (MonadError QErr m, MonadReader r m, Has TypeMap r)
  => InputValueParser a m
  -> G.GType
  -> a
  -> m AnnInpVal
validateInputValue inpValParser ty val =
  case ty of
    G.TypeNamed nullability nt ->
      validateNamedTypeVal inpValParser (nullability, nt) val
    G.TypeList nullability lt  ->
      validateList inpValParser (nullability, lt) val

withParsed
  :: (Monad m)
  => G.GType
  -> (val -> m (P specificVal))
  -> val
  -> (Maybe specificVal -> m AnnGValue)
  -> m AnnInpVal
withParsed ty valParser val fn = do
  parsedVal <- valParser val
  case unP parsedVal of
    Nothing              -> AnnInpVal ty Nothing <$> fn Nothing
    Just (Right v)       -> AnnInpVal ty Nothing <$> fn (Just v)
    Just (Left (var, v)) -> return $ v { _aivVariable = Just var }
