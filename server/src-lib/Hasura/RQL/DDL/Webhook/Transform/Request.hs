{-# LANGUAGE DeriveAnyClass #-}

-- | Types and subroutines related to constructing transformations on
-- HTTP requests.
module Hasura.RQL.DDL.Webhook.Transform.Request
  ( -- ** Request Transformation Context
    RequestTransformCtx (..),
    mkReqTransformCtx,

    -- * Templating
    TemplatingEngine (..),
    Template (..),
    Version (..),
    runRequestTemplateTransform,
    validateRequestTemplateTransform,
    validateRequestTemplateTransform',

    -- * Unescaped
    runUnescapedRequestTemplateTransform,
    runUnescapedRequestTemplateTransform',
    validateRequestUnescapedTemplateTransform,
    validateRequestUnescapedTemplateTransform',
  )
where

-------------------------------------------------------------------------------

import Control.Arrow (left)
import Control.Lens qualified as Lens
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Kriti.Functions qualified as KFunc
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation, fromEither)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class (Template (..), TemplatingEngine (..), TransformErrorBundle (..), UnescapedTemplate, encodeScalar, wrapUnescapedTemplate)
import Hasura.Session (SessionVariables)
import Kriti.Error qualified as Kriti
import Kriti.Parser qualified as Kriti
import Network.HTTP.Client.Transformable qualified as HTTP

-------------------------------------------------------------------------------

-- | Common context that is made available to all request transformations.
data RequestTransformCtx = RequestTransformCtx
  { rtcBaseUrl :: Maybe Aeson.Value,
    rtcBody :: Aeson.Value,
    rtcSessionVariables :: Maybe SessionVariables,
    rtcQueryParams :: Maybe Aeson.Value,
    rtcEngine :: TemplatingEngine
  }

instance ToJSON RequestTransformCtx where
  toJSON RequestTransformCtx {..} =
    let required =
          [ "body" .= rtcBody,
            "session_variables" .= rtcSessionVariables
          ]
        optional =
          [ ("base_url" .=) <$> rtcBaseUrl,
            ("query_params" .=) <$> rtcQueryParams
          ]
     in Aeson.object (required <> catMaybes optional)

-- | A smart constructor for constructing the 'RequestTransformCtx'
--
-- XXX: This function makes internal usage of 'TE.decodeUtf8', which throws an
-- impure exception when the supplied 'ByteString' cannot be decoded into valid
-- UTF8 text!
mkReqTransformCtx ::
  Text ->
  Maybe SessionVariables ->
  TemplatingEngine ->
  HTTP.Request ->
  RequestTransformCtx
mkReqTransformCtx url sessionVars rtcEngine reqData =
  let rtcBaseUrl = Just $ Aeson.toJSON url
      rtcBody =
        let mBody = Lens.view HTTP.body reqData >>= Aeson.decode @Aeson.Value
         in fromMaybe Aeson.Null mBody
      rtcSessionVariables = sessionVars
      rtcQueryParams =
        let queryParams =
              Lens.view HTTP.queryParams reqData & fmap \(key, val) ->
                (TE.decodeUtf8 key, fmap TE.decodeUtf8 val)
         in Just $ Aeson.toJSON queryParams
   in RequestTransformCtx {..}

-------------------------------------------------------------------------------

-- | A helper function for executing transformations from a 'Template'
-- and a 'RequestTransformCtx'.
--
-- NOTE: This and all related funtions are hard-coded to Kriti at the
-- moment. When we add additional template engines this function will
-- need to take a 'TemplatingEngine' parameter.
runRequestTemplateTransform ::
  Template ->
  RequestTransformCtx ->
  Either TransformErrorBundle Aeson.Value
runRequestTemplateTransform template RequestTransformCtx {rtcEngine = Kriti, ..} =
  let context =
        [ ("$body", rtcBody),
          ("$session_variables", Aeson.toJSON rtcSessionVariables)
        ]
          <> catMaybes
            [ ("$query_params",) <$> rtcQueryParams,
              ("$base_url",) <$> rtcBaseUrl
            ]
      kritiFuncs = KFunc.sessionFunctions rtcSessionVariables
      eResult = KFunc.runKritiWith (unTemplate $ template) context kritiFuncs
   in eResult & left \kritiErr ->
        let renderedErr = Aeson.toJSON kritiErr
         in TransformErrorBundle [renderedErr]

-- TODO: Should this live in 'Hasura.RQL.DDL.Webhook.Transform.Validation'?
validateRequestTemplateTransform ::
  TemplatingEngine ->
  Template ->
  Either TransformErrorBundle ()
validateRequestTemplateTransform Kriti (Template template) =
  bimap packBundle (const ()) $ Kriti.parser $ TE.encodeUtf8 template
  where
    packBundle = TransformErrorBundle . pure . Aeson.toJSON . Kriti.serialize

validateRequestTemplateTransform' ::
  TemplatingEngine ->
  Template ->
  Validation TransformErrorBundle ()
validateRequestTemplateTransform' engine =
  fromEither . validateRequestTemplateTransform engine

-------------------------------------------------------------------------------

-- | 'RequestTransform' Versioning
data Version
  = V1
  | V2
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance FromJSON Version where
  parseJSON v = do
    version :: Int <- Aeson.parseJSON v
    case version of
      1 -> pure V1
      2 -> pure V2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

instance ToJSON Version where
  toJSON = \case
    V1 -> Aeson.toJSON @Int 1
    V2 -> Aeson.toJSON @Int 2

-------------------------------------------------------------------------------

-- | A helper function for executing Kriti transformations from a
-- 'UnescapedTemplate' and a 'RequestTrasformCtx'.
--
-- The difference from 'runRequestTemplateTransform' is that this
-- function will wrap the template text in double quotes before
-- running Kriti.
runUnescapedRequestTemplateTransform ::
  RequestTransformCtx ->
  UnescapedTemplate ->
  Either TransformErrorBundle ByteString
runUnescapedRequestTemplateTransform context unescapedTemplate = do
  result <-
    runRequestTemplateTransform
      (wrapUnescapedTemplate unescapedTemplate)
      context
  encodeScalar result

-- | Run a Kriti transformation with an unescaped template in
-- 'Validation' instead of 'Either'.
runUnescapedRequestTemplateTransform' ::
  RequestTransformCtx ->
  UnescapedTemplate ->
  Validation TransformErrorBundle ByteString
runUnescapedRequestTemplateTransform' context unescapedTemplate =
  fromEither $
    runUnescapedRequestTemplateTransform context unescapedTemplate

-- TODO: Should this live in 'Hasura.RQL.DDL.Webhook.Transform.Validation'?
validateRequestUnescapedTemplateTransform ::
  TemplatingEngine ->
  UnescapedTemplate ->
  Either TransformErrorBundle ()
validateRequestUnescapedTemplateTransform engine =
  validateRequestTemplateTransform engine . wrapUnescapedTemplate

validateRequestUnescapedTemplateTransform' ::
  TemplatingEngine ->
  UnescapedTemplate ->
  Validation TransformErrorBundle ()
validateRequestUnescapedTemplateTransform' engine =
  fromEither . validateRequestUnescapedTemplateTransform engine
