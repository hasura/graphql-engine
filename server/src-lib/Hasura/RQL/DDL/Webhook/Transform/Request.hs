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
import Data.Aeson qualified as J
import Data.Aeson.Kriti.Functions qualified as KFunc
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation, fromEither)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class (Template (..), TemplatingEngine (..), TransformErrorBundle (..), UnescapedTemplate, encodeScalar, wrapUnescapedTemplate)
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx (..), Version (..))
import Hasura.Session (SessionVariables)
import Kriti.Error qualified as Kriti
import Kriti.Parser qualified as Kriti
import Network.HTTP.Client.Transformable qualified as HTTP

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
  let rtcBaseUrl = Just $ J.toJSON url
      rtcBody =
        let mBody = Lens.preview (HTTP.body . HTTP._RequestBodyLBS) reqData >>= J.decode @J.Value
         in fromMaybe J.Null mBody
      rtcSessionVariables = sessionVars
      rtcQueryParams =
        let queryParams =
              Lens.view HTTP.queryParams reqData & fmap \(key, val) ->
                (TE.decodeUtf8 key, fmap TE.decodeUtf8 val)
         in Just $ J.toJSON queryParams
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
  Either TransformErrorBundle J.Value
runRequestTemplateTransform template RequestTransformCtx {rtcEngine = Kriti, ..} =
  let context =
        [ ("$body", rtcBody),
          ("$session_variables", J.toJSON rtcSessionVariables)
        ]
          <> catMaybes
            [ ("$query_params",) <$> rtcQueryParams,
              ("$base_url",) <$> rtcBaseUrl
            ]
      kritiFuncs = KFunc.sessionFunctions rtcSessionVariables
      eResult = KFunc.runKritiWith (unTemplate $ template) context kritiFuncs
   in eResult & left \kritiErr ->
        let renderedErr = J.toJSON kritiErr
         in TransformErrorBundle [renderedErr]

-- TODO: Should this live in 'Hasura.RQL.DDL.Webhook.Transform.Validation'?
validateRequestTemplateTransform ::
  TemplatingEngine ->
  Template ->
  Either TransformErrorBundle ()
validateRequestTemplateTransform Kriti (Template template) =
  bimap packBundle (const ()) $ Kriti.parser $ TE.encodeUtf8 template
  where
    packBundle = TransformErrorBundle . pure . J.toJSON . Kriti.serialize

validateRequestTemplateTransform' ::
  TemplatingEngine ->
  Template ->
  Validation TransformErrorBundle ()
validateRequestTemplateTransform' engine =
  fromEither . validateRequestTemplateTransform engine

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
  fromEither
    $ runUnescapedRequestTemplateTransform context unescapedTemplate

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
