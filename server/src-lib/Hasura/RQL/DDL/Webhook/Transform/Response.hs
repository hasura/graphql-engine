-- | Types and subroutines related to constructing transformations on
-- HTTP responses.
module Hasura.RQL.DDL.Webhook.Transform.Response
  ( -- ** Request Transformation Context
    ResponseTransformCtx (..),
    runResponseTemplateTransform,

    -- * Unescaped
    runUnescapedResponseTemplateTransform,
    runUnescapedResponseTemplateTransform',
  )
where

-------------------------------------------------------------------------------

import Control.Arrow (left)
import Data.Aeson qualified as Aeson
import Data.Aeson.Kriti.Functions as KFunc
import Data.ByteString (ByteString)
import Data.Validation (Validation, fromEither)
import Hasura.Prelude
import Hasura.RQL.DDL.Webhook.Transform.Class
  ( Template (..),
    TemplatingEngine (Kriti),
    TransformErrorBundle (..),
    UnescapedTemplate,
    encodeScalar,
    wrapUnescapedTemplate,
  )
import Hasura.Session (SessionVariables)

-------------------------------------------------------------------------------

-- | Common context that is made available to all response transformations.
data ResponseTransformCtx = ResponseTransformCtx
  { responseTransformBody :: Aeson.Value,
    -- NOTE: This is a @Nothing@ if you have a Response Transform but no Request Transform:
    responseTransformReqCtx :: Aeson.Value,
    responseSessionVariables :: Maybe SessionVariables,
    responseTransformEngine :: TemplatingEngine
  }

-------------------------------------------------------------------------------

-- | A helper function for executing transformations from a 'Template'
-- and a 'ResponseTransformCtx'.
--
-- NOTE: This and all related funtions are hard-coded to Kriti at the
-- moment. When we add additional template engines this function will
-- need to take a 'TemplatingEngine' parameter.
runResponseTemplateTransform ::
  Template ->
  ResponseTransformCtx ->
  Either TransformErrorBundle Aeson.Value
runResponseTemplateTransform template ResponseTransformCtx {responseTransformEngine = Kriti, ..} =
  let context = [("$body", responseTransformBody), ("$request", Aeson.toJSON responseTransformReqCtx)]
      customFunctions = KFunc.sessionFunctions responseSessionVariables
      eResult = KFunc.runKritiWith (unTemplate $ template) context customFunctions
   in eResult & left \kritiErr ->
        let renderedErr = Aeson.toJSON kritiErr
         in TransformErrorBundle [renderedErr]

-- | Run an 'UnescapedTemplate' with a 'ResponseTransformCtx'.
runUnescapedResponseTemplateTransform ::
  ResponseTransformCtx ->
  UnescapedTemplate ->
  Either TransformErrorBundle ByteString
runUnescapedResponseTemplateTransform context unescapedTemplate = do
  result <- runResponseTemplateTransform (wrapUnescapedTemplate unescapedTemplate) context
  encodeScalar result

-- | Run an 'UnescapedTemplate' with a 'ResponseTransformCtx' in 'Validation'.
runUnescapedResponseTemplateTransform' ::
  ResponseTransformCtx ->
  UnescapedTemplate ->
  Validation TransformErrorBundle ByteString
runUnescapedResponseTemplateTransform' context unescapedTemplate =
  fromEither $
    runUnescapedResponseTemplateTransform context unescapedTemplate
