-- | Add metadata tags as comments to SQL queries.
module Data.SqlCommenter
  ( sqlCommenterGoogle,
    sqlCommenterStandard,
    Attribute,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Text.Extended (commaSeparated)
import Hasura.Prelude
import Hasura.QueryTags
import Network.URI.Encode qualified as URI

-- | query-tags format as defined in the Spec <https://google.github.io/sqlcommenter/spec/#sql-commenter>
sqlCommenterGoogle :: QueryTagsAttributes -> QueryTagsComment
sqlCommenterGoogle qtAttributes
  | null attributes = emptyQueryTagsComment
  | otherwise = QueryTagsComment $ createSQLComment $ generateCommentTags (NE.fromList attributes)
  where
    createSQLComment comment = " /* " <> comment <> " */"
    attributes = _unQueryTagsAttributes qtAttributes

-- | Default 'Query Tags' format in the Hasura GraphQL Engine
-- Creates simple 'key=value' pairs of query tags. No sorting, No URL encoding.
-- If the format of query tags is not mentioned in the metadata then, query-tags
-- are formatted using hte below format
sqlCommenterStandard :: QueryTagsAttributes -> QueryTagsComment
sqlCommenterStandard qtAttributes
  | null attributes = emptyQueryTagsComment
  | otherwise = QueryTagsComment $ createSQLComment $ generateComment (NE.fromList attributes)
  where
    generateComment attr = commaSeparated [k <> "=" <> v | (k, v) <- NE.toList attr]
    createSQLComment comment = " /* " <> comment <> " */"
    attributes = _unQueryTagsAttributes qtAttributes

-- | Top-level algorithm to generate the string comment from list of
-- 'Attribute's
-- Spec <https://google.github.io/sqlcommenter/spec/#sql-commenter>
-- See Note [Ambiguous SQLCommenterGoogle Specification]
generateCommentTags :: NE.NonEmpty Attribute -> Text
generateCommentTags attributes =
  let -- 1. URL encode the key,value pairs. What the spec calls serialization.
      -- https://google.github.io/sqlcommenter/spec/#key-serialization-algorithm
      -- https://google.github.io/sqlcommenter/spec/#value-serialization-algorithm
      encoded = NE.map urlEncodePair attributes
      -- 2. Sort the pairs. Spec: https://google.github.io/sqlcommenter/spec/#sorting
      -- Note the 'sort' from 'Data.List' works by sorting the first element in
      -- the pair. And sorting on 'Text' is lexicographic.
      sorted = NE.sort encoded
      -- 3. Finally, serialize the pairs into a CSV. What the spec calls concatenation
      -- https://google.github.io/sqlcommenter/spec/#concatenation
      serialized = serializePairs sorted
   in serialized
  where
    urlEncodePair (k, v) = (URI.encodeText k, URI.encodeText v)
    serializePairs pairs = commaSeparated [k <> "='" <> v <> "'" | (k, v) <- NE.toList pairs]

-- | NOTE: [Ambiguous SQLCommenterGoogle Specification]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The specification is ambiguos/unclear about the following two steps:
--
-- 1. [Comment Escaping](https://google.github.io/sqlcommenter/spec/#comment-escaping)
-- ~~~~~~~~~~~~~~~~~~~~
--  The Spec states that:
--
--    > If a comment already exists within a SQL statement, we MUST NOT mutate that statement.
--
--  Oddly, the implementation of above rule/statement is not uniform among it's various language
--  libraries. That along with the fact that the above statement is clear for the scenarios when
--  we have properly formed comment, but does not state anything about what to do when the comments
--  are malformed is why it was decided to skip this step.
--
--  I have created a issue regarding this at https://github.com/google/sqlcommenter/issues/57
--
--  When discussed the above problem with Tiru, it was decided that since in our case we do not have
--  any other sql comments in the prepared statement, this step won't affect us thus it's okay to
--  skip this.
--
-- 2. [Escaping Meta Characters](https://google.github.io/sqlcommenter/spec/#meta-characters)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--  The Spec states that:
--
--    > Meta characters such as `'` should be escaped with a slash `\`.
--
--  And the algorithm in brief is:
--    1. URL encode the Key/Value
--    2. Escape the meta-characters withing the raw value; a single quote `'` becomes `\'`
--
--  From the above statement, it could be understood that the meta-characters in context of
--  sqlcommenter is `'`. And 'Network.URI.Encode.encodeText' is capable of encoding `'`
--  without any need of escaping.
--
--  ```
--  Prelude Network.URI.Encode> encodeText"'"
--  "%27"
--
--  Prelude Network.URI.Encode> encodeText "\'"
--  "%27"
--  ```
--
--  During the investigation, we also found that the reference implementation of the sqlcommenter
--  in other languages did not even do this step:
--    1. https://github.com/google/sqlcommenter/blob/master/python/sqlcommenter-python/google/cloud/sqlcommenter/__init__.py#L29
--    2. https://github.com/google/sqlcommenter/blob/master/java/sqlcommenter-java/src/main/java/com/google/cloud/sqlcommenter/threadlocalstorage/State.java#L179
--
--  The above can be summarized as:
--    1. The meta-characters in the context of sqlcommenter is `'`
--    2. Reference implementation of sqlcommenter does not include the step
--    3. 'Network.URI.Encode.encodeText' can encode `'` without any need of escaping
--
--  Thus, on the basis of the above three facts we decided to ignore the 'Escape Meta Characters' step.
