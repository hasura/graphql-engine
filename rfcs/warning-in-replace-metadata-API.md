# Warning in `replace_metadata` API

Warnings are an important part of REST APIs as they allow the API to communicate
additional information to the client that may be relevant to the request being
made. Warnings can also help the API consumer to identify and fix issues with
the API. For example, if a client receives a warning saying "the SQL triggers
cannot be cleaned up while removing the sources", the developer can investigate
the issue and delete the SQL triggers manually.

Currently, the metadata APIs don't throw warnings (except for the
inconsistencies when `allow_inconsistent_metadata` is set to `True` in the
`replace_metadata` API).

## Proposed API output

Add a root level field called `warnings` (a list of Objects). This field will
only exist if there are warnings, i.e. if there are no warnings, this field
would not exist in the response.

The output of a `replace_metadata` API call will look something like the
following for successful calls (with warnings):
```json
{
  "message": "success",
  "warnings": [
    {
      "path": "$.args.metadata.sources[0]",
      "message": "the SQL triggers cannot be cleaned up while removing the source \"default\""
    },
    {
      "path": "$.args.metadata.sources[0].table[1].event_triggers[0]",
      "message": "the event trigger with name \"system.send_email\" may not work as expected, hasura suggests to use only alphanumeric, underscore and hyphens in an event trigger name"
    }
  ]
}
```

In case of successfull API call (without warning), the API response remains
unchanged. i.e.
```json
{
  "message": "success"
}
```

| :memo: NOTE                |
|:---------------------------|
| We do not have a situation where a `replace_metadata` metadata API call could fail and introduce a side-effect that we cannot rollback. Therefore, implementing a warning for such a scenario is not necessary at this time. However, if this situation were to arise in the future, a warning may be useful. We will revisit the possibility of adding this enhancement at a later date. |


Note:

- In case of unsuccessful API calls, the API response will not contain any warnings.

- Throwing warning should never short-circuit the workflow.

## Strict-mode

We should also include a strict-mode (`fail_on_warning: true`). In strict-mode, the warnings in `replace_metadata` API call will be fatal and the will result in error. The metadata changes will not be applied in such cases. The arguments of the `replace_metadata` will look something like the following:
```json
{
  "fail_on_warning": ...,
  "allow_inconsistent_metadata": ...,
  "metadata": {
    ...
  }
}
```

## Possible use cases

1. If a source is inconsistent and it is removed via `replace_metadata`, then Postgres triggers (because of Event Triggers) are not cleaned. The response is success but ideally there should be a warning to clean the triggers manually.
2. Adding an inherited role with overlapping permissions successfully creates the inherited role but marks it inconsistent. The response of `replace_metadata` is success but there should be a warning because the inherited role is created but inconsistent.
3. (Cloud only). Adding a time limit > 60 will go through but it doesn't take affect because there is an infra limit of 60. The response of `replace_metadata` is success but should have been a warning.
4. Applying a metadata from a pro hasura having some pro-only content (like event trigger auto cleanup) to an OSS hasura goes through, but the features doesn't work. The response of `replace_metadata` is success but should have been a warning.
5. Possibly report the new non-fatal inconsistencies created [inconsistencies related to inherited permissions where the metadata API doesn't fail] as warnings.

## Open questions

- What can be the fields of warning?
- For unsuccessful API calls, should we show warnings and error separately?
  Or should we include warnings as error?

## Possible implemetation using `MonadWriter`

Writer monad allows us to accumulate some logs (or warnings in our case) while
performing actions applicatively.

### Writer monad with Except monad

We use `Except` monad for handling errors in API handlers. Before adding the
`MonadWriter` in our monad stack, we should consider it's implications and
behaviour with other monads in our monad stack (specially `MonadError` or
`ExceptT`).

<details>
<summary>example use of Writer monad along with Except monad</summary>

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO, MonadIO)
import Prelude
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Writer.Class

type Warnings = [String]

example ::
  (
    MonadIO m,
    MonadError String m,
    MonadWriter Warnings m
  ) =>
  Int -> m Int
example x = do
  tell ["warning: starting the operation"]
  when (x < 0) $ throwError "error: negative input"
  when (x < 10) $ tell ["warning: input is less than 10"]
  when (x > 20) $ tell ["warning: input is more than 20"]
  liftIO $ putStrLn "hello" -- performs some action for non error case
  return x

handleInput :: Int -> IO ()
handleInput input = do
  putStrLn $ "--------- input: "<> show input <>" ---------"
  (res, warnings) <- CPS.runWriterT $ runExceptT (example input)
  print warnings
  case res of
    Left err -> print err
    Right result -> print result
  putStrLn "-----------------------------"


main :: IO ()
main = do
  handleInput (-1)
  handleInput 5
  handleInput 15
  handleInput 25

-- >>> main
-- --------- input: -1 ---------
-- ["warning: starting the operation"]
-- "error: negative input"
-- -----------------------------
--
-- --------- input: 5 ---------
-- hello
-- ["warning: starting the operation","warning: input is less than 10"]
-- 5
-- -----------------------------
--
-- --------- input: 15 ---------
-- hello
-- ["warning: starting the operation"]
-- 15
-- -----------------------------
--
-- --------- input: 25 ---------
-- hello
-- ["warning: starting the operation","warning: input is more than 20"]
-- 25
-- -----------------------------
```
</details>


Writer monad behaves as expected in the example above. However we need to be
careful because they are known for memory leaks.

## Final implementation
There are a few changes in the implementation of warnings from this RFC:
- We are using `MonadWarnings` (based on `StateT`) for collecting warnings.
- `MonadWarnings` provides a `warn` method to add a warning.
- We have omitted the `path` field from the warning object. This can be added later if needed.
- We have included `type` and `name` fields in the warning object. This is to make it easier to identify the object
  which caused the warning.

An example use of `MonadWarnings` is shown below:
```haskell
import Hasura.RQL.DDL.Warnings

someMetadataAPIHandler :: args -> m EncJSON
someMetadataAPIHandler args = successMsgWithWarnings $ do
  -- do some stuff
  let warning = MetadataWarning (MOSource defaultSource) "some warning message"
  -- we use the `warn` method to add a warning
  warn $ warning
  -- do some more stuff
  pure ()
```
## Future enhancements
- Extend `warnings` to other metadata APIs. Please note that we may end up in
a situation where an API call fails and it made some change which was not
rolled back and thus we would like to throw that as a warning or error.
