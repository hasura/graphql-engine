{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Database.Redis.Cluster.Command where

import Data.Char(toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.HashMap.Strict as HM
import Database.Redis.Types(RedisResult(decode))
import Database.Redis.Protocol(Reply(..))

data Flag
    = Write
    | ReadOnly
    | DenyOOM
    | Admin
    | PubSub
    | NoScript
    | Random
    | SortForScript
    | Loading
    | Stale
    | SkipMonitor
    | Asking
    | Fast
    | MovableKeys
    | Other BS.ByteString deriving (Show, Eq)


data AritySpec = Required Integer | MinimumRequired Integer deriving (Show)

data LastKeyPositionSpec = LastKeyPosition Integer | UnlimitedKeys Integer deriving (Show)

newtype InfoMap = InfoMap (HM.HashMap String CommandInfo)

-- Represents the result of the COMMAND command, which returns information
-- about the position of keys in a request
data CommandInfo = CommandInfo
    { name :: BS.ByteString
    , arity :: AritySpec
    , flags :: [Flag]
    , firstKeyPosition :: Integer
    , lastKeyPosition :: LastKeyPositionSpec
    , stepCount :: Integer
    } deriving (Show)

instance RedisResult CommandInfo where
    decode (MultiBulk (Just
        [ Bulk (Just commandName)
        , Integer aritySpec
        , MultiBulk (Just replyFlags)
        , Integer firstKeyPos
        , Integer lastKeyPos
        , Integer replyStepCount])) = do
            parsedFlags <- mapM parseFlag replyFlags
            lastKey <- parseLastKeyPos
            return $ CommandInfo
                { name = commandName
                , arity = parseArity aritySpec
                , flags = parsedFlags
                , firstKeyPosition = firstKeyPos
                , lastKeyPosition = lastKey
                , stepCount = replyStepCount
                } where
        parseArity int = case int of
            i | i >= 0 -> Required i
            i -> MinimumRequired $ abs i
        parseFlag :: Reply -> Either Reply Flag
        parseFlag (SingleLine flag) = return $ case flag of
            "write" -> Write
            "readonly" -> ReadOnly
            "denyoom" -> DenyOOM
            "admin" -> Admin
            "pubsub" -> PubSub
            "noscript" -> NoScript
            "random" -> Random
            "sort_for_script" -> SortForScript
            "loading" -> Loading
            "stale" -> Stale
            "skip_monitor" -> SkipMonitor
            "asking" -> Asking
            "fast" -> Fast
            "movablekeys" -> MovableKeys
            other -> Other other
        parseFlag bad = Left bad
        parseLastKeyPos :: Either Reply LastKeyPositionSpec
        parseLastKeyPos = return $ case lastKeyPos of
            i | i < 0 -> UnlimitedKeys (-i - 1)
            i -> LastKeyPosition i
    -- since redis 6.0
    decode (MultiBulk (Just
        [ name@(Bulk (Just _))
        , arity@(Integer _)
        , flags@(MultiBulk (Just _))
        , firstPos@(Integer _)
        , lastPos@(Integer _)
        , step@(Integer _)
        , MultiBulk _  -- ACL categories
        ])) =
        decode (MultiBulk (Just [name, arity, flags, firstPos, lastPos, step]))
    -- since redis 7.0
    decode (MultiBulk (Just
        [ name@(Bulk (Just _))
        , arity@(Integer _)
        , flags@(MultiBulk (Just _))
        , firstPos@(Integer _)
        , lastPos@(Integer _)
        , step@(Integer _)
        , MultiBulk _  -- ACL categories
        , MultiBulk _  -- Tips
        , MultiBulk _  -- Key specifications
        , MultiBulk _  -- Sub commands
        ])) =
        decode (MultiBulk (Just [name, arity, flags, firstPos, lastPos, step]))

    decode e = Left e

newInfoMap :: [CommandInfo] -> InfoMap
newInfoMap = InfoMap . HM.fromList . map (\c -> (Char8.unpack $ name c, c))

keysForRequest :: InfoMap -> [BS.ByteString] -> Maybe [BS.ByteString]
keysForRequest _ ["DEBUG", "OBJECT", key] =
    -- `COMMAND` output for `DEBUG` would let us believe it doesn't have any
    -- keys, but the `DEBUG OBJECT` subcommand does.
    Just [key]
keysForRequest _ ["QUIT"] =
    -- The `QUIT` command is not listed in the `COMMAND` output.
    Just []
keysForRequest (InfoMap infoMap) request@(command:_) = do
    info <- HM.lookup (map toLower $ Char8.unpack command) infoMap
    keysForRequest' info request
keysForRequest _ [] = Nothing

keysForRequest' :: CommandInfo -> [BS.ByteString] -> Maybe [BS.ByteString]
keysForRequest' info request
    | isMovable info =
        parseMovable request
    | stepCount info == 0 =
        Just []
    | otherwise = do
        let possibleKeys = case lastKeyPosition info of
                LastKeyPosition end -> take (fromEnum $ 1 + end - firstKeyPosition info) $ drop (fromEnum $ firstKeyPosition info) request
                UnlimitedKeys end ->
                    drop (fromEnum $ firstKeyPosition info) $
                       take (length request - fromEnum end) request
        return $ takeEvery (fromEnum $ stepCount info) possibleKeys

isMovable :: CommandInfo -> Bool
isMovable CommandInfo{..} = MovableKeys `elem` flags

parseMovable :: [BS.ByteString] -> Maybe [BS.ByteString]
parseMovable ("SORT":key:_) = Just [key]
parseMovable ("EVAL":_:rest) = readNumKeys rest
parseMovable ("EVALSHA":_:rest) = readNumKeys rest
parseMovable ("ZUNIONSTORE":_:rest) = readNumKeys rest
parseMovable ("ZINTERSTORE":_:rest) = readNumKeys rest
parseMovable ("XREAD":rest) = readXreadKeys rest
parseMovable ("XREADGROUP":"GROUP":_:_:rest) = readXreadgroupKeys rest
parseMovable _ = Nothing

readXreadKeys :: [BS.ByteString] -> Maybe [BS.ByteString]
readXreadKeys ("COUNT":_:rest) = readXreadKeys rest
readXreadKeys ("BLOCK":_:rest) = readXreadKeys rest
readXreadKeys ("STREAMS":rest) = Just $ take (length rest `div` 2) rest
readXreadKeys _ = Nothing

readXreadgroupKeys :: [BS.ByteString] -> Maybe [BS.ByteString]
readXreadgroupKeys ("COUNT":_:rest) = readXreadKeys rest
readXreadgroupKeys ("BLOCK":_:rest) = readXreadKeys rest
readXreadgroupKeys ("NOACK":rest) = readXreadKeys rest
readXreadgroupKeys ("STREAMS":rest) = Just $ take (length rest `div` 2) rest
readXreadgroupKeys _ = Nothing

readNumKeys :: [BS.ByteString] -> Maybe [BS.ByteString]
readNumKeys (rawNumKeys:rest) = do
    numKeys <- readMaybe (Char8.unpack rawNumKeys)
    return $ take numKeys rest
readNumKeys _ = Nothing
-- takeEvery 1 [1,2,3,4,5] ->[1,2,3,4,5]
-- takeEvery 2 [1,2,3,4,5] ->[1,3,5]
-- takeEvery 3 [1,2,3,4,5] ->[1,4]
takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing
