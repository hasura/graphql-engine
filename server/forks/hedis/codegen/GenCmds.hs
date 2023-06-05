{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Prelude hiding (interact)
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (ByteString, interact)
import Data.Char
import Data.Foldable (asum)
import Data.Function (on)
import qualified Data.HashMap.Lazy as HM (toList)
import Data.List (intercalate, sortBy, groupBy, intersperse)
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import Data.Text.Encoding (encodeUtf8)


--------------------------------------------------------------------------------
-- README
--
-- * New groups have to be listed in cmdGroups below.
-- * The command blacklist allows to specify the name of a manual
--   implementation.

-- |Whitelist for the command groups
groupCmds :: Cmds -> Cmds
groupCmds (Cmds cmds) =
    Cmds [cmd | cmd <- cmds, group <- groups, cmdGroup cmd == group]
  where
    groups = [ "generic"
             , "string"
             , "list"
             , "set"
             , "sorted_set"
             , "hash"
             , "hyperloglog"
             -- , "pubsub" commands implemented in Database.Redis.PubSub
             -- , "transactions" implemented in Database.Redis.Transactions
             , "connection"
             , "server"
             , "scripting"
             -- not implemented:
             -- , "cluster"
             -- , "geo"
             ]

-- |Blacklisted commands, optionally paired with the name of their
--  implementation in the "Database.Redis.ManualCommands" module.
blacklist :: [(String, Maybe ([String],[String]))]
blacklist = [ manual "AUTH" ["auth"]
            , manual "SELECT" ["select"]
            , manual "PING" ["ping"]
            , manual "OBJECT"
                ["objectRefcount","objectEncoding","objectIdletime"]
            , manualWithType "TYPE" ["getType"] ["RedisType(..)"]
            , manual "EVAL" ["eval"]
            , manual "EVALSHA" ["evalsha"]
            , manualWithType "SORT"
                ["sort","sortStore"]
                ["SortOpts(..)","defaultSortOpts","SortOrder(..)"]
            , manual "BITCOUNT" ["bitcount", "bitcountRange"]
            , manual "BITOP" ["bitopAnd", "bitopOr", "bitopXor", "bitopNot"]
            , manual "LINSERT" ["linsertBefore", "linsertAfter"]
            , manualWithType "SLOWLOG"
                ["slowlogGet", "slowlogLen", "slowlogReset"]
                ["Slowlog(..)"]
            , manualWithType "ZINTERSTORE"
                ["zinterstore","zinterstoreWeights"]
                ["Aggregate(..)"]
            , manual "ZRANGE" ["zrange", "zrangeWithscores"]
            , manual "ZRANGEBYSCORE"
                ["zrangebyscore", "zrangebyscoreWithscores"
                ,"zrangebyscoreLimit", "zrangebyscoreWithscoresLimit"]
            , manual "ZREVRANGE" ["zrevrange", "zrevrangeWithscores"]
            , manual "ZREVRANGEBYSCORE"
                ["zrevrangebyscore", "zrevrangebyscoreWithscores"
                ,"zrevrangebyscoreLimit", "zrevrangebyscoreWithscoresLimit"]
            , manual "ZUNIONSTORE" ["zunionstore","zunionstoreWeights"]
            , manualWithType "SET"
                ["set", "setOpts"]
                ["Condition(..)", "SetOpts(..)"]
            , manualWithType "ZADD"
                ["zadd", "zaddOpts"]
                ["ZaddOpts(..)", "defaultZaddOpts"]
            , manualWithType "MIGRATE"
                ["migrate", "migrateMultiple"]
                ["MigrateOpts(..)", "defaultMigrateOpts"]
            , manual "RESTORE"
                ["restore", "restoreReplace"]
            , manualWithType "CLIENT REPLY"
                ["clientReply"]
                ["ReplyMode"]
            , manualWithType "SCRIPT DEBUG"
                ["scriptDebug"]
                ["DebugMode"]
            , manual "SRANDMEMBER" ["srandmember", "srandmemberN"]
            , manual "SPOP" ["spop", "spopN"]
            , manual "INFO" ["info", "infoSection"]
            , manual "EXISTS" ["exists"]
            , manualWithType "SCAN"
                ["scan", "scanOpts"]
                ["Cursor", "cursor0", "ScanOpts(..)", "defaultScanOpts"]
            , manual "SSCAN" ["sscan", "sscanOpts"]
            , manual "HSCAN" ["hscan", "hscanOpts"]
            , manual "ZSCAN" ["zscan", "zscanOpts"]
            , manualWithType "ZRANGEBYLEX"
                ["zrangebylex, zrangebylexLimit"]
                ["RangeLex(..)"]
            , unimplemented "COMMAND"
            , unimplemented "COMMAND GETKEYS"
            , unimplemented "ROLE"
            , unimplemented "CLIENT KILL"
            , unimplemented "ZREVRANGEBYLEX"
            , unimplemented "ZRANGEBYSCORE"
            , unimplemented "ZREVRANGEBYSCORE"
            , unimplemented "MONITOR"        -- debugging command
            , unimplemented "SYNC"           -- internal command
            , unimplemented "SHUTDOWN"       -- kills server, throws exception
            , unimplemented "DEBUG SEGFAULT" -- crashes the server
            ]
  where
    unimplemented cmd  = (cmd, Nothing)
    manual cmd aliases = (cmd, Just (aliases, []))
    manualWithType cmd aliases types = (cmd, Just (aliases, types))

-- Read JSON from STDIN, write Haskell module source to STDOUT.
main :: IO ()
main = interact generateCommands

generateCommands :: ByteString -> ByteString
generateCommands json =
    toLazyByteString . hsFile . groupCmds . fromJust $ decode' json

-- Types to represent Commands. They are parsed from the JSON input.
data Cmd = Cmd { cmdName, cmdGroup :: String
               , cmdRetType        :: Maybe String
               , cmdArgs           :: [Arg]
               , cmdSummary        :: String
               , cmdSince          :: Maybe String
               }
    deriving (Show)

newtype Cmds = Cmds [Cmd]

instance Show Cmds where
    show (Cmds cmds) = intercalate "\n" (map show cmds)

data Arg = Arg { argName, argType :: String }
         | Pair Arg Arg
         | Multiple Arg
         | Command String
         | Enum [String]
    deriving (Show)

instance FromJSON Cmds where
    parseJSON (Object cmds) = do
        Cmds <$> forM (HM.toList cmds) (\(name,Object cmd) -> do
            let cmdName = unpack $ encodeUtf8 name
            cmdGroup   <- cmd .: "group"
            cmdRetType <- cmd .:? "returns"
            cmdSummary <- cmd .: "summary"
            cmdSince   <- cmd .:? "since"
            cmdArgs    <- cmd .:? "arguments" .!= []
                            <|> error ("failed to parse args: " ++ cmdName)
            return Cmd{..})

instance FromJSON Arg where
    parseJSON (Object arg) = asum
        [ parseEnum, parseCommand, parseMultiple
        , parsePair, parseSingle, parseSingleList
        ] <|> error ("failed to parse arg: " ++ show arg)
      where
        parseSingle = do
            argName     <- arg .: "name"
            argType     <- arg .: "type"
            return Arg{..}
        -- "multiple": "true"
        parseMultiple = do
            True <- arg .: "multiple"
            Multiple <$> (parsePair <|> parseSingle)
        -- example: SUBSCRIBE
        parseSingleList = do
            [argName]   <- arg .: "name"
            [argType]   <- arg .: "type"
            return Arg{..}
        -- example: MSETNX
        parsePair = do
            [name1,name2] <- arg .: "name"
            [typ1,typ2]   <- arg .: "type"
            return $ Pair Arg{ argName = name1, argType = typ1 }
                          Arg{ argName = name2, argType = typ2 }
        -- example: ZREVRANGEBYSCORE
        parseCommand = do
            s <- arg .: "command"
            return $ Command s
        -- example: LINSERT
        parseEnum = do
            enum <- arg .: "enum"
            return $ Enum enum


-- Generate source code for Haskell module exporting the given commands.
hsFile :: Cmds -> Builder
hsFile (Cmds cmds) = mconcat
    [ fromString "-- Generated by GenCmds.hs. DO NOT EDIT.\n\n\
                 \{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}\n\n\
                 \module Database.Redis.Commands (\n"
    , exportList cmds
    , newline
    , unimplementedCmds
    , fromString ") where\n\n"
    , imprts, newline
    , mconcat (map fromCmd cmds)
    , newline
    , newline
    ]

unimplementedCmds :: Builder
unimplementedCmds =
    fromString "-- * Unimplemented Commands\n\
               \-- |These commands are not implemented, as of now. Library\n\
               \--  users can implement these or other commands from\n\
               \--  experimental Redis versions by using the 'sendRequest'\n\
               \--  function.\n"
    `mappend`
    mconcat (map unimplementedCmd unimplemented)
  where
    unimplemented = map fst . filter (isNothing . snd) $ blacklist
    unimplementedCmd cmd = mconcat
        [ fromString "--\n"
        , fromString "-- * "
        , fromString cmd, fromString " ("
        , cmdDescriptionLink cmd
        , fromString ")\n"
        , fromString "--\n"
        ]

exportList :: [Cmd] -> Builder
exportList cmds =
    mconcat . map exportGroup . groupBy ((==) `on` cmdGroup) $
        sortBy (comparing cmdGroup) cmds
  where
    exportGroup group = mconcat
        [ newline
        , fromString "-- ** ", fromString $ translateGroup (head group)
        , newline
        , mconcat . map exportCmd $ sortBy (comparing cmdName) group
        ]
    exportCmd cmd@Cmd{..}
        | implemented cmd = exportCmdNames cmd
        | otherwise       = mempty

    implemented Cmd{..} =
        case lookup cmdName blacklist of
            Nothing       -> True
            Just (Just _) -> True
            Just Nothing  -> False

    translateGroup Cmd{..} = case cmdGroup of
        "generic"      -> "Keys"
        "string"       -> "Strings"
        "list"         -> "Lists"
        "set"          -> "Sets"
        "sorted_set"   -> "Sorted Sets"
        "hash"         -> "Hashes"
        "hyperloglog"  -> "HyperLogLogs"
        -- "pubsub"       ->
        -- "transactions" -> "Transactions"
        "connection"   -> "Connection"
        "server"       -> "Server"
        "scripting"    -> "Scripting"
        _              -> error $ "untranslated group: " ++ cmdGroup

exportCmdNames :: Cmd -> Builder
exportCmdNames Cmd{..} = types `mappend` functions
  where
    types = mconcat $ flip map typeNames
        (\name -> mconcat [fromString name, fromString ",\n"])

    functions = mconcat $ flip map funNames
        (\name -> mconcat [fromString name, fromString ", ", haddock, newline])

    funNames = case lookup cmdName blacklist of
        Nothing            -> [camelCase cmdName]
        Just (Just (xs,_)) -> xs
        Just Nothing       -> error "unhandled"

    typeNames = case lookup cmdName blacklist of
        Nothing            -> []
        Just (Just (_,ts)) -> ts
        Just Nothing       -> error "unhandled"

    dropTrailingDot s = case reverse s of
        ('.':rest) -> reverse rest
        _          -> s

    haddock = mconcat
        [ fromString "-- |", fromString (dropTrailingDot cmdSummary)
        , fromString " ("
        , cmdDescriptionLink cmdName
        , fromString ")."
        , if length funNames > 1
            then mconcat
                [ fromString " The Redis command @"
                , fromString cmdName
                , fromString "@ is split up into '"
                , mconcat . map fromString $ intersperse "', '" funNames
                , fromString "'."
                ]
            else mempty
        , case cmdSince of
            Just ver -> mconcat [fromString " Since Redis ", fromString ver]
            Nothing  -> fromString " In unstable version of Redis"
        ]

cmdDescriptionLink :: String -> Builder
cmdDescriptionLink cmd = mconcat $ map fromString
    [ "<http://redis.io/commands/"
    , map (\c -> if isSpace c then '-' else toLower c) cmd
    , ">"
    ]

newline :: Builder
newline = fromChar '\n'

imprts :: Builder
imprts = mconcat $ flip map moduls (\modul ->
    fromString "import " `mappend` fromString modul `mappend`newline)
  where
    moduls = [ "Prelude hiding (min,max)" -- name shadowing warnings.
             , "Data.ByteString (ByteString)"
             , "Database.Redis.ManualCommands"
             , "Database.Redis.Types"
             , "Database.Redis.Core"
             ]

blackListed :: Cmd -> Bool
blackListed Cmd{..} = isJust $ lookup cmdName blacklist


fromCmd :: Cmd -> Builder
fromCmd cmd@Cmd{..}
    | blackListed cmd = mempty
    | otherwise       = mconcat [sig, newline, fun, newline, newline]
  where
    sig = mconcat
            [ fromString name
            , fromString "\n    :: (RedisCtx m f)"
            , fromString "\n    => "
            , mconcat $ map argumentType cmdArgs
            , fromString "m (f ", retType cmd, fromString ")"
            ]
    fun = mconcat
            [ fromString name, fromString " "
            , mconcat $ intersperse (fromString " ") (map argumentName cmdArgs)
            , fromString " = "
           , fromString "sendRequest ([\""
            , mconcat $ map fromString $ intersperse "\",\"" $ words cmdName
            , fromString "\"]"
            , mconcat $ map argumentList cmdArgs
            , fromString " )"
            ]
    name = camelCase cmdName

retType :: Cmd -> Builder
retType Cmd{..} = maybe err translate cmdRetType
  where
    err = error $ "Command without return type: " ++ cmdName
    translate t = fromString $ case t of
        "status"       -> "Status"
        "bool"         -> "Bool"
        "integer"      -> "Integer"
        "maybe-integer"-> "(Maybe Integer)"
        "key"          -> "ByteString"
        "maybe-key"    -> "(Maybe ByteString)"
        "string"       -> "ByteString"
        "maybe-string" -> "(Maybe ByteString)"
        "list-string"  -> "[ByteString]"
        "list-maybe"   -> "[Maybe ByteString]"
        "list-bool"    -> "[Bool]"
        "hash"         -> "[(ByteString,ByteString)]"
        "set"          -> "[ByteString]"
        "maybe-pair"   -> "(Maybe (ByteString,ByteString))"
        "double"       -> "Double"
        "maybe-double" -> "(Maybe Double)"
        "reply"        -> "Reply"
        "time"         -> "(Integer,Integer)"
        _              -> error $ "untranslated return type: " ++ t


argumentList :: Arg -> Builder
argumentList a = fromString " ++ " `mappend` go a
  where
    go (Multiple p@(Pair _a _a')) = mconcat
        [ fromString "concatMap (\\(x,y) -> [encode x,encode y])"
        , argumentName p
        ]
    go (Multiple a) = fromString "map encode " `mappend` argumentName a
    go a@Arg{..}    = mconcat
        [ fromString "[encode ", argumentName a, fromString "]" ]

argumentName :: Arg -> Builder
argumentName a = go a
  where
    go (Multiple a) = go a
    go (Pair a a')  = fromString . camelCase $ argName a ++ " " ++ argName a'
    go a@Arg{..}    = name a
    name Arg{..}    = fromString (camelCase argName)

argumentType :: Arg -> Builder
argumentType a = mconcat [ go a
                         , fromString " -- ^ ", argumentName a
                         , fromString "\n    -> "
                         ]
  where
    go (Multiple a) =
        mconcat [fromString "[", go a, fromString "]"]
    go (Pair a a')  =
        mconcat [fromString "(", go a, fromString ",", go a', fromString ")"]
    go a@Arg{..}    = translateArgType a
    go a = error ("failed to user argument type: " ++ show a)

    translateArgType Arg{..} = fromString $ case argType of
        "integer"    -> "Integer"
        "string"     -> "ByteString"
        "key"        -> "ByteString"
        "pattern"    -> "ByteString"
        "posix time" -> "Integer"
        "double"     -> "Double"
        _            -> error $ "untranslated arg type: " ++ argType


--------------------------------------------------------------------------------
-- HELPERS
--

-- |Convert all-uppercase string to camelCase
camelCase :: String -> String
camelCase s = case split (map toLower s) of
    []   -> ""
    w:ws -> concat $ w : map upcaseFirst ws
  where
    upcaseFirst []     = ""
    upcaseFirst (c:cs) = toUpper c : cs

    -- modified version of Data.List.words
    split s = case dropWhile (not . isAlphaNum) s of
                "" -> []
                s' -> w:split s'' where (w,s'') = break (not . isAlphaNum) s'
