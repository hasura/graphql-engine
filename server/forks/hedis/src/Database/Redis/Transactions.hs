{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP, OverloadedStrings, FlexibleInstances, MultiParamTypeClasses,
    GeneralizedNewtypeDeriving #-}

module Database.Redis.Transactions (
    watch, unwatch, multiExec,
    Queued(), TxResult(..), RedisTx(),
) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.State.Strict
import Control.DeepSeq
import GHC.Generics
import Data.ByteString (ByteString)
import Data.Vector (Vector, fromList, (!))

import Database.Redis.Core
import Database.Redis.Protocol
import Database.Redis.Types


-- |Command-context inside of MULTI\/EXEC transactions. Use 'multiExec' to run
--  actions of this type.
--
--  In the 'RedisTx' context, all commands return a 'Queued' value. It is a
--  proxy object for the /actual/ result, which will only be available after
--  finishing the transaction.
newtype RedisTx a = RedisTx (StateT Int Redis a)
    deriving (Monad, MonadIO, Functor, Applicative)

runRedisTx :: RedisTx a -> Redis a
runRedisTx (RedisTx r) = evalStateT r 0

instance MonadRedis RedisTx where
    liftRedis = RedisTx . lift

instance RedisCtx RedisTx Queued where
    returnDecode _queued = RedisTx $ do
        -- future index in EXEC result list
        i <- get
        put (i+1)
        return $ Queued (decode . (! i))

-- |A 'Queued' value represents the result of a command inside a transaction. It
--  is a proxy object for the /actual/ result, which will only be available
--  after returning from a 'multiExec' transaction.
--
--  'Queued' values are composable by utilizing the 'Functor', 'Applicative' or
--  'Monad' interfaces.
data Queued a = Queued (Vector Reply -> Either Reply a)

instance Functor Queued where
    fmap f (Queued g) = Queued (fmap f . g)

instance Applicative Queued where
    pure x                = Queued (const $ Right x)
    Queued f <*> Queued x = Queued $ \rs -> do
                                        f' <- f rs
                                        x' <- x rs
                                        return (f' x')

instance Monad Queued where
    return         = pure
    Queued x >>= f = Queued $ \rs -> do
                                x' <- x rs
                                let Queued f' = f x'
                                f' rs

-- | Result of a 'multiExec' transaction.
data TxResult a
    = TxSuccess a
    -- ^ Transaction completed successfully. The wrapped value corresponds to
    --   the 'Queued' value returned from the 'multiExec' argument action.
    | TxAborted
    -- ^ Transaction aborted due to an earlier 'watch' command.
    | TxError String
    -- ^ At least one of the commands returned an 'Error' reply.
    deriving (Show, Eq, Generic)

instance NFData a => NFData (TxResult a)

-- |Watch the given keys to determine execution of the MULTI\/EXEC block
--  (<http://redis.io/commands/watch>).
watch
    :: [ByteString] -- ^ key
    -> Redis (Either Reply Status)
watch key = sendRequest ("WATCH" : key)

-- |Forget about all watched keys (<http://redis.io/commands/unwatch>).
unwatch :: Redis (Either Reply Status)
unwatch  = sendRequest ["UNWATCH"]


-- |Run commands inside a transaction. For documentation on the semantics of
--  Redis transaction see <http://redis.io/topics/transactions>.
--
--  Inside the transaction block, command functions return their result wrapped
--  in a 'Queued'. The 'Queued' result is a proxy object for the actual
--  command\'s result, which will only be available after @EXEC@ing the
--  transaction.
--
--  Example usage (note how 'Queued' \'s 'Applicative' instance is used to
--  combine the two individual results):
--
--  @
--  runRedis conn $ do
--      set \"hello\" \"hello\"
--      set \"world\" \"world\"
--      helloworld <- 'multiExec' $ do
--          hello <- get \"hello\"
--          world <- get \"world\"
--          return $ (,) \<$\> hello \<*\> world
--      liftIO (print helloworld)
--  @
multiExec :: RedisTx (Queued a) -> Redis (TxResult a)
multiExec rtx = do
    -- We don't need to catch exceptions and call DISCARD. The pool will close
    -- the connection anyway.
    _        <- multi
    Queued f <- runRedisTx rtx
    r        <- exec
    case r of
        MultiBulk rs ->
            return $ maybe
                TxAborted
                (either (TxError . show) TxSuccess . f . fromList)
                rs
        _ -> error $ "hedis: EXEC returned " ++ show r

multi :: Redis (Either Reply Status)
multi = sendRequest ["MULTI"]

exec :: Redis Reply
exec = either id id <$> sendRequest ["EXEC"]
