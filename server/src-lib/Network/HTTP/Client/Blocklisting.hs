module Network.HTTP.Client.Blocklisting
  ( block,
    Blocklist (..),
  )
where

import Hasura.Prelude
import Net.IPv4 qualified as IPv4
import Net.IPv6 qualified as IPv6
import Network.HTTP.Client.Restricted qualified as Restricted
import Network.Socket

data Blocklist = Blocklist
  { ipv4Blocklist :: [IPv4.IPv4Range],
    ipv6Blocklist :: [IPv6.IPv6Range]
  }
  deriving (Show, Generic)

instance Semigroup Blocklist where
  Blocklist ipv4Lst1 ipv6Lst1 <> Blocklist ipv4Lst2 ipv6Lst2 = Blocklist (ipv4Lst1 ++ ipv4Lst2) (ipv6Lst1 ++ ipv6Lst2)

instance Monoid Blocklist where
  mempty = Blocklist [] []

-- | Determine whether the given address is blocked by the given blocklist.
-- NOTE: Only restricts IPv4 and IPv6 addresses. Other address families are
-- not restricted.
block :: Blocklist -> AddrInfo -> Restricted.Decision
block blocklist addr =
  if sockAddrInBlocklist $ addrAddress addr
    then Restricted.Deny
    else Restricted.Allow
  where
    sockAddrInBlocklist = \case
      (SockAddrInet _ hostAddr) -> any (IPv4.member $ ipv4Addr hostAddr) (ipv4Blocklist blocklist)
      (SockAddrInet6 _ _ hostAddr6 _) -> any (IPv6.member $ ipv6Addr hostAddr6) (ipv6Blocklist blocklist)
      _ -> False
    ipv4Addr = IPv4.fromTupleOctets . hostAddressToTuple
    ipv6Addr = IPv6.fromTupleWord32s
