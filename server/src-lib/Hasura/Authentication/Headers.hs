module Hasura.Authentication.Headers
  ( contentLengthHeader,
    gzipHeader,
    jsonHeader,
    requestIdHeader,
    sqlHeader,
    sensitiveHeaders,
    commonClientHeadersIgnored,
  )
where

import Data.HashSet qualified as HashSet
import Hasura.Prelude
import Network.HTTP.Types qualified as HTTP

jsonHeader :: HTTP.Header
jsonHeader = ("Content-Type", "application/json; charset=utf-8")

sqlHeader :: HTTP.Header
sqlHeader = ("Content-Type", "application/sql; charset=utf-8")

gzipHeader :: HTTP.Header
gzipHeader = ("Content-Encoding", "gzip")

requestIdHeader :: (IsString a) => a
requestIdHeader = "x-request-id"

contentLengthHeader :: (IsString a) => a
contentLengthHeader = "Content-Length"

sensitiveHeaders :: HashSet HTTP.HeaderName
sensitiveHeaders =
  HashSet.fromList
    [ "Access-Token",
      "Authorization",
      "Cookie"
    ]

-- ignore the following request headers from the client
commonClientHeadersIgnored :: (IsString a) => [a]
commonClientHeadersIgnored =
  [ "Content-Length",
    "Content-MD5",
    "User-Agent",
    "Host",
    "Origin",
    "Referer",
    "Accept",
    "Accept-Encoding",
    "Accept-Language",
    "Accept-Datetime",
    "Cache-Control",
    "Connection",
    "DNT",
    "Content-Type"
  ]
