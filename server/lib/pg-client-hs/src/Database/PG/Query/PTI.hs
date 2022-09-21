{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Database.PG.Query.PTI where

-------------------------------------------------------------------------------

import Data.Word (Word32)
import Database.PostgreSQL.LibPQ qualified as PQ
import Prelude

-------------------------------------------------------------------------------

mkOid :: Word32 -> PQ.Oid
mkOid = PQ.Oid . fromIntegral

-- * Constants

-------------------------
auto = mkOid 0

abstime = mkOid 702

aclitem = mkOid 1033

bit = mkOid 1560

bool = mkOid 16

box = mkOid 603

bpchar = mkOid 1042

bytea = mkOid 17

char = mkOid 18

cid = mkOid 29

cidr = mkOid 650

circle = mkOid 718

cstring = mkOid 2275

date = mkOid 1082

daterange = mkOid 3912

float4 = mkOid 700

float8 = mkOid 701

gtsvector = mkOid 3642

inet = mkOid 869

int2 = mkOid 21

int2vector = mkOid 22

int4 = mkOid 23

int4range = mkOid 3904

int8 = mkOid 20

int8range = mkOid 3926

interval = mkOid 1186

json = mkOid 114

jsonb = mkOid 3802

line = mkOid 628

lseg = mkOid 601

macaddr = mkOid 829

money = mkOid 790

name = mkOid 19

numeric = mkOid 1700

numrange = mkOid 3906

oid = mkOid 26

oidvector = mkOid 30

path = mkOid 602

point = mkOid 600

polygon = mkOid 604

record = mkOid 2249

refcursor = mkOid 1790

regclass = mkOid 2205

regconfig = mkOid 3734

regdictionary = mkOid 3769

regoper = mkOid 2203

regoperator = mkOid 2204

regproc = mkOid 24

regprocedure = mkOid 2202

regtype = mkOid 2206

reltime = mkOid 703

text = mkOid 25

tid = mkOid 27

time = mkOid 1083

timestamp = mkOid 1114

timestamptz = mkOid 1184

timetz = mkOid 1266

tinterval = mkOid 704

tsquery = mkOid 3615

tsrange = mkOid 3908

tstzrange = mkOid 3910

tsvector = mkOid 3614

txid_snapshot = mkOid 2970

unknown = mkOid 705

uuid = mkOid 2950

varbit = mkOid 1562

varchar = mkOid 1043

void = mkOid 2278

xid = mkOid 28

xml = mkOid 142

-- Array Types

text_arr = mkOid 1009
