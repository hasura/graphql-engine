-- This is the default lua script that is used when
-- a script is not specified by the user.

-- This expects that the url specified confirms to
-- the graphql url POST spec.

local gqbench = require "bench-lib-wrk2"

local req_body = ""

local results_dir = nil

function init(args)
  req_body = gqbench.init(args)
  results_dir = args[2]
end

function request()
  return gqbench.request(wrk, req_body)
end

function done(s, l, r)
  return gqbench.done(s, l, r, results_dir)
end
