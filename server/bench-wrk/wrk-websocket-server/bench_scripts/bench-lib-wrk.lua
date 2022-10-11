-- A library that can be used in custom lua scripts for benchmarking

local _M = {}

local json = require "cjson"
local os = require "os"

function _M.init(args)
  local query = args[1]
  return json.encode({query=query}), args[2], args[3]
end

function _M.request(wrk, req_body, auth_header_key, auth_header_value)
  wrk.method = "POST"
  wrk.headers["Content-Type"] = "application/json"
  if auth_header_key ~= nil and auth_header_value ~= nil then
     wrk.headers[auth_header_key] = auth_header_value
  end
  wrk.body = req_body
  return wrk.format()
end

local function get_stat_summary(stat)
  local dist = {}
  for _, p in pairs({ 95, 98, 99 }) do
    dist[tostring(p)] = stat:percentile(p)
  end
  return {
    min=stat.min,
    max=stat.max,
    stdev=stat.stdev,
    mean=stat.mean,
    dist=dist
  }
end

local function getTime()
  return os.date("%c %Z")
end

function _M.done(summary, latency, requests)
  io.stderr:write(
    json.encode({
        time=getTime(),
        -- Latency info from wrk framework is not that useful
        -- latency=get_stat_summary(latency),
        summary=summary,
        requests=get_stat_summary(requests)
    })
  )
  io.stderr:write('\n')
end

return _M
