-- A library that can be used in custom lua scripts for benchmarking

local _M = {}

local json = require "cjson"
local os = require "os"
-- Use `null` to represent any NaN or Inf:
json.encode_invalid_numbers("null") 

function _M.init(args)
  local query = args[1]
  return json.encode({query=query}), args[2], args[3], args[4]
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

function _M.done(summary, latency, requests, results_dir)
  local summary_file = io.open(results_dir .. '/summary.json','w')
  local summary_output = json.encode({
        time=getTime(),
        latency=get_stat_summary(latency),
        summary=summary,
        requests=get_stat_summary(requests)
  })
  io.stderr:write(summary_output)
  summary_file:write(summary_output .. '\n')
  summary_file:close()
  latencies_file = io.open(results_dir .. '/latencies','w')
  for i = 1, summary.requests do
    if (latency[i] ~= 0)
    then
      latencies_file:write(latency[i] .. '\n')
    end
  end	
  latencies_file:close()
end

return _M
