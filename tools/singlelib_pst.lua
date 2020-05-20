--[[
 singlelib_pst.lua
 Similar to parallellib_pst.lua, but without the ZeroMQ libraries for single processing.
 (c) 2020 TOFWERK

 adapted from parallellib.lua
 (c) 2014-2015 Scientific Instrument Services, Inc. Licensed SIMION 8.2.
 v2015-06-05
--]]

local M = {}

local function depack(...)
  return {n=select('#',...), ...}
end

-- utility function
local function split_string(s)
  local t = {}

  local tok = ''
  local pos = 1
  while pos <= #s do
    local p,pos2 = s:match('^([^\\|]+)()', pos)
    if p then
      tok = tok .. p
      pos = pos2
    else
      local pos2 = s:match('^%|()', pos)
      if pos2 then
        t[#t+1] = tok
        tok = ''
        pos = pos2
      else
        local p,pos2 = s:match('^\\(.?)()', pos)
        if p == '' then p = '\\' end
        tok = tok .. p
        pos = pos2
      end
    end
  end
  
  return t
end

-- Simple serialization function for data transfer.
local function serialize(...)
  local t = depack(...)
  for i=1,t.n do
    local v = t[i]
    if type(v) == 'string' then
      v = v:gsub('\\', '\\\\')
      v = v:gsub('|', '\\|')
      v = 'S'..v
    elseif type(v) == 'number' then
      v = tostring(v)
    elseif type(v) == 'boolean' then
      v = tostring(v)
    elseif type(v) == 'nil' then
      v = tostring(v)
    else
      error(type(v)..' not implemented')
    end
    t[i] = v..'|'
  end
  
  local s = table.concat(t, '')
  return s
end

-- Simple unserialization function for data transfer.
local function unserialize(s)
  local t = split_string(s)
  t.n = #t
  for i=1,t.n do
    local ss = t[i]
    if ss ~= '' and ss:sub(1,1) == 'S' then
      t[i] = ss:sub(2)
    elseif ss == 'nil' then
      t[i] = nil
    elseif ss == 'false' then
      t[i] = false
    elseif ss == 'true' then
      t[i] = true
    else
      local v = tonumber(ss)
      if v then
        t[i] = v
      else
        assert('unrecognized '..v)
      end
    end
  end
  return unpack(t, 1, t.n)
end

--[[
 Creates single-threaded runner.
--]]
function M.runner(master)
  local o = {}
  
  local njobs = 0

  -- Schedule a single job.
  function o:run(...)
    local s = ...
    njobs = njobs + 1
    print('job', njobs, s)
    local result = serialize(o.jobrun(unserialize(s)))
    o.jobresult(unserialize(result))
  end
  
  function o:process(master)
    o.jobsetup()
  end
  
  return o
end

return M
