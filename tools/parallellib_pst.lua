--[[
 parallellib.lua
 SIMION parallel processing Fly'm library.
 (c) 2014-2015 Scientific Instrument Services, Inc. Licensed SIMION 8.2.
 v2015-06-05
 
 adapted by Patrick Sturm (pst)
 (c) 2018-2020 TOFWERK
--]]

require "zmq"
require "zmq.poller"

local M = {_VERSION='v2020-04-28'}

local function get_context()
  local context = _G.zmq_context
  if not context then
    context = zmq.init(1)
    _G.zmq_context = context
  end
  return context
end

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
 Creates runner.
--]]
function M.runner(master)
  local o = {}

  --o.context = zmq.init(1)
  o.context = get_context()
  
  local njobs = 0
  
  -- IP address of master computer
  -- localhost (if all workers are on master) or IP address of master (if workers are also on remote computers).
  local MASTER_ADDRESS = "127.0.0.1"

  -- Default addresses of sockets.
  o.MASTER_PUSH        = "tcp://" .. MASTER_ADDRESS .. ":5557"
  o.MASTER_PULL        = "tcp://" .. MASTER_ADDRESS .. ":5558"
  o.MASTER_PUBLISH     = "tcp://" .. MASTER_ADDRESS .. ":5559"
  
  -- Schedule a single job.
  function o:run(...)
    -- local s = serialize(...)
    local s = ... -- pst: serialized string is directly provided to run()
    njobs = njobs + 1
    print('job', njobs, s)
    o.sender:send(s)
  end
  
  -- Define jobrun callback function.
  o.jobrun = function() end
  
  -- Define jobresult callback function.
  o.jobresult = function() end
  
  -- Define jobsetup callback function.
  o.jobsetup = function() end
  
  -- Perform all processing, synchronous.
  -- master is type of runner: 0 (client), 1 (master), 2 (master+client).
  function o:process(master)
    o.is_worker = master == 0 or master == 2
    o.is_master = master == 1 or master == 2

    -- Set up master connections.
    if o.is_master and not o.sender then
      o.sender = o.context:socket(zmq.PUSH)
      o.sender:bind(o.MASTER_PUSH)
      o.receiver = o.context:socket(zmq.PULL)
      o.receiver:bind(o.MASTER_PULL)
    end

    -- Set up worker connections.
    if o.is_worker and not o.wreceiver then    
      o.wreceiver = o.context:socket(zmq.PULL)
      o.wreceiver:connect(o.MASTER_PUSH)
      o.wsender = o.context:socket(zmq.PUSH)
      o.wsender:connect(o.MASTER_PULL)
      o.wcontrol = o.context:socket(zmq.SUB)
      o.wcontrol:connect(o.MASTER_PUBLISH)
      o.wcontrol:setopt(zmq.SUBSCRIBE, "") -- filter
    end
  
    if o.is_master and o.jobsetup then
      if MASTER_ADDRESS == "127.0.0.1" then
        simion.sleep(1)
      else
        simion.sleep(10) -- pst: wait longer for remote workers to connect.
      end
      o.jobsetup()
    end

    local poller = zmq.poller(3)
    local nreceived = 0
    if o.is_master and o.jobresult then
      poller:add(o.receiver, zmq.POLLIN, function(sock)
        local s = o.receiver:recv()
        print('got', s)
        o.jobresult(unserialize(s))
        nreceived = nreceived + 1
      end)
    end
    if o.is_worker and o.jobrun then
      local ngot = 0
      poller:add(o.wreceiver, zmq.POLLIN, function(sock)
        local s = o.wreceiver:recv()
        ngot = ngot + 1 
        print('received', ngot, s)
        local result = serialize(o.jobrun(unserialize(s)))
        print('sending', ngot, result)
        o.wsender:send(result)
      end)
      poller:add(o.wcontrol, zmq.POLLIN, function(sock)
        local s = o.wcontrol:recv()
        print('control', s)
        if s == "close_children" then
          o.QUIT = true
        end
      end)
    end
    
    -- IMPROVE: key 27 (ESC) handling?
    while (not o.is_master or nreceived < njobs) and key() ~= 27 and not o.QUIT do
      poller:poll(100)
    end

    o:close()
  end
  
  function o:close_children(runner)
    -- print 'closing'  -- pst: commented out
    local control = o.context:socket(zmq.PUB)
    control:bind(o.MASTER_PUBLISH)
    if MASTER_ADDRESS == "127.0.0.1" then
      simion.sleep(1)
    else
      simion.sleep(5) -- pst: wait longer for remote workers to connect.
    end
    control:send('close_children')
    control:close()
  end
  
  function o:close()
    if o.sender    then o.sender:close() end
    if o.receiver  then o.receiver:close() end
    if o.wsender   then o.wsender:close() end
    if o.wreceiver then o.wreceiver:close() end
    if o.wcontrol  then o.wcontrol:close() end
    --o.context:term()
  end
  
  return o
end

function M.clone()
  --print(simion._debug.simion_exe)
  --print(simion.wb.path)
  local slash = package.config:sub(1,1)  -- native directory separator / or \
  local exe = simion._internal.simion_exe:gsub('/', slash) -- native path
  local cmd = 'start "SIMION" "' .. exe .. '" fly --adjustable master=0 "' .. simion.wb.path .. '"'
  print(cmd)
  os.execute(cmd)
end

-- Launch Parallel Toolbar.
function M.toolbar(runner)
  simion.early_access(8.2)
  simion.experimental.dialog {
    title='Parallel Toolbar',
    modal=false,
    {'num processes', type='text', value=3, id='nproc'},
    {'Launch Instances', type='button', id='launch'},
    {'Close Instances', type='button', id='close'},
    update = function(t,e)
      if e.id == 'launch' then
        local nproc = tonumber(t.nproc) or 0
        for i=1,nproc do M.clone() end
      elseif e.id == 'close' then
        if runner then runner:close_children() end
      end
    end
  }
end
_G.toolbar = M.toolbar


--[==[TEST
local s = serialize([[|a\||]],nil,false,true,1.2,"ok")
print(s)
local t = {unserialize(s)}
assert(t[1] == [[|a\||]], t[1])
assert(t[2] == nil)
assert(t[3] == false)
assert(t[4] == true)
assert(t[5] == 1.2)
assert(t[6] == "ok")
assert(t[7] == nil)
--]==]

return M
