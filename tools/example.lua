--[[
 Example SIMION Lua workbench program for SIMIONtuneR.

 Note: this is not a working workbench program, it just contains the additional
 code used by SIMIONtuneR as an example.

 Patrick Sturm, 2018
 (c) Tofwerk AG
 --]]

simion.workbench_program()

adjustable _entrance_voltage  = 30.5
adjustable _exit_voltage      = 30
adjustable _extract_voltage   = 20
adjustable _lens1_voltage     = -40
adjustable _lens2_voltage     = -100
adjustable _lens3_voltage     = -200
adjustable _lens4_voltage     = -300
adjustable _lens5_voltage     = -100

-- SIMIONtuneR variables
-- *** DO NOT CHANGE ***
adjustable master = 2  -- How this process runs: 0=worker, 1=master, 2=worker and master
adjustable tuneR = 0  -- tuneR mode: 0="no", 1="yes"
adjustable maxn = 1  -- maximum number of ions flown for each tuneR run
adjustable zmq = 0  -- ZeroMQ library
local LFS = require "lfs"  -- Load LuaFileSystem library.
local tuneRdir = LFS.currentdir().."\\tuneR\\"  -- tuneR results directory

-- SIMION segment.flym() function.
function segment.flym()
  
  -- SIMIONtuneR code
  if tuneR==1 then
    
    -- load processing library
    local PLIB
    if zmq==1 then
    	PLIB = simion.import 'parallellib_pst.lua'
    	-- Note: adjust master's IP address in parallellib_pst.lua if it is not localhost.
    else
    	PLIB = simion.import 'singlelib_pst.lua'
    end
  
    -- Create runner.
    local runner = PLIB.runner()
    
    -- *** TO BE ADJUSTED ***
    -- All controls defined in SIMIONtuneR_config.toml need to be assigned 
    -- (in the same order) in jobrun(i, ...).
    function runner.jobrun(i,V1,V2,V3,V4,V5,V6)
    
      _extract_voltage = V1
      _lens1_voltage = V2
      _lens2_voltage = V3
      _lens3_voltage = V4
      _lens4_voltage = V5
      _lens5_voltage = V6
      
      run()
    
      -- Resolution and sensitivity are the responses to optimize (e.g. defined in segment.terminate_run())
      return i,resolution,sensitivity
    end
    
    -- Submit run jobs.
    function runner.jobsetup()
      local file = assert(io.open(tuneRdir.."runs.txt"))
      for line in file:lines() do
      	runner:run(line)
      end
    end
    
    -- Processe each run job result.
    function runner.jobresult(run_no,resolution,sensitivity)
      fp = assert(io.open(tuneRdir.."results.txt","a+")) 
      fp:write(string.format("%i|%.3f|%.6f\n", run_no, resolution or 0, sensitivity))
      io.close(fp)
    end
    
    sim_trajectory_image_control = 3
    
    runner:process(master)

  else  -- tuneR==0
    run()
  end
  
end

-- SIMION terminate_run() function.
function segment.terminate_run()
  -- define the response variables here:
  -- resolution = ...
  -- sensitivity = ...
end