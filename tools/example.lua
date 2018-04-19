--[[
 Example SIMION Lua workbench program for SIMIONtuneR.

 Note: this is not a working workbench program, it just contains the additional
 code used by SIMIONtuneR as an example.

 Patrick Sturm, 2018
 (c) Tofwerk AG
 --]]

simion.workbench_program()

-- adjustable variables
adjustable _entrance_voltage  = 30.5
adjustable _exit_voltage      = 30
adjustable _extract_voltage   = 20
adjustable _lens1_voltage     = -40
adjustable _lens2_voltage     = -100
adjustable _lens3_voltage     = -200
adjustable _lens4_voltage     = -300
adjustable _lens5_voltage     = -100

-------------------------------------------------------------------------------------------------------
-- SIMIONtuneR
-------------------------------------------------------------------------------------------------------
-- load LuaFileSystem library
local LFS = require "lfs"

-- load parallel processing library
local PLIB = simion.import 'parallellib_pst.lua'

-- How this process runs: 0=worker, 1=master, 2=worker and master
adjustable master = 2

-- SIMIONtuneR variables, do not change
adjustable tuneR = 0  -- tuneR mode: 0="no", 1="yes"
adjustable maxn = 1000  -- (maximum) number of ions flown for each tuneR run
local tuneRdir = LFS.currentdir().."\\tuneR\\"

-- processing run jobs -------------------------------------------------------------------------------

-- Create runner.
local runner = PLIB.runner()

-- Submits run jobs to workers. Master runs this.
function runner.jobsetup()
  local file = assert(io.open(tuneRdir.."runs.txt"))
  for line in file:lines() do
    runner:run(line)
  end
end
  
-- Performs each run job. Worker runs this.
-- All controls defined in tuneR_config.toml need to be assigned in the same order here.
function runner.jobrun(i,V1,V2,V3,V4,V5,V6)
  sim_trajectory_image_control = 3

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

-- Processes each run job result from worker. Master runs this.
function runner.jobresult(run_no,resolution,sensitivity)
  fp = assert(io.open(tuneRdir.."results.txt","a+")) 
  fp:write(string.format("%i|%.3f|%.6f\n", run_no, resolution or 0, sensitivity))
  io.close(fp)
end
-------------------------------------------------------------------------------------------------------

function segment.flym()

  if tuneR==1 then
    runner:process(master)
  else
    run()
  end
  
end
