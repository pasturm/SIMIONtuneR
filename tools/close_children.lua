--[[
 close_children.lua
 This is used by the SIMIONtuneR package to close SIMION child processes.
 
 Patrick Sturm, 2018
 (c) Tofwerk AG
--]]

function script_path()
   local str = debug.getinfo(2, "S").source:sub(2)
   return str:match("(.*/)")
end

-- load parallellib.lua
local M = dofile(script_path().."parallellib_pst.lua")

-- Create runner.
local runner = M.runner()

-- Close children.
runner:close_children()
