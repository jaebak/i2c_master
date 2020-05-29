-------------------------------------------------------------------------------
--
--     File Name : Firmware_pkg.vhd
--          Date : 
--        Author : 
--
-------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package Firmware_pkg is

  ---- Flag for synthesis/simulation
  -- For simulation
  constant in_simulation : BOOLEAN := false
  -- synthesis translate_off
  or true
  -- synthesis translate_on
  ;
  constant in_synthesis : BOOLEAN := not in_simulation;

end Firmware_pkg;
