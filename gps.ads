-------------------------------------------------------------------------------
-- GPS
--
-- Copyright © 2016 Eric Laursen
--
-- This code is available under the "MIT License".
-- Please see the file COPYING in this distribution for license terms.
--
-- Purpose:
--   This package interfaces to the Adafruit Ultimate GPS Breakout v3
--   (https://www.adafruit.com/product/746) based on the MediaTek MT3339
--   chipset.
-------------------------------------------------------------------------------

with Ada.Calendar; use Ada.Calendar;

package GPS is
   
   type Position_Type is
      record
	 -- For latitude and longitude: positive values denote Northern and
	 --   Eastern hemispheres, respectively; negative values denote
	 --   Southern and Western hemispheres, respectively.
	 Latitude      : Float range  -90.0 ..  90.0;
	 Longitude     : Float range -180.0 .. 180.0;

	 Current_Time  : Time;  -- Universal Coordinated Time (UTC)
      end record;
   
   function Get_Position return Position_Type;
   
end GPS;
