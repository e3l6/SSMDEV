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

package body GPS is
   
   ----------------------------------------------------------------------------
   function Get_Position return Position_Type is
      
      Position : Position_Type;
	
   begin
      -- GPS interface will be built once the toolchain for the Tiva-C ARM
      --   is in place and functioning.  Hard setting position to Rogue Hall,
      --   Portland, Oregon (1717-C SW Park Ave, 97201).
      
      Position.Latitude     :=   45.512_850_3;
      Position.Longitude    := -122.685_360_3;
      Position.Current_Time := Clock;
      
      return Position;
   end Get_Position;
   
end GPS;
