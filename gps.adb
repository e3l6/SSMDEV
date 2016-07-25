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
      
      Position       : Position_Type;
      Seconds_Of_Day : Day_Duration;   -- Used to round the time down to the
                                       --   nearest minute for testing
      
   begin
      -- GPS interface will be built once the toolchain for the Tiva-C ARM
      --   is in place and functioning.  Hard setting position to Rogue Hall,
      --   Portland, Oregon (1717-C SW Park Ave, 97201).
      
      --  Position.Latitude     :=   45.512_850_3;
      --  Position.Longitude    := -122.685_360_3;
      
      -- Everything rounded down to the nearest minute for testing against
      --   http://www.pveducation.org/pvcdrom/properties-of-sunlight/
      --     sun-position-calculator
      Position.Latitude     :=   45.0;
      Position.Longitude    := -122.0;
      Position.Current_Time := Clock;
      Seconds_Of_Day        := Seconds (Position.Current_Time);
      Position.Current_Time := 
        Position.Current_time - 
        Day_Duration ((Integer (Seconds_Of_Day) mod 60));
      
      --  ---------------
      --  -- Debugging --
      --  ---------------
      
      --  Put ("Lat:  ");  Put (Position.Latitude, 4, 3, 0);
      --  Put (" Long: "); Put (Position.Longitude, 4, 3, 0);
      --  New_Line (2);
      
      --  Put ("Seconds:    "); Put (Integer (Seconds_Of_Day)); New_Line;
      --  Put ("Difference: "); Put (Integer (Seconds_Of_Day) mod 60); New_Line;
      --  Put ("Adjusted:   "); Put (Integer (Seconds (Position.Current_Time)));
      --  New_Line;
      
      return Position;
   end Get_Position;
   
end GPS;
