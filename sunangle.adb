-------------------------------------------------------------------------------
-- SUNANGLE
--
-- Copyright © 2016 Eric Laursen
--
-- This code is available under the "MIT License".
-- Please see the file COPYING in this distribution for license terms.
--
-- Purpose:
--   This package computes the the sun angle (azimuth and elevation) using
--   latitude, longitude, and UTC.  The formulae used are taken from
--   http://www.pveducation.org/pvcdrom/properties-of-sunlight/suns-position
-------------------------------------------------------------------------------

package body Sunangle is
   
   ----------------------------------------------------------------------------
   function Calculate (Position : in GPS.Position_Type) return Angle_Type is
      
      Return_Value                 : Angle_Type;
      Local_Standard_Time          : Time;
      
      Local_Standard_Time_Meridian : Float;
      Equation_Of_Time             : Float;
      B                            : Float; -- Equation_Of_Time correction
                                            --   coefficient
      Day_Of_Year                  : Float;
      Time_Correction_Factor       : Float;
      Local_Solar_Time             : Float;
      Hour_Angle                   : Float;
      Declination                  : Float;
      
      -------------------------------------------------------------------------
      function To_Julian_Day (Current_Time : Time) return Float is
	 
	 Beginning_Of_Year  : Time;
	 Current_Year       : Year_Number;
	 Days_Difference    : Day_Count;
	 Seconds_Difference : Duration;
	 Leap_Seconds       : Leap_Seconds_Count;
	 
      begin
         Current_Year      := Ada.Calendar.Formatting.Year (Current_Time);         
	 Beginning_Of_Year := Ada.Calendar.Formatting.Time_Of 
           (Current_Year, 1, 1); -- Jan 1, this year
	 
	 Difference (Current_Time, Beginning_Of_Year,
		     Days_Difference, Seconds_Difference, Leap_Seconds);
	 
	 return Float (Days_Difference);
      end To_Julian_Day;
      -------------------------------------------------------------------------
      
   begin
      
      Local_Standard_Time_Meridian := Float'Truncation (Position.Longitude / 
							  Degrees_In_hour) *
        Degrees_In_Hour;
      
      -- Use local time based on longitude.  This is purely geographic and
      --   doesn't use standard time zones, only adds or subtracts 1 hour
      --   for every 15 degrees East or West (respectively) from the Prime
      --   Meridian.
      Local_Standard_Time := Position.Current_Time +
        Duration (Local_Standard_Time_Meridian / Degrees_In_Hour *
                    Seconds_In_Hour);
      
      Day_Of_Year := To_Julian_Day (Local_Standard_Time);
      
      B := (360.0 / 365.0) * (Day_Of_Year - 81.0);
      
      Equation_Of_Time := 9.87 * Sin (2.0 * B, Degrees_Cycle) - 
                          7.53 * Cos (B, Degrees_Cycle) -
                          1.5  * Sin (B, Degrees_Cycle);
      
      Time_Correction_Factor := 4.0 * (Position.Longitude -
                                       Local_Standard_Time_Meridian) +
                                Equation_Of_Time;
      
      Local_Solar_Time := (Float (Hour (Local_Standard_Time)) +
        Float (Minute (Local_Standard_Time)) / Minutes_In_Hour +
        Float (Second (Local_Standard_Time)) / Seconds_In_Hour) +
        (Time_Correction_Factor / Minutes_In_hour);
      
      Hour_Angle := Degrees_In_Hour * (Local_Solar_Time - 12.0);
      
      Declination := 23.45 * Sin (B, Degrees_Cycle);
      
      Return_Value.Elevation :=
        Arcsin (Sin (Declination, Degrees_Cycle) *
                  Sin (Position.Latitude, Degrees_Cycle) +
                  Cos (Declination, Degrees_Cycle) *
                  Cos (Position.Latitude, Degrees_Cycle) *
                  Cos (Hour_Angle, Degrees_Cycle), Degrees_Cycle);
      
      Return_Value.Azimuth := 
        Arccos ((Sin (Declination, Degrees_Cycle) *
                   Sin (Position.Latitude, Degrees_Cycle) -
                   Cos (Declination, Degrees_Cycle) *
                   Cos (Position.Latitude, Degrees_Cycle) *
                   Cos (Hour_Angle, Degrees_Cycle)) /
                  Cos (Return_Value.Elevation, Degrees_Cycle),
                Degrees_Cycle);
      
      -- The above Azimuth calculation is only good for the local morning.
      --   Check to see if the LST is after solar noon and compensate.
      if (Local_Solar_Time > 12.0) then
         Return_Value.Azimuth := 360.0 - Return_Value.Azimuth;
      end if;
      
      Return_Value.Angle_Time := Position.Current_Time;
      
      --  ----------------------------
      --  -- Diagnostic & Debugging --
      --  ----------------------------
      
      --  Put ("UTC:   ");
      --  Put (Ada.Calendar.Formatting.Image (Position.Current_Time, False, 0));
      --  New_Line;
      
      --  Put ("Local: ");
      --  Put (Ada.Calendar.Formatting.Image (Local_Standard_Time, False, 0));
      --  New_Line;
      
      --  Put ("Lat:   "); Put (Position.Latitude, 4, 3, 0); New_Line;
      --  Put ("Long:  "); Put (Position.Longitude, 4, 3, 0); New_Line;
      --  New_Line;
      --  Put ("LSTM:  "); Put (Local_Standard_Time_Meridian, 4, 3, 0); New_Line;
      --  Put ("B:     "); Put (B, 4, 3, 0); New_Line;
      --  Put ("EoT:   "); Put (Equation_Of_Time, 4, 3, 0); New_Line;
      --  Put ("TC:    "); Put (Time_Correction_Factor, 4, 3, 0); New_Line;
      --  Put ("LST:   "); Put (Local_Solar_Time, 4, 3, 0); New_Line;
      --  Put ("HRA:   "); Put (Hour_Angle, 4, 3, 0); New_Line;
      --  Put ("Decl:  "); Put (Declination, 4, 3, 0); New_Line;
      --  Put ("Az:    "); Put (Return_Value.Azimuth, 4, 3, 0); New_Line;
      --  Put ("El:    "); Put (Return_Value.Elevation, 4, 3, 0); New_Line;
      --  New_Line;
      
      return (Return_Value);
   end Calculate;
   
end Sunangle;


