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

with Ada.Calendar;
with GPS;

package Sunangle is
   
   ----------------------------------------------------------------------------
   -- Constants
   ----------------------------------------------------------------------------
   
   Degrees_Cycle   : constant Float :=  360.0;  -- For use in A.N.E_F.Sin()
   Degrees_In_Hour : constant Float :=   15.0;  -- Degrees of longitude per
                                                --   standard time zone
   Minutes_In_Hour : constant Float :=   60.0;
   Seconds_In_Hour : constant Float := 3600.0;
   
   
   ----------------------------------------------------------------------------
   -- Types
   ----------------------------------------------------------------------------
   
   subtype Azimuth_Type   is Float range   0.0 .. 360.0;
   subtype Elevation_Type is Float range -90.0 ..  90.0;
   
   type Angle_Type is
      record
	 Azimuth    : Azimuth_Type;
	 Elevation  : Elevation_Type;
         Angle_Time : Ada.Calendar.Time;
      end record;
   
   
   ----------------------------------------------------------------------------
   -- Functions & Procedures
   ----------------------------------------------------------------------------
   
   ----------------------------------------------------------------------------
   -- Calculate_Current_Angle
   --
   -- Purpose:
   --   This function returns the sun angle at the given position and time.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Current_Angle
     (Position : in     GPS.Position_Type) return Angle_Type;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_B
   --
   -- Purpose:
   --   This function returns the Equation of Time correction coeffecient for
   --   the given position and day.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_B
     (Position : in     GPS.Position_Type) return Float;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Declination
   --
   -- Purpose:
   --   This function returns the declination for the given position.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Declination
     (Position : in     GPS.Position_Type) return Float;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Equation_Of_Time
   --
   -- Purpose:
   --   This function returns the Equation of Time for the given position
   --   and day.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Equation_Of_Time
     (Position : in     GPS.Position_Type) return Float;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Local_Solar_Time
   --
   -- Purpose:
   --   This function returns the Local Solar Time for the given position
   --   and day.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Local_Solar_Time
     (Position : in     GPS.Position_Type) return Float;
      
      
   ----------------------------------------------------------------------------
   -- Calculate_Local_Standard_Time
   --
   -- Purpose:
   --   This function returns the Local Standard Time for the given position
   --   and day. Uses local time based on longitude.  This is purely geographic
   --   and doesn't use standard time zones, only adds or subtracts 1 hour
   --   for every 15 degrees East or West (respectively) from the Prime
   --   Meridian.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Local_Standard_Time
     (Position : in     GPS.Position_Type) return Ada.Calendar.Time;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Local_Standard_Time_Meridian
   --
   -- Purpose:
   --   This function returns the Local Standard Time for the given position
   --   and day. Uses local time based on longitude.  This is purely geographic
   --   and doesn't use standard time zones, only adds or subtracts 1 hour
   --   for every 15 degrees East or West (respectively) from the Prime
   --   Meridian.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Local_Standard_Time_Meridian
     (Position : in     GPS.Position_Type) return Float;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Time_Correction_Factor
   --
   -- Purpose:
   --   This function returns the Time Correction Factor for the given
   --   position.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Time_Correction_Factor
     (Position : in     GPS.Position_Type) return Float;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Sunrise
   --
   -- Purpose:
   --   This function returns the time of sunrise for the given position
   --   and day.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Sunrise
     (Position : in     Gps.Position_Type) return Ada.Calendar.Time;
   
   
   ----------------------------------------------------------------------------
   -- Calculate_Sunset
   --
   -- Purpose:
   --   This function returns the time of sunset for the given position
   --   and day.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function Calculate_Sunset
     (Position : in     Gps.Position_Type) return Ada.Calendar.Time;
   
   
   ----------------------------------------------------------------------------
   -- To_Julian_Day
   --
   -- Purpose:
   --   This function returns the day number of the year for the given time.
   -- Exceptions:
   --   None.
   ----------------------------------------------------------------------------
   function To_Julian_Day 
     (Current_Time : in     Ada.Calendar.Time) return Float;

end Sunangle;
