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

with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Calendar;                       use Ada.Calendar;
with Ada.Calendar.Arithmetic;            use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;            use Ada.Calendar.Formatting;
with GPS;

-- with Ada.Text_IO;              use Ada.Text_IO;
-- with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
-- with Ada.Float_Text_IO;        use Ada.Float_Text_IO;

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
         Angle_Time : Time;
      end record;
   
   ---------------------------------------------------------------------------
   -- Functions & Procedures
   ----------------------------------------------------------------------------
   
   function Calculate (Position : in GPS.Position_Type) return Angle_Type;
   
end Sunangle;
