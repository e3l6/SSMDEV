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

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;        use Ada.Float_Text_IO;

package body Sunangle is
   
   ----------------------------------------------------------------------------
   function Calculate_Current_Angle
     (Position : in     GPS.Position_Type) return Angle_Type is
      
      Declination      : Float;
      Hour_Angle       : Float;
      Local_Solar_Time : Float;
      Return_Value     : Angle_Type;
            
   begin
      
      Local_Solar_Time := Calculate_Local_Solar_Time (Position);
      Declination      := Calculate_Declination      (Position);
      Hour_Angle       := Degrees_In_Hour * (Local_Solar_Time - 12.0);
      
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
      
      --  ---------------
      --  -- Debugging --
      --  ---------------
      
      --  Put ("UTC:   ");
      --  Put (Ada.Calendar.Formatting.Image (Position.Current_Time, False, 0));
      --  New_Line;
      
      --  Put ("Local: ");
      --  Put (Ada.Calendar.Formatting.Image (Local_Solar_Time, False, 0));
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
   end Calculate_Current_Angle;
   
   
   ----------------------------------------------------------------------------
   function Calculate_B
     (Position : in     GPS.Position_Type) return Float is      
      
      Day_Of_Year         : Float;
      Local_Standard_Time : Time;
      Return_Value        : Float;
      
   begin
      Local_Standard_Time := Calculate_Local_Standard_Time (Position);      
      Day_Of_Year         := To_Julian_Day (Local_Standard_Time);
      Return_value        := (360.0 / 365.0) * (Day_Of_Year - 81.0);
      
      return (Return_Value);
   end Calculate_B;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Declination
     (Position : in     GPS.Position_Type) return Float is
      
      B            : Float;
      Return_Value : Float;
      
   begin
      B            := Calculate_B (Position);
      Return_Value := 23.45 * Sin (B, Degrees_Cycle);
      
      return (Return_Value);
   end Calculate_Declination;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Equation_Of_Time
     (Position : in     GPS.Position_Type) return Float is
      
      B            : Float;  -- Correction coefficient
      Return_Value : Float;
      
   begin
      B := Calculate_B (Position);
      
      Return_Value := 
        9.87 * Sin (2.0 * B, Degrees_Cycle) - 
        7.53 * Cos (B, Degrees_Cycle) -
        1.5  * Sin (B, Degrees_Cycle);
      
      return (Return_Value);
   end Calculate_Equation_Of_Time;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Local_Solar_Time
     (Position : in     GPS.Position_Type) return Float is
      
      Local_Standard_Time    : Time;
      Time_Correction_Factor : Float;
      Return_Value           : Float;
      
   begin
      Local_Standard_Time       := Calculate_Local_Standard_Time    (Position);
      Time_Correction_Factor    := Calculate_Time_Correction_Factor (Position);
      
      Return_Value :=
        (Float (Hour (Local_Standard_Time)) +
           Float (Minute (Local_Standard_Time)) / Minutes_In_Hour +
           Float (Second (Local_Standard_Time)) / Seconds_In_Hour) +
        (Time_Correction_Factor / Minutes_In_Hour);
      
      return (Return_Value);
   end Calculate_Local_Solar_Time;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Local_Standard_Time
     (Position : in     GPS.Position_Type) return Time is
      
      Local_Standard_Time_Meridian : Float;
      Return_Value                 : Time;
      
   begin
      Local_Standard_Time_Meridian :=
        Calculate_Local_Standard_Time_Meridian (Position);
      
      -- Use local time based on longitude.  This is purely geographic and
      --   doesn't use standard time zones, only adds or subtracts 1 hour
      --   for every 15 degrees East or West (respectively) from the Prime
      --   Meridian.
      Return_Value :=
        Position.Current_Time +
        Duration (Local_Standard_Time_Meridian / 
                    Degrees_In_Hour *
                    Seconds_In_Hour);
      
      return Return_Value;
   end Calculate_Local_Standard_Time;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Local_Standard_Time_Meridian
     (Position : in     GPS.Position_Type) return Float is
      
      Return_Value : Float;
      
   begin
      Return_value :=
        Float'Truncation (Position.Longitude / Degrees_In_hour) *
        Degrees_In_Hour;
      
      return (Return_Value);
   end Calculate_Local_Standard_Time_Meridian;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Sunrise 
     (Position : in     GPS.Position_Type) return Time is
      
      Declination            : Float;
      Time_Correction_Factor : Float;
      Number_Of_Seconds      : Day_Duration;
      Return_Value           : Time;
      
   begin
      Declination            := Calculate_Declination            (Position);
      Time_Correction_Factor := Calculate_Time_Correction_Factor (Position);
      
      Number_Of_Seconds := 
        Day_Duration ((12.0 - (1.0 / 15.0) * 
                         Arccos (-Tan (Position.Latitude, Degrees_Cycle) * 
                                   Tan (Declination, Degrees_Cycle),
                                 Degrees_Cycle) -
                         (Time_Correction_Factor / 60.0)) * Seconds_In_Hour);
      
      Return_Value :=
        Ada.Calendar.Time_Of 
        (Year    => Ada.Calendar.Year  (Position.Current_Time),
         Month   => Ada.Calendar.Month (Position.Current_Time),
         Day     => Ada.Calendar.Day   (Position.Current_Time),
         Seconds => Number_Of_Seconds);
      
      return (Return_Value);
   end Calculate_Sunrise;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Sunset
     (Position : in     GPS.Position_Type) return Time is
      
      Declination            : Float;
      Time_Correction_Factor : Float;
      Number_Of_Seconds      : Day_Duration;
      Return_Value           : Time;
      
   begin
      Declination            := Calculate_Declination            (Position);
      Time_Correction_Factor := Calculate_Time_Correction_Factor (Position);
      
      Number_Of_Seconds := 
        Day_Duration ((12.0 + (1.0 / 15.0) * 
                         Arccos (-Tan (Position.Latitude, Degrees_Cycle) * 
                                   Tan (Declination, Degrees_Cycle),
                                 Degrees_Cycle) -
                         (Time_Correction_Factor / 60.0)) * Seconds_In_Hour);
      
      Return_Value :=
        Ada.Calendar.Time_Of 
        (Year    => Ada.Calendar.Year  (Position.Current_Time),
         Month   => Ada.Calendar.Month (Position.Current_Time),
         Day     => Ada.Calendar.Day   (Position.Current_Time),
         Seconds => Number_Of_Seconds);
      
      return (Return_Value);
   end Calculate_Sunset;
   
   
   ----------------------------------------------------------------------------
   function Calculate_Time_Correction_Factor
     (Position : in     GPS.Position_Type) return Float is
      
      Equation_Of_Time             : Float;
      Local_Standard_Time_Meridian : Float;
      Return_Value                 : Float;
      
   begin
      Equation_Of_Time := Calculate_Equation_Of_Time (Position);
      
      Local_Standard_Time_Meridian :=
        Calculate_Local_Standard_Time_Meridian (Position);
      
      Return_Value :=
        4.0 * (Position.Longitude - Local_Standard_Time_Meridian) +
        Equation_Of_Time;
      
      return (Return_Value);
   end Calculate_Time_Correction_Factor;
   
   
   ----------------------------------------------------------------------------
   function To_Julian_Day (Current_Time : in     Time) return Float is
      
      Beginning_Of_Year  : Time;
      Current_Year       : Year_Number;
      Days_Difference    : Day_Count;
      Seconds_Difference : Duration;
      Leap_Seconds       : Leap_Seconds_Count;
      
   begin
      Current_Year      := Ada.Calendar.Formatting.Year (Current_Time);
      
      Beginning_Of_Year :=                                  -- Jan 1, this year
        Ada.Calendar.Formatting.Time_Of (Current_Year, 1, 1);
            
      Difference (Current_Time,
                  Beginning_Of_Year,
                  Days_Difference,
                  Seconds_Difference,
                  Leap_Seconds);
      
      return Float (Days_Difference);
   end To_Julian_Day;
      
end Sunangle;


