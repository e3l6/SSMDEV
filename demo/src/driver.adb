-------------------------------------------------------------------------------
-- DRIVER
--
-- Copyright © 2016 Eric Laursen
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- Purpose:
--   This package reads the GPS fix data (GGA mesg) NMEA sentence output from
--   the GPS and displays it on the console.
-------------------------------------------------------------------------------

with Ada.Real_Time;          use Ada.Real_Time;
with Ada.Text_IO;            use Ada.Text_IO;
with Interfaces.STM32.USART;
with GPS;

package body Driver is

   package USART renames Interfaces.STM32.USART; use USART;

   task body Controller is
          C : Character;
   begin
      GPS.Initialize;
      Put_Line ("GPS UART initialized at 19200 bps");

      Put_Line ("Setting GPS update rate to 1 Hz");
      GPS.Put_Line ("$PMTK220,1000*1F");

      Put      ("Setting GPS data output frequencies (GPS fix data only, ");
      Put_Line ("every 5 updates)");
      GPS.Put_Line ("$PMTK314,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0*2D");

--        Put_Line ("Querying current nav speed threshold");
--        GPS.Put_Line ("$PMTK447*35");

--        Put_Line ("Setting GPS baudrate to 57600 bps");
--        GPS.Put_Line ("$PMTK251,57600*2C");
--
--        GPS.Initialize (57_600);
--        Put_Line ("GPS UART reinitialize at 57600 bps");

      loop
         GPS.Get (C);
         Put (C);
         delay until Clock + Microseconds (50);
      end loop;
   end Controller;

end Driver;
