------------------------------------------------------------------------------
-- MOTOR
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
--   This package demonstrates the ability to control a stepper motor attached
--   to the green LED GPIO pin of the STM32F407 Discovery microcontroller.
------------------------------------------------------------------------------

with LEDs;           use LEDs;
with Button;         use Button;
with Ada.Real_Time;  use Ada.Real_Time;

package body Motor is

   task body Controller is

   begin
      loop
         if Button.Current_Direction = Counterclockwise then
            Off (Red);
         else
            On (Red);
         end if;

         On (Green);

         delay until Clock + Microseconds (250);

         Off (Green);

         delay until Clock + Microseconds (250);
      end loop;
   end Controller;

end Motor;
