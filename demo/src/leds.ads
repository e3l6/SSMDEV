-------------------------------------------------------------------------------
-- LEDS
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
--   This package sets up access to the GPIO pins attached to the four onboard
--   LEDs of the STM32F407 Discovery microcontroller. It is based on code found 
--   in Jim Caillard's "ada-2hu" repository at https://github/yaon/ada-2hu
-------------------------------------------------------------------------------

with STM32F4;       use STM32F4;
with STM32F4.GPIO;  use STM32F4.GPIO;

package LEDs is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Pin range Pin_12 .. Pin_15;

   --  As a result of the representation clause, avoid iterating directly over
   --  the type since that will require an implicit lookup in the generated 
   --  code of the loop.  Such usage seems unlikely so this direct 
   --  representation is reasonable, and efficient.

   Green  : User_LED renames Pin_12;
   Orange : User_LED renames Pin_13;
   Red    : User_LED renames Pin_14;
   Blue   : User_LED renames Pin_15;

   procedure Off (This : User_LED) with Inline;
   procedure On  (This : User_LED) with Inline;

   procedure All_Off with Inline;
   procedure All_On  with Inline;

end LEDs;
