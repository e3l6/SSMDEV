-------------------------------------------------------------------------------
-- BUTTON
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
--   This package attaches the user button of the STM32F407 Discovery
--   microcontroller to an interrupt. It is based on code found in
--   Jim Caillard's "ada-2hu" repository at https://github/yaon/ada-2hu
-------------------------------------------------------------------------------

package Button is
   pragma Elaborate_Body;

   type Directions is (Clockwise, Counterclockwise);

   function Current_Direction return Directions;

end Button;
