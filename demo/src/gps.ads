-------------------------------------------------------------------------------
-- GPS
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
--   This package interfaces to the Adafruit Ultimate GPS Breakout v3
--   (https://www.adafruit.com/product/746) based on the MediaTek MT3339
--   chipset. It uses the USART2 interface of the STM32F407 Discovery board.
--
--   It is based on System.Text_IO package of the GNAT Runtime Components
--   for the Ravenscar SFP profile found in AdaCore's GNAT GPL 2016 for
--   ARM ELF format (hosted on Linux).
-------------------------------------------------------------------------------

package GPS is
   --  The interface uses two subprograms for each direction: one for the ready
   --  status and one for the action. This is done on purpose to avoid busy
   --  waiting loops in the body.

   procedure Initialize (Baudrate : in Integer := 19_200);
   --  Must be called before all other subprograms to initialize the service.
   --  We avoid the use of elaboration to make this package preelaborated.

   Initialized : Boolean := False;
   --  Set to True (by Initialize) when the service is initialized. Having this
   --  variable outside allows reinitialization of the service.

   --------------
   --  Output  --
   --------------

   function Is_Tx_Ready return Boolean;
   --  Return True if it is possible to call Put. This function can be used for
   --  checking that the output register of an UART is empty before write a
   --  new character on it. For non blocking output system, this function can
   --  always return True. Once this function has returned True, it must always
   --  return True before the next call to Put.

   procedure New_Line;
   --  Write a LF or CR + LF to the GPS.

   procedure Put (C : Character);
   --  Write a character to the GPS. Must be called only when Is_Tx_Ready
   --  has returned True before, otherwise its behaviour is undefined.

   procedure Put (Item : String);
   --  Write an entire string to the GPS.

   procedure Put_Line (Item : String);
   --  Write an entire string to the GPS, followed by New_line.

   function Use_Cr_Lf_For_New_Line return Boolean;
   --  Return True if New_Line should output CR + LF, otherwise it will output
   --  only LF.

   -------------
   --  Input  --
   -------------

   function Is_Rx_Ready return Boolean;
   --  Return True is a character can be read by Get. On systems where is it
   --  difficult or impossible to know wether a character is available, this
   --  function can always return True and Get will be blocking.

   function Get return Character;
   --  Read a character from the GPS. Must be called only when Is_Rx_Ready
   --  has returned True, otherwise behaviour is undefined.

   procedure Get (C : out character);
   --  Performs Is_Rx_Ready on the GPS USART register then gets a character
   --  when Is_Rx_Ready = True.

   procedure Get (Item : out String);
   --  Read an entire string from the GPS.
end GPS;
