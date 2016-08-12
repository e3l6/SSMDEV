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

with STM32F4.Reset_Clock_Control; use STM32F4.Reset_Clock_Control;
with STM32F4.SYSCONFIG_Control;   use STM32F4.SYSCONFIG_Control;
with System;                      use System;

package body LEDs is

   LED_Port : aliased GPIO_Port
     with Volatile, Address => System'To_Address (GPIOD_Base);
   LED_Pins : constant GPIO_Pins (1 .. 4) := (Pin_12, Pin_13, Pin_14, Pin_15);


   ----------------------------------------------------------------------------
   procedure Off (This : User_LED) is
   begin
      Clear (LED_Port, This);
   end Off;


   ----------------------------------------------------------------------------
   procedure On (This : User_LED) is
   begin
      Set (LED_Port, This);
   end On;


   ----------------------------------------------------------------------------
   procedure All_Off is
   begin
      Clear (LED_Port, LED_Pins);
   end All_Off;


   ----------------------------------------------------------------------------
   procedure All_On is
   begin
      Set (LED_Port, LED_Pins);
   end All_On;


   ----------------------------------------------------------------------------
   procedure Initialize is
      LED_Config : GPIO_Port_Configuration;
   begin
      --  Enable clock for GPIO-D
      GPIOD_Clock_Enable;

      --  Configure PD12-15
      LED_Config := (Mode_Out, Push_Pull, Speed_100MHz, Floating, False);
      Configure_IO (LED_Port, LED_Pins, LED_Config);
   end Initialize;

begin
   Initialize;
end LEDs;
