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

with Ada.Interrupts.Names;
with Ada.Real_Time;                use Ada.Real_Time;
with STM32F4;                      use STM32F4;
with STM32F4.GPIO;                 use STM32F4.GPIO;
with STM32F4.Reset_Clock_Control;  use STM32F4.Reset_Clock_Control;
with STM32F4.SYSCONFIG_Control;    use STM32F4.SYSCONFIG_Control;
with System;                       use System;

package body Button is

   protected Button is
      pragma Interrupt_Priority;

      function Current_Direction return Directions;

   private
      procedure Interrupt_Handler;
      pragma Attach_Handler
         (Interrupt_Handler,
          Ada.Interrupts.Names.EXTI0_Interrupt);

      Direction : Directions := Clockwise;  -- arbitrary
      Last_Time : Time := Clock;
   end Button;

   Button_Port   : aliased GPIO_Port with
     Volatile, Address => System'To_Address (GPIOA_Base);
   Button_Pin    : constant GPIO_Pin := Pin_0;

   Debounce_Time : constant Time_Span := Milliseconds (500);


   ----------------------------------------------------------------------------
   protected body Button is

      function Current_Direction return Directions is
      begin
         return Direction;
      end Current_Direction;

      procedure Interrupt_Handler is
         Now : constant Time := Clock;
      begin
         Clear_External_Interrupt (Button_Pin);

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            if Direction = Counterclockwise then
               Direction := Clockwise;
            else
               Direction := Counterclockwise;
            end if;

            Last_Time := Now;
         end if;
      end Interrupt_Handler;

   end Button;


   ----------------------------------------------------------------------------
   function Current_Direction return Directions is
   begin
      return Button.Current_Direction;
   end Current_Direction;


   ----------------------------------------------------------------------------
   procedure Initialize is
      Button_Config : GPIO_Port_Configuration;
   begin
      GPIOA_Clock_Enable;

      --  Configure PA0
      Button_Config := (Mode_In, Push_Pull, Speed_100MHz, Pull_Down, False);

      Configure_IO      (Button_Port, Button_Pin, Button_Config);
      Configure_Trigger (Button_Port, Button_Pin, Interrupt_Rising_Edge);
   end Initialize;

begin
   Initialize;
end Button;
