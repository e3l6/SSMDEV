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

with Ada.Real_Time;          use Ada.Real_Time;
with Interfaces;             use Interfaces;
with Interfaces.Bit_Types;   use Interfaces.Bit_Types;
with Interfaces.STM32.RCC;   use Interfaces.STM32.RCC;
with Interfaces.STM32.GPIO;  use Interfaces.STM32.GPIO;
with Interfaces.STM32.USART; use Interfaces.STM32.USART;
with System.STM32;           use System.STM32;
with System.BB.Parameters;
  pragma Unreferenced (System.BB.Parameters);

package body GPS is

   AF_USART2   : constant Interfaces.Bit_Types.UInt4 := 7;
   -- Alternate Function nr 7 from fig. 26 of the STM32F407 reference manual
   --   (ST RM0090) (p274) dtd May 2016.


   ----------------------------------------------------------------------------
   procedure Initialize (Baudrate : in Integer := 19_200) is
      use System.BB.Parameters;

      System_Clocks : constant RCC_System_Clocks := System.STM32.System_Clocks;
      APB_Clock     : constant Positive :=
                       Positive (System_Clocks.PCLK2);
      Int_Divider   : constant Positive := (25 * APB_Clock) / (4 * Baudrate);
      Frac_Divider  : constant Natural  := Int_Divider rem 100;

   begin
      Initialized := True;

      RCC_Periph.APB1ENR.USART2EN := 1;
      RCC_Periph.AHB1ENR.GPIOAEN  := 1;

      GPIOA_Periph.MODER.Arr     (2 .. 3) := (Mode_AF,     Mode_AF);
      GPIOA_Periph.OSPEEDR.Arr   (2 .. 3) := (Speed_50MHz, Speed_50MHz);
      GPIOA_Periph.OTYPER.OT.Arr (2 .. 3) := (Push_Pull,   Push_Pull);
      GPIOA_Periph.PUPDR.Arr     (2 .. 3) := (Pull_Up,     Pull_Up);
      GPIOA_Periph.AFRL.Arr      (2 .. 3) := (AF_USART2,   AF_USART2);

      USART2_Periph.BRR :=
        (DIV_Fraction => UInt4  (((Frac_Divider * 16 + 50) / 100) mod 16),
         DIV_Mantissa => UInt12 (Int_Divider / 100),
         others => <>);
      USART2_Periph.CR1 :=
        (UE => 1,
         RE => 1,
         TE => 1,
         others => <>);
      USART2_Periph.CR2 := (others => <>);
      USART2_Periph.CR3 := (others => <>);
   end Initialize;


   ----------------------------------------------------------------------------
   function Is_Tx_Ready return Boolean is
     (USART2_Periph.SR.TC = 1);


   ----------------------------------------------------------------------------
   function Is_Rx_Ready return Boolean is
     (USART2_Periph.SR.RXNE = 1);


   ----------------------------------------------------------------------------
   function Get return Character is
     (Character'Val (USART2_Periph.DR.DR));


   ----------------------------------------------------------------------------
   procedure Get (C : out character) is
   begin
      while not Is_Rx_Ready loop
         delay until Clock + Microseconds (1);
      end loop;

      C := Get;
   end Get;


   ----------------------------------------------------------------------------
   procedure Get (Item : out String) is
   begin
      for J in Item'Range loop
         Get (Item (J));
      end loop;
   end Get;


   ----------------------------------------------------------------------------
   procedure New_Line is
   begin
      if Use_Cr_Lf_For_New_Line then
         Put (ASCII.CR);
      end if;

      Put (ASCII.LF);
   end New_Line;


   ----------------------------------------------------------------------------
   procedure Put (C : Character) is
   begin
      while not Is_Tx_Ready loop
         delay until Clock + Microseconds (1);
      end loop;

      USART2_Periph.DR.DR := Character'Pos (C);
   end Put;


   ----------------------------------------------------------------------------
   procedure Put (Item : String) is
   begin
      for J in Item'Range loop
         Put (Item (J));
      end loop;
   end Put;


   ----------------------------------------------------------------------------
   procedure Put_Line (Item : String) is
   begin
      Put (Item);
      New_Line;
   end Put_Line;


   ----------------------------------------------------------------------------
   function Use_Cr_Lf_For_New_Line return Boolean is (True);
end GPS;
