-----------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2014, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

package body BDD.Tables is

   ------------
   -- Create --
   ------------

   function Create return Table is
      Self : Table;
      R    : constant access Table_Record := new Table_Record;
   begin
      Self.Set (R);
      return Self;
   end Create;

   -----------------------
   -- Add_Row_As_String --
   -----------------------

   procedure Add_Row_As_String
     (Self : Table;
      Row  : String)
   is
      Rows : constant Unbounded_String_Array :=
        Split (Row, '|', Omit_Empty_Lines => False);
      R    : Integer;
   begin
      if Self.Get.Names.Is_Empty then
         for Col in Rows'First + 1 .. Rows'Last loop
            Self.Set_Column_Name
              (Col - 1,
               Trim (To_String (Rows (Col)), Ada.Strings.Both));
         end loop;
      else
         R := Self.Height + 1;
         for Col in Rows'First + 1 .. Rows'Last loop
            Self.Put
              (Row => R, Column => Col - 1,
               Value => Trim (To_String (Rows (Col)), Ada.Strings.Both));
         end loop;
      end if;
   end Add_Row_As_String;

   ---------------------
   -- Set_Column_Name --
   ---------------------

   procedure Set_Column_Name
     (Self   : Table;
      Column : Positive;
      Name   : String)
   is
      R : constant not null access Table_Record := Self.Get;
   begin
      R.Names.Reserve_Capacity
        (Ada.Containers.Count_Type (Integer'Max (Column, Self.Width)));
      if Column > R.Names.Last_Index then
         R.Names.Insert (Column, Name);
      else
         R.Names.Replace_Element (Column, Name);
      end if;
   end Set_Column_Name;

   ---------
   -- Put --
   ---------

   procedure Put
     (Self   : Table;
      Column : Positive;
      Row    : Positive;
      Value  : String)
   is
      procedure Do_Update (R : in out String_Vectors.Vector);
      procedure Do_Update (R : in out String_Vectors.Vector) is
      begin
         R.Insert (Column, Value);
      end Do_Update;

   begin
      if Row > Integer (Self.Get.Rows.Length) then
         declare
            R : String_Vectors.Vector;
         begin
            R.Reserve_Capacity
              (Ada.Containers.Count_Type (Integer'Max (Column, Self.Width)));
            Self.Get.Rows.Append (R);
         end;
      end if;

      Self.Get.Rows.Update_Element (Row, Do_Update'Unrestricted_Access);
   end Put;

   ---------
   -- Get --
   ---------

   function Get
     (Self   : Table;
      Column : Positive;
      Row    : Positive)
      return String
   is
   begin
      return Self.Get.Rows.Element (Row).Element (Column);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self   : Table;
      Column : String;
      Row    : Positive)
      return String is
   begin
      return Self.Get (Self.Column_Number (Column), Row);
   end Get;

   -----------
   -- Width --
   -----------

   function Width (Self : Table) return Natural is
   begin
      return Natural (Self.Get.Names.Length);
   end Width;

   ------------
   -- Height --
   ------------

   function Height (Self : Table) return Natural is
   begin
      return Natural (Self.Get.Rows.Length);
   end Height;

   -------------------
   -- Column_Number --
   -------------------

   function Column_Number (Self : Table; Name : String) return Natural is
      Index : Positive := 1;
   begin
      for C of Self.Get.Names loop
         if C = Name then
            return Index;
         end if;
         Index := Index + 1;
      end loop;
      return 0;
   end Column_Number;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self : Table; File : Ada.Text_IO.File_Type; Prefix : String := "")
   is
      R : constant access Table_Record := Self.Get;
      W : constant Integer := Self.Width;

      Widths : array (1 .. W) of Natural := (others => 0);
      Idx    : Integer;

   begin
      Idx := Widths'First;
      for N of R.Names loop
         Widths (Idx) := Integer'Max (Widths (Idx), N'Length);
         Idx := Idx + 1;
      end loop;

      for Row_Content of R.Rows loop
         Idx := Widths'First;
         for V of Row_Content loop
            Widths (Idx) := Integer'Max (Widths (Idx), V'Length);
            Idx := Idx + 1;
         end loop;
      end loop;

      Put (File, Prefix);
      Idx := Widths'First;
      for N of R.Names loop
         Put (File, "| "
              & N
              & (1 .. Widths (Idx) - N'Length => ' ')
              & " ");
         Idx := Idx + 1;
      end loop;
      Put_Line (File, "|");

      for Row_Content of R.Rows loop
         Put (File, Prefix);

         Idx := Widths'First;
         for V of Row_Content loop
            Put (File, "| "
                 & V
                 & (1 .. Widths (Idx) - V'Length => ' ')
                 & " ");
            Idx := Idx + 1;
         end loop;

         Put_Line (File, "|");
      end loop;
   end Display;

end BDD.Tables;
