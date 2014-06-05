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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with BDD.Media;         use BDD.Media;
with GNATCOLL.Traces;   use GNATCOLL.Traces;

package body BDD.Asserts is
   -----------------
   -- BDD.Asserts --
   -----------------

   Me : constant Trace_Handle := Create ("BDD.ASSERTS");

   type Error_With_Table is new Error_Details with record
      Val1, Val2 : BDD.Tables.Table;
   end record;
   type Error_With_Table_Access is access all Error_With_Table'Class;

   overriding procedure Display
     (Self   : not null access Error_With_Table;
      Output : not null access Media_Writer'Class;
      Status : Scenario_Status;
      Prefix : String := "");

   procedure Diff
     (Expected, Actual : BDD.Tables.Table;
      On_Error : access procedure (Msg : String);
      On_Cell  : access function
        (Row                : Integer;
         Column_In_Expected : Integer;
         Expected, Actual   : String;
         Status             : Scenario_Status)
      return Boolean);
   --  Do a diff between the two tables, and calls On_Cell for each cell in
   --  the expected table.
   --  When On_Cell returns False, stops iterating.
   --  On_Error is called when the diff could not be completed

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Val : Boolean;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
   is
   begin
      if not Val then
         Raise_From_Msg
           (Msg      => Msg,
            Details  => "",
            Location => Location,
            Entity   => Entity);
      end if;
   end Assert;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Expected, Actual : BDD.Tables.Table;
      On_Error : access procedure (Msg : String);
      On_Cell  : access function
        (Row                : Integer;
         Column_In_Expected : Integer;
         Expected, Actual   : String;
         Status             : Scenario_Status)
      return Boolean)
   is
      Columns : array (1 .. Expected.Height) of Integer;
      --  What column of Actual matches the column of Expected.

   begin
      --  Do we have column names ?
      if Actual.Get_Column_Name (1) /= "" then
         for C in Columns'Range loop
            Columns (C) :=
              Actual.Column_Number (Expected.Get_Column_Name (C));

            if Columns (C) = 0 then
               On_Error
                 ("Column not found in actual result: '"
                  & Expected.Get_Column_Name (C) & "'");
               return;
            end if;
         end loop;
      else
         for C in Columns'Range loop
            Columns (C) := C;
         end loop;
      end if;

      For_All_Rows :
      for Row in 1 .. Expected.Height loop
         for Column in 1 .. Expected.Width loop
            declare
               E : constant String := Trim
                 (Expected.Get (Column => Column, Row => Row),
                  Ada.Strings.Both);
               A : constant String := Trim
                 (Actual.Get (Column => Columns (Column), Row => Row),
                  Ada.Strings.Both);
               Status : constant Scenario_Status :=
                 (if E /= A then Status_Failed else Status_Passed);
            begin
               exit For_All_Rows when not On_Cell
                 (Row                => Row,
                  Column_In_Expected => Column,
                  Expected           => E,
                  Actual             => A,
                  Status             => Status);
            end;
         end loop;
      end loop For_All_Rows;
   end Diff;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Expected, Actual : BDD.Tables.Table;
      Msg      : String := "";
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity)
   is
      procedure On_Error (Msg : String);
      procedure On_Error (Msg : String) is
         E : constant Error_Details_Access := new Error_Details;
      begin
         E.Set_Details
           (Details  => Msg,
            Msg      => "",
            Location => Location,
            Entity   => Entity);
         E.Raise_Exception;
      end On_Error;

      function On_Cell
        (Row                : Integer;
         Column_In_Expected : Integer;
         E, A               : String;
         Status             : Scenario_Status) return Boolean;
      function On_Cell
        (Row                : Integer;
         Column_In_Expected : Integer;
         E, A               : String;
         Status             : Scenario_Status) return Boolean
      is
         Error  : Error_With_Table_Access;
      begin
         if Status = Status_Failed then
            Trace (Me, "Diff row=" & Row'Img
                   & " column=" & Column_In_Expected'Img
                   & " '" & E & "' != '" & A & "'");

            Error := new Error_With_Table;
            Error.Set_Details
              (Details  => "",
               Msg      => Msg,
               Location => Location,
               Entity   => Entity);
            Error.Val1 := Expected;
            Error.Val2 := Actual;
            Error.Raise_Exception;
            return False;
         end if;
         return True;
      end On_Cell;

   begin
     Diff (Expected, Actual, On_Error'Access, On_Cell'Access);
   end Assert;

   -------------
   -- Display --
   -------------

   overriding procedure Display
     (Self   : not null access Error_With_Table;
      Output : not null access Media_Writer'Class;
      Status : Scenario_Status;
      Prefix : String := "")
   is
      pragma Unreferenced (Status);
      Current_Row : Integer := -1;
      Widths : array (1 .. Self.Val1.Width) of Natural := (others => 0);

      procedure On_Error (Msg : String) is null;

      function Compute_Width
        (Row                : Integer;
         Column_In_Expected : Integer;
         E, A               : String;
         Status             : Scenario_Status) return Boolean;
      function Compute_Width
        (Row                : Integer;
         Column_In_Expected : Integer;
         E, A               : String;
         Status             : Scenario_Status) return Boolean
      is
         pragma Unreferenced (Row);
         W : Integer;
      begin
         if Status = Status_Failed then
            W := A'Length + E'Length + 4;
         else
            W := A'Length;
         end if;

         Widths (Column_In_Expected) := Integer'Max
           (Widths (Column_In_Expected), W);
         return True;
      end Compute_Width;

      function On_Cell
        (Row                : Integer;
         Column_In_Expected : Integer;
         E, A               : String;
         Status             : Scenario_Status) return Boolean;
      function On_Cell
        (Row                : Integer;
         Column_In_Expected : Integer;
         E, A               : String;
         Status             : Scenario_Status) return Boolean
      is
      begin
         if Row /= 1 and then Row /= Current_Row then
            Output.End_Row;
         end if;

         if Row /= Current_Row then
            Current_Row := Row;
            Output.Start_Row (Indent => Prefix);
         end if;

         if Status = Status_Failed then
            Output.Display_Cell
              (A & " /= " & E,
               Cell_Width => Widths (Column_In_Expected),
               Header     => False,
               Status     => Status);
         else
            Output.Display_Cell
              (A,
               Cell_Width => Widths (Column_In_Expected),
               Header     => False,
               Status     => Status);
         end if;
         return True;
      end On_Cell;

   begin
      Diff (Self.Val1, Self.Val2, On_Error'Access, Compute_Width'Access);

      Output.Start_Table;
      Diff (Self.Val1, Self.Val2, On_Error'Access, On_Cell'Access);
      Output.End_Row;
      Output.End_Table;
   end Display;

end BDD.Asserts;
