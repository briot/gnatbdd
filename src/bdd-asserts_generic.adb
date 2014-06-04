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

with BDD.Formatters;        use BDD.Formatters;

package body BDD.Asserts_Generic is

   Current_Exception : Assert_Error := No_Error;
   --  The details associated with the current exception.
   --  There is only one exception being processed at a time (in a task-free
   --  environment), so we need only store a single instance, to avoid
   --  wasting memory. Since we do not know when an exception_occurrence is
   --  no longer used, we would not know when to free the memory otherwise).

   ---------
   -- Get --
   ---------

   function Get (E : Exception_Occurrence) return Assert_Error is
   begin
      if Exception_Identity (E) = Unexpected_Result'Identity then
         return Current_Exception;
      else
         return No_Error;
      end if;
   end Get;

   -------------
   -- Details --
   -------------

   function Details (Self : Assert_Error) return Error_Details_Access is
   begin
      return Error_Details_Access (Self.Get);
   end Details;

   -----------------
   -- Set_Details --
   -----------------

   procedure Set_Details
     (Self     : not null access Error_Details;
      Details  : String := "";
      Msg      : String := "";
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity)
   is
   begin
      Self.Details  := To_Unbounded_String (Details);
      Self.Msg      := To_Unbounded_String (Msg);
      Self.Location := To_Unbounded_String ("at " & Entity & "::" & Location);
   end Set_Details;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (Self : not null access Error_Details) is
   begin
      Current_Exception.Set (Self);

      raise Unexpected_Result with
        To_String (Self.Msg) & ' ' & To_String (Self.Details)
        & To_String (Self.Location);
   end Raise_Exception;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self   : Assert_Error;
      Term   : not null access GNATCOLL.Terminal.Terminal_Info'Class;
      File   : Ada.Text_IO.File_Type;
      Prefix : String := "")
   is
   begin
      Display (Self.Details, Term, File, Prefix);
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self   : not null access Error_Details;
      Term   : not null access GNATCOLL.Terminal.Terminal_Info'Class;
      File   : Ada.Text_IO.File_Type;
      Prefix : String := "")
   is
      pragma Unreferenced (Term);
   begin
      if Self.Msg /= "" then
         Indent (File, To_String (Self.Msg), Prefix => Prefix);
         if Element (Self.Msg, Length (Self.Msg)) /= ASCII.LF then
            New_Line (File);
         end if;
      end if;

      if Self.Details /= "" then
         Indent (File, To_String (Self.Details), Prefix => Prefix);
         if Element (Self.Details, Length (Self.Details)) /= ASCII.LF then
            New_Line (File);
         end if;
      end if;

      Indent (File, To_String (Self.Location), Prefix => Prefix);
      New_Line (File);
   end Display;

   --------------------
   -- From_Exception --
   --------------------

   function From_Exception (E : Exception_Occurrence) return Assert_Error is
      Error : constant Error_Details_Access := new Error_Details;
      Result : Assert_Error;
   begin
      Error.Set_Details (Details => Exception_Information (E));
      Result.Set (Error);
      return Result;
   end From_Exception;

   -------------
   -- Asserts --
   -------------

   package body Asserts is

      ------------
      -- Assert --
      ------------

      procedure Assert
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
         Error : Error_Details_Access;
      begin
         if not Operator (Val1, Val2) then
            Error := new Error_Details;
            Error.Set_Details
              (Msg     => Msg,
               Details =>
                 Image (Val1) & ' ' & Not_Operator_Image & ' ' & Image (Val2),
               Location => Location,
               Entity   => Entity);
            Error.Raise_Exception;
         end if;
      end Assert;
   end Asserts;

end BDD.Asserts_Generic;
