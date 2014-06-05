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

   procedure Raise_Exception (Self : not null access Error_Details'Class) is
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
      Output : not null access BDD.Media.Media_Writer'Class;
      Status : Scenario_Status;
      Prefix : String := "")
   is
   begin
      Display (Self.Details, Output, Status, Prefix);
   end Display;

   -------------
   -- Display --
   -------------

   procedure Display
     (Self   : not null access Error_Details;
      Output : not null access BDD.Media.Media_Writer'Class;
      Status : Scenario_Status;
      Prefix : String := "")
   is
   begin
      Output.Start_Step_Messages (Status => Status);

      if Self.Msg /= "" then
         Output.Indent (To_String (Self.Msg), Prefix => Prefix);
         if Element (Self.Msg, Length (Self.Msg)) /= ASCII.LF then
            Output.New_Line;
         end if;
      end if;

      if Self.Details /= "" then
         Output.Indent (To_String (Self.Details), Prefix => Prefix);
         if Element (Self.Details, Length (Self.Details)) /= ASCII.LF then
            Output.New_Line;
         end if;
      end if;

      Output.Indent (To_String (Self.Location), Prefix => Prefix);
      Output.New_Line;
      Output.End_Step_Messages;
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

   --------------------
   -- Raise_From_Msg --
   --------------------

   procedure Raise_From_Msg
     (Msg, Details : String;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity)
   is
      Error : constant Error_Details_Access := new Error_Details;
   begin
      Error.Set_Details
        (Msg     => Msg,
         Details => Details,
         Location => Location,
         Entity   => Entity);
      Error.Raise_Exception;
   end Raise_From_Msg;

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
      begin
         if not Operator (Val1, Val2) then
            Raise_From_Msg
              (Msg     => Msg,
               Details =>
                 Image (Val1) & ' ' & Not_Operator_Image & ' ' & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert;
   end Asserts;

   --------------------
   -- Asserts_Simple --
   --------------------

   package body Asserts_Simple is

      ------------
      -- Assert --
      ------------

      procedure Assert
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if not (Val1 = Val2) then
            Raise_From_Msg
              (Msg     => Msg,
               Details => Image (Val1) & " /= " & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert;

      ----------------------
      -- Assert_Not_Equal --
      ----------------------

      procedure Assert_Not_Equal
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if Val1 = Val2 then
            Raise_From_Msg
              (Msg     => Msg,
               Details => Image (Val1) & " = " & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert_Not_Equal;

      ----------------------
      -- Assert_Less_Than --
      ----------------------

      procedure Assert_Less_Than
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if not (Val1 < Val2) then
            Raise_From_Msg
              (Msg     => Msg,
               Details => Image (Val1) & " >= " & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert_Less_Than;

      --------------------------
      -- Assert_Less_Or_Equal --
      --------------------------

      procedure Assert_Less_Or_Equal
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if not (Val1 <= Val2) then
            Raise_From_Msg
              (Msg     => Msg,
               Details => Image (Val1) & " > " & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert_Less_Or_Equal;

      -------------------------
      -- Assert_Greater_Than --
      -------------------------

      procedure Assert_Greater_Than
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if Val1 <= Val2 then
            Raise_From_Msg
              (Msg     => Msg,
               Details => Image (Val1) & " <= " & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert_Greater_Than;

      -----------------------------
      -- Assert_Greater_Or_Equal --
      -----------------------------

      procedure Assert_Greater_Or_Equal
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      is
      begin
         if Val1 < Val2 then
            Raise_From_Msg
              (Msg     => Msg,
               Details => Image (Val1) & " < " & Image (Val2),
               Location => Location,
               Entity   => Entity);
         end if;
      end Assert_Greater_Or_Equal;

   end Asserts_Simple;

end BDD.Asserts_Generic;
