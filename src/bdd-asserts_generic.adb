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

with Ada.Assertions;        use Ada.Assertions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNATCOLL.Utils;        use GNATCOLL.Utils;

package body BDD.Asserts_Generic is

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, String, Ada.Strings.Hash, "=", "=");

   Messages : String_Maps.Map;
   Id : Positive := 1;  --  a unique id for all raised exceptions

   ---------------------------
   -- Raise_Assertion_Error --
   ---------------------------

   procedure Raise_Assertion_Error
     (Msg      : String;
      Details  : String;
      Location : String;
      Entity   : String)
   is
      Full : constant String :=
        (if Msg /= "" then Msg & ASCII.LF else "")
        & Details & ASCII.LF & "at " & Entity & " " & Location;
      M : constant String :=
        '@' & Image (Id, Min_Width => 0) & '@' & Msg & Details;
      Actual : String renames
        M (M'First .. M'First + Integer'Min (M'Length - 1, 200));
   begin
      --  Preserve as much as possible of the message (in case this exception
      --  is handled by code other than GNATBdd), but insert a unique id at
      --  the beginning so that we can retrieve the full message from the map.

      Id := Id + 1;
      Messages.Include (Actual, Full);
      raise Unexpected_Result with Actual;
   end Raise_Assertion_Error;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (E : Exception_Occurrence) return String is
      Actual : constant String := Exception_Message (E);
      C      : constant String_Maps.Cursor := Messages.Find (Actual);
   begin
      if not String_Maps.Has_Element (C) then
         return Actual;
      else
         return String_Maps.Element (C);
      end if;
   end Get_Message;

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
            Raise_Assertion_Error
              (Msg,
               Image (Val1) & ' ' & Operator_Image & ' ' & Image (Val2),
               Location, Entity);
         end if;
      end Assert;
   end Asserts;

end BDD.Asserts_Generic;
