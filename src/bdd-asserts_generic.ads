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

--  An assertion library for use with GNATBDD

with Ada.Exceptions;   use Ada.Exceptions;
with GNAT.Source_Info;

package BDD.Asserts_Generic is

   procedure Raise_Assertion_Error
     (Msg      : String;
      Details  : String;
      Location : String;
      Entity   : String);
   --  Raises an assertion error, storing the full message in a temporary
   --  location since exception messages are limited in length and would not
   --  show the whole message

   function Get_Message (E : Exception_Occurrence) return String;
   --  Retrieve the whole message from the exception (which should have been
   --  raises by Raise_Assert_Error above, or one of the Assert procedures
   --  below).

   generic
      type T is limited private;
      with function Image (V : T) return String;
      with function "=" (V1, V2 : T) return Boolean is <>;

   package Asserts is

      procedure Assert
        (Val1, Val2 : T;
         Msg        : String := "";
         Location   : String := GNAT.Source_Info.Source_Location;
         Entity     : String := GNAT.Source_Info.Enclosing_Entity);
      --  Compare two elements for equality.
      --  You could provide a detailed message through Msg, which will help
      --  understand the error later on.
      --  You are not expected to provide actual values for Location and
      --  Entity, which are used to automatically retrieve the location where
      --  the error occurred.

   end Asserts;

end BDD.Asserts_Generic;
