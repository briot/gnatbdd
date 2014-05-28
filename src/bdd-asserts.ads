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
--
--  Each of these subprograms raises an Assert_Error with enough information
--  to make the exception message useful.

with BDD.Asserts_Generic;   use BDD.Asserts_Generic;
with GNAT.Source_Info;

package BDD.Asserts is
   package BAG renames BDD.Asserts_Generic;

   package Integer_Asserts is new BAG.Asserts (Integer, Integer'Image, "=");
   procedure Assert
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integer_Asserts.Assert;

end BDD.Asserts;
