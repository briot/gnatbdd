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
with BDD.Tables;            use BDD.Tables;
with GNAT.Source_Info;

package BDD.Asserts is
   package BAG renames BDD.Asserts_Generic;

   --------------
   -- Booleans --
   --------------

   procedure Assert
     (Val : Boolean;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity);

   --------------
   -- Integers --
   --------------

   package Integers is new BAG.Asserts_Simple (Integer, Integer'Image);

   procedure Assert
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integers.Assert;
   procedure Assert_Not_Equal
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integers.Assert_Not_Equal;
   procedure Assert_Less_Than
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integers.Assert_Less_Than;
   procedure Assert_Greater_Than
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integers.Assert_Greater_Than;
   procedure Assert_Less_Or_Equal
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integers.Assert_Less_Or_Equal;
   procedure Assert_Greater_Or_Equal
     (Val1, Val2 : Integer;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Integers.Assert_Greater_Or_Equal;

   ------------
   -- Floats --
   ------------

   package Floats   is new BAG.Asserts_Simple (Float, Float'Image);

   procedure Assert
     (Val1, Val2 : Float;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Floats.Assert;
   procedure Assert_Not_Equal
     (Val1, Val2 : Float;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Floats.Assert_Not_Equal;
   procedure Assert_Less_Than
     (Val1, Val2 : Float;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Floats.Assert_Less_Than;
   procedure Assert_Greater_Than
     (Val1, Val2 : Float;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Floats.Assert_Greater_Than;
   procedure Assert_Less_Or_Equal
     (Val1, Val2 : Float;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Floats.Assert_Less_Or_Equal;
   procedure Assert_Greater_Or_Equal
     (Val1, Val2 : Float;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Floats.Assert_Greater_Or_Equal;

   -------------
   -- Strings --
   -------------

   function Identity (Str : String) return String is (Str);
   package Strings is new BAG.Asserts_Simple (String, Identity);

   procedure Assert
     (Val1, Val2 : String;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Strings.Assert;
   procedure Assert_Not_Equal
     (Val1, Val2 : String;
      Msg : String := "";
      Location   : String := GNAT.Source_Info.Source_Location;
      Entity     : String := GNAT.Source_Info.Enclosing_Entity)
      renames Strings.Assert_Not_Equal;

   ------------
   -- Tables --
   ------------

   procedure Assert
     (Expected, Actual : BDD.Tables.Table;
      Msg      : String := "";
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity);
   --  Compare two tables.
   --  Although it would be possible to do the comparison yourself, cell by
   --  cell, it is better to use this procedure. It will provide a nicer
   --  formatting of the output.
   --  The Actual table must have at least as many columns as Expected,
   --  although it could contain more (which allows you to share code to create
   --  the table). When there are more columns, the comparison is done as
   --  follows:
   --     * Either column names are provided, and they are used.
   --     * or the Expected.Height first column are compared with Expected.

end BDD.Asserts;
