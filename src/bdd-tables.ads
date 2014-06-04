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

--  This package provides support for manipulating, comparing and displaying
--  tables.

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Text_IO;          use Ada.Text_IO;
with GNATCOLL.Refcount;    use GNATCOLL.Refcount;
with GNATCOLL.Terminal;    use GNATCOLL.Terminal;

package BDD.Tables is

   type Table is tagged private;
   --  A reference-counted representation of a feature table

   No_Table : constant Table;

   function Create return Table;
   --  Create a new empty table

   procedure Add_Row_As_String
     (Self : Table;
      Row  : String);
   --  Add a row to the table. Row must be formatted as in:
   --      | Col1 | Col2 | Col3 |
   --
   --  When this is the first row added, the contents provides the name of
   --  the columns. If Set_Column_Name has already been called, Row is never
   --  assumed to contain the title of the columns.

   procedure Set_Column_Name
     (Self   : Table;
      Column : Positive;
      Name   : String);
   function Get_Column_Name
     (Self   : Table;
      Column : Positive) return String;
   --  Set the name of the Column-th column

   procedure Put
     (Self   : Table;
      Column : Positive;
      Row    : Positive;
      Value  : String);
   --  Set the value in the table.

   function Get
     (Self   : Table;
      Column : Positive;
      Row    : Positive)
      return String;
   function Get
     (Self   : Table;
      Column : String;
      Row    : Positive)
      return String;
   --  Get an element from the table.
   --  The column can be specified as a string to use the name of the column.

   function Column_Number (Self : Table; Name : String) return Natural;
   --  Return the number for the column with a specific Name, or 0 if not found

   function Width  (Self : Table) return Natural;
   function Height (Self : Table) return Natural;
   --  Return the size of the table

   procedure Display
     (Self : Table; File : Ada.Text_IO.File_Type; Prefix : String := "");
   --  Display the table in File.
   --  Each line is lead by a Prefix.

private
   package String_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, String);
   package Row_Vectors is new Ada.Containers.Vectors
     (Positive, String_Vectors.Vector, String_Vectors."=");

   type Table_Record is new GNATCOLL.Refcount.Refcounted with record
      Names  : String_Vectors.Vector;
      Width  : Natural := 0;
      Rows   : Row_Vectors.Vector;   --  does not include column titles
   end record;

   package Table_Pointers is new GNATCOLL.Refcount.Smart_Pointers
     (Table_Record);
   type Table is new Table_Pointers.Ref with null record;

   No_Table : constant Table := (Table_Pointers.Null_Ref with null record);

end BDD.Tables;
