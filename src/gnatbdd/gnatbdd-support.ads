------------------------------------------------------------------------------
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

--  Support utilities for gnatbdd

with GNAT.Command_Line;   use GNAT.Command_Line;
with GNAT.Strings;        use GNAT.Strings;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

package Gnatbdd.Support is

   ------------------
   -- Command line --
   ------------------

   type Configuration is record
      Project_Name : GNATCOLL.VFS.Virtual_File;
      Steps_Dirs   : GNATCOLL.VFS.File_Array_Access;
      Driver       : GNAT.Strings.String_Access := new String'("driver");
   end record;
   --  Various configurations from the command line

   procedure Setup_Command_Line_Switches
     (Config : in out Command_Line_Configuration);
   --  Register the supported command line switches

   procedure Parse_Command_Line
     (Result   : in out Configuration;
      Config   : Command_Line_Configuration;
      Switches : GNAT.Strings.String_List_Access := null);
   --  Parse command line switches.
   --  If Switches is null, the actual command line is parsed, otherwise the
   --  switches are extracted from Switches. Switches supports a limited
   --  subset of all switches (for instance it cannot contain -P, since we
   --  assume Switches already comes from the project
   --  This procedure will free Switches.

   -------------
   -- Project --
   -------------

   Gnatbdd_Switches_Attr : constant Attribute_Pkg_List :=
     Build ("gnatbdd", "switches");

   procedure Parse_Project
     (Tree   : out Project_Tree;
      Config : Configuration);
   --  Parse the project

end Gnatbdd.Support;
