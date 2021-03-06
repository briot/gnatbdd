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

--  Support for code generation (detecting user-defined steps,...)

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

package Gnatbdd.Codegen is

   type Steps_Finder is tagged private;

   procedure Free (Self : in out Steps_Finder);
   --  Free memory used by Self

   procedure Discover_Steps
     (Self             : in out Steps_Finder;
      Extension        : Filesystem_String := ".ads";
      Object_Dir       : GNATCOLL.VFS.Virtual_File;
      Tree             : GNATCOLL.Projects.Project_Tree;
      Driver           : String;
      Extra_Steps_Dirs : GNATCOLL.VFS.File_Array_Access);
   --  Parse all specs in the project's source directories or in
   --  Extra_Steps_Dirs, to find the definition of steps.

private
   type Steps_Finder is tagged record
      Files : GNATCOLL.VFS.File_Array_Access;
   end record;
end Gnatbdd.Codegen;
