------------------------------------------------------------------------------
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

--  This subprogram generates the test driver by including all the
--  step definitions provided by the user, as well as the predefined
--  steps, regular expressions and mockups.

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Strings;      use GNAT.Strings;
with Gnatbdd.Codegen;   use Gnatbdd.Codegen;
with Gnatbdd.Support;   use Gnatbdd.Support;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

procedure Gnatbdd.Main is
   Me : constant Trace_Handle := Create ("BDD");

   Finder : Steps_Finder;
   Config : Command_Line_Configuration;
   Tree   : Project_Tree;
   Setup  : Configuration;

begin
   GNATCOLL.Traces.Parse_Config_File;
   Setup_Command_Line_Switches (Config);
   Parse_Command_Line (Setup, Config);

   if Setup.Project_Name = No_File then
      Put_Line (Standard_Error, "No project specified");
      Set_Exit_Status (Failure);
      return;
   end if;

   Parse_Project (Tree, Setup);

   declare
      Ads_Extension : constant String :=
        Tree.Root_Project.Attribute_Value
          (Attribute    => Specification_Suffix_Attribute,
           Index        => "ada",
           Default      => ".ads",
           Use_Extended => True);
      Switches_From_Project : String_List_Access :=
        Tree.Root_Project.Attribute_Value
          (Attribute    => Gnatbdd_Switches_Attr,
           Use_Extended => True);
   begin
      if Switches_From_Project /= null then
         Trace (Me, "Parsing switches found in project");
         Parse_Command_Line (Setup, Config, Switches_From_Project);
         Switches_From_Project := null;  --  freed above
      end if;

      Discover_Steps
        (Finder,
         Extension        => +Ads_Extension,
         Object_Dir       => Tree.Root_Project.Object_Dir,
         Tree             => Tree,
         Extra_Steps_Dirs => Setup.Steps_Dirs);
   end;

exception
   when GNAT.Command_Line.Exit_From_Command_Line
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter
      =>
      null;
end Gnatbdd.Main;
