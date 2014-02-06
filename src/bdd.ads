-----------------------------------------------------------------------------
--                             g N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

with GNAT.Strings;      use GNAT.Strings;
with GNATCOLL.Terminal;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

package BDD is

   Cst_Features         : constant String := "Feature:";
   Cst_Scenario         : constant String := "Scenario:";
   Cst_Scenario_Outline : constant String := "Scenario Outline:";
   Cst_Given            : constant String := "Given";
   Cst_And              : constant String := "And";
   Cst_Then             : constant String := "Then";
   Cst_But              : constant String := "But";
   Cst_When             : constant String := "When";
   Cst_Background       : constant String := "Background";
   Cst_Examples         : constant String := "Examples:";
   Cst_Scenarios        : constant String := "Scenarios:";
   --  The keywords when parsing a .feature file

   Features_Directory : GNATCOLL.VFS.Virtual_File :=
     Create_From_Base ("features");
   --  The parent directory for all features file.

   Features_File_Ext : aliased GNAT.Strings.String_Access :=
     new String'(".feature");
   --  Extension for the features file

   Colors : GNATCOLL.Terminal.Supports_Color := GNATCOLL.Terminal.Auto;
   --  Whether we should use colors in the output

   type Output_Type is
     (Output_Quiet,
      Output_Dots,
      Output_Hide_Passed,
      Output_Full);
   Output : Output_Type := Output_Dots;
   --  The kind of output the user expects

   procedure Command_Line_Switches;
   --  Handles the command line switches

   procedure Main;
   --  The main loop, which discovers and then runs all the tests

end BDD;
