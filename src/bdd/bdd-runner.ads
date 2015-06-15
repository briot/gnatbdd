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

--  Manipulating features files

with Ada.Calendar;    use Ada.Calendar;
with BDD.Features;    use BDD.Features;
with BDD.Formatters;  use BDD.Formatters;
with BDD.Parser;      use BDD.Parser;

package BDD.Runner is

   type Feature_Runner is new BDD.Parser.Abstract_Feature_Runner with private;
   --  This type is responsible for running each of the features that are
   --  registered.
   --  You can either register features files explicitly, or by using the
   --  Discover procedure below.

   procedure Discover
     (Self      : in out Feature_Runner;
      Extension : Filesystem_String := ".feature";
      Directory : GNATCOLL.VFS.Virtual_File := Create_From_Base ("features"));
   --  Analyze Directory recursively to find all features files.
   --  Extension is the file extension for such files.

   procedure Register
     (Self      : in out Feature_Runner;
      File      : GNATCOLL.VFS.Virtual_File);
   procedure Register
     (Self      : in out Feature_Runner;
      Files     : GNATCOLL.VFS.File_Array);
   --  Register one or more features file explicitly.

   procedure Run
     (Self           : in out Feature_Runner;
      Format         : not null access BDD.Formatters.Formatter'Class;
      Parser         : in out BDD.Parser.Feature_Parser'Class);
   --  Run all features and their scenarios.
   --
   --  Each of the features file is parsed through Parser. This allows you to
   --  support various syntaxes for the files.
   --
   --  The features are run in alphabetical order of the file name, and the
   --  scenarios are run in the order they were defined in in the features
   --  file.

   procedure Run_Start (Self : in out Feature_Runner);
   --  Called before the first feature is run

   procedure Run_End (Self : in out Feature_Runner);
   --  Called after the last feature has been run.

   overriding procedure Scenario_End
     (Self       : in out Feature_Runner;
      Background : BDD.Features.Scenario;
      Scenario   : BDD.Features.Scenario);

   procedure Add_Step_Runner
     (Self   : in out Feature_Runner;
      Runner : not null Step_Runner);
   --  Add one more function that tests for step definitions.
   --  Any number of those can be registered.

private
   type Feature_Runner is new BDD.Parser.Abstract_Feature_Runner with record
      Files  : GNATCOLL.VFS.File_Array_Access;
      Format : access BDD.Formatters.Formatter'Class;

      Steps_Stats        : Count_Array := (others => 0);
      Scenario_Stats     : Count_Array := (others => 0);
      Features_Count     : Natural := 0;
      Current_Feature_Id : Integer := -1;

      Runners : Step_Runner_Lists.List;

      Start : Ada.Calendar.Time;
   end record;

end BDD.Runner;
