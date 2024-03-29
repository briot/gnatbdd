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

--  The formatters are used to prepare the output for the user.
--  They do not perform the actual output, which is done via BDD.Media, in
--  various formats.

private with Ada.Containers.Doubly_Linked_Lists;
with BDD.Features;      use BDD.Features;
with BDD.Media;         use BDD.Media;

package BDD.Formatters is

   type Formatter is abstract tagged private;
   type Formatter_Access is access all Formatter'Class;

   procedure Init
     (Self    : in out Formatter;
      Output  : not null access Media_Writer'Class);
   --  Prepare the output for a specific media.

   procedure Scenario_Start
     (Self     : in out Formatter;
      Scenario : BDD.Features.Scenario) is null;
   --  Display information about a feature and a scenario just before the
   --  scenario is run.

   procedure Scenario_Completed
     (Self     : in out Formatter;
      Scenario : BDD.Features.Scenario) is null;
   --  Called when a scenario has completed.
   --  Its status has already been set

   procedure Nested_Scenario_Start
     (Self     : in out Formatter;
      Scenario : BDD.Features.Scenario;
      Is_First : Boolean) is null;
   procedure Nested_Scenario_Completed
     (Self     : in out Formatter;
      Scenario : BDD.Features.Scenario) is null;
   --  Called when a nested scenario starts or has completed.
   --  These are the scenarios generated from an Outline Scenario examples.
   --  Its status has already been set

   procedure Step_Completed
     (Self     : in out Formatter;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class) is null;
   --  Called when a step has completed.
   --  Step.Status has been set appropriately

   type Count_Array is array (Scenario_Status) of Natural;

   procedure All_Features_Completed
     (Self      : in out Formatter;
      Features  : Natural;
      Scenarios : Count_Array;
      Steps     : Count_Array;
      Elapsed   : Duration);
   --  Called when all features have been full run.
   --  This can be used to display summaries

   function Create_Formatter return not null access Formatter'Class;
   --  Create the formatter to use, depending on BDD.Output

   ----------
   -- Full --
   ----------

   type Formatter_Full is new Formatter with private;
   --  A formatter that displays all the features, scenarios and steps that
   --  are executed

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario);
   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario);
   overriding procedure Step_Completed
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class);
   overriding procedure Nested_Scenario_Start
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario;
      Is_First : Boolean);

   ----------
   -- Dots --
   ----------

   type Formatter_Dots is new Formatter with private;

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Dots;
      Scenario : BDD.Features.Scenario);
   overriding procedure All_Features_Completed
     (Self      : in out Formatter_Dots;
      Features  : Natural;
      Scenarios : Count_Array;
      Steps     : Count_Array;
      Elapsed   : Duration);

   -----------
   -- Quiet --
   -----------

   type Formatter_Quiet is new Formatter with private;

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Quiet;
      Scenario : BDD.Features.Scenario);
   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Quiet;
      Scenario : BDD.Features.Scenario);

   -----------------
   -- Hide_Passed --
   -----------------

   type Formatter_Hide_Passed is new Formatter with private;

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Hide_Passed;
      Scenario : BDD.Features.Scenario);
   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Hide_Passed;
      Scenario : BDD.Features.Scenario);

private
   package Scenario_Lists is new Ada.Containers.Doubly_Linked_Lists
     (BDD.Features.Scenario);

   type Formatter is abstract tagged record
      Last_Displayed_Feature_Id : Integer := -1;
      Output                    : Media_Writer_Access;
   end record;

   type Formatter_Full  is new Formatter with null record;

   type Formatter_Dots  is new Formatter with record
      Failed : Scenario_Lists.List;
      --  Failed scenarios, so that we can display them in the end

   end record;

   type Formatter_Quiet is new Formatter with null record;
   type Formatter_Hide_Passed is new Formatter with null record;

end BDD.Formatters;
