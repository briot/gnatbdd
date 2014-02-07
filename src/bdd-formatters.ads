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

--  The formatters are used to display output for the user

with BDD.Features;      use BDD.Features;
with GNATCOLL.Terminal; use GNATCOLL.Terminal;

package BDD.Formatters is

   type Formatter is abstract tagged private;

   procedure Init
     (Self    : in out Formatter;
      Term    : GNATCOLL.Terminal.Terminal_Info_Access);
   --  Prepare the output for a specific terminal.
   --  This controls, among other things, whether the output supports colors.

   procedure Scenario_Start
     (Self     : in out Formatter;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class) is null;
   --  Display information about a feature and ascenario just before the
   --  scenario is run.

   procedure Scenario_Completed
     (Self     : in out Formatter;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status) is null;
   --  Called when a scenario has completed

   procedure Step_Completed
     (Self     : in out Formatter;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class) is null;
   --  Called when a step has completed.
   --  Step.Status has been set appropriately

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
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Full;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status);
   overriding procedure Step_Completed
     (Self     : in out Formatter_Full;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class);

   ----------
   -- Dots --
   ----------

   type Formatter_Dots is new Formatter with private;

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Dots;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status);

   -----------
   -- Quiet --
   -----------

   type Formatter_Quiet is new Formatter with private;

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Quiet;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Quiet;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status);

   -----------------
   -- Hide_Passed --
   -----------------

   type Formatter_Hide_Passed is new Formatter with private;

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Hide_Passed;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Hide_Passed;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status);

private
   type Formatter is abstract tagged record
      Term : GNATCOLL.Terminal.Terminal_Info_Access;
      Last_Displayed_Feature_Id : Integer := -1;
      Progress_Displayed : Boolean := False;
   end record;

   type Formatter_Full  is new Formatter with null record;

   type Formatter_Dots  is new Formatter with null record;

   type Formatter_Quiet is new Formatter with null record;
   type Formatter_Hide_Passed is new Formatter with null record;

end BDD.Formatters;
