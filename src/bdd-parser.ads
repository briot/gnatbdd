-----------------------------------------------------------------------------
--                             g N A T C O L L                              --
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

--  A parser for the features files

with BDD.Features;     use BDD.Features;

package BDD.Parser is

   Syntax_Error : exception;
   --  Raised when reading one of the features file raises a syntax error.

   type Abstract_Feature_Runner is interface;
   --  The type that is responsible for running the features and scenarios
   --  found by the parser.

   procedure Feature_Start
     (Self     : in out Abstract_Feature_Runner;
      Feature  : not null access BDD.Features.Feature_Record'Class) is null;
   --  Called on the first line of a feature.
   --  At this stage, only the name and file or the feature are known, but none
   --  of its scenarios

   procedure Scenario_Start
     (Self     : in out Abstract_Feature_Runner;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class) is null;
   --  Called on the first line of a scenario.
   --  At this stage, only the name and location of the scenario are known, but
   --  none of its steps.

   procedure Scenario_End
     (Self     : in out Abstract_Feature_Runner;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class) is null;
   --  Called when the last step in a scenario has been seen.

   procedure Feature_End
     (Self     : in out Abstract_Feature_Runner;
      Feature  : not null access BDD.Features.Feature_Record'Class) is null;
   --  Called when the last line of a feature has been seen.

   type Feature_Parser is tagged private;

   procedure Parse
     (Self     : in out Feature_Parser;
      File     : GNATCOLL.VFS.Virtual_File;
      Runner   : in out Abstract_Feature_Runner'Class);
   --  Parses a .feature file.
   --
   --  Calls Runner.Run_Scenario for each scenario found.
   --
   --  Raises Syntax_Error when the file does not contain valid syntax.

private
   type Feature_Parser is tagged null record;

end BDD.Parser;
