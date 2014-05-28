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

with GNAT.Command_Line; use GNAT.Command_Line;
with GNATCOLL.Terminal; use GNATCOLL.Terminal;

package body BDD is

   ---------------------------
   -- Command_Line_Switches --
   ---------------------------

   procedure Command_Line_Switches is
      procedure Callback (Switch, Parameter, Section : String);
      --  Called for each switch on the command line

      procedure Callback (Switch, Parameter, Section : String) is
         pragma Unreferenced (Section);
      begin
         if Switch = "--features" then
            BDD.Features_Directory := Create_From_Base (+Parameter);

         elsif Switch = "--tags" then
            null;  --  ??? not handled yet

         elsif Switch = "--color" then
            if Parameter = "yes" then
               BDD.Colors := GNATCOLL.Terminal.Yes;
            elsif Parameter = "no" then
               BDD.Colors := GNATCOLL.Terminal.No;
            else
               BDD.Colors := GNATCOLL.Terminal.Auto;
            end if;

         elsif Switch = "--output" then
            if Parameter = "quiet" then
               BDD.Output := Output_Quiet;
            elsif Parameter = "hide_passed" then
               BDD.Output := Output_Hide_Passed;
            elsif Parameter = "full" then
               BDD.Output := Output_Full;
            else
               BDD.Output := Output_Dots;
            end if;

         elsif Switch = "--log" then
            null;  --  ??? not handled yet
         end if;
      end Callback;

      Config : Command_Line_Configuration;
   begin
      Set_Usage
        (Config,
         Usage       => "[switches] [file:line1:line2] [file#num1#num2]",
         Help        =>
           "Run each of the scenario defined in features files.");

      Define_Switch
        (Config,
         Long_Switch => "--features=",
         Argument    => "DIR",
         Help => "Directory in which to recursively look for features files"
         & " (default 'features')");

      Define_Switch
        (Config,
         Output      => BDD.Features_File_Ext'Access,
         Long_Switch => "--ext=",
         Argument    => ".EXT",
         Help        => "Extension for features file (default '.feature')");

      Define_Switch
        (Config,
         Long_Switch => "--tags=",
         Argument    => "EXPR",
         Help        => "Defines which scenarios to run. EXPR can be one of"
         & ASCII.LF
         & "        @tag1,@tag2  (run scenarios with either @tag1 or @tag2)"
         & ASCII.LF
         & "        ~@tag1,@tag2 (run scenarios with neither @tag1 nor @tag2)"
         & ASCII.LF
         & "        Use multiple --tags switches to AND multiple tags");

      Define_Switch
        (Config,
         Long_Switch => "--color=",
         Argument    => "YESNO",
         Help      => "Whether to use colors in the output (AUTO|yes|no)");

      Define_Switch
        (Config,
         Long_Switch => "--output=",
         Argument    => "format",
         Help   => "Controls the output format (quiet|DOTS|hide_passed|full)");

      Define_Switch
        (Config,
         Long_Switch => "--log=",
         Argument    => "DIR",
         Help       => "Directory to store the log files (default is 'logs')");

      Getopt (Config, Callback'Unrestricted_Access);
   end Command_Line_Switches;

end BDD;
