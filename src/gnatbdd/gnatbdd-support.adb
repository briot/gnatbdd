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

with Ada.Command_Line;   use Ada.Command_Line;
with Ada.Text_IO;        use Ada.Text_IO;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

package body Gnatbdd.Support is
   Me : constant Trace_Handle := Create ("BDD.SUPPORT");

   procedure Error (Msg : String);
   pragma No_Return (Error);
   --  Report an error to the main

   -----------
   -- Error --
   -----------

   procedure Error (Msg : String) is
   begin
      if Msg /= "" then
         Put_Line (Standard_Error, Msg);
      end if;

      Set_Exit_Status (Failure);
      raise GNAT.Command_Line.Invalid_Parameter;
   end Error;

   ---------------------------------
   -- Setup_Command_Line_Switches --
   ---------------------------------

   procedure Setup_Command_Line_Switches
     (Config : in out Command_Line_Configuration)
   is
   begin
      Set_Usage
        (Config,
         Usage => "-Pproject [switches]",
         Help  => "Generate and compile the test driver");

      Define_Switch
        (Config,
         Switch   => "-P:",
         Argument => "PROJECT",
         Help    => "Project to load to find the sources of your application");

      Define_Switch
        (Config,
         Long_Switch => "--steps=",
         Argument    => "DIR",
         Help        => "Specify a directory to search for step definitions");
   end Setup_Command_Line_Switches;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
     (Result   : in out Configuration;
      Config   : Command_Line_Configuration;
      Switches : GNAT.Strings.String_List_Access := null)
   is
      procedure On_Switch (Switch, Parameter, Section : String);
      procedure On_Switch (Switch, Parameter, Section : String) is
         pragma Unreferenced (Section);
      begin
         Trace (Me, "Seen switch " & Switch & " '" & Parameter & "'");
         if Switch = "--steps" then
            if Switches = null then
               Append (Result.Steps_Dirs, Create_From_Base (+Parameter));
            else
               Append (Result.Steps_Dirs,
                       Create_From_Dir
                         (Create (Result.Project_Name.Dir_Name), +Parameter));
            end if;

         elsif Switch = "-P" then
            if Switches /= null then
               Error ("-P cannot be specified in gnatbdd'switches");
            end if;

            Result.Project_Name := Create_From_Base (+Parameter);
            if not Result.Project_Name.Is_Regular_File then
               Result.Project_Name := Create_From_Base (+Parameter & ".gpr");
               if not Result.Project_Name.Is_Regular_File then
                  Error ("Project file not found");
               end if;
            end if;
         end if;
      end On_Switch;

      Parser : Opt_Parser;
   begin
      if Switches /= null then
         Initialize_Option_Scan (Parser, Command_Line => Switches);
         Getopt (Config, On_Switch'Unrestricted_Access, Parser);
         Free (Parser);
      else
         Getopt (Config, On_Switch'Unrestricted_Access);
      end if;
   end Parse_Command_Line;

   -------------------
   -- Parse_Project --
   -------------------

   procedure Parse_Project
     (Tree   : out Project_Tree;
      Config : Configuration)
   is
      Err : constant String := Register_New_Attribute
        (Name    => "switches",
         Pkg     => "gnatbdd",
         Is_List => True);
   begin
      if Err /= "" then
         Error (Err);
      end if;

      Tree.Load (Root_Project_Path => Config.Project_Name,
                 Errors            => Ada.Text_IO.Put_Line'Access);

   exception
      when Invalid_Project =>
         Error ("");
   end Parse_Project;

end Gnatbdd.Support;
