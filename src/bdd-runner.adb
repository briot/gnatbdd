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

package body BDD.Runner is

   --------------
   -- Discover --
   --------------

   procedure Discover
     (Self      : in out Feature_Runner;
      Extension : Filesystem_String         := ".feature";
      Directory : GNATCOLL.VFS.Virtual_File := Create_From_Base ("features"))
   is
      Files : File_Array_Access := Directory.Read_Dir_Recursive
        (Extension => Extension, Filter => Files_Only);
   begin
      if Files /= null then
         for F in Files'Range loop
            Self.Register (Files (F));
         end loop;
         Unchecked_Free (Files);
      end if;
   end Discover;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self      : in out Feature_Runner;
      File      : GNATCOLL.VFS.Virtual_File) is
   begin
      Append (Self.Files, File);
   end Register;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self      : in out Feature_Runner;
      Files     : GNATCOLL.VFS.File_Array) is
   begin
      Append (Self.Files, Files);
   end Register;

   ---------
   -- Run --
   ---------

   procedure Run
     (Self   : in out Feature_Runner;
      Format : not null access BDD.Formatters.Formatter'Class;
      Parser : in out BDD.Parser.Feature_Parser'Class)
   is
   begin
      if Self.Files /= null then
         Self.Run_Start;
         Self.Format := Format;
         Sort (Self.Files.all);
         for F in Self.Files'Range loop
            Parser.Parse (Self.Files (F), Self);
         end loop;

         Self.Run_End;
         Self.Format := null;
      end if;
   end Run;

   ---------------
   -- Run_Start --
   ---------------

   procedure Run_Start (Self : in out Feature_Runner) is
   begin
      Self.Steps_Stats := (others => 0);
      Self.Scenario_Stats := (others => 0);
      Self.Features_Count := 0;
      Self.Current_Feature_Id := -1;
      Self.Start := Ada.Calendar.Clock;
   end Run_Start;

   -------------
   -- Run_End --
   -------------

   procedure Run_End (Self : in out Feature_Runner) is
   begin
      Self.Format.All_Features_Completed
        (Features  => Self.Features_Count,
         Scenarios => Self.Scenario_Stats,
         Steps     => Self.Steps_Stats,
         Elapsed   => Ada.Calendar.Clock - Self.Start);
   end Run_End;

   ------------------
   -- Scenario_End --
   ------------------

   overriding procedure Scenario_End
     (Self       : in out Feature_Runner;
      Background : BDD.Features.Scenario;
      Scenario   : BDD.Features.Scenario)
   is
      Show_Steps : Boolean;

      procedure Run_Step
        (Scenario : BDD.Features.Scenario;
         Step     : not null access Step_Record'Class);
      --  Run a specific step of the scenario

      procedure Run_Scenario_And_Background (Nested : BDD.Features.Scenario);
      --  Execute the background scenario, and then Scenario itself.
      --  In the case of outline scenarios, this is called once for each line
      --  in the examples, since we want to execute the background for each.

      --------------
      -- Run_Step --
      --------------

      procedure Run_Step
        (Scenario : BDD.Features.Scenario;
         Step     : not null access Step_Record'Class)
      is
         Execute : constant Boolean := Scenario.Status = Status_Passed;
      begin
         Step.Run (Execute, Self.Runners);

         if Execute then
            Scenario.Set_Status (Step.Status);
         end if;

         if Show_Steps then
            Self.Steps_Stats (Step.Status) :=
              Self.Steps_Stats (Step.Status) + 1;

            Self.Format.Step_Completed (Scenario, Step);
         end if;
      end Run_Step;

      ---------------------------------
      -- Run_Scenario_And_Background --
      ---------------------------------

      procedure Run_Scenario_And_Background (Nested : BDD.Features.Scenario) is
         Is_Nested : constant Boolean := Nested /= Scenario;
      begin
         Nested.Set_Status (Status_Passed);

         if Background /= No_Scenario then
            Show_Steps := Scenario.Get_Feature.Id /= Self.Current_Feature_Id;
            Background.Set_Status (Status_Passed);

            if Show_Steps then
               Self.Format.Scenario_Start (Background);
               Background.Foreach_Step (Run_Step'Access);
               Self.Format.Scenario_Completed (Background);
            else
               Background.Foreach_Step (Run_Step'Access);
            end if;

            if Background.Status = Status_Passed then
               Nested.Set_Status (Status_Passed);
            else
               Nested.Set_Status (Status_Skipped);
            end if;
         end if;

         Show_Steps := True;

         if not Is_Nested then
            Self.Format.Scenario_Start (Nested);
            Nested.Foreach_Step (Run_Step'Access);
            Self.Format.Scenario_Completed (Nested);

            Self.Scenario_Stats (Nested.Status) :=
              Self.Scenario_Stats (Nested.Status) + 1;

         else
            Nested.Foreach_Step (Run_Step'Access);
         end if;

         if Is_Nested then
            case Nested.Status is
               when Status_Passed | Status_Undefined | Status_Skipped =>
                  null;  --  Do not change the status of Scenario
               when Status_Failed =>
                  Scenario.Set_Status (Status_Failed);
            end case;

         end if;

         if Scenario.Get_Feature.Id /= Self.Current_Feature_Id then
            Self.Features_Count := Self.Features_Count + 1;
            Self.Current_Feature_Id := Scenario.Get_Feature.Id;
         end if;
      end Run_Scenario_And_Background;

   begin
      case Scenario.Kind is
         when Kind_Scenario =>
            Run_Scenario_And_Background (Scenario);

         when Kind_Background =>
            null;

         when Kind_Outline =>
            Scenario.Set_Status (Status_Passed);

            Self.Format.Scenario_Start (Scenario);
            Scenario.Foreach_Scenario
              (Run_Scenario_And_Background'Access);
            Self.Format.Scenario_Completed (Scenario);

            Self.Scenario_Stats (Scenario.Status) :=
              Self.Scenario_Stats (Scenario.Status) + 1;
      end case;
   end Scenario_End;

   ---------------------
   -- Add_Step_Runner --
   ---------------------

   procedure Add_Step_Runner
     (Self   : in out Feature_Runner;
      Runner : not null Step_Runner)
   is
   begin
      Self.Runners.Append (Runner);
   end Add_Step_Runner;

end BDD.Runner;
