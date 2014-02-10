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

with Ada.Exceptions; use Ada.Exceptions;
with BDD.Steps;      use BDD.Steps;
with GNATCOLL.Utils; use GNATCOLL.Utils;

package body BDD.Runner is

   --------------
   -- Discover --
   --------------

   procedure Discover
     (Self      : in out Feature_Runner;
      Extension : Filesystem_String         := ".feature";
      Directory : GNATCOLL.VFS.Virtual_File := Create_From_Base ("features"))
   is
      Files : File_Array_Access;
   begin
      if Directory.Is_Directory then
         Files := Directory.Read_Dir (Files_Only);
         for F in Files'Range loop
            if Files (F).File_Extension = Extension then
               Self.Register (Files (F));
            end if;
         end loop;
         Unchecked_Free (Files);

         Files := Directory.Read_Dir (Dirs_Only);
         for F in Files'Range loop
            if Files (F).Base_Name /= "."
              and then Files (F).Base_Name /= ".."
            then
               Self.Discover (Extension, Files (F));
            end if;
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

      procedure Run_Step
        (Scenario : BDD.Features.Scenario;
         Step     : not null access Step_Record'Class)
      is
         Execute : constant Boolean := Scenario.Status = Status_Passed;
         Text    : constant String := Step.Text;
         First   : Integer := Text'First;

      begin
         --  Run the step, or at least check whether it is defined.
         if Execute then
            Step.Set_Status (Status_Passed);
         else
            Step.Set_Status (Status_Skipped);
         end if;

         --  Skip the leading 'Given|Then|...' keywords, which are irrelevant
         --  for the purpose of the match

         while First <= Text'Last
           and then not Is_Whitespace (Text (First))
         loop
            First := First + 1;
         end loop;
         Skip_Blanks (Text, First);

         begin
            --  Will set status to undefined if necessary
            BDD.Steps.Run_Step
              (Step, Text (First .. Text'Last), Execute => Execute);
            --   if Step.Status = Status_Undefined then
            --      --  ??? Could run some predefined steps here
            --      null;
            --   end if;

         exception
            when E : others =>
               Step.Set_Status (Status_Failed, Exception_Information (E));
         end;

         if Execute then
            if Show_Steps then
               Self.Steps_Stats (Step.Status) :=
                 Self.Steps_Stats (Step.Status) + 1;
            end if;

            Scenario.Set_Status (Step.Status);
         end if;

         if Show_Steps then
            Self.Format.Step_Completed (Scenario, Step);
         end if;
      end Run_Step;

   begin
      if Scenario.Kind = Kind_Scenario then
         Scenario.Set_Status (Status_Passed);

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
               Scenario.Set_Status (Status_Passed);
            else
               Scenario.Set_Status (Status_Skipped);
            end if;
         end if;

         Show_Steps := True;
         Self.Format.Scenario_Start (Scenario);
         Scenario.Foreach_Step (Run_Step'Access);
         Self.Format.Scenario_Completed (Scenario);

         Self.Scenario_Stats (Scenario.Status) :=
           Self.Scenario_Stats (Scenario.Status) + 1;

         if Scenario.Get_Feature.Id /= Self.Current_Feature_Id then
            Self.Features_Count := Self.Features_Count + 1;
            Self.Current_Feature_Id := Scenario.Get_Feature.Id;
         end if;
      end if;
   end Scenario_End;

end BDD.Runner;
