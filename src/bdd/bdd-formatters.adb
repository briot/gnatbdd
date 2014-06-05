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

with Ada.Containers;      use Ada.Containers;
with BDD.Asserts_Generic; use BDD.Asserts_Generic;
with BDD.Tables;          use BDD.Tables;
with GNATCOLL.Terminal;   use GNATCOLL.Terminal;
with GNATCOLL.Utils;      use GNATCOLL.Utils;
with GNAT.Regpat;         use GNAT.Regpat;

package body BDD.Formatters is

   function Scenario_Name (Scenario : BDD.Features.Scenario) return String;
   --  Return a string used to identify the scenario for the user.

   procedure Display_Scenario_Header
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario);
   --  Display the feature and scenario headers, as needed.

   procedure Display_Scenario_And_Steps
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario);
   --  Display a scenario and all its steps

   procedure Display_Step
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class);
   --  Display the text for a specific step

   ----------
   -- Init --
   ----------

   procedure Init
     (Self    : in out Formatter;
      Output  : not null access Media_Writer'Class)
   is
   begin
      Self.Output := Output;
   end Init;

   -------------------
   -- Scenario_Name --
   -------------------

   function Scenario_Name (Scenario : BDD.Features.Scenario) return String is
   begin
      return +Scenario.Get_Feature.File.Relative_Path (Features_Directory)
        & (if Scenario.Index /= Positive'Last
           then "#" & Image (Scenario.Index, 1)
           else "")
        & ":" & Image (Scenario.Line, 1);
   end Scenario_Name;

   --------------------------------
   -- Display_Scenario_And_Steps --
   --------------------------------

   procedure Display_Scenario_And_Steps
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario)
   is
      procedure Show_Step
        (Scenario : BDD.Features.Scenario;
         Step     : not null access BDD.Features.Step_Record'Class);
      --  Display a step for a scenario

      procedure Show_Scenario (Scenario : BDD.Features.Scenario);
      --  Display the details for one of the scenarios (either the scenario
      --  from the toplevel, or one of the scenarios generated from an outline)

      procedure Show_Step
        (Scenario : BDD.Features.Scenario;
         Step     : not null access BDD.Features.Step_Record'Class) is
      begin
         Display_Step (Self, Scenario, Step);
      end Show_Step;

      procedure Show_Scenario (Scenario : BDD.Features.Scenario) is
      begin
         Scenario.Foreach_Step (Show_Step'Access);
         Self.Output.New_Line;
      end Show_Scenario;

   begin
      Display_Scenario_Header (Self, Scenario);

      case Scenario.Kind is
         when Kind_Scenario | Kind_Background =>
            Scenario.Foreach_Step (Show_Step'Access);
            Self.Output.New_Line;

         when Kind_Outline =>
            Scenario.Foreach_Scenario (Show_Scenario'Access);
      end case;
   end Display_Scenario_And_Steps;

   -----------------------------
   -- Display_Scenario_Header --
   -----------------------------

   procedure Display_Scenario_Header
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario)
   is
      F : constant Feature'Class := Scenario.Get_Feature;
   begin
      if F.Id /= Self.Last_Displayed_Feature_Id then
         Self.Output.Display_Feature (F.Name, F.Description);
         Self.Last_Displayed_Feature_Id := F.Id;
      end if;

      Self.Output.Display_Scenario
        (Prefix        => Scenario.Prefix,
         Name          => Scenario.Name,
         Longuest_Step => Scenario.Longuest_Step,
         Location      => Scenario_Name (Scenario));
   end Display_Scenario_Header;

   ------------------
   -- Display_Step --
   ------------------

   procedure Display_Step
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class)
   is
   begin
      Self.Output.Display_Step
        (Text          => Step.Text,
         Status        => Step.Status,
         Longuest_Step => Scenario.Longuest_Step,
         Info          => Step.Match_Info,
         Location      =>
           +Scenario.Get_Feature.File.Relative_Path (Features_Directory)
           & ":" & Image (Step.Line, 1));

      declare
         Multi : constant String := Step.Multiline;
      begin
         if Multi /= "" then
            Self.Output.Start_Step_Messages (Status => Step.Status);
            Self.Output.Writeln ("      """"""");
            Self.Output.Indent (Multi, Prefix => "      ");
            Self.Output.Writeln ("      """"""");
            Self.Output.End_Step_Messages;
         end if;
      end;

      declare
         Error : constant Assert_Error := Step.Error_Details;
      begin
         if Error /= No_Error then
            Display (Error,
                     Self.Output,
                     Status => Step.Status,
                     Prefix => "      ");
         else
            --  Display the default step's table, since it full matched or was
            --  not even run.
            if Step.Table /= No_Table then
               Step.Table.Display (Self.Output, Prefix => "      ");
            end if;
         end if;
      end;
   end Display_Step;

   ----------------------------
   -- All_Features_Completed --
   ----------------------------

   procedure All_Features_Completed
     (Self      : in out Formatter;
      Features  : Natural;
      Scenarios : Count_Array;
      Steps     : Count_Array;
      Elapsed   : Duration)
   is
      procedure Put_Stats (Total : Natural; Stats : Count_Array);
      --  Display the stats

      procedure Put_Stats (Total : Natural; Stats : Count_Array) is
         Is_First : Boolean := True;
      begin
         if Total /= 0 then
            Self.Output.Write (" (");

            for S in Stats'Range loop
               if Stats (S) /= 0 then
                  if not Is_First then
                     Self.Output.Write (", ");
                  end if;
                  Is_First := False;

                  Self.Output.Set_Color (Foreground => BDD.Step_Colors (S));
                  Self.Output.Write (Image (Stats (S), 1));
                  Self.Output.Write (" ");

                  case S is
                     when Status_Passed    =>
                        Self.Output.Write ("passed");
                     when Status_Failed    =>
                        Self.Output.Write ("failed");
                     when Status_Skipped   =>
                        Self.Output.Write ("skipped");
                     when Status_Undefined =>
                        Self.Output.Write ("undefined");
                  end case;

                  Self.Output.Set_Color (Style => Reset_All);
               end if;
            end loop;

            Self.Output.Write (")");
         end if;
         Self.Output.New_Line;
      end Put_Stats;

      Sc_Count : Natural := 0;
      St_Count : Natural := 0;
      Minutes  : Natural;
      Seconds  : Duration;

   begin
      Self.Output.Clear_Progress;

      for S in Scenarios'Range loop
         Sc_Count := Sc_Count + Scenarios (S);
      end loop;

      for S in Steps'Range loop
         St_Count := St_Count + Steps (S);
      end loop;

      Self.Output.New_Line;
      Self.Output.Writeln (Image (Features, 1) & " features");

      Self.Output.Write (Image (Sc_Count, 1) & " scenarios");
      Put_Stats (Sc_Count, Scenarios);

      Self.Output.Write (Image (St_Count, 1) & " steps");
      Put_Stats (St_Count, Steps);

      Minutes := Integer (Elapsed / 60.0);
      Seconds := Elapsed - Duration (Minutes) * 60.0;

      declare
         S : constant String := Seconds'Img;
      begin
         Self.Output.Writeln
           (Image (Minutes, 1) & "m" & S (S'First + 1 .. S'First + 5) & "s");
      end;
   end All_Features_Completed;

   --------------------
   -- Scenario_Start --
   --------------------

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario)
   is
   begin
      Display_Scenario_Header (Self, Scenario);
   end Scenario_Start;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario)
   is
      pragma Unreferenced (Scenario);
   begin
      Self.Output.New_Line;
   end Scenario_Completed;

   ---------------------------
   -- Nested_Scenario_Start --
   ---------------------------

   overriding procedure Nested_Scenario_Start
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario;
      Is_First : Boolean)
   is
      pragma Unreferenced (Scenario);
   begin
      if not Is_First then
         Self.Output.New_Line;
      end if;
   end Nested_Scenario_Start;

   --------------------
   -- Step_Completed --
   --------------------

   overriding procedure Step_Completed
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class) is
   begin
      Display_Step (Self, Scenario, Step);
   end Step_Completed;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Dots;
      Scenario : BDD.Features.Scenario)
   is
   begin
      Self.Output.Set_Color (Foreground => BDD.Step_Colors (Scenario.Status));

      case Scenario.Status is
         when Status_Passed    => Self.Output.Write (".");
         when Status_Failed    => Self.Output.Write ("F");
         when Status_Undefined => Self.Output.Write ("U");
         when Status_Skipped   => Self.Output.Write ("-");
      end case;

      Self.Output.Set_Color (Style => Reset_All);

      if Scenario.Status /= Status_Passed then
         Self.Failed.Append (Scenario);
      end if;
   end Scenario_Completed;

   ----------------------------
   -- All_Features_Completed --
   ----------------------------

   overriding procedure All_Features_Completed
     (Self      : in out Formatter_Dots;
      Features  : Natural;
      Scenarios : Count_Array;
      Steps     : Count_Array;
      Elapsed   : Duration)
   is
   begin
      Self.Output.Clear_Progress;

      if Self.Failed.Length /= 0 then
         Self.Output.New_Line;
         for S of Self.Failed loop
            Display_Scenario_And_Steps (Self, S);
         end loop;
         Self.Failed.Clear;
      end if;

      All_Features_Completed   --  inherited
        (Formatter (Self), Features, Scenarios, Steps, Elapsed);
   end All_Features_Completed;

   --------------------
   -- Scenario_Start --
   --------------------

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Quiet;
      Scenario : BDD.Features.Scenario)
   is
   begin
      Self.Output.Display_Progress (Scenario_Name (Scenario));
   end Scenario_Start;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Quiet;
      Scenario : BDD.Features.Scenario)
   is
      pragma Unreferenced (Scenario);
   begin
      Self.Output.Clear_Progress;
   end Scenario_Completed;

   --------------------
   -- Scenario_Start --
   --------------------

   procedure Scenario_Start
     (Self     : in out Formatter_Hide_Passed;
      Scenario : BDD.Features.Scenario) is
   begin
      Self.Output.Display_Progress (Scenario_Name (Scenario));
   end Scenario_Start;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Hide_Passed;
      Scenario : BDD.Features.Scenario)
   is
   begin
      case Scenario.Status is
         when Status_Passed | Status_Skipped =>
            null;

         when Status_Failed | Status_Undefined =>
            Self.Output.Clear_Progress;
            Display_Scenario_And_Steps (Self, Scenario);
      end case;
   end Scenario_Completed;

   ----------------------
   -- Create_Formatter --
   ----------------------

   function Create_Formatter return not null access Formatter'Class is
   begin
      case BDD.Output is
         when Output_Hide_Passed =>
            return new Formatter_Hide_Passed;
         when Output_Quiet =>
            return new Formatter_Quiet;
         when Output_Dots =>
            return new Formatter_Dots;
         when Output_Full =>
            return new Formatter_Full;
      end case;
   end Create_Formatter;

end BDD.Formatters;
