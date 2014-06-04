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
with Ada.Text_IO;         use Ada.Text_IO;
with BDD.Asserts_Generic; use BDD.Asserts_Generic;
with BDD.Tables;          use BDD.Tables;
with GNATCOLL.Utils;      use GNATCOLL.Utils;
with GNAT.Regpat;         use GNAT.Regpat;

package body BDD.Formatters is

   Scenario_Indent : constant String := "  ";
   Step_Indent     : constant String := "    ";
   --  Visual indentation in the display

   function Scenario_Name (Scenario : BDD.Features.Scenario) return String;
   --  Return a string used to identify the scenario for the user.

   procedure Display_Location
     (Self     : Formatter'Class; Scenario : BDD.Features.Scenario);
   procedure Display_Location
     (Self     : Formatter'Class;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class;
      Extra    : String := "");
   --  Display location information for the scenario or the step

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

   procedure Display_Progress
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario);
   --  Display the name of the scenario being run, if supported by the
   --  terminal

   function Text_And_Align
     (Scenario : BDD.Features.Scenario;
      Text     : String) return String;
   --  Display text (assuming at the beginning of a line), and adds trailing
   --  spaces so that the cursor is left aligned at a column suitable for
   --  displaying the location.

   procedure Clear_Progress (Self : in out Formatter'Class);
   --  Clear progress indicator if needed.

   ----------
   -- Init --
   ----------

   procedure Init
     (Self    : in out Formatter;
      Term    : GNATCOLL.Terminal.Terminal_Info_Access)
   is
   begin
      Self.Term := Term;
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
         New_Line;
      end Show_Scenario;

   begin
      Display_Scenario_Header (Self, Scenario);

      case Scenario.Kind is
         when Kind_Scenario | Kind_Background =>
            Scenario.Foreach_Step (Show_Step'Access);
            New_Line;

         when Kind_Outline =>
            Scenario.Foreach_Scenario (Show_Scenario'Access);
      end case;
   end Display_Scenario_And_Steps;

   ----------------------
   -- Display_Location --
   ----------------------

   procedure Display_Location
     (Self     : Formatter'Class;
      Scenario : BDD.Features.Scenario)
   is
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => Grey);
      Put ("# " & Scenario_Name (Scenario));
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);
      New_Line;
   end Display_Location;

   ----------------------
   -- Display_Location --
   ----------------------

   procedure Display_Location
     (Self     : Formatter'Class;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class;
      Extra    : String := "")
   is
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => Grey);
      Put ("# "
           & Extra
           & (+Scenario.Get_Feature.File.Relative_Path (Features_Directory))
           & ":" & Image (Step.Line, 1));
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);
      New_Line;
   end Display_Location;

   --------------------
   -- Text_And_Align --
   --------------------

   function Text_And_Align
     (Scenario : BDD.Features.Scenario;
      Text     : String) return String
   is
      Long : constant Natural := Scenario.Longuest_Step + Step_Indent'Length;
   begin
      return Text & (1 .. 1 + Long - Text'Length => ' ');
   end Text_And_Align;

   ----------------------
   -- Display_Progress --
   ----------------------

   procedure Display_Progress
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario)
   is
      Width : constant Integer := Self.Term.Get_Width;
   begin
      if Width /= -1 then
         declare
            N : constant String := Scenario_Name (Scenario);
         begin
            Self.Term.Beginning_Of_Line;
            Self.Term.Clear_To_End_Of_Line;
            Self.Term.Set_Color
              (Term       => Ada.Text_IO.Standard_Output,
               Foreground => Grey);
            Put
              ("Running: "
               & N (N'First .. Integer'Min (N'Last, N'First + Width - 10)));
            Self.Term.Set_Color
              (Term       => Ada.Text_IO.Standard_Output,
               Style      => Reset_All);
            Self.Term.Beginning_Of_Line;

            Self.Progress_Displayed := True;
         end;
      end if;
   end Display_Progress;

   --------------------
   -- Clear_Progress --
   --------------------

   procedure Clear_Progress (Self : in out Formatter'Class) is
   begin
      if Self.Progress_Displayed then
         Self.Progress_Displayed := False;

         Self.Term.Beginning_Of_Line;
         Self.Term.Clear_To_End_Of_Line;
      end if;
   end Clear_Progress;

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
         Put_Line (Cst_Features & ' ' & F.Name);
         Put_Line (F.Description);
         Self.Last_Displayed_Feature_Id := F.Id;
      end if;

      Put (Text_And_Align
           (Scenario,
              Scenario_Indent & Scenario.Prefix & ' ' & Scenario.Name));
      Display_Location (Self, Scenario);
   end Display_Scenario_Header;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (File   : Ada.Text_IO.File_Type;
      Text   : String;
      Prefix : String := "")
   is
      Start, Last : Integer;
   begin
      Start := Text'First;
      while Start <= Text'Last loop
         Last := Line_End (Text, Start);
         if Last < Start then  --  empty line
            New_Line (File);
            Start := Last + 2;
         else
            Put (File, Prefix & Text (Start .. Last));
            Start := Last + 1;
         end if;
      end loop;
   end Indent;

   ------------------
   -- Display_Step --
   ------------------

   procedure Display_Step
     (Self     : in out Formatter'Class;
      Scenario : BDD.Features.Scenario;
      Step     : not null access BDD.Features.Step_Record'Class)
   is
      Info : constant Match_Array := Step.Match_Info;
   begin
      if Self.Term.Has_Colors then
         declare
            N          : constant String :=
              Text_And_Align (Scenario, Step_Indent & Step.Text);
            H          :
              array (Step_Indent'Length + 1 .. N'Length) of Boolean :=
              (others => False);
            First, Last : Integer;
         begin
            for M in 1 .. Info'Last loop
               if Info (M) /= No_Match then
                  H (Info (M).First + Step_Indent'Length ..
                       Info (M).Last + Step_Indent'Length) :=
                    (others => True);
               end if;
            end loop;

            First := N'First + Step_Indent'Length;
            Put (N (N'First .. First - 1));  --  indentation

            while First <= N'Last loop
               Last := First;
               while Last <= N'Last and then not H (Last) loop
                  Last := Last + 1;
               end loop;
               if First <= Last - 1 then
                  Self.Term.Set_Color
                    (Term       => Ada.Text_IO.Standard_Output,
                     Foreground => BDD.Step_Colors (Step.Status));
                  Put (N (First .. Last - 1));
               end if;

               First := Last;
               while First <= N'Last and then H (First) loop
                  First := First + 1;
               end loop;

               if Last <= First - 1 then
                  Self.Term.Set_Color
                    (Term       => Ada.Text_IO.Standard_Output,
                     Foreground => BDD.Config_Color);
                  Put (N (Last .. First - 1));
               end if;
            end loop;
         end;

      else
         Put (Text_And_Align (Scenario, Step_Indent & Step.Text));
      end if;

      if Self.Term.Has_Colors then
         Display_Location (Self, Scenario, Step);
      else
         case Step.Status is
            when Status_Passed    =>
               Display_Location (Self, Scenario, Step, "[OK] ");
            when Status_Failed    =>
               Display_Location (Self, Scenario, Step, "[FAILED] ");
            when Status_Undefined =>
               Display_Location (Self, Scenario, Step, "[UNDEFINED] ");
            when Status_Skipped   =>
               Display_Location (Self, Scenario, Step, "[SKIPPED] ");
         end case;
      end if;

      declare
         Multi : constant String := Step.Multiline;
      begin
         if Multi /= "" then
            Self.Term.Set_Color
              (Term       => Ada.Text_IO.Standard_Output,
               Foreground => BDD.Step_Colors (Step.Status));
            Put_Line ("      """"""");
            Indent (Ada.Text_IO.Standard_Output, Multi, Prefix => "      ");
            Put_Line ("      """"""");
         end if;
      end;

      declare
         Error : constant Assert_Error := Step.Error_Details;
      begin
         Self.Term.Set_Color
           (Term       => Ada.Text_IO.Standard_Output,
            Foreground => BDD.Step_Colors (Step.Status));

         if Error /= No_Error then
            Display (Error, Self.Term, Ada.Text_IO.Standard_Output,
                     Prefix => "      ");
         else
            --  Display the default step's table, since it full matched or was
            --  not even run.
            if Step.Table /= No_Table then
               Step.Table.Display
                 (Ada.Text_IO.Standard_Output, Prefix => "      ");
            end if;
         end if;
      end;

      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);
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
            Put (" (");

            for S in Stats'Range loop
               if Stats (S) /= 0 then
                  if not Is_First then
                     Put (", ");
                  end if;
                  Is_First := False;

                  Self.Term.Set_Color
                    (Term       => Ada.Text_IO.Standard_Output,
                     Foreground => BDD.Step_Colors (S));

                  Put (Image (Stats (S), 1));
                  Put (" ");

                  case S is
                     when Status_Passed    => Put ("passed");
                     when Status_Failed    => Put ("failed");
                     when Status_Skipped   => Put ("skipped");
                     when Status_Undefined => Put ("undefined");
                  end case;

                  Self.Term.Set_Color
                    (Term       => Ada.Text_IO.Standard_Output,
                     Style      => Reset_All);
               end if;
            end loop;

            Put (")");
         end if;
         New_Line;
      end Put_Stats;

      Sc_Count : Natural := 0;
      St_Count : Natural := 0;
      Minutes  : Natural;
      Seconds  : Duration;

   begin
      Clear_Progress (Self);

      for S in Scenarios'Range loop
         Sc_Count := Sc_Count + Scenarios (S);
      end loop;

      for S in Steps'Range loop
         St_Count := St_Count + Steps (S);
      end loop;

      New_Line;

      Put_Line (Image (Features, 1) & " features");

      Put (Image (Sc_Count, 1) & " scenarios");
      Put_Stats (Sc_Count, Scenarios);

      Put (Image (St_Count, 1) & " steps");
      Put_Stats (St_Count, Steps);

      Minutes := Integer (Elapsed / 60.0);
      Seconds := Elapsed - Duration (Minutes) * 60.0;

      declare
         S : constant String := Seconds'Img;
      begin
         Put_Line
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
      pragma Unreferenced (Self, Scenario);
   begin
      New_Line;
   end Scenario_Completed;

   ---------------------------
   -- Nested_Scenario_Start --
   ---------------------------

   overriding procedure Nested_Scenario_Start
     (Self     : in out Formatter_Full;
      Scenario : BDD.Features.Scenario;
      Is_First : Boolean)
   is
      pragma Unreferenced (Self, Scenario);
   begin
      if not Is_First then
         New_Line;
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
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => BDD.Step_Colors (Scenario.Status));

      case Scenario.Status is
         when Status_Passed    => Put (".");
         when Status_Failed    => Put ("F");
         when Status_Undefined => Put ("U");
         when Status_Skipped   => Put ("-");
      end case;

      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);

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
      Clear_Progress (Self);

      if Self.Failed.Length /= 0 then
         New_Line;
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
      Display_Progress (Self, Scenario);
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
      Clear_Progress (Self);
   end Scenario_Completed;

   --------------------
   -- Scenario_Start --
   --------------------

   procedure Scenario_Start
     (Self     : in out Formatter_Hide_Passed;
      Scenario : BDD.Features.Scenario)
   is
   begin
      Display_Progress (Self, Scenario);
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
            Clear_Progress (Self);
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
