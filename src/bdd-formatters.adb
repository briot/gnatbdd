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

with Ada.Text_IO;    use Ada.Text_IO;
with GNATCOLL.Utils; use GNATCOLL.Utils;

package body BDD.Formatters is

   Scenario_Indent : constant String := "  ";
   Step_Indent     : constant String := "    ";
   --  Visual indentation in the display

   function Scenario_Name
     (Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
      return String;
   --  Return a string used to identify the scenario for the user.

   procedure Display_Location
     (Self     : Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   procedure Display_Location
     (Self     : Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class;
      Extra    : String := "");
   --  Display location information for the scenario or the step

   procedure Display_Scenario_Header
     (Self     : in out Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   --  Display the feature and scenario headers, as needed.

   procedure Display_Scenario_And_Steps
     (Self     : in out Formatter_Hide_Passed;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   --  Display a scenario and all its steps

   procedure Display_Step
     (Self     : in out Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class);
   --  Display the text for a specific step

   procedure Display_Progress
     (Self     : in out Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class);
   --  Display the name of the scenario being run, if supported by the
   --  terminal

   procedure Put_And_Align
     (Scenario : not null access BDD.Features.Scenario_Record'Class;
      Text     : String);
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

   function Scenario_Name
     (Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
      return String
   is
   begin
      return +Feature.File.Relative_Path (Features_Directory)
        & "#" & Image (Scenario.Index, 1)
        & ":" & Image (Scenario.Line, 1);
   end Scenario_Name;

   --------------------------------
   -- Display_Scenario_And_Steps --
   --------------------------------

   procedure Display_Scenario_And_Steps
     (Self     : in out Formatter_Hide_Passed;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
      procedure Show_Step
        (Step : not null access BDD.Features.Step_Record'Class);
      --  Display a step for a scenario

      procedure Show_Step
        (Step : not null access BDD.Features.Step_Record'Class) is
      begin
         Display_Step (Self, Feature, Scenario, Step);
      end Show_Step;
   begin
      Display_Scenario_Header (Self, Feature, Scenario);
      Scenario.Foreach_Step (Show_Step'Access);
      New_Line;
   end Display_Scenario_And_Steps;

   ----------------------
   -- Display_Location --
   ----------------------

   procedure Display_Location
     (Self     : Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => Grey);
      Put ("# " & Scenario_Name (Feature, Scenario));
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
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class;
      Extra    : String := "")
   is
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => Grey);
      Put ("# "
           & Extra
           & (+Feature.File.Relative_Path (Features_Directory))
           & ":" & Image (Step.Line, 1));
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);
      New_Line;
   end Display_Location;

   -------------------
   -- Put_And_Align --
   -------------------

   procedure Put_And_Align
     (Scenario : not null access BDD.Features.Scenario_Record'Class;
      Text     : String)
   is
      Long : constant Natural := Scenario.Longuest_Step + Step_Indent'Length;
   begin
      Put (Text);
      Put ((1 .. 1 + Long - Text'Length => ' '));
   end Put_And_Align;

   ----------------------
   -- Display_Progress --
   ----------------------

   procedure Display_Progress
     (Self     : in out Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
      Width : constant Integer := Self.Term.Get_Width;
   begin
      if Width /= -1 then
         declare
            N : constant String := Scenario_Name (Feature, Scenario);
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
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
   begin
      if Feature.Id /= Self.Last_Displayed_Feature_Id then
         Put_Line (Cst_Features & ' ' & Feature.Name);
         Put_Line (Feature.Description);
         Self.Last_Displayed_Feature_Id := Feature.Id;
      end if;

      Put_And_Align
        (Scenario, Scenario_Indent & Cst_Scenario & ' ' & Scenario.Name);
      Display_Location (Self, Feature, Scenario);
   end Display_Scenario_Header;

   ------------------
   -- Display_Step --
   ------------------

   procedure Display_Step
     (Self     : in out Formatter'Class;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class)
   is
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => BDD.Step_Colors (Step.Status));
      Put_And_Align (Scenario, Step_Indent & Step.Text);

      if Self.Term.Has_Colors then
         Self.Term.Set_Color
           (Term       => Ada.Text_IO.Standard_Output,
            Style      => Reset_All);
         Display_Location (Self, Feature, Step);
      else
         case Step.Status is
            when Status_Passed    =>
               Display_Location (Self, Feature, Step, "[OK] ");
            when Status_Failed    =>
               Display_Location (Self, Feature, Step, "[FAILED] ");
            when Status_Undefined =>
               Display_Location (Self, Feature, Step, "[UNDEFINED] ");
            when Status_Skipped   =>
               Display_Location (Self, Feature, Step, "[SKIPPED] ");
         end case;
      end if;

   end Display_Step;

   --------------------
   -- Scenario_Start --
   --------------------

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Full;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
   begin
      Clear_Progress (Self);
      Display_Scenario_Header (Self, Feature, Scenario);
      Display_Progress (Self, Feature, Scenario);
   end Scenario_Start;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Full;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status)
   is
      pragma Unreferenced (Feature, Scenario, Status);
   begin
      Clear_Progress (Self);
      New_Line;
   end Scenario_Completed;

   --------------------
   -- Step_Completed --
   --------------------

   overriding procedure Step_Completed
     (Self     : in out Formatter_Full;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Step     : not null access BDD.Features.Step_Record'Class) is
   begin
      Display_Step (Self, Feature, Scenario, Step);
   end Step_Completed;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Dots;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status)
   is
      pragma Unreferenced (Feature, Scenario);
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => BDD.Step_Colors (Status));

      case Status is
         when Status_Passed    => Put (".");
         when Status_Failed    => Put ("F");
         when Status_Undefined => Put ("U");
         when Status_Skipped   => Put ("-");
      end case;

      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);
   end Scenario_Completed;

   --------------------
   -- Scenario_Start --
   --------------------

   overriding procedure Scenario_Start
     (Self     : in out Formatter_Quiet;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
   begin
      Display_Progress (Self, Feature, Scenario);
   end Scenario_Start;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Quiet;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status)
   is
      pragma Unreferenced (Feature, Scenario, Status);
   begin
      Clear_Progress (Self);
   end Scenario_Completed;

   --------------------
   -- Scenario_Start --
   --------------------

   procedure Scenario_Start
     (Self     : in out Formatter_Hide_Passed;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class)
   is
   begin
      Display_Progress (Self, Feature, Scenario);
   end Scenario_Start;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : in out Formatter_Hide_Passed;
      Feature  : not null access BDD.Features.Feature_Record'Class;
      Scenario : not null access BDD.Features.Scenario_Record'Class;
      Status   : BDD.Scenario_Status)
   is
   begin
      case Status is
         when Status_Passed | Status_Skipped =>
            null;

         when Status_Failed | Status_Undefined =>
            Clear_Progress (Self);
            Display_Scenario_And_Steps (Self, Feature, Scenario);
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
