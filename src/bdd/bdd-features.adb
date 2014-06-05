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

with Ada.Strings.Fixed;          use Ada.Strings, Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

package body BDD.Features is
   Me : constant Trace_Handle := Create ("BDD.FEATURES");

   Substitution_Re : constant Pattern_Matcher := Compile ("<([^>]+)>");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Match_Array, Match_Array_Access);

   protected type Ids is
      procedure Get_Next (Id : out Integer);
      --  Get the next unique id

   private
      Current : Integer := 0;
   end Ids;

   ---------
   -- Ids --
   ---------

   protected body Ids is

      --------------
      -- Get_Next --
      --------------

      procedure Get_Next (Id : out Integer) is
      begin
         Current := Current + 1;
         Id := Current;
      end Get_Next;

   end Ids;

   Feature_Ids : Ids;

   ------------
   -- Create --
   ------------

   function Create
     (File : GNATCOLL.VFS.Virtual_File;
      Name : String)
      return Feature
   is
      R : constant not null access Feature_Record := new Feature_Record;
      Self : Feature;
   begin
      Self.Set (R);
      R.Name := new String'(Trim (Name, Both));
      R.File := File;
      Feature_Ids.Get_Next (R.Id);
      Trace (Me, "Create feature " & R.Id'Img);
      return Self;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Feature_Record) is
   begin
      Trace (Me, "Free feature" & Self.Id'Img);
      Self.File := No_File;
      Self.Id := -1;
      Free (Self.Name);
      Self.Description := Null_Unbounded_String;
   end Free;

   ----------
   -- Name --
   ----------

   function Name (Self : Feature) return String is
   begin
      if Self.Get.Name = null then
         return "";
      end if;
      return Self.Get.Name.all;
   end Name;

   ----------
   -- File --
   ----------

   function File (Self : Feature) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Get.File;
   end File;

   --------
   -- Id --
   --------

   function Id (Self : Feature) return Integer is
   begin
      return Self.Get.Id;
   end Id;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Scenario_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scenario_Array, Scenario_Array_Access);
   begin
      Trace (Me, "Free scenario index=" & Self.Index'Img);
      Self.Name  := Null_Unbounded_String;
      Self.Line  := 1;
      Self.Index := 1;
      Self.Kind  := Kind_Scenario;
      Self.Feature := No_Feature;
      Unchecked_Free (Self.Example_Scenarios);
      Self.Steps.Clear;
   end Free;

   ----------
   -- Name --
   ----------

   function Name (Self : Scenario) return String is
   begin
      return To_String (Self.Get.Name);
   end Name;

   ----------
   -- Line --
   ----------

   function Line (Self : Scenario) return Positive is
   begin
      return Self.Get.Line;
   end Line;

   -----------
   -- Index --
   -----------

   function Index (Self : Scenario) return Positive is
   begin
      return Self.Get.Index;
   end Index;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Scenario) return Scenario_Kind is
   begin
      return Self.Get.Kind;
   end Kind;

   -----------------
   -- Get_Feature --
   -----------------

   function Get_Feature (Self : Scenario) return Feature'Class is
   begin
      return Self.Get.Feature;
   end Get_Feature;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (Self : Scenario; Status : BDD.Scenario_Status) is
   begin
      Self.Get.Status := Status;
   end Set_Status;

   ------------
   -- Status --
   ------------

   function Status (Self : Scenario) return BDD.Scenario_Status is
   begin
      return Self.Get.Status;
   end Status;

   ---------
   -- Add --
   ---------

   procedure Add (Self : Scenario; S : not null access Step_Record'Class) is
   begin
      Self.Get.Steps.Append (S);
   end Add;

   ------------------
   -- Foreach_Step --
   ------------------

   procedure Foreach_Step
     (Self : Scenario;
      Callback : not null access procedure
        (Scenario : BDD.Features.Scenario;
         Step     : not null access Step_Record'Class))
   is
      SR  : constant access Scenario_Record := Self.Get;
   begin
      for S of SR.Steps loop
         Callback (Self, S);
      end loop;
   end Foreach_Step;

   -------------------
   -- Longuest_Step --
   -------------------

   function Longuest_Step (Self : Scenario) return Natural is
      R : constant not null access Scenario_Record'Class := Self.Get;
   begin
      if R.Longuest_Step = -1 then
         R.Longuest_Step := Length (R.Name) + Cst_Scenario'Length + 1;
         for S of R.Steps loop
            R.Longuest_Step := Integer'Max (R.Longuest_Step, Length (S.Text));
         end loop;
      end if;
      return R.Longuest_Step;
   end Longuest_Step;

   ------------
   -- Create --
   ------------

   function Create
     (Feature : BDD.Features.Feature'Class;
      Kind  : Scenario_Kind;
      Name  : String;
      Line  : Positive;
      Index : Positive) return Scenario
   is
      Self : Scenario;
      R    : constant not null access Scenario_Record := new Scenario_Record;
   begin
      Self.Set (R);
      R.Name    := To_Unbounded_String (Trim (Name, Both));
      R.Line    := Line;
      R.Index   := Index;
      R.Kind    := Kind;
      R.Feature := BDD.Features.Feature (Feature);
      Trace (Me, "Create scenario index=" & Index'Img);
      return Self;
   end Create;

   -----------------
   -- Description --
   -----------------

   function Description (Self : Feature) return String is
   begin
      return To_String (Self.Get.Description);
   end Description;

   ------------------------
   -- Add_To_Description --
   ------------------------

   procedure Add_To_Description (Self : Feature; Descr : String) is
   begin
      --  Indent the description as it will be displayed
      Append (Self.Get.Description, "  " & Descr & ASCII.LF);
   end Add_To_Description;

   ------------
   -- Create --
   ------------

   function Create
     (Text : String; Line : Positive) return not null access Step_Record
   is
      Self : constant Step := new Step_Record;
   begin
      Self.Text   := To_Unbounded_String (Text);
      Self.Line   := Line;
      Self.Status := Status_Undefined;
      Trace (Me, "Create step at line" & Line'Img);
      return Self;
   end Create;

   ----------------------
   -- Add_To_Multiline --
   ----------------------

   procedure Add_To_Multiline
     (Self : not null access Step_Record'Class; Text : String) is
   begin
      Append (Self.Multiline, Text & ASCII.LF);
   end Add_To_Multiline;

   ----------
   -- Line --
   ----------

   function Line (Self : not null access Step_Record) return Positive is
   begin
      return Self.Line;
   end Line;

   ----------
   -- Text --
   ----------

   function Text (Self : not null access Step_Record) return String is
   begin
      return To_String (Self.Text);
   end Text;

   ---------------
   -- Multiline --
   ---------------

   function Multiline (Self : not null access Step_Record) return String is
   begin
      return To_String (Self.Multiline);
   end Multiline;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Step_Record) is
   begin
      Trace (Me, "Free step at line" & Self.Line'Img);
      Self.Text      := Null_Unbounded_String;
      Self.Multiline := Null_Unbounded_String;
      Self.Error     := No_Error;
      Self.Status    := Status_Passed;
      Self.Table     := No_Table;
      Unchecked_Free (Self.Match);
      Self.Line      := 1;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Step) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Step_Record'Class, Step);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (List : in out Step_List) is
   begin
      for S of List loop
         Free (S);
      end loop;

      Step_Lists.Clear (Step_Lists.List (List));  --  inherited
   end Clear;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Self      : not null access Step_Record;
      Status    : BDD.Scenario_Status;
      Details   : Assert_Error := No_Error)
   is
   begin
      Self.Status := Status;
      Self.Error  := Details;
   end Set_Status;

   -------------------
   -- Error_Details --
   -------------------

   function Error_Details
     (Self : not null access Step_Record) return Assert_Error is
   begin
      return Self.Error;
   end Error_Details;

   ------------
   -- Status --
   ------------

   function Status
     (Self   : not null access Step_Record) return BDD.Scenario_Status is
   begin
      return Self.Status;
   end Status;

   ------------
   -- Prefix --
   ------------

   function Prefix (Self : Scenario) return String is
   begin
      case Self.Get.Kind is
         when Kind_Scenario   => return Cst_Scenario;
         when Kind_Background => return Cst_Background;
         when Kind_Outline    => return Cst_Scenario_Outline;
      end case;
   end Prefix;

   ------------------
   -- Add_To_Table --
   ------------------

   procedure Add_To_Table
     (Self : not null access Step_Record'Class; Row : String)
   is
   begin
      if Self.Table = No_Table then
         Self.Table := Create;
      end if;

      Self.Table.Add_Row_As_String (Row);
   end Add_To_Table;

   ---------------------
   -- Add_Example_Row --
   ---------------------

   procedure Add_Example_Row (Self : Scenario; Row  : String) is
      SR : constant access Scenario_Record := Self.Get;
   begin
      if SR.Examples = No_Table then
         SR.Examples := Create;
      end if;
      SR.Examples.Add_Row_As_String (Row);
   end Add_Example_Row;

   -----------
   -- Table --
   -----------

   function Table
     (Self : not null access Step_Record) return BDD.Tables.Table
   is
   begin
      return Self.Table;
   end Table;

   --------------------
   -- Set_Match_Info --
   --------------------

   procedure Set_Match_Info
     (Self  : not null access Step_Record;
      Match : GNAT.Regpat.Match_Array)
   is
   begin
      Unchecked_Free (Self.Match);
      Self.Match := new Match_Array'(Match);
   end Set_Match_Info;

   ----------------
   -- Match_Info --
   ----------------

   function Match_Info
     (Self : not null access Step_Record) return GNAT.Regpat.Match_Array
   is
   begin
      if Self.Match /= null then
         return Self.Match.all;
      else
         return Match_Array'(1 .. 0 => No_Match);
      end if;
   end Match_Info;

   --------------------
   -- Should_Execute --
   --------------------

   function Should_Execute
     (Self    : not null access Step_Record'Class;
      Text    : String;
      Matches : in out GNAT.Regpat.Match_Array;
      Regexp  : GNAT.Regpat.Pattern_Matcher)
      return Boolean
   is
   begin
      Match (Regexp, Text, Matches);
      if Matches (0) /= No_Match then
         Self.Set_Match_Info (Matches);
         return True;
      end if;
      return False;
   end Should_Execute;

   ---------
   -- Run --
   ---------

   procedure Run
     (Step         : not null access Step_Record'Class;
      Execute      : Boolean;
      Step_Runners : Step_Runner_Lists.List)
   is
      Text    : constant String := To_String (Step.Text);
      First   : Integer := Text'First;
   begin
      Step.Set_Status (Status_Undefined);

      --  Skip the leading 'Given|Then|...' keywords, which are irrelevant
      --  for the purpose of the match

      while First <= Text'Last
        and then not Is_Whitespace (Text (First))
      loop
         First := First + 1;
      end loop;
      Skip_Blanks (Text, First);

      for R of Step_Runners loop
         begin
            --  Run the step, or at least check whether it is defined.
            if Execute then
               Step.Set_Status (Status_Passed);
            else
               Step.Set_Status (Status_Skipped);
            end if;

            --  Will set status to undefined if necessary
            R (Step, Text (First .. Text'Last), Execute => Execute);
            exit when Step.Status /= Status_Undefined;

         exception
            when E : Unexpected_Result =>
               Step.Set_Status (Status_Failed, Get (E));
               exit;
            when E : others =>
               Step.Set_Status (Status_Failed, From_Exception (E));
               exit;
         end;
      end loop;
   end Run;

   ----------------------
   -- Foreach_Scenario --
   ----------------------

   procedure Foreach_Scenario
     (Self     : Scenario;
      Callback : not null access procedure (Scenario : BDD.Features.Scenario))
   is
      SR  : constant access Scenario_Record := Self.Get;
      Tmp : Scenario;
      Tmp_Step : Step;

      function Substitute (Text : String; Row  : Positive) return String;
      --  Update the text of S to substitute text with the examples.
      --  This always properly restores the original text

      function Substitute (Text : String; Row  : Positive) return String is
         T       : Unbounded_String := To_Unbounded_String (Text);
         Matches : Match_Array (0 .. 1);
      begin
         loop
            declare
               Tmp : constant String := To_String (T);
            begin
               Match (Substitution_Re, Tmp, Matches);
               exit when Matches (0) = No_Match;
               Replace_Slice
                 (T, Matches (0).First, Matches (0).Last,
                  By => SR.Examples.Get
                    (Column => Tmp (Matches (1).First .. Matches (1).Last),
                     Row    => Row));
            end;
         end loop;
         return To_String (T);
      end Substitute;

   begin
      case SR.Kind is
         when Kind_Background | Kind_Scenario =>
            Callback (Self);

         when Kind_Outline =>
            if SR.Example_Scenarios = null then
               SR.Example_Scenarios := new Scenario_Array
                 (1 .. SR.Examples.Height);

               for Row in SR.Example_Scenarios'Range loop
                  Trace (Me, "Create new scenario");
                  Tmp := Create
                    (Feature => Self.Get_Feature,
                     Kind    => Kind_Scenario,
                     Name    => Self.Name,
                     Line    => Self.Line,
                     Index   => Self.Index);

                  for S of SR.Steps loop
                     Tmp_Step := Create
                       (Text => Substitute (To_String (S.Text), Row),
                        Line => S.Line);
                     Tmp_Step.Multiline := S.Multiline;
                     Tmp_Step.Table     := S.Table;

                     Tmp.Add (Tmp_Step);
                  end loop;

                  SR.Example_Scenarios (Row) := Tmp;
               end loop;
            end if;

            for Row in SR.Example_Scenarios'Range loop
               Callback (SR.Example_Scenarios (Row));
            end loop;
      end case;
   end Foreach_Scenario;

end BDD.Features;
