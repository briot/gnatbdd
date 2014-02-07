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

package body BDD.Features is
   Me : constant Trace_Handle := Create ("BDD.FEATURES");

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
   begin
      Trace (Me, "Free scenario index=" & Self.Index'Img);
      Self.Name  := Null_Unbounded_String;
      Self.Line  := 1;
      Self.Index := 1;
      Self.Kind  := Kind_Scenario;
      Self.Feature := No_Feature;
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
   begin
      for S of Self.Get.Steps loop
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
     (Self   : not null access Step_Record;
      Status : BDD.Scenario_Status)
   is
   begin
      Self.Status := Status;
   end Set_Status;

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

end BDD.Features;
