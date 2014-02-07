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

package body BDD.Features is

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
      return not null access Feature_Record
   is
      Self : constant Feature := new Feature_Record;
   begin
      Self.Name := new String'(Trim (Name, Both));
      Self.File := File;
      Feature_Ids.Get_Next (Self.Id);
      return Self;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Feature_Record) is
   begin
      Self.File := No_File;
      Self.Id := -1;
      Free (Self.Name);
      Self.Description := Null_Unbounded_String;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Feature) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Feature_Record'Class, Feature);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Name --
   ----------

   function Name (Self : not null access Feature_Record) return String is
   begin
      if Self.Name = null then
         return "";
      end if;
      return Self.Name.all;
   end Name;

   ----------
   -- File --
   ----------

   function File
     (Self : not null access Feature_Record)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end File;

   --------
   -- Id --
   --------

   function Id (Self : not null access Feature_Record) return Integer is
   begin
      return Self.Id;
   end Id;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Scenario_Record) is
   begin
      Self.Name  := Null_Unbounded_String;
      Self.Line  := 1;
      Self.Index := 1;
      Self.Kind  := Kind_Scenario;
      Self.Steps.Clear;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Scenario) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scenario_Record'Class, Scenario);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Name --
   ----------

   function Name (Self : not null access Scenario_Record) return String is
   begin
      return To_String (Self.Name);
   end Name;

   ----------
   -- Line --
   ----------

   function Line (Self : not null access Scenario_Record) return Positive is
   begin
      return Self.Line;
   end Line;

   -----------
   -- Index --
   -----------

   function Index (Self : not null access Scenario_Record) return Positive is
   begin
      return Self.Index;
   end Index;

   ----------
   -- Kind --
   ----------

   function Kind
     (Self : not null access Scenario_Record) return Scenario_Kind is
   begin
      return Self.Kind;
   end Kind;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : not null access Scenario_Record;
      S    : not null access Step_Record'Class)
   is
   begin
      Self.Steps.Append (S);
   end Add;

   ------------------
   -- Foreach_Step --
   ------------------

   procedure Foreach_Step
     (Self : not null access Scenario_Record;
      Callback : not null access procedure
        (S : not null access Step_Record'Class))
   is
   begin
      for S of Self.Steps loop
         Callback (S);
      end loop;
   end Foreach_Step;

   -------------------
   -- Longuest_Step --
   -------------------

   function Longuest_Step
     (Self : not null access Scenario_Record) return Natural is
   begin
      if Self.Longuest_Step = -1 then
         Self.Longuest_Step := Length (Self.Name) + Cst_Scenario'Length + 1;
         for S of Self.Steps loop
            Self.Longuest_Step := Integer'Max
              (Self.Longuest_Step, Length (S.Text));
         end loop;
      end if;
      return Self.Longuest_Step;
   end Longuest_Step;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Self  : not null access Scenario_Record;
      Kind  : Scenario_Kind;
      Name  : String;
      Line  : Positive;
      Index : Positive) is
   begin
      Self.Name  := To_Unbounded_String (Trim (Name, Both));
      Self.Line  := Line;
      Self.Index := Index;
      Self.Kind  := Kind;
   end Set_Attributes;

   -----------------
   -- Description --
   -----------------

   function Description
     (Self : not null access Feature_Record) return String is
   begin
      return To_String (Self.Description);
   end Description;

   ------------------------
   -- Add_To_Description --
   ------------------------

   procedure Add_To_Description
     (Self : not null access Feature_Record; Descr : String) is
   begin
      --  Indent the description as it will be displayed
      Append (Self.Description, "  " & Descr & ASCII.LF);
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

end BDD.Features;
