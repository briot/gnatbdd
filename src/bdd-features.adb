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

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Feature) is
   begin
      Self.File := No_File;
      Self.Id := -1;
      Free (Self.Name);
      Self.Description := Null_Unbounded_String;
   end Free;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : in out Feature; Name : String) is
   begin
      Free (Self.Name);
      Self.Name := new String'(Trim (Name, Both));
   end Set_Name;

   ----------
   -- Name --
   ----------

   function Name (Self : Feature) return String is
   begin
      if Self.Name = null then
         return "";
      end if;
      return Self.Name.all;
   end Name;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Self : in out Feature; File : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.File := File;
   end Set_File;

   ----------
   -- File --
   ----------

   function File (Self : Feature) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end File;

   -------------------
   -- Set_Unique_Id --
   -------------------

   procedure Set_Unique_Id (Self : in out Feature; Id : Integer) is
   begin
      Self.Id := Id;
   end Set_Unique_Id;

   --------
   -- Id --
   --------

   function Id (Self : Feature) return Integer is
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

   function Description (Self : Feature) return String is
   begin
      return To_String (Self.Description);
   end Description;

   ---------------------
   -- Add_Description --
   ---------------------

   procedure Add_Description (Self : in out Feature; Descr : String) is
   begin
      --  Indent the description as it will be displayed
      Append (Self.Description, "  " & Descr & ASCII.LF);
   end Add_Description;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Self : not null access Step_Record'Class; Text : String)
   is
   begin
      Append (Self.Text, Text);
   end Set_Text;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Step_Record) is
   begin
      Self.Text := Null_Unbounded_String;
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

end BDD.Features;
