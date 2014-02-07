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

--  A feature and its scenarios.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Refcount;     use GNATCOLL.Refcount;

package BDD.Features is

   ----------
   -- Step --
   ----------

   type Step_Record (<>) is tagged private;
   type Step is access all Step_Record'Class;
   --  A step is owned by its scenario, so must not be freed explictly.

   function Create
     (Text : String; Line : Positive) return not null access Step_Record;
   --  Create a new step.
   --  The line references the file contain the feature in which the step is
   --  found.

   procedure Add_To_Multiline
     (Self : not null access Step_Record'Class; Text : String);
   --  Add some contents to the final multi-line string.
   --  Text is always added a ASCII.LF

   function Line (Self : not null access Step_Record) return Positive;
   function Text (Self : not null access Step_Record) return String;
   function Multiline (Self : not null access Step_Record) return String;
   --  Return the components of the step

   procedure Set_Status
     (Self   : not null access Step_Record;
      Status : BDD.Scenario_Status);
   function Status
     (Self   : not null access Step_Record) return BDD.Scenario_Status;
   --  Set the status for a specific step

   -------------
   -- Feature --
   -------------

   type Feature is tagged private;
   --  An object that represents a single feature and all its scenarios.
   --  A specific features file might contain several features, but this object
   --  only represents one of them.
   --  This is a ref-counted type

   No_Feature : constant Feature;

   function Create
     (File : GNATCOLL.VFS.Virtual_File;
      Name : String) return Feature;
   --  Create and initialize a new feature

   function Id (Self : Feature) return Integer;
   --  Set a unique id for the feature

   function Name (Self : Feature) return String;
   --  The name of the feature

   function File (Self : Feature) return GNATCOLL.VFS.Virtual_File;
   --  The file in which the feature is defined

   function Description (Self : Feature) return String;
   procedure Add_To_Description (Self : Feature; Descr : String);
   --  Add some description information. Add_Description will be called once
   --  for each line in the description.

   --------------
   -- Scenario --
   --------------

   type Scenario is tagged private;
   --  A scenario to be run within a feature.
   --  This is a ref-counted type.

   No_Scenario : constant Scenario;

   type Scenario_Kind is (Kind_Scenario, Kind_Background, Kind_Outline);

   function Create
     (Feature : BDD.Features.Feature'Class;
      Kind    : Scenario_Kind;
      Name    : String;
      Line    : Positive;
      Index   : Positive) return Scenario;
   --  Creates a new scenario.

   function Name        (Self : Scenario) return String;
   function Line        (Self : Scenario) return Positive;
   function Index       (Self : Scenario) return Positive;
   function Kind        (Self : Scenario) return Scenario_Kind;
   function Get_Feature (Self : Scenario) return Feature'Class;
   --  Retrieve the attributes of Self

   procedure Add (Self : Scenario; S : not null access Step_Record'Class);
   --  Add a new step

   procedure Foreach_Step
     (Self     : Scenario;
      Callback : not null access procedure
        (S : not null access Step_Record'Class));
   --  Iterate over each step

   procedure Set_Status (Self : Scenario; Status : BDD.Scenario_Status);
   function Status (Self : Scenario) return BDD.Scenario_Status;
   --  Set the status for a specific step

   function Longuest_Step (Self : Scenario) return Natural;
   --  The length of the longuest step (for display purposes)

private
   type Step_Record is tagged record
      Line      : Positive;
      Text      : Ada.Strings.Unbounded.Unbounded_String;
      Multiline : Ada.Strings.Unbounded.Unbounded_String;
      Status    : BDD.Scenario_Status;
   end record;

   package Step_Lists is new Ada.Containers.Doubly_Linked_Lists (Step);
   type Step_List is new Step_Lists.List with null record;
   overriding procedure Clear (List : in out Step_List);

   procedure Free (Self : in out Step_Record);
   procedure Free (Self : in out Step);
   --  Free the memory associated with Self

   type Feature_Record is new GNATCOLL.Refcount.Refcounted with record
      File        : GNATCOLL.VFS.Virtual_File;
      Name        : GNAT.Strings.String_Access;
      Id          : Integer;
      Description : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   overriding procedure Free (Self : in out Feature_Record);
   package Feature_Pointers is new GNATCOLL.Refcount.Smart_Pointers
     (Feature_Record);
   type Feature is new Feature_Pointers.Ref with null record;

   No_Feature : constant Feature :=
     (Feature_Pointers.Null_Ref with null record);

   type Scenario_Record is new Refcounted with record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Line          : Positive := 1;
      Index         : Positive := 1;
      Kind          : Scenario_Kind := Kind_Scenario;
      Steps         : Step_List;
      Longuest_Step : Integer := -1;
      Status        : BDD.Scenario_Status;
      Feature       : BDD.Features.Feature;
   end record;

   overriding procedure Free (Self : in out Scenario_Record);

   package Scenario_Pointers is new GNATCOLL.Refcount.Smart_Pointers
     (Scenario_Record);
   type Scenario is new Scenario_Pointers.Ref with null record;

   No_Scenario : constant Scenario :=
     (Scenario_Pointers.Null_Ref with null record);

end BDD.Features;
