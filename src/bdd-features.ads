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

package BDD.Features is

   ----------
   -- Step --
   ----------

   type Step_Record (<>) is tagged private;
   type Step is access all Step_Record'Class;

   function Create
     (Text : String; Line : Positive) return not null access Step_Record;
   --  Create a new step.
   --  The line references the file contain the feature in which the step is
   --  found.

   procedure Add_To_Multiline
     (Self : not null access Step_Record'Class; Text : String);
   --  Add some contents to the final multi-line string.
   --  Text is always added a ASCII.LF

   procedure Free (Self : in out Step_Record);
   procedure Free (Self : in out Step);
   --  Free the memory associated with Self

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

   --------------
   -- Scenario --
   --------------

   type Scenario_Record is tagged private;
   type Scenario is access all Scenario_Record'Class;
   --  A scenario to be run within a feature

   procedure Free (Self : in out Scenario_Record);
   procedure Free (Self : in out Scenario);
   --  Free the memory associated with Self

   type Scenario_Kind is (Kind_Scenario, Kind_Background, Kind_Outline);

   procedure Set_Attributes
     (Self  : not null access Scenario_Record;
      Kind  : Scenario_Kind;
      Name  : String;
      Line  : Positive;
      Index : Positive);
   --  Set the line of the feature file at which the scenario is defined, and
   --  its index within its Feature.

   function Name  (Self : not null access Scenario_Record) return String;
   function Line  (Self : not null access Scenario_Record) return Positive;
   function Index (Self : not null access Scenario_Record) return Positive;
   function Kind
     (Self : not null access Scenario_Record) return Scenario_Kind;
   --  Retrieve the attributes of Self

   procedure Add
     (Self : not null access Scenario_Record;
      S    : not null access Step_Record'Class);
   --  Add a new step

   procedure Foreach_Step
     (Self : not null access Scenario_Record;
      Callback : not null access procedure
        (S : not null access Step_Record'Class));
   --  Iterate over each step

   function Longuest_Step
     (Self : not null access Scenario_Record) return Natural;
   --  The length of the longuest step (for display purposes)

   -------------
   -- Feature --
   -------------

   type Feature_Record (<>) is tagged limited private;
   type Feature is access all Feature_Record'Class;
   --  An object that represents a single feature and all its scenarios.
   --  A specific features file might contain several features, but this object
   --  only represents one of them.
   --
   --  This type has a discriminant to force the allocation through Create,
   --  which ensures the feature has a unique id.

   function Create
     (File : GNATCOLL.VFS.Virtual_File;
      Name : String)
      return not null access Feature_Record;
   --  Create and initialize a new feature

   procedure Free (Self : in out Feature);
   procedure Free (Self : in out Feature_Record);
   --  Free the memory associated with Self

   function Id (Self : not null access Feature_Record) return Integer;
   --  Set a unique id for the feature

   function Name (Self : not null access Feature_Record) return String;
   --  The name of the feature

   function File
     (Self : not null access Feature_Record) return GNATCOLL.VFS.Virtual_File;
   --  The file in which the feature is defined

   function Description (Self : not null access Feature_Record) return String;
   procedure Add_To_Description
     (Self : not null access Feature_Record; Descr : String);
   --  Add some description information. Add_Description will be called once
   --  for each line in the description.

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

   type Scenario_Record is tagged record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Line      : Positive := 1;
      Index     : Positive := 1;
      Kind      : Scenario_Kind := Kind_Scenario;
      Steps     : Step_List;

      Longuest_Step : Integer := -1;
   end record;
   --  Make sure this type can be put in a list and automatically reclaim
   --  storage when the list is clearer.

   type Feature_Record is tagged limited record
      File        : GNATCOLL.VFS.Virtual_File;
      Name        : GNAT.Strings.String_Access;
      Id          : Integer;
      Description : Ada.Strings.Unbounded.Unbounded_String;
   end record;
end BDD.Features;
