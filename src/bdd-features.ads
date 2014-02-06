-----------------------------------------------------------------------------
--                             g N A T C O L L                              --
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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;          use GNAT.Strings;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

package BDD.Features is

   --------------
   -- Scenario --
   --------------

   type Scenario is tagged private;
   --  A scenario to be run within a feature

   procedure Free (Self : in out Scenario);
   --  Free the memory associated with Self

   procedure Set_Name
     (Self  : in out Scenario;
      Name  : String;
      Line  : Positive;
      Index : Positive);
   --  Set the line of the feature file at which the scenario is defined, and
   --  its index within its Feature.

   procedure Set_Is_Outline (Self : in out Scenario);
   --  The scenario is in fact an outline, which will run itself multiple
   --  times and use values from a table to generate each test.

   -------------
   -- Feature --
   -------------

   type Feature is tagged limited private;
   --  An object that represents a single feature and all its scenarios.
   --  A specific features file might contain several features, but this object
   --  only represents one of them.

   procedure Free (Self : in out Feature);
   --  Free the memory associated with Self

   procedure Set_Name (Self : in out Feature; Name : String);
   function Name (Self : Feature) return String;
   --  The name of the feature

   procedure Set_File
     (Self : in out Feature; File : GNATCOLL.VFS.Virtual_File);
   function File (Self : Feature) return GNATCOLL.VFS.Virtual_File;
   --  The file in which the feature is defined

   procedure Add (Self : in out Feature; Scenar : Scenario'Class);
   --  Add a copy of the scenario to the feature

private
   type Scenario is tagged record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Line      : Positive := 1;
      Index     : Positive := 1;
      Outline   : Boolean := False;
   end record;
   --  Make sure this type can be put in a list and automatically reclaim
   --  storage when the list is clearer.

   package Scenario_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Scenario'Class);

   type Feature is tagged limited record
      File      : GNATCOLL.VFS.Virtual_File;
      Name      : GNAT.Strings.String_Access;
      Scenarios : Scenario_Lists.List;
   end record;
end BDD.Features;
