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

with Ada.Strings.Fixed;   use Ada.Strings, Ada.Strings.Fixed;

package body BDD.Features is

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Feature) is
   begin
      Self.File := No_File;
      Free (Self.Name);
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

   ---------
   -- Add --
   ---------

   procedure Add (Self : in out Feature; Scenar : Scenario'Class) is
   begin
      Self.Scenarios.Append (Scenar);
   end Add;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Scenario) is
   begin
      Self.Name    := Null_Unbounded_String;
      Self.Line    := 1;
      Self.Index   := 1;
      Self.Outline := False;
   end Free;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Self  : in out Scenario;
      Name  : String;
      Line  : Positive;
      Index : Positive)
   is
   begin
      Self.Name  := To_Unbounded_String (Trim (Name, Both));
      Self.Line  := Line;
      Self.Index := Index;
   end Set_Name;

   --------------------
   -- Set_Is_Outline --
   --------------------

   procedure Set_Is_Outline (Self : in out Scenario) is
   begin
      Self.Outline := True;
   end Set_Is_Outline;

end BDD.Features;
