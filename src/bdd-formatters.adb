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

   procedure Display_Location
     (Self     : Formatter'Class;
      Feature  : BDD.Features.Feature'Class;
      Scenario : BDD.Features.Scenario'Class);
   --  Display location information for the scenario

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

   ----------------------
   -- Display_Location --
   ----------------------

   procedure Display_Location
     (Self     : Formatter'Class;
      Feature  : BDD.Features.Feature'Class;
      Scenario : BDD.Features.Scenario'Class)
   is
   begin
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Foreground => Grey);
      Put ("# "
           & (+Feature.File.Relative_Path (Features_Directory))
           & "#" & Image (Scenario.Index, 1)
           & ":" & Image (Scenario.Line, 1));
      Self.Term.Set_Color
        (Term       => Ada.Text_IO.Standard_Output,
         Style      => Reset_All);
      New_Line;
   end Display_Location;

   ---------------------
   -- Display_Feature --
   ---------------------

   overriding procedure Display_Feature
     (Self    : Formatter_Full;
      Feature : BDD.Features.Feature'Class)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line (Cst_Features & ' ' & Feature.Name);
      Put_Line (Feature.Description);
   end Display_Feature;

   ----------------------
   -- Display_Scenario --
   ----------------------

   overriding procedure Display_Scenario
     (Self     : Formatter_Full;
      Feature  : BDD.Features.Feature'Class;
      Scenario : BDD.Features.Scenario'Class)
   is
   begin
      Put ("  " & Cst_Scenario & ' ' & Scenario.Name & "  ");
      Display_Location (Self, Feature, Scenario);
   end Display_Scenario;

   ------------------------
   -- Scenario_Completed --
   ------------------------

   overriding procedure Scenario_Completed
     (Self     : Formatter_Full;
      Feature  : BDD.Features.Feature'Class;
      Scenario : BDD.Features.Scenario'Class)
   is
      pragma Unreferenced (Self, Feature, Scenario);
   begin
      New_Line;
   end Scenario_Completed;

end BDD.Formatters;
