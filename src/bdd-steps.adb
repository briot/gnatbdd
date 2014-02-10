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

with GNAT.Regpat;   use GNAT.Regpat;

package body BDD.Steps is

   Re_1 : constant Pattern_Matcher := Compile
     ("^Given a user named '(.*)'$");
   Re_2 : constant Pattern_Matcher := Compile
     ("^Given I am sitting at my desk$");
   Re_3 : constant Pattern_Matcher := Compile
     ("^Then I should create great software$");

   procedure Do_Step_1 (Name : String);
   procedure Do_Step_2;
   procedure Do_Step_3;

   ---------------
   -- Do_Step_1 --
   ---------------

   procedure Do_Step_1 (Name : String) is
      pragma Unreferenced (Name);
   begin
      null;
   end Do_Step_1;

   ---------------
   -- Do_Step_2 --
   ---------------

   procedure Do_Step_2 is
   begin
      raise Constraint_Error;
   end Do_Step_2;

   ---------------
   -- Do_Step_3 --
   ---------------

   procedure Do_Step_3 is
   begin
      raise Constraint_Error;
   end Do_Step_3;

   --------------
   -- Run_Step --
   --------------

   procedure Run_Step
     (Step    : not null access BDD.Features.Step_Record'Class;
      Execute : Boolean)
   is
      Text : constant String := Step.Text;
      Matches : Match_Array (0 .. 10);
   begin
      Match (Re_1, Text, Matches);
      if Matches (0) /= No_Match then
         Step.Set_Match_Info (Matches);
         if Execute then
            Do_Step_1 (Name => Text (Matches (1).First .. Matches (1).Last));
         end if;
         return;
      end if;

      Match (Re_2, Text, Matches);
      if Matches (0) /= No_Match then
         Step.Set_Match_Info (Matches);
         if Execute then
            Do_Step_2;
         end if;
         return;
      end if;

      Match (Re_3, Text, Matches);
      if Matches (0) /= No_Match then
         Step.Set_Match_Info (Matches);
         if Execute then
            Do_Step_3;
         end if;
         return;
      end if;

      Step.Set_Status (Status_Undefined);
   end Run_Step;

end BDD.Steps;
