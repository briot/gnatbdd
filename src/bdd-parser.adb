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

with GNATCOLL.Traces;     use GNATCOLL.Traces;
with GNATCOLL.Utils;      use GNATCOLL.Utils;
with GNAT.Strings;        use GNAT.Strings;

package body BDD.Parser is
   Me : constant Trace_Handle := Create ("BDD.PARSER");

   Cst_Features         : constant String := "Feature:";
   Cst_Scenario         : constant String := "Scenario:";
   Cst_Scenario_Outline : constant String := "Scenario Outline:";
   Cst_Given            : constant String := "Given";
   Cst_And              : constant String := "And";
   Cst_Then             : constant String := "Then";
   Cst_But              : constant String := "But";
   Cst_When             : constant String := "When";
   Cst_Background       : constant String := "Background";
   Cst_Examples         : constant String := "Examples:";
   Cst_Scenarios        : constant String := "Scenarios:";
   --  The keywords when parsing a file

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self : Feature_Parser;
      File : GNATCOLL.VFS.Virtual_File;
      Callback : access procedure (F : BDD.Features.Feature))
   is
      pragma Unreferenced (Self);

      type State_Type is (None, In_Feature, In_Scenario, In_String,
                          In_Outline, In_Examples);
      State  : State_Type := None;
      F      : Feature;
      Scenar : Scenario;
      Buffer : GNAT.Strings.String_Access := File.Read_File;
      Index  : Integer := Buffer'First;
      Line   : Natural := 0;
      Index_In_Feature : Natural := 0;
      String_Indent  : Natural := 0;  --  indentation of the mutli-line string
      Line_S, Line_E : Integer;       --  extents of the current line
      First_Char     : Integer;       --  first non-blank character in line
      String_Line_Start : Integer;    --  first line of multi-line string

      procedure Finish_Scenario;
      procedure Finish_Feature;
      --  Called when the end of a scenario or a feature are seen

      ---------------------
      -- Finish_Scenario --
      ---------------------

      procedure Finish_Scenario is
      begin
         case State is
            when In_Scenario | In_Outline | In_Examples =>
               F.Add (Scenar);
               Free (Scenar);
               State := In_Feature;

            when others =>
               null;
         end case;
      end Finish_Scenario;

      --------------------
      -- Finish_Feature --
      --------------------

      procedure Finish_Feature is
      begin
         Finish_Scenario;
         if State /= None then
            Callback (F);
            Free (F);
         end if;
         State := None;
      end Finish_Feature;

   begin
      Trace (Me, "Parsing " & File.Display_Full_Name);

      while Index < Buffer'Last loop
         Line_S := Index;
         Line_E := Line_End (Buffer.all, Line_S);

         Index := Line_E + 1;
         if Buffer (Index) = ASCII.CR then
            Index := Index + 1;
         end if;
         if Buffer (Index) = ASCII.LF then
            Index := Index + 1;
         end if;

         Line   := Line + 1;

         if Active (Me) then
            Trace (Me, "Line " & Image (Line, 3, Padding => ' ')
                   & " " & Buffer (Line_S .. Line_E));
         end if;

         First_Char := Line_S;
         Skip_Blanks (Buffer (Line_S .. Line_E), First_Char);

         if Starts_With (Buffer (First_Char .. Line_E), """""""") then
            if State = In_String then
               State := In_Scenario;
            elsif State = In_Scenario then
               State := In_String;
               String_Indent := First_Char - Line_S;
               String_Line_Start := Line;
            else
               raise Syntax_Error with "Multi-line strings only allowed in"
                 & " steps, at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

         elsif State = In_String then
            if First_Char  < Line_S + String_Indent - 1 then
               Trace (Me, "MANU String="
                      & Buffer (First_Char .. Line_E));
            elsif Line_S + String_Indent - 1 <= Line_E then
               Trace (Me, "MANU String="
                      & Buffer (Line_S + String_Indent - 1 .. Line_E));
            else
               Trace (Me, "MANU String=");
            end if;

         elsif First_Char > Line_E then
            --  ignore blank lines
            null;

         elsif Buffer (First_Char) = '|' then
            case State is
               when In_Scenario | In_Outline =>
                  Trace (Me, "MANU Table=" & Buffer (First_Char .. Line_E));

               when In_Examples =>
                  Trace (Me, "MANU Example=" & Buffer (First_Char .. Line_E));

               when others =>
                  raise Syntax_Error with "Tables only allowed in"
                    & " steps, at " & File.Display_Full_Name & ":"
                    & Image (Line, 1);
            end case;

         elsif Buffer (First_Char) = '@' then
            --  ??? parse tags
            null;

         elsif Buffer (First_Char) = '#' then
            --  Ignore comment lines
            null;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Features) then
            Finish_Feature;
            F.Set_File (File);
            F.Set_Name (Buffer (First_Char + Cst_Features'Length .. Line_E));
            Index_In_Feature := 0;
            State := In_Feature;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Background) then
            if State = None then
               raise Syntax_Error with "Background must be defined within a"
                 & " Feature at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            Finish_Scenario;

            --  ??? handle background
            null;

            State := In_Scenario;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Scenario) then
            if State = None then
               raise Syntax_Error with "Scenario must be defined within a"
                 & " Feature at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            Finish_Scenario;
            Index_In_Feature := Index_In_Feature + 1;
            Scenar.Set_Name
              (Name  => Buffer (First_Char + Cst_Scenario'Length .. Line_E),
               Line  => Line,
               Index => Index_In_Feature);
            State := In_Scenario;

         elsif Starts_With (Buffer (First_Char .. Line_E),
                            Cst_Scenario_Outline)
         then
            if State = None then
               raise Syntax_Error with
                 "Scenario Outline must be defined within a Feature at "
                 & File.Display_Full_Name & ":" & Image (Line, 1);
            end if;

            Finish_Scenario;
            Index_In_Feature := Index_In_Feature + 1;
            Scenar.Set_Is_Outline;
            Scenar.Set_Name
              (Name  => Buffer (First_Char + Cst_Scenario'Length .. Line_E),
               Line  => Line,
               Index => Index_In_Feature);
            State := In_Outline;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Scenarios) then
            if State /= In_Outline then
               raise Syntax_Error with
                 "Scenarios must be defined within a Scenario Outline, at "
                 & File.Display_Full_Name & ":" & Image (Line, 1);
            end if;
            State := In_Examples;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Examples) then
            if State /= In_Outline then
               raise Syntax_Error with
                 "Examples must be defined within a Scenario Outline, at "
                 & File.Display_Full_Name & ":" & Image (Line, 1);
            end if;
            State := In_Examples;

         elsif Starts_With (Buffer (First_Char .. Line_E), Cst_Given)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_And)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_Then)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_But)
           or else Starts_With (Buffer (First_Char .. Line_E), Cst_When)
         then
            if State /= In_Scenario
              and then State /= In_Outline
            then
               raise Syntax_Error with "Step must be defined with in a"
                 & " Scenario at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;

            --  ??? Should handle the step
            null;

         else
            if State = None then
               raise Syntax_Error with "Expected line starting with "
                 & Cst_Features & " at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);

            elsif State = In_Feature then
               --  Ignored, these are the comments for the feature
               null;

            elsif State = In_Scenario
              or else State = In_Outline
            then
               raise Syntax_Error with "Expected line starting with Given/And/"
                 & "But/When/Then at " & File.Display_Full_Name & ":"
                 & Image (Line, 1);
            end if;
         end if;
      end loop;

      if State = In_String then
         raise Syntax_Error with "Multi-line string must end with """""" at "
           & File.Display_Full_Name & ":" & Image (String_Line_Start, 1);
      end if;

      Finish_Feature;
      Free (Buffer);
   end Parse;

end BDD.Parser;
