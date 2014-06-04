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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Regpat;             use GNAT.Regpat;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

package body BDD.Codegen is
   Me : constant Trace_Handle := Create ("BDD.CODEGEN");

   Cst_Procedure  : constant String := "procedure ";
   Cst_Procedure_Re : constant Pattern_Matcher := Compile
     ("procedure\s+"
      & "([\w+]+)"  --  group 1: name of the procedure
      & "\s*"
      & "(\("       --  group 2: paramter list, including surrounding parens
      & "(.*?)"     --  group 3: list of parameters
      & "\))?(\s+with .*?)?;",    --  end of group 2
      Case_Insensitive or Single_Line);
   Cst_Package_Re : constant Pattern_Matcher :=
     Compile ("^package ([_\.\w]+)", Case_Insensitive or Multiple_Lines);
   Cst_Comment_Re : constant Pattern_Matcher :=
     Compile ("--\s*@(given|then|when)\s+");

   type Generated_Data is record
      Matchers : Unbounded_String;
      --  Code extract that, for a given step, checks all known step definition
      --  and execute the corresponding subprogram if needed.

      Regexps  : Unbounded_String;
      --  Code extract that declares all the regexps used by the step
      --  definitions.

      Withs    : Unbounded_String;
      --  Code extract for the "with" clauses

      Steps_Count : Natural := 0;
      --  Number of registered steps.

      Max_Parameter_Count : Natural := 0;
      --  Number of parameters for the step that requires the most of them.
   end record;

   function Trim (S : String) return String;
   --  Trim all whitespaces (including ASCII.LF and ASCII.CR, as opposed to
   --  what Ada.Strings.Fixed does) at both ends of S

   function Escape (S : String) return String;
   --  Return a version of S encoded for an Ada string (double quotes are
   --  duplicated for instance).

   function String_To_Type (Typ, Value : String) return String;
   --  Generate code to converts a string (parsed from a regexp) into another
   --  Ada type

   procedure Check_Steps
     (Self    : in out Steps_Finder'Class;
      File    : Virtual_File;
      Data    : in out Generated_Data);
   --  Check whether File contains any step definition, and register those.
   --  The pattern looks for "Step_Regexp"

   procedure Parse_Subprogram_Def
     (Contents     : String;
      Package_Name : String;
      Regexp       : String;
      Found        : in out Boolean;
      Pos          : in out Integer;
      Data         : in out Generated_Data);
   --  Parse a single procedure definition (the one starting just after Pos)
   --  Pos is left after the declaration (if the latter could
   --  be parsed).
   --  Found is set to True if at least one step definition was found, and left
   --  unchanged otherwise

   type Param_Description is record
      Name    : GNAT.Strings.String_Access;
      Of_Type : GNAT.Strings.String_Access;
   end record;

   type Param_List is array (Natural range <>) of Param_Description;
   type Param_List_Access is access all Param_List;
   procedure Free (Self : in out Param_List_Access);

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Param_List_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Param_List, Param_List_Access);
   begin
      if Self /= null then
         for P in Self'Range loop
            Free (Self (P).Name);
            Free (Self (P).Of_Type);
         end loop;
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Steps_Finder) is
   begin
      Unchecked_Free (Self.Files);
   end Free;

   ----------
   -- Trim --
   ----------

   function Trim (S : String) return String is
      First : Integer := S'First;
      Last  : Integer := S'Last;
   begin
      while First <= S'Last and then Is_Whitespace (S (First)) loop
         First := First + 1;
      end loop;

      while Last >= S'First and then Is_Whitespace (S (Last)) loop
         Last := Last - 1;
      end loop;

      return S (First .. Last);
   end Trim;

   ------------
   -- Escape --
   ------------

   function Escape (S : String) return String is
      Count : Natural := 0;
      S2_Idx : Natural;
   begin
      for C in S'Range loop
         if S (C) = '"' then
            Count := Count + 1;
         end if;
      end loop;

      if Count = 0 then
         return S;
      end if;

      return S2 : String (S'First .. S'Last + Count) do
         S2_Idx := S2'First;
         for C in S'Range loop
            S2 (S2_Idx) := S (C);
            if S (C) = '"' then
               S2 (S2_Idx + 1) := '"';
               S2_Idx := S2_Idx + 2;
            else
               S2_Idx := S2_Idx + 1;
            end if;
         end loop;
      end return;
   end Escape;

   --------------------
   -- String_To_Type --
   --------------------

   function String_To_Type (Typ, Value : String) return String is
   begin
      if To_Lower (Typ) = "string" then
         return Value;
      else
         return Typ & "'Value (" & Value & ")";
      end if;
   end String_To_Type;

   --------------------------
   -- Parse_Subprogram_Def --
   --------------------------

   procedure Parse_Subprogram_Def
     (Contents     : String;
      Package_Name : String;
      Regexp       : String;
      Found        : in out Boolean;
      Pos          : in out Integer;
      Data         : in out Generated_Data)
   is
      Matches     : Match_Array (0 .. 3);
      Subprogram  : GNAT.Strings.String_Access;
      Colon       : Integer;
      List        : Param_List_Access;
      List_Last   : Integer := 0;
      Expected_Params : Natural;

      Table_Param : GNAT.Strings.String_Access;
      --  Name of the parameter for a Table (null if no table expected)

   begin
      Trace (Me, "Found regexp " & Regexp);

      --  Check this is a valid regular expression

      begin
         declare
            Re : constant Pattern_Matcher := Compile (Regexp);
         begin
            Expected_Params := Paren_Count (Re);
         end;
      exception
         when Expression_Error =>
            Put_Line
              (Standard_Error,
               "Error: invalid regular expression for step '"
               & Regexp & "'");
            return;
      end;

      Skip_Blanks (Contents, Pos);
      if Pos > Contents'Last
        or else not Starts_With
          (Contents (Pos .. Contents'Last), Cst_Procedure)
      then
         Put_Line
           (Standard_Error,
            "Error: The step definition for '" & Regexp & "' must be"
            & " followed immediately by its subprogram");
         return;
      end if;

      Match (Cst_Procedure_Re, Contents, Matches, Data_First => Pos);
      if Matches (1) = No_Match then
         Put_Line
           (Standard_Error,
            "Could not find name of subprogram for '" & Regexp & "'");
         return;
      end if;

      Pos := Matches (0).Last;

      Subprogram := new String'
        (Package_Name & '.'
         & Contents (Matches (1).First .. Matches (1).Last));

      --  Parse the list of parameters

      if Matches (3) /= No_Match then
         declare
            Params : String_List_Access := Split
              (Contents (Matches (3).First .. Matches (3).Last),
               On => ';');
         begin
            List := new Param_List (Params'Range);
            for P in Params'Range loop
               Colon := Find_Char (Params (P).all, ':');
               if Colon > Params (P)'Last then
                  Put_Line
                    (Standard_Error,
                     "Error while parsing subprogram declaration for step '"
                     & Regexp & "'");
                  return;
               end if;

               declare
                  D : String renames Params (P).all;
                  Name : constant String := Trim (D (D'First .. Colon - 1));
                  Typ  : constant String := Trim (D (Colon + 1 .. D'Last));
               begin
                  if P > Expected_Params then
                     --  Is this a Table parameter ?
                     if Typ = "BDD.Tables.Table" then
                        if Table_Param /= null then
                           Put_Line
                             (Standard_Error,
                              "Multiple table parameters is unsupported for"
                              & " step '" & Regexp & "'");
                           Free (List);
                           Free (Subprogram);
                           return;
                        end if;
                        Table_Param := new String'(Name);
                     else
                        Put_Line
                          (Standard_Error,
                           "Mismatch between the number of parenthesis in the"
                           & " regexp and the subprogram parameters for step '"
                           & Regexp & "'");
                        Free (List);
                        Free (Subprogram);
                        return;
                     end if;
                  else
                     List (P) :=
                       (Name    => new String'(Name),
                        Of_Type => new String'(Typ));
                     List_Last := P;
                  end if;
               end;
            end loop;

            Free (Params);
         end;
      end if;

      if List_Last /= Expected_Params then
         Put_Line
           (Standard_Error,
            "Mismatch between the number of parenthesis in the"
            & " regexp and the subprogram parameters for step '"
            & Regexp & "'");
         Free (List);
         Free (Subprogram);
         return;
      end if;

      --  Generate code for matchers

      Found := True;

      Data.Steps_Count := Data.Steps_Count + 1;
      Data.Max_Parameter_Count := Integer'Max
        (Data.Max_Parameter_Count, List_Last);

      if Data.Steps_Count /= 1 then
         Append (Data.Matchers, "      els");
      else
         Append (Data.Matchers, "      ");
      end if;
      Append (Data.Matchers,
              "if Step.Should_Execute (Text, Matches, Re_"
              & Image (Data.Steps_Count, Min_Width => 0) & ") then"
              & ASCII.LF
              & "         if Execute then" & ASCII.LF
              & "            " & Subprogram.all);

      if List_Last = 0
        and then Table_Param = null
      then
         Append (Data.Matchers, ";");
      else
         Append (Data.Matchers, " (");

         for L in List'First .. List_Last loop
            if L /= List'First then
               Append (Data.Matchers, ",");
            end if;
            Append (Data.Matchers,
                    ASCII.LF & "               "
                    & List (L).Name.all
                    & " => "
                    & String_To_Type
                      (List (L).Of_Type.all,
                       "Text (Matches ("
                       & Image (1 + L - List'First, Min_Width => 0)
                       & ").First .. Matches ("
                       & Image (1 + L - List'First, Min_Width => 0)
                       & ").Last)"));
         end loop;

         if Table_Param /= null then
            if List_Last > 0 then
               Append (Data.Matchers, ",");
            end if;
            Append (Data.Matchers,
                    ASCII.LF & "               "
                    & Table_Param.all
                    & " => Step.Table");
         end if;

         Append (Data.Matchers, ");");
      end if;

      Append (Data.Matchers,
              ASCII.LF & "         end if;" & ASCII.LF);

      --  Generate code for regexps

      Append (Data.Regexps,
              "   Re_"
              & Image (Data.Steps_Count, Min_Width => 0)
              & " : constant Pattern_Matcher := Compile" & ASCII.LF
              & "      (""" & Escape (Regexp) & """);" & ASCII.LF);

      Free (Table_Param);
      Free (Subprogram);
      Free (List);
   end Parse_Subprogram_Def;

   -----------------
   -- Check_Steps --
   -----------------

   procedure Check_Steps
     (Self    : in out Steps_Finder'Class;
      File    : Virtual_File;
      Data    : in out Generated_Data)
   is
      pragma Unreferenced (Self);

      Contents    : GNAT.Strings.String_Access := File.Read_File;
      Pos, Start  : Integer;
      Matches     : Match_Array (0 .. 1);
      Last        : Integer;
      Pack_Start, Pack_End : Integer;
      Found       : Boolean := False;
   begin
      if Contents /= null then
         if Active (Me) then
            Trace (Me, "Check steps in " & File.Display_Full_Name);
         end if;

         Pos := Contents'First;

         --  Find the start of the package
         Match (Cst_Package_Re, Contents.all, Matches, Data_First => Pos);
         if Matches (1) = No_Match then
            return;
         end if;

         Pack_Start := Matches (1).First;
         Pack_End   := Matches (1).Last;

         while Pos <= Contents'Last loop
            Match (Cst_Comment_Re, Contents.all, Matches, Data_First => Pos);
            exit when Matches (0) = No_Match;

            Pos := Matches (0).Last;
            Skip_Blanks (Contents.all, Pos);
            Last := EOL (Contents (Pos .. Contents'Last));

            Start := Pos;
            Pos := Last + 1;  --  After ASCII.LF
            Parse_Subprogram_Def
              (Contents.all,
               Package_Name => Contents (Pack_Start .. Pack_End),
               Regexp       => Contents (Start .. Last - 1),
               Found        => Found,
               Data         => Data,
               Pos          => Pos);
         end loop;

         if Found then
            Append (Data.Withs,
                    "with " & Contents (Pack_Start .. Pack_End)
                    & ';' & ASCII.LF);
         end if;

         Free (Contents);
      end if;
   end Check_Steps;

   --------------------
   -- Discover_Steps --
   --------------------

   procedure Discover_Steps
     (Self      : in out Steps_Finder;
      Extension : Filesystem_String := ".ads";
      Directory : GNATCOLL.VFS.Virtual_File)
   is
      Files : File_Array_Access := Directory.Read_Dir_Recursive
        (Extension => Extension, Filter => Files_Only);
      Data        : Generated_Data;
      F : File_Type;
   begin
      if Files /= null then
         for F in Files'Range loop
            Check_Steps (Self, File => Files (F), Data => Data);
         end loop;

         Unchecked_Free (Files);
      end if;

      Create (F, Out_File, "obj/driver.adb");
      Put_Line (F, "--  Automatically generated");
      Put_Line (F, "with BDD;          use BDD;");
      Put_Line (F, "with BDD.Main;     use BDD.Main;");
      Put_Line (F, "with BDD.Features; use BDD.Features;");
      Put_Line (F, "with BDD.Runner;   use BDD.Runner;");
      Put_Line (F, "with GNAT.Regpat;  use GNAT.Regpat;");
      Put_Line (F, To_String (Data.Withs));
      Put_Line (F, "procedure Driver is");
      New_Line (F);
      Put_Line (F, To_String (Data.Regexps));
      Put_Line (F, "   procedure Run_Steps");
      Put_Line
        (F,
         "      (Step    : not null access BDD.Features.Step_Record'Class;");
      Put_Line (F, "       Text    : String;");
      Put_Line (F, "       Execute : Boolean)");
      Put_Line (F, "   is");
      Put_Line (F, "      Matches : aliased Match_Array (0 .. "
                & Image (Data.Max_Parameter_Count, Min_Width => 0)
                & ");");
      Put_Line (F, "   begin");
      Put (F, To_String (Data.Matchers));
      Put_Line (F, "      else");
      Put_Line (F, "         Step.Set_Status (Status_Undefined);");
      Put_Line (F, "      end if;");
      Put_Line (F, "   end Run_Steps;");
      New_Line (F);
      Put_Line (F, "   Runner : Feature_Runner;");
      Put_Line (F, "begin");
      Put_Line
        (F, "   Runner.Add_Step_Runner (Run_Steps'Unrestricted_Access);");
      Put_Line (F, "   BDD.Main.Main (Runner);");
      Put_Line (F, "end Driver;");
      Close (F);

      Create (F, Out_File, "obj/driver.gpr");
      Put_Line (F, "with ""gnatcoll"";");
      Put_Line (F, "project Driver is");
      Put_Line (F, "   for Main use (""driver.adb"");");
      Put_Line (F, "   for Source_Dirs use (""."",");
      Put_Line (F, "      ""../src/"",");
      Put_Line (F, "      """ & Directory.Display_Full_Name & "/**"");");
      Put_Line (F, "   package Binder is");
      Put_Line (F, "      for Switches (""Ada"") use (""-E"", ""-g"");");
      Put_Line (F, "   end Binder;");
      Put_Line (F, "   package Compiler is");
      Put_Line (F, "      for Switches (""Ada"") use (""-g"");");
      Put_Line (F, "   end Compiler;");
      Put_Line (F, "end Driver;");
      Close (F);
   end Discover_Steps;

end BDD.Codegen;
