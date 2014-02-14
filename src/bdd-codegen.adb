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

with Ada.Text_IO;          use Ada.Text_IO;
with GNAT.Regpat;          use GNAT.Regpat;
with GNATCOLL.Utils;       use GNATCOLL.Utils;

package body BDD.Codegen is

   Cst_Procedure  : constant String := "procedure ";
   Cst_Procedure_Re : constant Pattern_Matcher := Compile
     ("procedure\s+"
      & "([\w+]+)"  --  group 1: name of the procedure
      & "\s*"
      & "(\("       --  group 2: paramter list, including surrounding parens
      & "(.*?)"     --  group 3: list of parameters
      & "\))?;",    --  end of group 2
      Case_Insensitive or Single_Line);
   Cst_Package_Re : constant Pattern_Matcher :=
     Compile ("^package ([_\.\w]+)", Case_Insensitive);
   Cst_Comment_Re : constant Pattern_Matcher :=
     Compile ("--\s*@(given|then|when)\s+");

   function Trim (S : String) return String;
   --  Trim all whitespaces (including ASCII.LF and ASCII.CR, as opposed to
   --  what Ada.Strings.Fixed does) at both ends of S

   procedure Check_Steps
     (Self    : in out Steps_Finder'Class;
      File    : Virtual_File);
   --  Check whether File contains any step definition, and register those.
   --  The pattern looks for "Step_Regexp"

   procedure Parse_Subprogram_Def
     (Contents     : String;
      Package_Name : String;
      Regexp       : String_Access;
      Pos          : in out Integer);
   --  Parse a single procedure definition (the one starting just after Pos)
   --  Pos is left after the declaration (if the latter could
   --  be parsed).

   type Param_Description is record
      Name    : GNAT.Strings.String_Access;
      Of_Type : GNAT.Strings.String_Access;
   end record;

   type Param_List is array (Natural range <>) of Param_Description;
   type Param_List_Access is access all Param_List;

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

   --------------------------
   -- Parse_Subprogram_Def --
   --------------------------

   procedure Parse_Subprogram_Def
     (Contents     : String;
      Package_Name : String;
      Regexp       : String_Access;
      Pos          : in out Integer)
   is
      Matches     : Match_Array (0 .. 3);
      Subprogram  : String_Access;
      Colon       : Integer;
      List        : Param_List_Access;
      Expected_Params : Natural;

   begin
      Put_Line ("MANU Found " & Regexp.all);

      --  Check this is a valid regular expression

      begin
         declare
            Re : constant Pattern_Matcher := Compile (Regexp.all);
         begin
            Expected_Params := Paren_Count (Re);
         end;
      exception
         when Expression_Error =>
            Put_Line
              (Standard_Error,
               "Error: invalid regular expression for step '"
               & Regexp.all & "'");
            return;
      end;

      Skip_Blanks (Contents, Pos);
      if Pos > Contents'Last
        or else not Starts_With
          (Contents (Pos .. Contents'Last), Cst_Procedure)
      then
         Put_Line
           (Standard_Error,
            "Error: The step definition for '" & Regexp.all & "' must be"
            & " following immediately by its subprogram");
         return;
      end if;

      Match (Cst_Procedure_Re, Contents, Matches, Data_First => Pos);
      if Matches (1) = No_Match then
         Put_Line
           (Standard_Error,
            "Could not find name of subprogram for '" & Regexp.all & "'");
         return;
      end if;

      Pos := Matches (0).Last;

      Subprogram := new String'
        (Package_Name & '.'
         & Contents (Matches (1).First .. Matches (1).Last));

      Put_Line ("MANU in subprogram " & Subprogram.all);

      --  Parse the list of parameters

      if Matches (3) /= No_Match then
         declare
            Params : String_List_Access := Split
              (Contents (Matches (3).First .. Matches (3).Last),
               On => ';');
         begin
            List := new Param_List (Params'Range);
            for P in Params'Range loop
               declare
                  D : String renames Params (P).all;
               begin
                  Colon := Find_Char (D, ':');
                  if Colon > D'Last then
                     Put_Line
                       (Standard_Error,
                        "Error while parsing subprogram declaration for step '"
                        & Regexp.all & "'");
                     return;
                  end if;

                  List (P) :=
                    (Name => new String'(Trim (D (D'First .. Colon - 1))),
                     Of_Type => new String'(Trim (D (Colon + 1 .. D'Last))));

                  Put_Line ("MANU Param Name=" & List (P).Name.all
                            & " Type=" & List (P).Of_Type.all);
               end;
            end loop;

            Free (Params);
         end;
      else
         List := new Param_List (1 .. 0);
      end if;

      if List'Length /= Expected_Params then
         Put_Line
           (Standard_Error,
            "Mismatch between the number of parenthesis in the regexp and the"
            & " subprogram parameters for step '" & Regexp.all & "'");
         return;
      end if;
   end Parse_Subprogram_Def;

   -----------------
   -- Check_Steps --
   -----------------

   procedure Check_Steps
     (Self    : in out Steps_Finder'Class;
      File    : Virtual_File)
   is
      pragma Unreferenced (Self);

      Contents    : String_Access := File.Read_File;
      Pos         : Integer;
      Matches     : Match_Array (0 .. 1);
      Last        : Integer;
      Pack        : String_Access;
      Regexp      : String_Access;
   begin
      if Contents /= null then
         Pos := Contents'First;

         --  Find the start of the package
         Match (Cst_Package_Re, Contents.all, Matches, Data_First => Pos);
         if Matches (1) = No_Match then
            return;
         end if;

         Pack := new String'
           (Contents (Matches (1).First .. Matches (1).Last));

         while Pos <= Contents'Last loop
            Match (Cst_Comment_Re, Contents.all, Matches, Data_First => Pos);
            exit when Matches (0) = No_Match;

            Pos := Matches (0).Last;
            Skip_Blanks (Contents.all, Pos);
            Last := EOL (Contents (Pos .. Contents'Last));

            Regexp := new String'(Contents (Pos .. Last - 1));
            Pos := Last + 1;  --  After ASCII.LF
            Parse_Subprogram_Def (Contents.all, Pack.all, Regexp, Pos);
         end loop;

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
   begin
      if Files /= null then
         for F in Files'Range loop
            Check_Steps (Self, File => Files (F));
         end loop;

         Unchecked_Free (Files);
      end if;
   end Discover_Steps;

end BDD.Codegen;
