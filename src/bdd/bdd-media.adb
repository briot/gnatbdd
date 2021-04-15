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

with Ada.Text_IO;       use Ada.Text_IO;
with GNATCOLL.Utils;    use GNATCOLL.Utils;

package body BDD.Media is

   type File_Type_Access is access Ada.Text_IO.File_Type;

   type File_Writer is new Media_Writer with record
      Term : GNATCOLL.Terminal.Terminal_Info_Access;
      File : File_Type_Access;
   end record;
   type File_Writer_Access is access File_Writer;
   overriding procedure Write
     (Self : not null access File_Writer;
      Text : String);
   overriding procedure Writeln
     (Self : not null access File_Writer;
      Text : String);
   overriding procedure Set_Color
     (Self       : not null access File_Writer;
      Foreground : ANSI_Color := Unchanged;
      Style      : ANSI_Style := Unchanged);
   overriding procedure New_Line (Self : not null access File_Writer);
   overriding function Has_Colors
     (Self : not null access File_Writer) return Boolean;

   type HTML_Writer is new Media_Writer with record
      Base          : not null access Media_Writer'Class;
      Current_Color : ANSI_Color := Black;
   end record;
   type HTML_Writer_Access is access HTML_Writer;
   overriding procedure Write
     (Self : not null access HTML_Writer;
      Text : String);
   overriding procedure Writeln
     (Self : not null access HTML_Writer;
      Text : String);
   overriding procedure Set_Color
     (Self       : not null access HTML_Writer;
      Foreground : ANSI_Color := Unchanged;
      Style      : ANSI_Style := Unchanged);
   overriding procedure New_Line (Self : not null access HTML_Writer);
   overriding function Has_Colors
     (Self : not null access HTML_Writer) return Boolean is (True);
   overriding procedure Display_Feature
     (Self        : not null access HTML_Writer;
      Name        : String;
      Description : String);
   overriding procedure Display_Scenario
     (Self          : not null access HTML_Writer;
      Prefix        : String;
      Name          : String;
      Longuest_Step : Natural;
      Location      : String);
   overriding procedure Display_Step
     (Self          : not null access HTML_Writer;
      Text          : String;
      Status        : Scenario_Status;
      Longuest_Step : Natural;
      Info          : GNAT.Regpat.Match_Array;
      Location      : String);
   overriding procedure Start_Table (Self : not null access HTML_Writer);
   overriding procedure End_Table (Self : not null access HTML_Writer);
   overriding procedure Start_Row
     (Self : not null access HTML_Writer; Indent : String);
   overriding procedure End_Row (Self : not null access HTML_Writer);
   overriding procedure Display_Cell
     (Self       : not null access HTML_Writer;
      Value      : String;
      Cell_Width : Natural;
      Header     : Boolean;
      Status     : Scenario_Status := Status_Passed);
   overriding procedure Start_Step_Messages
     (Self   : not null access HTML_Writer;
      Status : Scenario_Status);
   overriding procedure End_Step_Messages (Self : not null access HTML_Writer);

   procedure Step_Parser
     (Text     : String;
      Info     : GNAT.Regpat.Match_Array;
      On_Chunk : not null access procedure
        (Chunk : String; Is_Match : Boolean));
   --  Split a step's text into chunk, those matching a parenthesis group and
   --  those that don't.

   -----------------
   -- Open_Stdout --
   -----------------

   function Open_Stdout return Media_Writer_Access is
      Result : constant not null File_Writer_Access := new File_Writer;
   begin
      Result.Term := new Terminal_Info;
      Result.Term.Init_For_Stdout (Colors => BDD.Colors);

      Result.Stdout_Term := Result.Term;

      Result.File := new File_Type'(Ada.Text_IO.Standard_Output);
      return Media_Writer_Access (Result);
   end Open_Stdout;

   ---------------
   -- Open_File --
   ---------------

   function Open_File
     (File : GNATCOLL.VFS.Virtual_File) return Media_Writer_Access
   is
      Result : constant not null File_Writer_Access := new File_Writer;
   begin
      Result.File := new File_Type;
      Create (Result.File.all, Out_File, File.Display_Full_Name);

      Result.Term := new Terminal_Info;
      Result.Term.Init_For_File (Colors => BDD.Colors);

      Result.Stdout_Term := new Terminal_Info;
      Result.Term.Init_For_Stdout (Colors => BDD.Colors);

      return Media_Writer_Access (Result);
   end Open_File;

   ---------------
   -- Open_HTML --
   ---------------

   function Open_HTML
     (Base : not null access Media_Writer'Class)
      return Media_Writer_Access
   is
      Result : constant not null HTML_Writer_Access :=
        new HTML_Writer'(Stdout_Term        => Base.Stdout_Term,
                         Progress_Displayed => False,
                         Base               => Base,
                         Current_Color      => Black);
   begin
      Base.Writeln ("<html>");
      Base.Writeln ("  <head>");
      Base.Writeln ("    <title>Testsuite result</title>");
      Base.Writeln ("    <style type='text/css'>");
      Base.Writeln ("body {font-size: 12px}");
      Base.Writeln ("p {margin: 0}");
      Base.Writeln ("h1 {margin-bottom: 0; font-size: large}");
      Base.Write   ("p.feature {margin-bottom: 10px; font-size: small}");

      Base.Writeln ("h2 {display: block; margin: 15px 0 0 0}");

      Base.Write   (".location {float: right; font-size: small;");
      Base.Write   ("   color: grey; margin-top:0; margin-bottom:0;");
      Base.Writeln ("   vertical-align: top}");

      Base.Writeln ("div.step {margin: 5px 0 0 30px; padding-left: 15px}");
      Base.Writeln (".STATUS_PASSED    {background-color: #DAFDB5;");
      Base.Writeln ("   color: #5F8E43; border-left: 5px solid #5F8E43;");
      Base.Writeln ("   border-bottom: 1px solid #5F9E43}");
      Base.Writeln (".STATUS_FAILED    {background-color: #C30A12;");
      Base.Writeln ("   color: white; border-left: 5px solid #7C050B;");
      Base.Writeln ("   border-bottom: 1px solid #7C050B}");
      Base.Writeln (".STATUS_UNDEFINED {background-color: #FEFBD2;");
      Base.Writeln ("   color: #8D3B26; border-left: 5px solid #7C050B;");
      Base.Writeln ("   border-bottom: 1px solid #7C050B;}");
      Base.Writeln (".STATUS_SKIPPED   {background-color: #DFFFFD;");
      Base.Writeln ("   border-left: 5px solid #2EF5F2;");
      Base.Writeln ("   border-bottom: 1px solid #2EF5F2}");

      Base.Writeln ("span.group {font-weight: bold}");

      Base.Writeln ("table {margin: 0 0 0 50px}");
      Base.Writeln ("th, td {padding: 0 10px 0 10px}");

      Base.Writeln ("pre {margin: 0 0 0 50px; font-size: 11px}");

      Base.Writeln ("    </style>");
      Base.Writeln ("  </head>");
      Base.Writeln ("  <body>");

      return Media_Writer_Access (Result);
   end Open_HTML;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : not null access File_Writer;
      Text : String)
   is
   begin
      Put (Self.File.all, Text);
   end Write;

   overriding procedure Write
     (Self : not null access HTML_Writer;
      Text : String)
   is
   begin
      Self.Base.Write (Text);
   end Write;

   -------------
   -- Writeln --
   -------------

   overriding procedure Writeln
     (Self : not null access File_Writer;
      Text : String)
   is
   begin
      Put_Line (Self.File.all, Text);
   end Writeln;

   overriding procedure Writeln
     (Self : not null access HTML_Writer;
      Text : String)
   is
   begin
      Self.Base.Writeln (Text);
   end Writeln;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line (Self : not null access File_Writer) is
   begin
      New_Line (Self.File.all);
   end New_Line;

   overriding procedure New_Line (Self : not null access HTML_Writer) is
   begin
      Self.Base.New_Line;  --  Writeln ("<br>");
   end New_Line;

   ------------
   -- Indent --
   ------------

   procedure Indent
     (Self   : not null access Media_Writer;
      Text   : String;
      Prefix : String := "")
   is
      Start, Last : Integer;
   begin
      Start := Text'First;
      while Start <= Text'Last loop
         Last := Line_End (Text, Start);
         if Last < Start then  --  empty line
            Media_Writer'Class (Self.all).Writeln ("");
            Start := Last + 2;
         else
            Media_Writer'Class (Self.all).Write
              (Prefix & Text (Start .. Last));
            Start := Last + 1;
         end if;
      end loop;
   end Indent;

   ---------------
   -- Set_Color --
   ---------------

   overriding procedure Set_Color
     (Self       : not null access File_Writer;
      Foreground : ANSI_Color := Unchanged;
      Style      : ANSI_Style := Unchanged)
   is
   begin
      Self.Term.Set_Color
        (Term       => Self.File.all,
         Foreground => Foreground,
         Style      => Style);
   end Set_Color;

   overriding procedure Set_Color
     (Self       : not null access HTML_Writer;
      Foreground : ANSI_Color := Unchanged;
      Style      : ANSI_Style := Unchanged)
   is
      Fg : ANSI_Color := Foreground;
   begin
      if Fg = Unchanged or else Fg = Reset then
         Fg := Black;
      end if;

      if Fg /= Self.Current_Color then
         if Self.Current_Color /= Black then
            Self.Base.Write ("</font>");
         end if;

         Self.Current_Color := Fg;

         case Fg is
            when Unchanged | Black | Reset => null;
            when Red     => Self.Base.Write ("<font color='red'>");
            when Green   => Self.Base.Write ("<font color='green'>");
            when Yellow  => Self.Base.Write ("<font color='yellow'>");
            when Blue    => Self.Base.Write ("<font color='blue'>");
            when Magenta => Self.Base.Write ("<font color='magenta'>");
            when Cyan    => Self.Base.Write ("<font color='cyan'>");
            when Grey    => Self.Base.Write ("<font color='grey'>");
         end case;
      end if;

      if Style = Reset_All then
         if Self.Current_Color /= Black then
            Self.Current_Color := Black;
            Self.Base.Write ("</font>");
         end if;
      end if;
   end Set_Color;

   ----------------
   -- Has_Colors --
   ----------------

   function Has_Colors
     (Self : not null access File_Writer)
      return Boolean
   is
   begin
      return Self.Term.Has_Colors;
   end Has_Colors;

   ----------------------
   -- Display_Progress --
   ----------------------

   procedure Display_Progress
     (Self : not null access Media_Writer;
      Text : String)
   is
      Width : constant Integer := Self.Stdout_Term.Get_Width;
   begin
      if Width /= -1 then
         Self.Stdout_Term.Beginning_Of_Line;
         Self.Stdout_Term.Clear_To_End_Of_Line;
         Self.Stdout_Term.Set_Color
           (Term       => Ada.Text_IO.Standard_Output,
            Foreground => Grey,
            Style      => Unchanged);
         Put
           (Ada.Text_IO.Standard_Output,
            "Running: "
            & Text (Text'First
                    .. Integer'Min (Text'Last, Text'First + Width - 10)));
         Self.Stdout_Term.Set_Color
           (Term       => Ada.Text_IO.Standard_Output,
            Style      => Reset_All);
         Self.Stdout_Term.Beginning_Of_Line;
         Self.Progress_Displayed := True;
      end if;
   end Display_Progress;

   --------------------
   -- Clear_Progress --
   --------------------

   procedure Clear_Progress (Self : not null access Media_Writer) is
   begin
      if Self.Progress_Displayed then
         Self.Progress_Displayed := False;
         Self.Stdout_Term.Beginning_Of_Line;
         Self.Stdout_Term.Clear_To_End_Of_Line;
      end if;
   end Clear_Progress;

   ---------------------
   -- Display_Feature --
   ---------------------

   procedure Display_Feature
     (Self        : not null access Media_Writer;
      Name        : String;
      Description : String)
   is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      S.Writeln (Cst_Features & ' ' & Name);
      S.Writeln (Description);
   end Display_Feature;

   overriding procedure Display_Feature
     (Self        : not null access HTML_Writer;
      Name        : String;
      Description : String)
   is
   begin
      Self.Base.Writeln
        ("<h1 class='feature'>" & Cst_Features & ' ' & Name & "</h1>");
      Self.Base.Writeln
        ("<p class='feature'>" & Description & "</p>");
   end Display_Feature;

   ------------------
   -- Text_And_Pad --
   ------------------

   function Text_And_Pad (Text : String; Length : Natural) return String is
   begin
      return Text & (1 .. 1 + Length - Text'Length => ' ');
   end Text_And_Pad;

   ----------------------
   -- Display_Scenario --
   ----------------------

   procedure Display_Scenario
     (Self          : not null access Media_Writer;
      Prefix        : String;
      Name          : String;
      Longuest_Step : Natural;
      Location      : String)
   is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      S.Write
        (Text_And_Pad
           (Scenario_Indent & Prefix & ' ' & Name,
            Longuest_Step + Step_Indent'Length));
      S.Set_Color (Grey);
      S.Writeln ("# " & Location);
      S.Set_Color (Style => Reset_All);
   end Display_Scenario;

   ----------------------
   -- Display_Scenario --
   ----------------------

   overriding procedure Display_Scenario
     (Self          : not null access HTML_Writer;
      Prefix        : String;
      Name          : String;
      Longuest_Step : Natural;
      Location      : String)
   is
      pragma Unreferenced (Longuest_Step);
   begin
      Self.Base.Writeln
        ("<h2 class='" & Prefix & "'>"
         & Prefix & ' ' & Name
         & " <p class='location'>" & Location & "</p></h2>");
   end Display_Scenario;

   -----------------
   -- Step_Parser --
   -----------------

   procedure Step_Parser
     (Text     : String;
      Info     : GNAT.Regpat.Match_Array;
      On_Chunk : not null access procedure
        (Chunk : String; Is_Match : Boolean))
   is
      H : array (Step_Indent'Length + 1 .. Text'Length) of Boolean :=
        (others => False);
      First, Last : Integer;
   begin
      for M in 1 .. Info'Last loop
         if Info (M) /= No_Match then
            H (Info (M).First + Step_Indent'Length ..
                 Info (M).Last + Step_Indent'Length) :=
              (others => True);
         end if;
      end loop;

      First := Text'First + Step_Indent'Length;
      On_Chunk (Text (Text'First .. First - 1), Is_Match => False);  --  indent

      while First <= Text'Last loop
         Last := First;
         while Last <= Text'Last and then not H (Last) loop
            Last := Last + 1;
         end loop;
         if First <= Last - 1 then
            On_Chunk (Text (First .. Last - 1), Is_Match => False);
         end if;

         First := Last;
         while First <= Text'Last and then H (First) loop
            First := First + 1;
         end loop;

         if Last <= First - 1 then
            On_Chunk (Text (Last .. First - 1), Is_Match => True);
         end if;
      end loop;
   end Step_Parser;

   ------------------
   -- Display_Step --
   ------------------

   procedure Display_Step
     (Self          : not null access Media_Writer;
      Text          : String;
      Status        : Scenario_Status;
      Longuest_Step : Natural;
      Info          : GNAT.Regpat.Match_Array;
      Location      : String)
   is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
      N : constant String := Text_And_Pad
        (Step_Indent & Text, Longuest_Step + Step_Indent'Length);

      procedure On_Chunk (Chunk : String; Is_Match : Boolean);
      procedure On_Chunk (Chunk : String; Is_Match : Boolean) is
      begin
         if Is_Match then
            S.Set_Color (Foreground => BDD.Config_Color);
         else
            S.Set_Color (Foreground => BDD.Step_Colors (Status));
         end if;

         S.Write (Chunk);
      end On_Chunk;

   begin
      if S.Has_Colors then
         Step_Parser (N, Info, On_Chunk'Access);
      else
         S.Write (N);
      end if;

      S.Set_Color (Grey);
      S.Write ("# ");

      if not S.Has_Colors then
         case Status is
            when Status_Passed    => S.Write ("[OK] ");
            when Status_Failed    => S.Write ("[FAILED] ");
            when Status_Undefined => S.Write ("[UNDEFINED] ");
            when Status_Skipped   => S.Write ("[SKIPPED] ");
         end case;
      end if;

      S.Write (Location);
      S.Set_Color (Style => Reset_All);
      S.New_Line;
   end Display_Step;

   ------------------
   -- Display_Step --
   ------------------

   overriding procedure Display_Step
     (Self          : not null access HTML_Writer;
      Text          : String;
      Status        : Scenario_Status;
      Longuest_Step : Natural;
      Info          : GNAT.Regpat.Match_Array;
      Location      : String)
   is
      pragma Unreferenced (Longuest_Step);

      procedure On_Chunk (Chunk : String; Is_Match : Boolean);
      procedure On_Chunk (Chunk : String; Is_Match : Boolean) is
      begin
         if Is_Match then
            Self.Base.Write ("<span class='group'>" & Chunk & "</span>");
         else
            Self.Base.Write (Chunk);
         end if;
      end On_Chunk;

   begin
      Self.Base.Write
        ("<div class='step " & Status'Img & "'>"
         & "<span class='location'>" & Location & "</span>"
         & "<span>");
      Step_Parser (Step_Indent & Text, Info, On_Chunk'Access);
      Self.Base.Writeln ("</span></div>");
   end Display_Step;

   ---------------
   -- Start_Row --
   ---------------

   procedure Start_Row
     (Self : not null access Media_Writer; Indent : String)
   is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      --  One space is added in Display_Cell
      S.Write (Indent (Indent'First .. Indent'Last - 1));
   end Start_Row;

   -------------
   -- End_Row --
   -------------

   procedure End_Row (Self : not null access Media_Writer) is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      S.Set_Color (Style => Reset_All);
      S.Writeln (" |");
   end End_Row;

   ------------------
   -- Display_Cell --
   ------------------

   procedure Display_Cell
     (Self       : not null access Media_Writer;
      Value      : String;
      Cell_Width : Natural;
      Header     : Boolean;
      Status     : Scenario_Status := Status_Passed)
   is
      pragma Unreferenced (Header);
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      S.Set_Color (Style => Reset_All);
      S.Write (" | ");

      S.Set_Color (Foreground => BDD.Step_Colors (Status));
      S.Write (Value & (1 .. Cell_Width - Value'Length => ' '));
   end Display_Cell;

   -----------------
   -- Start_Table --
   -----------------

   overriding procedure Start_Table (Self : not null access HTML_Writer) is
   begin
      Self.Base.Writeln ("<table cellspacing='0' cellpadding='0'>");
   end Start_Table;

   ---------------
   -- End_Table --
   ---------------

   overriding procedure End_Table (Self : not null access HTML_Writer) is
   begin
      Self.Base.Writeln ("</table>");
   end End_Table;

   ---------------
   -- Start_Row --
   ---------------

   overriding procedure Start_Row
     (Self : not null access HTML_Writer; Indent : String)
   is
      pragma Unreferenced (Indent);
   begin
      Self.Base.Writeln ("  <tr>");
   end Start_Row;

   -------------
   -- End_Row --
   -------------

   overriding procedure End_Row (Self : not null access HTML_Writer) is
   begin
      Self.Base.Writeln ("  </tr>");
   end End_Row;

   ------------------
   -- Display_Cell --
   ------------------

   overriding procedure Display_Cell
     (Self       : not null access HTML_Writer;
      Value      : String;
      Cell_Width : Natural;
      Header     : Boolean;
      Status     : Scenario_Status := Status_Passed)
   is
      pragma Unreferenced (Cell_Width);
   begin
      if Header then
         Self.Base.Write ("    <th class='" & Status'Img & "'>"
                          & Value & "</th>");
      else
         Self.Base.Write ("    <td class='" & Status'Img & "'>"
                          & Value & "</td>");
      end if;
   end Display_Cell;

   -------------------------
   -- Start_Step_Messages --
   -------------------------

   procedure Start_Step_Messages
     (Self   : not null access Media_Writer;
      Status : Scenario_Status)
   is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      S.Set_Color (Foreground => BDD.Step_Colors (Status));
   end Start_Step_Messages;

   -----------------------
   -- End_Step_Messages --
   -----------------------

   procedure End_Step_Messages (Self : not null access Media_Writer) is
      S : constant Media_Writer_Access := Media_Writer_Access (Self);
   begin
      S.Set_Color (Style => Reset_All);
   end End_Step_Messages;

   -------------------------
   -- Start_Step_Messages --
   -------------------------

   overriding procedure Start_Step_Messages
     (Self   : not null access HTML_Writer;
      Status : Scenario_Status)
   is
   begin
      Self.Base.Writeln ("<pre class='" & Status'Img & "'/>");
   end Start_Step_Messages;

   -----------------------
   -- End_Step_Messages --
   -----------------------

   overriding procedure End_Step_Messages
     (Self : not null access HTML_Writer)
   is
   begin
      Self.Base.Writeln ("</pre>");
   end End_Step_Messages;

end BDD.Media;
