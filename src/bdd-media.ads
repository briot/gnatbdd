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

--  Output to different media (stdout, files, html, json,...)

with GNAT.Regpat;        use GNAT.Regpat;
with GNATCOLL.Terminal;  use GNATCOLL.Terminal;

package BDD.Media is

   type Media_Writer is abstract tagged private;
   type Media_Writer_Access is access all Media_Writer'Class;

   function Open_Stdout return Media_Writer_Access;
   --  Will output to stdout

   function Open_HTML
     (Base : not null access Media_Writer'Class)
      return Media_Writer_Access;
   --  A writer that will output HTML text on the base writer

   function Open_File
     (File : GNATCOLL.VFS.Virtual_File) return Media_Writer_Access;
   --  Output to a specific file

   procedure Write
     (Self : not null access Media_Writer;
      Text : String) is abstract;
   procedure Writeln
     (Self : not null access Media_Writer;
      Text : String) is abstract;
   procedure New_Line (Self : not null access Media_Writer) is abstract;
      --  Write some text (possibly followed by a newline)

   procedure Indent
     (Self   : not null access Media_Writer;
      Text   : String;
      Prefix : String := "");
   --  Print a multi-line text so that each line is indented by Prefix

   procedure Set_Color
     (Self       : not null access Media_Writer;
      Foreground : ANSI_Color := Unchanged;
      Style      : ANSI_Style := Unchanged) is abstract;
   --  Change the current color (when colors are supported)

   function Has_Colors
     (Self : not null access Media_Writer)
      return Boolean is abstract;
   --  Whether colors are used in the output

   procedure Display_Progress
     (Self : not null access Media_Writer;
      Text : String);
   procedure Clear_Progress (Self : not null access Media_Writer);
   --  Display a progress bar (in general at the bottom of the screen).
   --  This is in general sent to the user's terminal, not to the file in
   --  which the output occurs.

   -------------------------
   -- Specific to GNATBDD --
   -------------------------

   Scenario_Indent : constant String := "  ";
   Step_Indent     : constant String := "    ";
   --  Visual indentation in the display

   procedure Display_Feature
     (Self        : not null access Media_Writer;
      Name        : String;
      Description : String);
   --  Display a feature header

   procedure Display_Scenario
     (Self          : not null access Media_Writer;
      Prefix        : String;
      Name          : String;
      Longuest_Step : Natural;
      Location      : String);
   --  Display a scenario header.
   --  Prefix is one of 'Scenario', 'Background', 'Outline',...
   --  Longuest_Step indicates the number of characters needed to display the
   --  longuest step's text

   procedure Display_Step
     (Self          : not null access Media_Writer;
      Text          : String;
      Status        : Scenario_Status;
      Longuest_Step : Natural;
      Info          : GNAT.Regpat.Match_Array;
      Location      : String);
   --  Display a single step.
   --  Info is used to find the location of the regexp's parenthesis groups,
   --  to highlight them specially.

   procedure Start_Table (Self : not null access Media_Writer) is null;
   procedure End_Table (Self : not null access Media_Writer) is null;
   procedure Start_Row (Self : not null access Media_Writer; Indent : String);
   procedure End_Row (Self : not null access Media_Writer);
   procedure Display_Cell
     (Self       : not null access Media_Writer;
      Value      : String;
      Cell_Width : Natural;
      Header     : Boolean;
      Status     : Scenario_Status := Status_Passed);
   --  Support for writing tables

   procedure Start_Step_Messages
     (Self   : not null access Media_Writer;
      Status : Scenario_Status);
   procedure End_Step_Messages (Self : not null access Media_Writer);
   --  Wraps the display of step messages

   function Text_And_Pad (Text : String; Length : Natural) return String;
   --  Return the text left-aligned, and with padding characters up to
   --  Length.

private
   type Media_Writer is abstract tagged record
      Progress_Displayed : Boolean := False;

      Stdout_Term : GNATCOLL.Terminal.Terminal_Info_Access;
      --  The stdout terminal (used to display progress bars)
   end record;

end BDD.Media;
