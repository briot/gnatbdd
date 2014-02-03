------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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
with GNAT.OS_Lib;    use GNAT.OS_Lib;

package body GNATCOLL.Terminal is

   On_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';

   Color_To_Win32 : constant array (ANSI_Color) of Integer :=
      (Unchanged => -1,
       Black     => 0,
       Red       => 4,
       Green     => 2,
       Yellow    => 6,
       Blue      => 1,
       Magenta   => 5,
       Cyan      => 3,
       Grey      => 7,
       Reset     => -1);

   Style_To_Win32 : constant array (ANSI_Style) of Integer :=
      (Unchanged => -1,
       Bright    => 16#08#,
       Dim       => 16#00#,  --  same as Normal
       Normal    => 16#00#,
       Reset_All => -1);

   procedure Win_Set_Console
      (Self  : Terminal_Info'Class;
       Attrs : Integer);
   --  Windows-specific implementation to change the attributes of the console

   ----------
   -- Init --
   ----------

   procedure Init
      (Self   : in out Terminal_Info;
       Term   : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
       Colors : Supports_Color := Auto)
   is
      function getConsoleScreenBufferInfo (Stderr : Integer) return Integer;
      pragma Import (C, getConsoleScreenBufferInfo,
                     "gnatcoll_get_console_screen_buffer_info");

      type Mod_32 is mod 2 ** 32;
      Attrs : Mod_32;
   begin
      Self.Is_Stderr := False;  --   ???

      if On_Windows then
         Attrs := Mod_32
            (getConsoleScreenBufferInfo(Boolean'Pos (Self.Is_Stderr)));
         if Attrs = -1 then
            Self.Colors := Unsupported;
         else
            case Attrs and 7 is
               when 0      => Self.Default_Fore := Black;
               when 1      => Self.Default_Fore := Blue;
               when 2      => Self.Default_Fore := Green;
               when 3      => Self.Default_Fore := Cyan;
               when 4      => Self.Default_Fore := Red;
               when 5      => Self.Default_Fore := Magenta;
               when 6      => Self.Default_Fore := Yellow;
               when others => Self.Default_Fore := Grey ;
            end case;

            case (Attrs / 16) and 7 is
               when 0      => Self.Default_Back := Black;
               when 1      => Self.Default_Back := Blue;
               when 2      => Self.Default_Back := Green;
               when 3      => Self.Default_Back := Cyan;
               when 4      => Self.Default_Back := Red;
               when 5      => Self.Default_Back := Magenta;
               when 6      => Self.Default_Back := Yellow;
               when others => Self.Default_Back := Grey ;
            end case;

            if (Attrs and 16#08#) /= 0 then
               Self.Default_Style := Bright;
            else
               Self.Default_Style := Normal;
            end if;

            Set_Has_Colors (Self, Term, Colors);
         end if;

      else
         Set_Has_Colors (Self, Term, Colors);
      end if;
   end Init;

   --------------------
   -- Set_Has_Colors --
   --------------------

   procedure Set_Has_Colors
      (Self    : in out Terminal_Info;
       Term    : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
       Support : Supports_Color := Auto)
   is
      Env : String_Access;
   begin
      case Support is
         when No =>
            Self.Colors := Unsupported;

         when Yes =>
            if On_Windows then
               Env := Getenv ("ANSICON");
               if Env = null or else Env.all = "" then
                  Self.Colors := WIN32_Sequences;
               else
                  Self.Colors := ANSI_Sequences;
               end if;
               Free (Env);
            else
               Self.Colors := ANSI_Sequences;
            end if;

         when Auto =>
            if On_Windows then
               --  ??? Should check that we have a tty, not a file
               Env := Getenv ("ANSICON");
               if Env = null or else Env.all = "" then
                  Self.Colors := WIN32_Sequences;
               else
                  Self.Colors := ANSI_Sequences;
               end if;
               Free (Env);
            else
               --  ??? Should check whether we have a 'tty'
               --  ??? if TermInfo.default_object.tigetnum("colors") == 0:
               --         unsupported
               --  ??? could look at the TERM environment variable
               Self.Colors := ANSI_Sequences;
            end if;
      end case;
   end Set_Has_Colors;

   ----------------
   -- Has_Colors --
   ----------------

   function Has_Colors (Self : Terminal_Info) return Boolean is
   begin
      return Self.Colors /= Unsupported;
   end Has_Colors;

   ---------------------
   -- Win_Set_Console --
   ---------------------

   procedure Win_Set_Console
      (Self  : Terminal_Info'Class;
       Attrs : Integer)
   is
      procedure Set_Console_Text_Attribute (Stderr : Integer; Attrs : Integer);
      pragma Import (C, Set_Console_Text_Attribute,
                     "gnatcoll_set_console_text_attribute");
   begin
      Set_Console_Text_Attribute (Boolean'Pos (Self.Is_Stderr), Attrs);
   end Win_Set_Console;

   ------------
   -- Set_Fg --
   ------------

   procedure Set_Fg
      (Self  : in out Terminal_Info;
       Color : ANSI_Color;
       Term  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output)
   is
   begin
      Set_Color (Self, Term, Color, Unchanged, Unchanged);
   end Set_Fg;

   ------------
   -- Set_Bg --
   ------------

   procedure Set_Bg
      (Self  : in out Terminal_Info;
       Color : ANSI_Color;
       Term  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output)
   is
   begin
      Set_Color (Self, Term, Unchanged, Color, Unchanged);
   end Set_Bg;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
      (Self  : in out Terminal_Info;
       Style : ANSI_Style;
       Term  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output)
   is
   begin
      Set_Color (Self, Term, Unchanged, Unchanged, Style);
   end Set_Style;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
      (Self       : in out Terminal_Info;
       Term       : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
       Foreground : ANSI_Color := Unchanged;
       Background : ANSI_Color := Unchanged;
       Style      : ANSI_Style := Unchanged)
   is
      Attrs : Integer := 0;
   begin
      case Self.Colors is
         when Unsupported =>
            null;

         when ANSI_Sequences =>
            case Style is
               when Unchanged => null;
               when Bright    => Put (Term, ASCII.ESC & "[1m");
               when Dim       => Put (Term, ASCII.ESC & "[2m");
               when Normal    => Put (Term, ASCII.ESC & "[22m");
               when Reset_All => Put (Term, ASCII.ESC & "[0m");
            end case;

            case Foreground is
               when Unchanged => null;
               when Black     => Put (Term, ASCII.ESC & "[30m");
               when Red       => Put (Term, ASCII.ESC & "[31m");
               when Green     => Put (Term, ASCII.ESC & "[32m");
               when Yellow    => Put (Term, ASCII.ESC & "[33m");
               when Blue      => Put (Term, ASCII.ESC & "[34m");
               when Magenta   => Put (Term, ASCII.ESC & "[35m");
               when Cyan      => Put (Term, ASCII.ESC & "[36m");
               when Grey      => Put (Term, ASCII.ESC & "[37m");
               when Reset     => Put (Term, ASCII.ESC & "[39m");
            end case;

            case Background is
               when Unchanged => null;
               when Black     => Put (Term, ASCII.ESC & "[40m");
               when Red       => Put (Term, ASCII.ESC & "[41m");
               when Green     => Put (Term, ASCII.ESC & "[42m");
               when Yellow    => Put (Term, ASCII.ESC & "[43m");
               when Blue      => Put (Term, ASCII.ESC & "[44m");
               when Magenta   => Put (Term, ASCII.ESC & "[45m");
               when Cyan      => Put (Term, ASCII.ESC & "[46m");
               when Grey      => Put (Term, ASCII.ESC & "[47m");
               when Reset     => Put (Term, ASCII.ESC & "[49m");
            end case;

         when WIN32_Sequences =>
            if Style = Reset_All then
               Self.Style := Self.Default_Style;
               Self.Fore  := Self.Default_Fore;
               Self.Back  := Self.Default_Back;
            elsif Style /= Unchanged then
               Self.Style := Style;
            end if;

            if Foreground = Reset then
               Self.Fore := Self.Default_Fore;
            elsif Foreground /= Unchanged then
               Self.Fore := Foreground;
            end if;
      
            if Background = Reset then
               Self.Back := Self.Default_Back ;
            elsif Background /= Unchanged then
               Self.Back := Background;
            end if;
      
            Attrs := Attrs + Style_To_Win32(Self.Style) +
               Color_To_Win32(Self.Fore) +
               Color_To_Win32(Self.Back) * 16;

            Win_Set_Console (Self, Attrs);
      end case;
   end Set_Color;

end GNATCOLL.Terminal;
