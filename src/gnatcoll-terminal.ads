------------------------------------------------------------------------------
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

--  This package provides a number of cross-platform subprograms to control
--  output in terminals, in particular colors.
--
--  On Windows, color sequences are either set using the standard WIN32 codes,
--  or if the package ANSICON (https://github.com/adoxa/ansicon/) is running it
--  will use the standard ANSI sequences.

with Ada.Text_IO;

package GNATCOLL.Terminal is

   type Terminal_Info is tagged private;
   --  Information about a terminal on which we output.
   --  This structure does not encapsulate the terminal itself, which is a
   --  limited type.
   --  By default, this is configured without support for colors. It is thus
   --  recommended to first call Init before you use this type.
   --  This type is almost always used in conjonction with a File_Type, which
   --  is where text is actually output. The properties of that File_Type are
   --  queried and cached in the Terminal_Info.

   type Supports_Color is (Yes, No, Auto);
   procedure Set_Has_Colors
      (Self    : in out Terminal_Info;
       Term    : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
       Support : Supports_Color := Auto);
   function Has_Colors (Self : Terminal_Info) return Boolean;
   --  Whether the terminals supports colors. You can use Set_Has_Colors
   --  to force the unconditional use of colors, or to disable it. The default
   --  is always to automatically detect whether they are supported.

   procedure Init
      (Self   : in out Terminal_Info;
       Term   : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
       Colors : Supports_Color := Auto);
   --  Init Self. If Colors is Auto, checks whether Self supports color
   --  output.

   type ANSI_Color is
      (Unchanged,
       Black,
       Red,
       Green,
       Yellow,
       Blue,
       Magenta,
       Cyan,
       Grey,
       Reset);
   --  The colors that can be output in a terminal (ANSI definitions).  The
   --  actual color that the user will see might be different, since a terminal
   --  might associate a different color to the same escape sequence.

   type ANSI_Style is
      (Unchanged,
       Bright,
       Dim,
       Normal,
       Reset_All);
   --  The style for the text. Some styles are not supported on some
   --  terminals, like Dim on the Windows console.

   procedure Set_Color
      (Self       : in out Terminal_Info;
       Term       : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
       Foreground : ANSI_Color := Unchanged;
       Background : ANSI_Color := Unchanged;
       Style      : ANSI_Style := Unchanged);
   --  Change the colors that will be used for subsequent output on the
   --  terminal.
   --  This procedure has no effect if Has_Colors returns False.
   --  In general, it is not recommended to output colors to files, so you
   --  should not use Set_Color in such a context.

   procedure Set_Fg
      (Self  : in out Terminal_Info;
       Color : ANSI_Color;
       Term  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);
   procedure Set_Bg
      (Self  : in out Terminal_Info;
       Color : ANSI_Color;
       Term  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);
   procedure Set_Style
      (Self  : in out Terminal_Info;
       Style : ANSI_Style;
       Term  : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);
   --  Override specific colors.

private
   type Color_Sequence_Type is (Unsupported, ANSI_Sequences, WIN32_Sequences);

   type Terminal_Info is tagged record
      Colors : Color_Sequence_Type := Unsupported;

      Fore   : ANSI_Color := Black;
      Back   : ANSI_Color := Grey;
      Style  : ANSI_Style := Normal;
      --  Current attributes (on Windows, all three must be changed at the
      --  same time)

      Default_Fore  : ANSI_Color := Black;
      Default_Back  : ANSI_Color := Grey;
      Default_Style : ANSI_Style := Normal;
      --  Default windows attributes (computed in Init)

      Is_Stderr : Boolean := False;
      --  Whether the associated terminal is stdout (windows only)
   end record;

end GNATCOLL.Terminal;
