with GNATCOLL.Terminal;   use GNATCOLL.Terminal;
with Ada.Text_IO;         use Ada.Text_IO;

procedure Test_Colors is
   Info : Terminal_Info;

   procedure Header (Name : String; Fg : ANSI_Color);
   procedure Header (Name : String; Fg : ANSI_Color) is
   begin
      Info.Set_Color (Standard_Output, Fg, Reset, Normal);
      Put (Name);
   end Header;

   procedure Show (Name : String; Bg : ANSI_Color);
   procedure Show (Name : String; Bg : ANSI_Color) is
   begin
      Info.Set_Color (Standard_Output, Reset, Bg, Normal);
      Put (Name);

      for Fg in Black .. Grey loop 
         Info.Set_Color (Standard_Output, Fg, Bg, Normal);
         Put ("X ");
         Info.Set_Color (Standard_Output, Style => Dim);
         Put ("X ");
         Info.Set_Color (Standard_Output, Style => Bright);
         Put ("X ");
         Info.Set_Color (Standard_Output, Style => Reset_All);
         Put (" ");
      end loop;

      New_Line;
   end Show;

begin
   Info.Init (Ada.Text_IO.Standard_Output, Auto);

   Header ("        ", Reset);
   Header ("black  ", Black);
   Header ("red    ", Red);
   Header ("green  ", Green);
   Header ("yellow ", Yellow);
   Header ("blue   ", Blue);
   Header ("magenta", Magenta);
   Header ("cyan   ", Cyan);
   Header ("white  ", Grey);
   New_Line;

   Show ("black   ", Black);
   Show ("red     ", Red);
   Show ("green   ", Green);
   Show ("yellow  ", Yellow);
   Show ("blue    ", Blue);
   Show ("magenta ", Magenta);
   Show ("cyan    ", Cyan);
   Show ("white   ", Grey);
end Test_Colors;
