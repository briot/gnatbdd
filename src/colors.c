
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

int gnatcoll_get_console_screen_buffer_info(int forStderr) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   CONSOLE_SCREEN_BUFFER_INFO csbiInfo;

   if (GetConsoleScreenBufferInfo (handle, &csbiInfo)) {
      return csbiInfo.wAttributes;
   }
#else
   return -1;
#endif
}

void gnatcoll_set_console_text_attribute(int forStderr, int attrs) {
#ifdef _WIN32
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   SetConsoleTextAttribute (handle, (WORD)attrs);
#endif
}

int gnatcoll_terminal_has_colors(int fd) {
#ifdef _WIN32
   return 0;  //  Unix only
#else
   //  Ideally, we should check the terminfo database and check the
   //  max_colors fields (from the command line, this is done with
   //  "tput colors"). However, this is fairly complex, and would
   //  drag in the curses library.
   //  For now, let's just assume that a tty always supports colors,
   //  which is true in this day and age for interactive terminals on
   //  all Unix platforms. A pipe will return 0 below, so will not have
   //  colors by default.
   //  ??? We could also check the value of the TERM environment variable,
   //  but this is very approximate at best.

   return isatty(fd);
#endif
}
