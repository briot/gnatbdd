
#ifdef _WIN32

#include <windows.h>

int gnatcoll_get_console_screen_buffer_info(int forStderr) {
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   CONSOLE_SCREEN_BUFFER_INFO csbiInfo;

   if (GetConsoleScreenBufferInfo (handle, &csbiInfo)) {
      return csbiInfo.wAttributes;
   }
   return -1;
}

void gnatcoll_set_console_text_attribute(int forStderr, int attrs) {
   const HANDLE handle =
      GetStdHandle (forStderr ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
   SetConsoleTextAttribute (handle, (WORD)attrs);
}

#else
int gnatcoll_get_console_screen_buffer_info(int forStderr) {
   return 0;
}
void gnatcoll_set_console_text_attribute(int forStderr, int attrs) {
}

#endif
