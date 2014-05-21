#include <windows.h>
#include "util.h"

void browser_open(const char* url) {
  ShellExecute(NULL, "open", url, NULL, NULL, SW_SHOWNORMAL);
}

