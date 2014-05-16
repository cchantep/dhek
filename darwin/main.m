#include <stdlib.h>
#include <sys/param.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <libgen.h>

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "../dist/build/dhek/dhek-tmp/Dhek/Launcher_stub.h"
extern void __stginit_DhekziLauncher(void);
#endif

#include <Cocoa/Cocoa.h>

#import "AppDelegate.h"

int main(int argc, char *argv[])
{
  // Haskell setup
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_DhekziLauncher);
#endif

  // GTK setup
  char exec_path[PATH_MAX];

  if (realpath(argv[0], exec_path) == 0) { 
    fprintf(stderr, "Fails to find executable path: %s\n", strerror(errno));
  } else {
    char* basedir = dirname(exec_path);
    int pathlen = strlen(basedir);

    // GTK modules path, containing 2.10.0/engines/libENGINE.so
    char modules_path[pathlen+26];
    strcpy(modules_path, basedir);
    strcat(modules_path, "/../Resources/lib/gtk-2.0");

    char theme_path[pathlen+51];
    strcpy(theme_path, basedir);
    strcat(theme_path, "/../Resources/themes/Gnome-Cupertino/gtk-2.0/gtkrc");

    printf("GTK modules path: %s\nWill load GTK theme: %s\n", 
           modules_path, theme_path);

    setenv("GTK_PATH", modules_path, 1);
    setenv("GTK2_RC_FILES", theme_path, 1);

    free(basedir);
  }

  // Then launch it as Cocoa app...
  [NSApplication sharedApplication]; // Ensure NSApp is init'ed
  [NSApp setDelegate: [AppDelegate new]];
  [NSApp run];
}
