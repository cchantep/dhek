#include <stdlib.h>
#include <sys/param.h>
#include <string.h>
#include <errno.h>
#include <libgen.h>

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Dhek/Launcher_stub.h"
extern void __stginit_DhekziLauncher(void);
#endif

#include <Cocoa/Cocoa.h>

#import "AppDelegate.h"

int main(int argc, char *argv[])
{
  char exec_path[PATH_MAX];

  if (realpath(argv[0], exec_path) == 0) { 
    NSLog(@"Fails to find executable path: %s\n", strerror(errno));
    
    return 1;
  } 

  // ---

  char* basedir = dirname(exec_path);
  int pathlen = strlen(basedir);

  // GTK modules path, containing 2.10.0/engines/libENGINE.so
  char modules_path[pathlen+26];
  strcpy(modules_path, basedir);
  strcat(modules_path, "/../Resources/lib/gtk-2.0");

  // GTK theme
  char theme_path[pathlen+51];
  strcpy(theme_path, basedir);
  strcat(theme_path, "/../Resources/themes/Gnome-Cupertino/gtk-2.0/gtkrc");

  // Pango RC
  char pangorc_path[pathlen+38];
  strcpy(pangorc_path, basedir);
  strcat(pangorc_path, "/../Resources/lib/pango/1.8.0/pangorc");

  char pangoreg_path[pathlen+44];
  strcpy(pangoreg_path, basedir);
  strcat(pangoreg_path, "/../Resources/lib/pango/1.8.0/pango.modules");

  NSString* nsPangoRcPath = [NSString stringWithUTF8String: pangorc_path];

  NSMutableString* pangoConfig = [[NSMutableString alloc] init];
  [pangoConfig appendFormat:@"[Pango]\nModuleFiles=%s", pangoreg_path];

  NSError* nsError;
  [pangoConfig writeToFile:nsPangoRcPath atomically:YES 
                  encoding:NSASCIIStringEncoding error:&nsError];

  // Pango library path, containing pango/1.8.0/modules/lib*.so
  char pangolib_path[pathlen+18]; 
  strcpy(pangolib_path, basedir);
  strcat(pangolib_path, "/../Resources/lib");

  // FontConfig
  char fontconfig_path[pathlen+25];
  strcpy(fontconfig_path, basedir);
  strcat(fontconfig_path, "/../Resources/fonts.conf");

  NSLog(@"GTK modules path: %s\nWill load GTK theme: %s\nPango configuration: %s\nPango library directory: %s\nFontConfig file: %s\n", modules_path, theme_path, pangorc_path, pangolib_path, fontconfig_path);

  setenv("GTK_PATH", modules_path, 1);
  setenv("GTK2_RC_FILES", theme_path, 1);
  setenv("PANGO_RC_FILE", pangorc_path, 1);
  setenv("PANGO_LIBDIR", pangolib_path, 1);
  setenv("FONTCONFIG_FILE", fontconfig_path, 1);

  free(basedir);

  // Haskell setup
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_DhekziLauncher);
#endif

  // Then launch it as Cocoa app...
  [NSApplication sharedApplication]; // Ensure NSApp is init'ed
  [NSApp setDelegate: [AppDelegate new]];
  [NSApp run];
}
