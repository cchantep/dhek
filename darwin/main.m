#include <Cocoa/Cocoa.h>
#include <stdio.h>

#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "../dist/build/dhek/dhek-tmp/Dhek/Launcher_stub.h"
extern void __stginit_DhekziLauncher(void);
#endif

#import "AppDelegate.h"

int main(int argc, char *argv[])
{
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_DhekziLauncher);
#endif

  [NSApplication sharedApplication]; // Ensure NSApp is init'ed
  [NSApp setDelegate: [AppDelegate new]];
  [NSApp run];
}
