#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "../dist/build/dhek/dhek-tmp/Dhek/Launcher_stub.h"
#endif

#import "AppDelegate.h"

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  launch();
}

- (void)applicationWillTerminate:(NSNotification *)aNotification
{
  hs_exit();
}

@end
