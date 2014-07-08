#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "../dist/build/dhek/dhek-tmp/Dhek/Launcher_stub.h"
#endif

#import "AppDelegate.h"

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  [self performSelectorOnMainThread:@selector(setUp) 
                         withObject:nil waitUntilDone:NO];
}

/*
  Wraps GTK/UI launch so that it can be runned in main NS run loop,
  not to block AppKit/UI thread.
*/
- (void)setUp
{
  launch();
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication {
  return YES;
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)sender {
  return NSTerminateNow;
}

- (void)applicationWillTerminate:(NSNotification *)aNotification
{
  NSLog(@"Termination of Cocoa application has been accepted");
  [self performSelectorOnMainThread:@selector(tearDown) 
                         withObject:nil waitUntilDone:NO];
}

/*
  Wraps Haskell release so that it can be done in main NS run loop,
  not to block AppKit/UI thread.
*/
- (void)tearDown
{
  NSLog(@"Tear down");
  hs_exit(); // Already stopped???
}

@end
