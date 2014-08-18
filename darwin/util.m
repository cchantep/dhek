#include <Cocoa/Cocoa.h>
#include <fontconfig/fontconfig.h>
#include "util.h"

/** Wraps ObjC call to be used as foreign function. */
void nsappTerminate() {
  NSLog(@"Terminate Cocoa application...");
  [NSApp terminate: nil];
}

void nsbrowserOpen(const char* url) {
  NSString* nsstr = [NSString stringWithUTF8String: url];
  NSURL* nsurl = [NSURL URLWithString: nsstr];

  [[NSWorkspace sharedWorkspace] openURL: nsurl];

  [nsurl release];
  [nsstr release];
}

unsigned int fcInit() {
  if (FcInit() == FcFalse) return 0;
  return 1;
}

extern void nsappTerminate();
extern void nsbrowserOpen(const char*);
extern unsigned int fcInit();
