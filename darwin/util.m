#include <Cocoa/Cocoa.h>
#include "util.h"

/** Wraps ObjC call to be used as foreign function. */
void nsappTerminate() {
  [NSApp terminate: nil];
}

extern void nsappTerminate();

void nsbrowserOpen(const char* url) {
  NSString* nsstr = [NSString stringWithUTF8String: url];
  NSURL* nsurl = [NSURL URLWithString: nsstr];

  [[NSWorkspace sharedWorkspace] openURL: nsurl];

  [nsurl release];
  [nsstr release];
}

extern void nsbrowserOpen(const char*);
