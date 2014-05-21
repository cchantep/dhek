#include <Cocoa/Cocoa.h>
#include "util.h"

/** Wraps ObjC call to be used as foreign function. */
void nsappTerminate() {
  [NSApp terminate: nil];
}

extern void nsappTerminate();

