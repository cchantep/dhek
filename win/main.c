#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Dhek/Launcher_stub.h"
extern void __stginit_DhekziLauncher(void);
#endif

int main(int argc, char *argv[])
{
  // Haskell setup
  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_DhekziLauncher);
#endif

  launch();
}
