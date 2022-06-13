#include "include/servant.h"

int main(int argc, char *argv[])
{
  if (detectArguments(argc, argv) != 1)
  {
    printUsage();
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }

  init();
  return 0;
}