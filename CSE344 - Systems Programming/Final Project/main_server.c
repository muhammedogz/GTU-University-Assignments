#include "include/server.h"

int main(int argc, char *argv[])
{
  if (init(argc, argv) != 0)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }
  return 0;
}