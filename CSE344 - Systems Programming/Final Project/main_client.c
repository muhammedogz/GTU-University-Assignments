#include "include/client.h"

int main(int argc, char *argv[])
{
  if (init(argc, argv) != 0)
  {
    dprintf(STDERR_FILENO, "%s: Client is exiting with error %d\n", getTime(), GLOBAL_ERROR);
    return -1;
  }
  return 0;
}