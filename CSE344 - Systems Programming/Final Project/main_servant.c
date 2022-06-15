#include "include/servant.h"

int main(int argc, char *argv[])
{

  if (init(argc, argv) < 0)
  {
    dprintf(STDERR_FILENO, "[!] Something went wrong\n");
    return -1;
  }
  return 0;
}