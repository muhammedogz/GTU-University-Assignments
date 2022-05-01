#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include "include/common.h"
#include "include/unnamed.h"

int main()
{
  printUnnamedHello();
  if (GLOBAL_ERROR != 0)
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
    return -1;
  }
  printUnnamedHello();
  return 0;
}
