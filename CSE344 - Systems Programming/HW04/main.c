#include <stdio.h>
#include <unistd.h>
#include "include/utils.h"

int main()
{

  printMessageWithTime(STDOUT_FILENO, "Hello World");
  printMessage(STDOUT_FILENO, "Hello World");
  dprintf(STDOUT_FILENO, "Error code: %d\n", GLOBAL_ERROR);
}