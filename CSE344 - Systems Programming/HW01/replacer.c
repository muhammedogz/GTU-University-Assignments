#include <stdio.h>
#include "replacer.h"

int detect_arguments(int argc, char *argv[])
{
  // print all arguments
  for (int i = 0; i < argc; i++)
  {
    printf("argv[%d] = %s\n", i, argv[i]);
  }

  printf("hello");
  return 0;
}