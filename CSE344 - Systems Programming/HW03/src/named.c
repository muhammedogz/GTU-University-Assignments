#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>
#include "../include/common.h"
#include "../include/named.h"

int detectArguments(int argc, char *argv[], char **inputFilePath, char **name)
{
  if (argc != 5)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int i_found = 0;
  int n_found = 0;
  while ((opt = getopt(argc, argv, "i:n:")) != -1)
  {
    switch (opt)
    {
    case 'i':
      if (i_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *inputFilePath = optarg;
      i_found = 1;
      break;
    case 'n':
      if (n_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *name = optarg;
      n_found = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  return 1;
}