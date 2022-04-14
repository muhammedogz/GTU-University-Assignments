#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../include/common.h"
#include "../include/client.h"

int detectArguments(int argc, char *argv[], char **pathToServerFifo, char **pathToDataFile)
{
  if (argc != 5)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int o_found = 0;
  int s_found = 0;
  while ((opt = getopt(argc, argv, "s:o:")) != -1)
  {
    switch (opt)
    {
    case 's':
      if (s_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *pathToServerFifo = optarg;
      s_found = 1;
      break;
    case 'o':
      if (o_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *pathToDataFile = optarg;
      o_found = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  return 1;
}

void invalidUsage()
{
  write(STDERR_FILENO, "Usage: ./client -s <server fifo path> -o <data file path>\n",
        strlen("Usage: ./client -s <server fifo path> -o <data file path>\n"));
}
