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
#include "../include/serverY.h"

int detectArguments(int argc, char *argv[], char **pathToServerFifo, char **pathToLogFile, int *poolSize, int *poolSize2, int *time_v)
{
  if (argc != 11)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int o_found = 0;
  int s_found = 0;
  int p_found = 0;
  int r_found = 0;
  int t_found = 0;
  while ((opt = getopt(argc, argv, "s:o:p:r:t:")) != -1)
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
      *pathToLogFile = optarg;
      o_found = 1;
      break;
    case 'p':
      if (p_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *poolSize = atoi(optarg);
      p_found = 1;
      break;
    case 'r':
      if (r_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *poolSize2 = atoi(optarg);
      r_found = 1;
      break;
    case 't':
      if (t_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      *time_v = atoi(optarg);
      t_found = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  return 1;
}

void invalid_usage()
{
  write(STDERR_FILENO, "Usage: ./serverY -s <pathToServerFifo> -o <pathToLogFile> -p <poolSize> -r <poolSize2> -t <time_v>\n",
        strlen("Usage: ./serverY -s <pathToServerFifo> -o <pathToLogFile> -p <poolSize> -r <poolSize2> -t <time_v>\n"));
}