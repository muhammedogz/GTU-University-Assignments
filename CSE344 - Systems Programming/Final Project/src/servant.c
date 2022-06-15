#include <stdio.h>
#include "../include/servant.h"

ServantVariables servantVariables;

int sigintReceived = 0;

void signalHandler()
{
  sigintReceived = 1;
  printf("\n");
  printf("[!] SIGINT received.\n");
}

void atexitHandler()
{
  freeList(servantVariables.cities, freeString);
}

int detectArguments(int argc, char *argv[])
{
  if (argc != 9)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int dFound = 0;
  int cFound = 0;
  int rFound = 0;
  int pFound = 0;
  while ((opt = getopt(argc, argv, "d:c:r:p:")) != -1)
  {
    switch (opt)
    {
    case 'd':
      if (dFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.directoryPath = optarg;
      dFound = 1;
      break;
    case 'c':
      if (cFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.cityInterval = optarg;
      cFound = 1;
      break;
    case 'r':
      if (rFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.ipAddress = optarg;
      rFound = 1;
      break;
    case 'p':
      if (pFound)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      servantVariables.port = atoi(optarg);
      pFound = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!dFound || !cFound || !rFound || !pFound)
  {
    dprintf(STDERR_FILENO, "[!] Invalid arguments.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // divide cities to two parts with delimeter -
  char *p = servantVariables.cityInterval;
  char *q = strchr(p, '-');
  if (q == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Invalid cities.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }
  *q = '\0';
  servantVariables.cityStart = atoi(p);
  servantVariables.cityEnd = atoi(q + 1);
  if (servantVariables.cityStart > servantVariables.cityEnd)
  {
    dprintf(STDERR_FILENO, "[!] City start must be less than city end.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if port is bigger than 2000
  if (servantVariables.port < 2000)
  {
    dprintf(STDERR_FILENO, "[!] Port must be bigger than 2000.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if given directory exist
  if (access(servantVariables.directoryPath, F_OK) == -1)
  {
    dprintf(STDERR_FILENO, "[!] Directory %s does not exist.\n", servantVariables.directoryPath);
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  List *list = NULL;
  if ((list = initializeList()) == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot initialize list.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  DIR *directory;
  int directoryCount = 0;
  struct dirent *dir;
  directory = opendir(servantVariables.directoryPath);
  if (directory)
  {
    while ((dir = readdir(directory)) != NULL)
    {
      if (strcmp(dir->d_name, ".") == 0 || strcmp(dir->d_name, "..") == 0)
      {
        continue;
      }

      directoryCount++;
      char *temp = malloc(sizeof(char) * (strlen(dir->d_name) + 1));
      strcpy(temp, dir->d_name);
      addNode(list, temp);

      // printf("%s\n", dir->d_name);
    }
    closedir(directory);
  }
  sortList(list, compareString);

  // check if directory count is equal to start and end city
  if (directoryCount - 2 < servantVariables.cityEnd - servantVariables.cityStart + 1)
  {
    dprintf(STDERR_FILENO, "[!] Directory count is not equal to start and end city.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  servantVariables.cities = NULL;
  if ((servantVariables.cities = getListInRangeString(list, servantVariables.cityStart - 1, servantVariables.cityEnd - 1)) == NULL)
  {
    dprintf(STDERR_FILENO, "[!] Cannot get cities.\n");
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }
  freeList(list, freeString);
  // printList(servantVariables.cities, printString);

  // print all
  // dprintf(STDOUT_FILENO, "%s: Input file 1: %s\n", getTime(), argumentInfo.directoryPath);
  // dprintf(STDOUT_FILENO, "%s: Input file 2: %s\n", getTime(), argumentInfo.cities);
  // dprintf(STDOUT_FILENO, "%s: IP address: %s\n", getTime(), argumentInfo.ipAddress);
  // dprintf(STDOUT_FILENO, "%s: Port: %d\n", getTime(), argumentInfo.port);
  // dprintf(STDOUT_FILENO, "%s: City start: %d\n", getTime(), argumentInfo.cityStart);
  // dprintf(STDOUT_FILENO, "%s: City end: %d\n", getTime(), argumentInfo.cityEnd);
  // dprintf(STDOUT_FILENO, "%s: Directory count: %d\n", getTime(), directoryCount);

  return 1;
}

int init(int argc, char *argv[])
{
  servantVariables.pid = getOwnPid();
  servantVariables.cities = NULL;
  servantVariables.directoryPath = NULL;
  servantVariables.ipAddress = NULL;
  servantVariables.port = 0;
  servantVariables.cityStart = 0;
  servantVariables.cityEnd = 0;
  servantVariables.totalRequestHandled = 0;
  servantVariables.cityInterval = NULL;

  if (detectArguments(argc, argv) != 1)
  {
    printUsage();
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }

  if ((GLOBAL_ERROR = initializeSignalAndAtexit(SIGINT, signalHandler, atexitHandler) != 0))
  {
    printError(STDERR_FILENO, GLOBAL_ERROR);
  }

  dprintf(STDOUT_FILENO, "%s: Servant %d: loaded dataset. cities %s-%s\n", getTime(), servantVariables.pid, (char *)listGetFirst(servantVariables.cities), (char *)listGetLast(servantVariables.cities));
  dprintf(STDOUT_FILENO, "%s: Servant %d: listenint at port %d\n", getTime(), servantVariables.pid, servantVariables.port);

  while (1)
    ;

  return 0;
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: ./servant -d <directory path> -c <cities file> -r <ip address> -p <port>\n");
}