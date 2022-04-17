#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>
#include <signal.h>
#include "include/common.h"
#include "include/serverY.h"

int globalRunningStatus = 1;
char *globalPathToServerFifo = NULL;
void sigint_handler(int signal)
{
  if (signal == SIGINT)
  {
    globalRunningStatus = 0;
    if (unlink(globalPathToServerFifo) == -1)
    {
      GLOBAL_ERROR = UNLINK_ERROR;
      return;
    }
  }
}

int main(int argc, char *argv[])
{

  signal(SIGINT, sigint_handler);

  char *pathToServerFifo = NULL;
  char *pathToLogFile = NULL;
  int poolSize = 0;
  int poolSize2 = 0;
  int time_v = 0;

  int logFileDescriptor = 0;
  int childrensInitialized = 0;

  // checks if server already running or not
  // if not, creates a temp file
  if (checkAlreadyRunning() == -1)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  if (detectArguments(argc, argv, &pathToServerFifo, &pathToLogFile, &poolSize, &poolSize2, &time_v) == -1)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    invalid_usage();
    exit(EXIT_FAILURE);
  }

  globalPathToServerFifo = pathToServerFifo;

  // open log file
  // logFileDescriptor =  open(pathToLogFile, O_WRONLY | O_APPEND | O_CREAT, 0666);
  logFileDescriptor = 1;
  if (logFileDescriptor == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    printError(logFileDescriptor, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  printMessageWithTime(logFileDescriptor, "ServerY started\n");
  int pipeBetweenServers[2];
  if (pipe(pipeBetweenServers) == -1)
  {
    GLOBAL_ERROR = PIPE_CREATION_ERROR;
    printError(logFileDescriptor, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  int *sharedMemory = (int *)createSharedMemoryChildY(poolSize);
  if (sharedMemory == NULL)
  {
    printError(logFileDescriptor, GLOBAL_ERROR);
    exit(EXIT_FAILURE);
  }

  int poolPipe[poolSize][2];
  for (int i = 0; i < poolSize; i++)
  {
    if (pipe(poolPipe[i]) == -1)
    {
      GLOBAL_ERROR = PIPE_CREATION_ERROR;
      printError(logFileDescriptor, GLOBAL_ERROR);
      exit(EXIT_FAILURE);
    }
  }

  pid_t childsY[poolSize];
  pid_t serverZID = createServerZ(pipeBetweenServers[0], pipeBetweenServers[1], logFileDescriptor, poolSize, poolSize2, time_v);
  printMessageWithTime(logFileDescriptor, "Instantiated serverZ\n");

  while (globalRunningStatus)
  {
    Matrix matrix;
    matrix.data = NULL;

    // reads matrix from server fifo
    matrix = readMatrix(pathToServerFifo);
    if (matrix.data == NULL)
    {
      if (globalRunningStatus == 0)
        break;
      printError(logFileDescriptor, GLOBAL_ERROR);
      exitGracefully(EXIT_FAILURE, logFileDescriptor);
    }

    int clientDown = matrix.clientDown;
    if (clientDown)
    {
      char clientID[10];
      sprintf(clientID, "%d", matrix.id);
      printMessageWithTime(logFileDescriptor, "Client PID#");
      printMessage(logFileDescriptor, clientID);
      printMessage(logFileDescriptor, " is down. Going with next client\n");
      continue;
    }

    if (childrensInitialized == 0)
    {
      for (int i = 0; i < poolSize; i++)
      {
        childsY[i] = fork();
        if (childsY[i] == -1)
        {
          GLOBAL_ERROR = FORK_ERROR;
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, logFileDescriptor);
        }
        else if (childsY[i] == 0) // child process
        {
          if (runChildY(poolPipe[i][1], poolPipe[i][0], logFileDescriptor, i, time_v, poolSize, 1) == -1)
          {
            printError(logFileDescriptor, GLOBAL_ERROR);
            exitGracefully(EXIT_FAILURE, logFileDescriptor);
          }

          exitGracefully(EXIT_SUCCESS, logFileDescriptor);
        }

        // close read end of pipes for parent process
        if (close(poolPipe[i][0]) == -1)
        {
          GLOBAL_ERROR = FILE_CLOSE_ERROR;
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, logFileDescriptor);
        }
      }
    }

    // send matrix to next available children
    for (int i = 0; i < poolSize; i++)
    {
      if (sharedMemory[i] == WORKER_AVAILABLE)
      {
        if (printWorkerInfo(logFileDescriptor, matrix, childsY[i], sharedMemory[poolSize], poolSize, WORKER_OF_Y) == -1)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, logFileDescriptor);
        }

        if (writeToPipe(poolPipe[i][1], &matrix) == -1)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, logFileDescriptor);
        }
        break;
      }

      if (sharedMemory[poolSize] > poolSize)
      {
        if (printWorkerInfo(logFileDescriptor, matrix, childsY[i], sharedMemory[poolSize] - 1, poolSize, FORWARD_TO_SERVER_Z) == -1)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, logFileDescriptor);
        }

        if (writeToPipe(pipeBetweenServers[1], &matrix) == -1)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, logFileDescriptor);
        }
        sharedMemory[poolSize + 3] += 1;
        break;
      }
    }

    childrensInitialized = 1;

    if (matrix.data != NULL)
      free(matrix.data);
  }

  // kill all processes
  for (int i = 0; i < poolSize; i++)
  {
    kill(childsY[i], SIGINT);
  }
  // kill z
  kill(serverZID, SIGINT);

  int invertibleCount = sharedMemory[poolSize + 1];
  int notInvertibleCount = sharedMemory[poolSize + 2];
  int forwardedCount = sharedMemory[poolSize + 3];
  int totalHandled = invertibleCount + notInvertibleCount;
  char invertibleCountString[10];
  char notInvertibleCountString[10];
  char totalHandledString[10];
  char forwardedCountString[10];
  sprintf(invertibleCountString, "%d", invertibleCount);
  sprintf(notInvertibleCountString, "%d", notInvertibleCount);
  sprintf(totalHandledString, "%d", totalHandled);
  sprintf(forwardedCountString, "%d", forwardedCount);
  printMessageWithTime(logFileDescriptor, "SIGINT received, terminating Z and exiting server Y. Total requests handled: ");
  printMessage(logFileDescriptor, totalHandledString);
  printMessage(logFileDescriptor, " invertible count: ");
  printMessage(logFileDescriptor, invertibleCountString);
  printMessage(logFileDescriptor, " not invertible count: ");
  printMessage(logFileDescriptor, notInvertibleCountString);
  printMessage(logFileDescriptor, " forwarded to Z count: ");
  printMessage(logFileDescriptor, forwardedCountString);
  printMessage(logFileDescriptor, "\n");
  exitGracefully(EXIT_SUCCESS, logFileDescriptor);
}