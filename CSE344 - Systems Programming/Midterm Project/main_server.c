#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>
#include "include/common.h"
#include "include/serverY.h"

int main(int argc, char *argv[])
{

  char *pathToServerFifo = NULL;
  char *pathToLogFile = NULL;
  int poolSize = 0;
  int poolSize2 = 0;
  int time_v = 0;

  int logFileDescriptor = 0;

  Matrix matrixTemp;
  matrixTemp.data = NULL;

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
    exitGracefully(EXIT_FAILURE, matrixTemp);
  }

  // open log file
  // logFileDescriptor =  open(pathToLogFile, O_WRONLY | O_APPEND | O_CREAT, 0666);
  logFileDescriptor = 1;
  if (logFileDescriptor == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    printError(logFileDescriptor, GLOBAL_ERROR);
    exitGracefully(EXIT_FAILURE, matrixTemp);
  }

  printMessageWithTime(logFileDescriptor, "ServerY started\n");
  int *sharedMemory = (int *)createSharedMemoryChildY(poolSize);

  int poolPipe[poolSize][2];
  for (int i = 0; i < poolSize; i++)
  {
    if (pipe(poolPipe[i]) == -1)
    {
      GLOBAL_ERROR = PIPE_CREATION_ERROR;
      printError(logFileDescriptor, GLOBAL_ERROR);
      exitGracefully(EXIT_FAILURE, matrixTemp);
    }
  }

  pid_t childsY[poolSize];

  int childrensInitialized = 0;

  while (1)
  {
    Matrix matrix;
    matrix.data = NULL;

    // reads matrix from server fifo
    matrix = readMatrix(pathToServerFifo);
    if (matrix.data == NULL)
    {
      printError(logFileDescriptor, GLOBAL_ERROR);
      exitGracefully(EXIT_FAILURE, matrix);
    }

    int runningState = matrix.runningState;
    if (runningState == 0)
      break;

    if (childrensInitialized == 0)
    {
      for (int i = 0; i < poolSize; i++)
      {
        childsY[i] = fork();
        if (childsY[i] == -1)
        {
          GLOBAL_ERROR = FORK_ERROR;
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, matrix);
        }
        else if (childsY[i] == 0) // child process
        {
          if (runChildY(poolPipe[i][1], poolPipe[i][0], logFileDescriptor, i, time_v, poolSize, 1) == -1)
          {
            printError(logFileDescriptor, GLOBAL_ERROR);
            exitGracefully(EXIT_FAILURE, matrix);
          }

          exitGracefully(EXIT_SUCCESS, matrix);
        }

        if (close(poolPipe[i][0]) == -1)
        {
          GLOBAL_ERROR = FILE_CLOSE_ERROR;
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, matrix);
        }
      }
    }

    // send matrix to next available children
    for (int i = 0; i < poolSize; i++)
    {
      if (sharedMemory[i] == WORKER_AVAILABLE)
      {
        if (printWorkerInfo(logFileDescriptor, matrix, childsY[i], sharedMemory[poolSize], poolSize) == -1)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, matrix);
        }

        if (writeToPipe(poolPipe[i][1], &matrix) == -1)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          exitGracefully(EXIT_FAILURE, matrix);
        }
        break;
      }

      if (sharedMemory[poolSize] > poolSize)
      {
        printMessageWithTime(logFileDescriptor, "No more workers available\n");
      }
    }

    childrensInitialized = 1;
  }

  // kill all processes
  for (int i = 0; i < poolSize; i++)
  {
    kill(childsY[i], SIGINT);
  }

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
  printMessage(logFileDescriptor, " forwarded count: ");
  printMessage(logFileDescriptor, forwardedCountString);
  printMessage(logFileDescriptor, "\n");
  exitGracefully(EXIT_SUCCESS, matrixTemp);
}