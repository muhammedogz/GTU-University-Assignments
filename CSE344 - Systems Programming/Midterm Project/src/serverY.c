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

Matrix readMatrix(const char *file)
{
  Matrix matrix;
  matrix.data = NULL;

  // if not a server fifo not exit, create it here
  if (mkfifo(file, 0666) == -1)
  {
    if (errno != EEXIST)
    {
      GLOBAL_ERROR = FILE_OPEN_ERROR;
      return matrix;
    }
  }

  int serverFileDescriptor = open(file, O_RDONLY);
  if (serverFileDescriptor < 0)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return matrix;
  }

  if (read(serverFileDescriptor, &matrix, sizeof(Matrix)) == -1)
  {
    GLOBAL_ERROR = FILE_READ_ERROR;
    return matrix;
  }

  matrix.data = (int *)malloc(matrix.row * matrix.column * sizeof(int));
  if (matrix.data == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return matrix;
  }

  if (read(serverFileDescriptor, matrix.data, matrix.row * matrix.column * sizeof(int)) == -1)
  {
    GLOBAL_ERROR = FILE_READ_ERROR;
    return matrix;
  }

  if (close(serverFileDescriptor) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return matrix;
  }

  if (unlink(file) == -1)
  {
    GLOBAL_ERROR = FILE_UNLINK_ERROR;
    return matrix;
  }

  return matrix;
}

Matrix readFromPipe(const int pipeFd)
{
  Matrix matrix;
  matrix.data = NULL;
  if (read(pipeFd, &matrix, sizeof(Matrix)) == -1)
  {
    GLOBAL_ERROR = PIPE_READ_ERROR;
    return matrix;
  }

  matrix.data = malloc(matrix.row * matrix.column * sizeof(int));
  if (matrix.data == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return matrix;
  }

  if (read(pipeFd, matrix.data, matrix.row * matrix.column * sizeof(int)) == -1)
  {
    GLOBAL_ERROR = PIPE_READ_ERROR;
    return matrix;
  }

  return matrix;
}

int writeToPipe(const int pipeFd, const Matrix *matrix)
{
  // write matrix to file
  if (write(pipeFd, matrix, sizeof(Matrix)) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }

  // write matrix data to file
  if (write(pipeFd, matrix->data, sizeof(int) * matrix->row * matrix->column) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }

  return 1;
}

int runChildY(const int closePipe, const int readPipe, const int logFileDescriptor, const int time_v, int runStatus)
{
  if (close(closePipe) == -1)
  {
    GLOBAL_ERROR = PIPE_CLOSE_ERROR;
    return -1;
  }

  int temp = time_v;
  temp = 1;
  if (temp == 1)
    temp = 2;

  Matrix matrix;
  matrix.data = NULL;

  int printStatus = 0;

  char workerIDString[10];
  int workerID = getpid();
  sprintf(workerIDString, "%d", workerID);

  while (runStatus)
  {
    // sleep(time_v);

    matrix = readFromPipe(readPipe);
    if (matrix.data == NULL)
      return -1;

    char clientFifo[10];
    sprintf(clientFifo, "%d", matrix.id);

    int invertible = detectMatrixInvertible(matrix);

    const char *invertibleMsg = invertible != 0 ? "invertible" : "not invertible";

    if (writeToClientFifo(clientFifo, invertible) == -1)
      return -1;

    printStatus = printMessageWithTime(logFileDescriptor, "Worker PID#");
    printStatus = printMessage(logFileDescriptor, workerIDString);
    printStatus = printMessage(logFileDescriptor, " responding to client PID#");
    printStatus = printMessage(logFileDescriptor, clientFifo);
    printStatus = printMessage(logFileDescriptor, ": the matrix is ");
    printStatus = printMessage(logFileDescriptor, invertibleMsg);
    printStatus = printMessage(logFileDescriptor, "\n");

    if (printStatus == -1)
    {
      GLOBAL_ERROR = PRINT_ERROR;
      return -1;
    }

    if (matrix.data != NULL)
      free(matrix.data);
  }

  return 1;
}

int printWorkerInfo(const int fd, const Matrix matrix, const pid_t workerID, const int i, const int poolSize)
{
  char workerIDString[10];
  char clientID[10];
  char matrixSize[10];
  char currentI[10];
  char poolSizeString[10];
  sprintf(workerIDString, "%d", workerID);
  sprintf(clientID, "%d", matrix.id);
  sprintf(matrixSize, "%d", matrix.row);
  sprintf(currentI, "%d", i);
  sprintf(poolSizeString, "%d", poolSize);

  int printStatus = 0;

  printStatus = printMessageWithTime(fd, "Worker PID#");
  printStatus = printMessage(fd, workerIDString);
  printStatus = printMessage(fd, " is handling Client PID#");
  printStatus = printMessage(fd, clientID);
  printStatus = printMessage(fd, ", matrix size ");
  printStatus = printMessage(fd, matrixSize);
  printStatus = printMessage(fd, "x");
  printStatus = printMessage(fd, matrixSize);
  printStatus = printMessage(fd, ", pool busy ");
  printStatus = printMessage(fd, currentI);
  printStatus = printMessage(fd, "/");
  printStatus = printMessage(fd, poolSizeString);
  printStatus = printMessage(fd, "\n");

  if (printStatus == -1)
    GLOBAL_ERROR = PRINT_ERROR;

  return printStatus;
}

int writeToClientFifo(const char *clientFifo, const int invertible)
{
  if (mkfifo(clientFifo, 0666) == -1)
  {
    if (errno != EEXIST)
    {
      GLOBAL_ERROR = FILE_OPEN_ERROR;
      return -1;
    }
  }

  int fd = open(clientFifo, O_WRONLY);
  if (fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  if (write(fd, &invertible, sizeof(int)) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }

  if (close(fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return -1;
  }

  return 1;
}

int checkAlreadyRunning()
{
  // if not create temp path file
  int fd = open(TEMP_PATH, O_RDONLY | O_CREAT | O_EXCL, 0666);
  if (fd == -1)
  {
    GLOBAL_ERROR = ALREADY_RUNNING;
    return -1;
  }

  // close
  if (close(fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return -1;
  }

  return 0;
}

int removeTempPath()
{
  // remove file
  if (remove(TEMP_PATH) == -1)
  {
    GLOBAL_ERROR = FILE_READ_ERROR;
    return -1;
  }

  return 0;
}

void exitGracefully(int status, Matrix matrix)
{
  if (removeTempPath() == -1)
    printError(STDERR_FILENO, GLOBAL_ERROR);

  if (matrix.data != NULL)
    free(matrix.data);

  // wait all child processes to prevent zombie processes
  while (wait(NULL) != -1 || errno != ECHILD)
    ;

  exit(status);
}

void invalid_usage()
{
  write(STDERR_FILENO, "Usage: ./serverY -s <pathToServerFifo> -o <pathToLogFile> -p <poolSize> -r <poolSize2> -t <time_v>\n",
        strlen("Usage: ./serverY -s <pathToServerFifo> -o <pathToLogFile> -p <poolSize> -r <poolSize2> -t <time_v>\n"));
}