#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/shm.h>
#include <sys/mman.h>
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

pid_t createServerZ(const int pipeRead, const int pipeWrite, const int fd, const int poolSize, const int poolSize2, const int time_v)
{
  pid_t serverZID = fork();
  if (serverZID == 0)
  {
    // serverZ
    if (close(pipeWrite) == -1) // close write end
    {
      GLOBAL_ERROR = PIPE_CLOSE_ERROR;
      printError(fd, GLOBAL_ERROR);
      exit(EXIT_FAILURE);
    }
    serverZ(pipeRead, fd, poolSize, poolSize2, time_v);
    exit(EXIT_SUCCESS); // if serverZ finished
  }
  else if (serverZID > 0)
  {
    // serverY
    if (close(pipeRead) == -1) // close read end
    {
      GLOBAL_ERROR = PIPE_CLOSE_ERROR;
      printError(fd, GLOBAL_ERROR);
      return -1;
    }
  }
  else
  {
    GLOBAL_ERROR = FORK_ERROR;
    printError(fd, GLOBAL_ERROR);
    return -1;
  }

  return serverZID;
}

Matrix readMatrix(const char *file)
{
  Matrix matrix;
  matrix.data = NULL;

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

int globalReadPipe = 0;
void sigint_childY_handler(int signal)
{
  if (signal == SIGINT)
  {

    if (globalReadPipe != 0)
    {
      close(globalReadPipe);
      globalReadPipe = 0;
    }

    exit(EXIT_SUCCESS);
  }
}

int runChildY(const int closePipe, const int readPipe, const int logFileDescriptor, const int turn, const int time_v, const int poolSize, int runStatus)
{
  signal(SIGINT, sigint_childY_handler);
  globalReadPipe = readPipe;
  if (close(closePipe) == -1) // close write end of pipe
  {
    GLOBAL_ERROR = PIPE_CLOSE_ERROR;
    return -1;
  }

  Matrix matrix;
  matrix.data = NULL;

  int printStatus = 0;

  char workerIDString[10];
  int workerID = getpid();
  sprintf(workerIDString, "%d", workerID);

  // use getSharedMemoryClientY
  int *workerAvailabe = (int *)getSharedMemoryChildY(poolSize);

  while (runStatus)
  {
    matrix = readFromPipe(readPipe);
    if (matrix.data == NULL)
    {
      printMessageWithTime(logFileDescriptor, "Child Read from pipe error\n");
    }

    workerAvailabe[turn] = WORKER_BUSY;
    workerAvailabe[poolSize] += 1;
    sleep(time_v);

    char clientFifo[10];
    sprintf(clientFifo, "%d", matrix.id);

    int invertible = detectMatrixInvertible(matrix);

    int completedRequest = 1;
    if (writeToClientFifo(clientFifo, invertible) == -1)
    {
      // if client is down
      completedRequest = 0;
    }

    if (completedRequest)
    {
      const char *invertibleMsg = invertible != 0 ? "invertible" : "not invertible";
      if (invertible != 0)
        workerAvailabe[poolSize + 1] += 1; // invertible count
      else
        workerAvailabe[poolSize + 2] += 1; // not invertible count

      printStatus = printMessageWithTime(logFileDescriptor, "Y: Worker PID#");
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
    }

    if (matrix.data != NULL)
      free(matrix.data);

    workerAvailabe[turn] = WORKER_AVAILABLE;
    workerAvailabe[poolSize] -= 1;
  }

  return 1;
}

int runChildZ(const int logFileDescriptor, const int turn, const int time_v, const int poolSize, const int poolSize2)
{
  // signal(SIGINT, sigint_childY_handler);

  Matrix *matrix = NULL;

  int printStatus = 0;

  char workerIDString[10];
  int workerID = getpid();
  sprintf(workerIDString, "%d", workerID);

  int *incrementInvertibleValues = (int *)getSharedMemoryChildY(poolSize);

  int *workersAvailabeZ = (int *)getSharedMemoryChildZ(poolSize2);

  while (1)
  {
    kill(getpid(), SIGSTOP); // pause, till signal SIGCONT is received

    workersAvailabeZ[turn] = WORKER_BUSY;
    workersAvailabeZ[poolSize2] += 1; // increment busy workers count

    matrix = (Matrix *)getSharedMemoryMatrix();
    if (matrix == NULL)
    {
      printMessageWithTime(logFileDescriptor, "Child Z: Error getting matrix from shared memory\n");
    }

    int *data = (int *)getSharedMemoryMatrixData(*matrix);
    if (data == NULL)
    {
      printError(logFileDescriptor, GLOBAL_ERROR);
    }

    matrix->data = malloc(matrix->row * matrix->column * sizeof(int));
    if (matrix->data == NULL)
    {
      printMessageWithTime(logFileDescriptor, "Child Z: Error allocating memory for matrix\n");
      GLOBAL_ERROR = INVALID_MALLOC;
    }

    for (int i = 0; i < matrix->row * matrix->column; i++)
      matrix->data[i] = data[i];

    char clientFifo[10];
    sprintf(clientFifo, "%d", matrix->id);

    sleep(time_v);

    const int invertible = detectMatrixInvertible(*matrix);
    int completedRequest = 1;
    if (writeToClientFifo(clientFifo, invertible) == -1)
    {
      printError(logFileDescriptor, GLOBAL_ERROR);
      completedRequest = 0;
    }

    if (completedRequest)
    {
      const char *invertibleMsg = invertible != 0 ? "invertible" : "not invertible";

      if (invertible != 0)
      {
        workersAvailabeZ[poolSize2 + 1] += 1;         // invertible count
        incrementInvertibleValues[poolSize + 1] += 1; // invertible count
      }
      else
      {
        workersAvailabeZ[poolSize2 + 2] += 1;         // invertible count
        incrementInvertibleValues[poolSize + 2] += 1; // not invertible count
      }

      printStatus = printMessageWithTime(logFileDescriptor, "Z: Worker PID#");
      printStatus = printMessage(logFileDescriptor, workerIDString);
      printStatus = printMessage(logFileDescriptor, " responding to client PID#");
      printStatus = printMessage(logFileDescriptor, clientFifo);
      printStatus = printMessage(logFileDescriptor, ": the matrix is ");
      printStatus = printMessage(logFileDescriptor, invertibleMsg);
      printStatus = printMessage(logFileDescriptor, "\n");
    }

    workersAvailabeZ[turn] = WORKER_AVAILABLE;
    workersAvailabeZ[poolSize2] -= 1;

    if (printStatus == -1)
    {
      GLOBAL_ERROR = PRINT_ERROR;
      return -1;
    }

    if (matrix->data != NULL)
      free(matrix->data);
  }

  return 1;
}

int globalServerZReadPipe = -1;
void sigint_handler_serverZ(int signal)
{
  if (signal == SIGINT)
  {
    if (globalServerZReadPipe != -1)
      close(globalServerZReadPipe);
  }
}

void serverZ(const int pipeFd, const int logFileDescriptor, const int poolSize, const int poolSize2, const int time_v)
{
  globalServerZReadPipe = pipeFd;
  signal(SIGINT, sigint_handler_serverZ);
  Matrix matrix;
  matrix.data = NULL;

  pid_t childsZ[poolSize2];

  int childsCreated = 0;

  int *sharedMemory = (int *)createSharedMemoryChildZ(poolSize2);

  while (1)
  {
    matrix = readFromPipe(pipeFd);
    if (matrix.data == NULL)
    {
      break;
    }

    if (childsCreated == 0)
    {
      for (int i = 0; i < poolSize2; i++)
      {
        childsZ[i] = fork();
        if (childsZ[i] == -1)
        {
          GLOBAL_ERROR = FORK_ERROR;
          return;
        }
        else if (childsZ[i] == 0)
        {
          runChildZ(logFileDescriptor, i, time_v, poolSize, poolSize2);
          exit(EXIT_SUCCESS);
        }
      }
    }

    // find next available worker
    for (int i = 0; i < poolSize2; i++)
    {
      if (sharedMemory[i] == WORKER_AVAILABLE)
      {
        if (printWorkerInfo(logFileDescriptor, matrix, childsZ[i], sharedMemory[poolSize2], poolSize2, WORKER_OF_Z) == -1)
        {
          GLOBAL_ERROR = PRINT_ERROR;
          return;
        }

        if ((createSharedMemoryMatrix(matrix)) == NULL)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          return;
        }
        if (createSharedMemoryMatrixData(matrix) == NULL)
        {
          printError(logFileDescriptor, GLOBAL_ERROR);
          return;
        }
        kill(childsZ[i], SIGCONT);

        break;
      }
    }

    childsCreated = 1;
  }

  int invertibleCount = sharedMemory[poolSize2 + 1];
  int notInvertibleCount = sharedMemory[poolSize2 + 2];
  int totalHandled = invertibleCount + notInvertibleCount;
  char invertibleCountString[10];
  char notInvertibleCountString[10];
  char totalHandledString[10];
  sprintf(invertibleCountString, "%d", invertibleCount);
  sprintf(notInvertibleCountString, "%d", notInvertibleCount);
  sprintf(totalHandledString, "%d", totalHandled);
  printMessageWithTime(logFileDescriptor, "Z: SIGINT received, exiting Server Z. Total requests handled: ");
  printMessage(logFileDescriptor, totalHandledString);
  printMessage(logFileDescriptor, " invertible count: ");
  printMessage(logFileDescriptor, invertibleCountString);
  printMessage(logFileDescriptor, " not invertible count: ");
  printMessage(logFileDescriptor, notInvertibleCountString);
  printMessage(logFileDescriptor, "\n");
}

int printWorkerInfo(const int fd, const Matrix matrix, const pid_t workerID, const int i, const int poolSize, const int type)
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

  if (type != FORWARD_TO_SERVER_Z)
  {
    if (type == 0)
      printStatus = printMessageWithTime(fd, "Y: Worker PID#");
    else if (type == 1)
      printStatus = printMessageWithTime(fd, "Z: Worker PID#");
    printStatus = printMessage(fd, workerIDString);
    printStatus = printMessage(fd, " is handling Client PID#");
    printStatus = printMessage(fd, clientID);
  }
  else if (type == FORWARD_TO_SERVER_Z)
  {
    printMessageWithTime(fd, "Forwarding request of client PID#");
    printMessage(fd, clientID);
    printMessage(fd, " to serverZ");
  }

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

void *createSharedMemoryChildY(const int poolSize)
{
  int newSize = poolSize + 4;
  int arr[poolSize];

  // fill array with WORKER_AVAILABLE
  for (int i = 0; i < poolSize; i++)
    arr[i] = WORKER_AVAILABLE;

  arr[poolSize] = 1;     // available workers count
  arr[poolSize + 1] = 0; // invertible matrix count
  arr[poolSize + 2] = 0; // not invertible matrix count
  arr[poolSize + 3] = 0; // forwarded matrix count to ServerZ

  int shm_fd = shm_open("childY", O_RDWR | O_CREAT, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  if (ftruncate(shm_fd, sizeof(int) * newSize) == -1)
  {
    GLOBAL_ERROR = FILE_TRUNCATE_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(int) * newSize, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  memcpy(sharedMemory, arr, sizeof(int) * newSize);

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *createSharedMemoryChildZ(const int poolSize)
{
  int newSize = poolSize + 3;
  int arr[poolSize];

  // fill array with WORKER_AVAILABLE
  for (int i = 0; i < poolSize; i++)
    arr[i] = WORKER_AVAILABLE;

  arr[poolSize] = 1;     // available workers count
  arr[poolSize + 1] = 0; // invertible matrix count
  arr[poolSize + 2] = 0; // not invertible matrix count

  int shm_fd = shm_open("childZ", O_RDWR | O_CREAT, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  if (ftruncate(shm_fd, sizeof(int) * newSize) == -1)
  {
    GLOBAL_ERROR = FILE_TRUNCATE_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(int) * newSize, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  memcpy(sharedMemory, arr, sizeof(int) * newSize);

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *createSharedMemoryMatrix(const Matrix matrix)
{
  int shm_fd = shm_open("sharedMatrix", O_RDWR | O_CREAT, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  if (ftruncate(shm_fd, sizeof(Matrix)) == -1)
  {
    GLOBAL_ERROR = FILE_TRUNCATE_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(Matrix), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  memcpy(sharedMemory, &matrix, sizeof(Matrix));
  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *createSharedMemoryMatrixData(const Matrix matrix)
{

  int data[matrix.column * matrix.row];
  for (int i = 0; i < matrix.column * matrix.row; i++)
    data[i] = matrix.data[i];
  int shm_fd = shm_open("sharedMatrixData", O_RDWR | O_CREAT, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  if (ftruncate(shm_fd, sizeof(int) * matrix.column * matrix.row) == -1)
  {
    GLOBAL_ERROR = FILE_TRUNCATE_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(int) * matrix.column * matrix.row, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  memcpy(sharedMemory, data, sizeof(int) * matrix.column * matrix.row);
  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *getSharedMemoryChildY(const int poolSize)
{
  int newSize = poolSize + 4;
  int shm_fd = shm_open("childY", O_RDWR, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(int) * newSize, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *getSharedMemoryChildZ(const int poolSize)
{
  int newSize = poolSize + 1;
  int shm_fd = shm_open("childZ", O_RDWR, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(int) * newSize, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *getSharedMemoryMatrix()
{
  int shm_fd = shm_open("sharedMatrix", O_RDWR, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(Matrix), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *getSharedMemoryMatrixData(const Matrix matrix)
{
  int shm_fd = shm_open("sharedMatrixData", O_RDWR, 0666);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(int) * matrix.column * matrix.row, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }
  return sharedMemory;
}

void exitGracefully(const int status, const int fd)
{
  if (removeTempPath() == -1)
    printError(fd, GLOBAL_ERROR);

  // wait all child processes to prevent zombie processes
  while (wait(NULL) != -1 || errno != ECHILD)
    ;

  // unlink shared memory
  shm_unlink("childY");
  shm_unlink("childZ");
  shm_unlink("sharedMatrix");
  shm_unlink("sharedMatrixData");

  exit(status);
}

int writeMatrix(const char *path, const Matrix *matrix)
{

  int fd = open(path, O_WRONLY);
  if (fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  // write matrix to file
  if (write(fd, matrix, sizeof(Matrix)) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }
  // write matrix data to file
  if (write(fd, matrix->data, sizeof(int) * matrix->row * matrix->column) == -1)
  {
    GLOBAL_ERROR = FILE_WRITE_ERROR;
    return -1;
  }

  // close
  if (close(fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return -1;
  }

  // if (unlink(path) == -1)
  // {
  //   GLOBAL_ERROR = FILE_UNLINK_ERROR;
  //   return -1;
  // }

  return 0;
}

void invalid_usage()
{
  write(STDERR_FILENO, "Usage: ./serverY -s <pathToServerFifo> -o <pathToLogFile> -p <poolSize> -r <poolSize2> -t <time_v>\n",
        strlen("Usage: ./serverY -s <pathToServerFifo> -o <pathToLogFile> -p <poolSize> -r <poolSize2> -t <time_v>\n"));
}