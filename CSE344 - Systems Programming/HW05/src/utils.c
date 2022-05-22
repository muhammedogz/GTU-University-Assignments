#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdarg.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <pthread.h>
#include <fcntl.h>
#include <complex.h>
#include <math.h>
#include "../include/utils.h"

// Global values to acces from all threads if necessary
static char *globalInputFilePath1, *globalInputFilePath2;
static char *globalOutputFilePath;
static int globalN, globalM;
static int globalReadCount;
static int globalThreadRunCount;
static int globalThreadRunCountForLast = 0;
static int globalCompletedThreadCount = 0;
static int globalRunningStatus = 1;

// Global matrixes
static int **globalMatrixA, **globalMatrixB, **globalMatrixC;
static double complex **globalMatrixFourier;

// Global mutex and condition variables
static pthread_mutex_t barrierMutex;
static pthread_cond_t barrierCond;

void sigint_handler(int signal)
{
  if (signal == SIGINT)
  {
    globalRunningStatus = 0;
    freeResources();
    dprintf(STDOUT_FILENO, "%s: SIGINT received. All resources are cleaned\n", getTime());
    exit(0);
  }
}

int detectArguments(int argc, char *argv[])
{

  if (argc != 11)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int i_found = 0;
  int j_found = 0;
  int o_found = 0;
  int n_found = 0;
  int m_found = 0;
  while ((opt = getopt(argc, argv, "i:j:o:n:m:")) != -1)
  {
    switch (opt)
    {
    case 'i':
      if (i_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalInputFilePath1 = optarg;
      i_found = 1;
      break;
    case 'j':
      if (j_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalInputFilePath2 = optarg;
      j_found = 1;
      break;
    case 'o':
      if (o_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalOutputFilePath = optarg;
      o_found = 1;
      break;
    case 'n':
      if (n_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalN = atoi(optarg);
      n_found = 1;
      break;
    case 'm':
      if (m_found)
      {
        GLOBAL_ERROR = INVALID_ARGUMENTS;
        return -1;
      }
      globalM = atoi(optarg);
      m_found = 1;
      break;
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  if (!i_found || !j_found || !o_found || !n_found || !m_found)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // N should bigger than 2 and M should be even
  if (globalN <= 2 || globalM % 2 != 0)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // check if files are existing
  if (access(globalInputFilePath1, F_OK) == -1 || access(globalInputFilePath2, F_OK) == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  globalReadCount = pow(2, globalN);
  globalThreadRunCount = globalReadCount / globalM;
  if (globalThreadRunCount == 0)
  {
    dprintf(STDOUT_FILENO, "%s: You entered very big M value. When you divide (2^n)/m. It gets 0,... . So, It assumed 1.\n", getTime());
    globalThreadRunCount = 1;
  }

  int tempTotal = globalThreadRunCount * globalM;

  if (tempTotal < globalReadCount)
  {
    dprintf(STDOUT_FILENO, "%s: Get a result m* (2^n)/m < (2*n). Last Thread will do the rest\n", getTime());
    globalThreadRunCountForLast = globalReadCount - tempTotal;
    dprintf(STDOUT_FILENO, "%s: Last Thread will do %d\n", getTime(), globalThreadRunCountForLast);
  }

  // print all
  // dprintf(STDOUT_FILENO, "%s: Input file 1: %s\n", getTime(), globalInputFilePath1);
  // dprintf(STDOUT_FILENO, "%s: Input file 2: %s\n", getTime(), globalInputFilePath2);
  // dprintf(STDOUT_FILENO, "%s: Output file: %s\n", getTime(), globalOutputFilePath);
  // dprintf(STDOUT_FILENO, "%s: N: %d\n", getTime(), globalN);
  // dprintf(STDOUT_FILENO, "%s: M: %d\n", getTime(), globalM);
  // dprintf(STDOUT_FILENO, "%s: NPow: %d\n", getTime(), globalReadCount);
  // dprintf(STDOUT_FILENO, "%s: Thread Run Count: %d\n", getTime(), globalThreadRunCount);

  return 1;
}

int readFiles()
{
  int fileFd1, fileFd2;
  if ((fileFd1 = open(globalInputFilePath1, O_RDONLY, S_IRUSR | S_IRGRP | S_IRGRP)) == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }
  if ((fileFd2 = open(globalInputFilePath2, O_RDONLY, S_IRUSR | S_IRGRP | S_IRGRP)) == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  char matrixAChar[globalReadCount][globalReadCount];
  char matrixBChar[globalReadCount][globalReadCount];

  for (int i = 0; i < globalReadCount; i++)
  {
    for (int j = 0; j < globalReadCount; j++)
    {
      if (read(fileFd1, &matrixAChar[i][j], sizeof(char)) == -1)
      {
        GLOBAL_ERROR = FILE_READ_ERROR;
        return -1;
      }
      if (read(fileFd2, &matrixBChar[i][j], sizeof(char)) == -1)
      {
        GLOBAL_ERROR = FILE_READ_ERROR;
        return -1;
      }
    }
  }

  close(fileFd1);
  close(fileFd2);

  // // print matrixAChar
  // dprintf(STDOUT_FILENO, "%s: Matrix A:\n", getTime());
  // for (int i = 0; i < globalReadCount; i++)
  // {
  //   for (int j = 0; j < globalReadCount; j++)
  //   {
  //     dprintf(STDOUT_FILENO, "%c", matrixAChar[i][j]);
  //   }
  //   dprintf(STDOUT_FILENO, "\n");
  // }

  // convert char to int and store in globalMatrixA and globalMatrixes
  globalMatrixA = (int **)malloc(sizeof(int *) * globalReadCount);
  globalMatrixB = (int **)malloc(sizeof(int *) * globalReadCount);
  globalMatrixC = (int **)malloc(sizeof(int *) * globalReadCount);

  // allocate complex double globalMatrixFourier
  globalMatrixFourier = (double complex **)malloc(sizeof(double complex *) * globalReadCount);
  for (int i = 0; i < globalReadCount; i++)
  {
    globalMatrixA[i] = (int *)malloc(sizeof(int) * globalReadCount);
    globalMatrixB[i] = (int *)malloc(sizeof(int) * globalReadCount);
    globalMatrixC[i] = (int *)malloc(sizeof(int) * globalReadCount);
    globalMatrixFourier[i] = (double complex *)malloc(sizeof(double complex) * globalReadCount);
  }

  for (int i = 0; i < globalReadCount; i++)
  {
    for (int j = 0; j < globalReadCount; j++)
    {
      globalMatrixA[i][j] = matrixAChar[i][j];
      globalMatrixB[i][j] = matrixBChar[i][j];
      globalMatrixC[i][j] = 0;
      globalMatrixFourier[i][j] = 0;
    }
  }

  // // print globalMatrixA
  // dprintf(STDOUT_FILENO, "%s: Matrix A:\n", getTime());
  // for (int i = 0; i < globalReadCount; i++)
  // {
  //   for (int j = 0; j < globalReadCount; j++)
  //   {
  //     dprintf(STDOUT_FILENO, "%d", globalMatrixA[i][j]);
  //   }
  //   dprintf(STDOUT_FILENO, "\n");
  // }

  dprintf(STDOUT_FILENO, "%s: Two matrices of size %dx%d have been read. The number of threads is %d\n", getTime(), globalReadCount, globalReadCount, globalM);

  return 0;
}

int init()
{
  if (readFiles() == -1)
  {
    return -1;
  }

  double totalStartTime = clock();

  if (pthread_mutex_init(&barrierMutex, NULL) != 0)
  {
    GLOBAL_ERROR = MUTEX_INIT_ERROR;
    return -1;
  }
  if (pthread_cond_init(&barrierCond, NULL) != 0)
  {
    GLOBAL_ERROR = COND_INIT_ERROR;
    return -1;
  }

  pthread_t threads[globalM];
  ThreadId thId[globalM];
  for (int i = 0; i < globalM; i++)
    thId[i].id = i;

  for (int i = 0; i < globalM; i++)
  {
    if (pthread_create(&threads[i], NULL, calcThreadFunction, (void *)&thId[i]) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_CREATION;
      return -1;
    }
  }

  for (int i = 0; i < globalM; i++)
  {
    if (pthread_join(threads[i], NULL) != 0)
    {
      GLOBAL_ERROR = INVALID_THREAD_JOIN;
      return -1;
    }
  }

  // write GlobalFourier to output file as csv
  int fileFd;
  if ((fileFd = open(globalOutputFilePath, O_WRONLY | O_CREAT, S_IWUSR | S_IRUSR | S_IRGRP | S_IRGRP)) == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return -1;
  }

  char temp[100];
  for (int i = 0; i < globalReadCount; i++)
  {
    for (int j = 0; j < globalReadCount; j++)
    {
      if (j != globalReadCount - 1)
        sprintf(temp, "%.3f + %.3fi,", crealf(globalMatrixFourier[i][j]), cimagf(globalMatrixFourier[i][j]));
      else
        sprintf(temp, "%.3f + %.3fi", crealf(globalMatrixFourier[i][j]), cimagf(globalMatrixFourier[i][j]));
      if (write(fileFd, temp, strlen(temp)) < 0)
      {
        GLOBAL_ERROR = FILE_WRITE_ERROR;
        return -1;
      }
    }
    if (write(fileFd, "\n", 1) < 0)
    {
      GLOBAL_ERROR = FILE_WRITE_ERROR;
      return -1;
    }
  }

  close(fileFd);

  dprintf(STDOUT_FILENO, "%s: The process has written the output file. The total time spent is %.3f seconds.\n", getTime(), (double)(clock() - totalStartTime) / CLOCKS_PER_SEC);

  return 0;
}

void *calcThreadFunction(void *arg)
{
  ThreadId *thId = (ThreadId *)arg;
  int threadId = thId->id;

  // Get time difference to find the time taken by each thread
  clock_t threadTimeStart = clock();
  int iterationCount = globalThreadRunCount;
  if (globalThreadRunCountForLast != 0 && threadId == globalM - 1)
    iterationCount = globalThreadRunCountForLast;

  for (int i = 0; i < iterationCount; i++)
  {
    int index = threadId * globalThreadRunCount + i;
    // dprintf(STDOUT_FILENO, "%s: Thread %d is running iteration %d\n", getTime(), threadId, index);
    for (int j = 0; j < globalReadCount; j++)
      for (int k = 0; k < globalReadCount; k++)
        globalMatrixC[j][index] += globalMatrixA[j][k] * globalMatrixB[k][index];
  }

  dprintf(STDOUT_FILENO, "%s: Thread %d calculated %d columns of matrix C\n", getTime(), threadId, iterationCount);

  // Barrier
  pthread_mutex_lock(&barrierMutex);
  ++globalCompletedThreadCount;
  if (globalCompletedThreadCount == globalM)
  {
    if (pthread_cond_broadcast(&barrierCond) != 0)
    {
      GLOBAL_ERROR = COND_BROADCAST_ERROR;
      return NULL;
    }
  }
  else
  {
    if (pthread_cond_wait(&barrierCond, &barrierMutex) != 0)
    {
      GLOBAL_ERROR = COND_WAIT_ERROR;
      return NULL;
    }
  }

  pthread_mutex_unlock(&barrierMutex);

  dprintf(STDOUT_FILENO, "%s: Thread %d has reached the rendezvous point in %.5f seconds\n", getTime(), threadId, (double)(clock() - threadTimeStart) / CLOCKS_PER_SEC);

  threadTimeStart = clock();

  double doubleGlobalReadCount = globalReadCount * 1.0;

  for (int i = 0; i < iterationCount; i++)
  {
    int index = threadId * globalThreadRunCount + i;
    for (int j = 0; j < globalReadCount; j++)
    {
      double complex fourierValue = 0 + 0 * I;
      for (int k = 0; k < globalReadCount; k++)
      {
        for (int l = 0; l < globalReadCount; l++)
        {
          double radian = (2 * M_PI * ((j * k + index * l) / doubleGlobalReadCount));
          fourierValue += (globalMatrixC[k][l] * ccos(radian)) - I * (globalMatrixC[k][l] * csin(radian));
        }
      }
      globalMatrixFourier[j][index] = fourierValue;
    }
  }

  dprintf(STDOUT_FILENO, "%s: Thread %d has finished the second part in %.3f seconds. \n", getTime(), threadId, (double)(clock() - threadTimeStart) / CLOCKS_PER_SEC);

  pthread_exit(NULL);
}

int freeResources()
{

  // free globalMatrixes
  for (int i = 0; i < globalReadCount; i++)
  {
    free(globalMatrixA[i]);
    free(globalMatrixB[i]);
    free(globalMatrixC[i]);
    free(globalMatrixFourier[i]);
  }
  free(globalMatrixA);
  free(globalMatrixB);
  free(globalMatrixC);
  free(globalMatrixFourier);

  return 0;
}

char *getTime()
{
  time_t rawtime;
  struct tm *timeinfo;
  time(&rawtime);
  timeinfo = localtime(&rawtime);
  char *timestamp = asctime(timeinfo);
  timestamp[strlen(timestamp) - 1] = '\0';

  return timestamp;
}

void printError(const int fd)
{
  char *error_message = NULL;
  int show_perror = 1;
  switch (GLOBAL_ERROR)
  {
  case INVALID_MALLOC:
    error_message = "Invalid malloc";
    break;
  case INVALID_ARGUMENTS:
    error_message = "Invalid arguments";
    show_perror = 0;
    printUsage();
    break;
  case FILE_OPEN_ERROR:
    error_message = "File open error";
    break;
  case FILE_WRITE_ERROR:
    error_message = "File write error";
    break;
  case FILE_READ_ERROR:
    error_message = "File read error";
    break;
  case FILE_LOCK_ERROR:
    error_message = "File lock error";
    break;
  case FILE_UNLOCK_ERROR:
    error_message = "File unlock error";
    break;
  case FILE_CLOSE_ERROR:
    error_message = "File close error";
    break;
  case FILE_UNLINK_ERROR:
    error_message = "File unlink error";
    break;
  case ALREADY_RUNNING:
    error_message = "Server already running, If it is not running, delete serverYTemp file.";
    break;
  case FILE_SEEK_ERROR:
    error_message = "File seek error";
    break;
  case INVALID_EXECVE:
    error_message = "Invalid execve";
    break;
  case INVALID_FORK:
    error_message = "Invalid fork";
    break;
  case PIPE_CREATION_ERROR:
    error_message = "Pipe creation error";
    break;
  case FORK_ERROR:
    error_message = "Fork error";
    break;
  case PIPE_READ_ERROR:
    error_message = "Pipe read error";
    break;
  case PIPE_CLOSE_ERROR:
    error_message = "Pipe close error";
    break;
  case PIPE_WRITE_ERROR:
    error_message = "Pipe write error";
    break;
  case INVALID_WAIT:
    error_message = "Invalid wait";
    show_perror = 0;
    break;
  case INVALID_MATRIX:
    error_message = "Invalid matrix. Matrix should be square (NxN). N >= 2";
    show_perror = 0;
    break;
  case PRINT_ERROR:
    error_message = "Print error";
    break;
  case FIRST_INITIALIZE_SERVER:
    error_message = "First start serverY. serverY is not working now.";
    show_perror = 0;
    break;
  case FILE_TRUNCATE_ERROR:
    error_message = "File truncate error";
    break;
  case FILE_MMAP_ERROR:
    error_message = "File mmap error";
    break;
  case UNLINK_ERROR:
    error_message = "Unlink error";
    break;
  case SEMAPHORE_OPEN_ERROR:
    error_message = "Semaphore open error";
    break;
  case SEMAPHORE_CLOSE_ERROR:
    error_message = "Semaphore close error";
    break;
  case SEMAPHORE_UNLINK_ERROR:
    error_message = "Semaphore unlink error";
    break;
  case WAITPID_ERROR:
    error_message = "Waitpid error";
    break;
  case INVALID_THREAD_CREATION:
    error_message = "Invalid thread creation";
    break;
  case INVALID_THREAD_JOIN:
    error_message = "Invalid thread join";
    break;
  case INVALID_THREAD_DETACH:
    error_message = "Invalid thread detach";
    break;
  case SEMAPHORE_GET_ERROR:
    error_message = "Semaphore get error";
    break;
  case SEMAPHORE_OPERATION_FAILED:
    error_message = "Semaphore operation failed";
    break;
  default:
    error_message = "Unknown error";
    dprintf(STDERR_FILENO, "Error Number: %d\n", GLOBAL_ERROR);
    break;
  }

  if (show_perror)
    perror(error_message);

  dprintf(fd, "%s\n", error_message);

  // terminate
  // exit(EXIT_FAILURE);
}

void printUsage()
{
  dprintf(STDOUT_FILENO, "Usage: hw5 \n");
  dprintf(STDOUT_FILENO, "  -i filePath1\t\t Filepath of the first file\n");
  dprintf(STDOUT_FILENO, "  -j filePath2\t\t Filepath of the second file\n");
  dprintf(STDOUT_FILENO, "  -o output\t\t Filepath of the output file\n");
  dprintf(STDOUT_FILENO, "  -n NUMBER\t\t Number of times to read input and n > 2\n");
  dprintf(STDOUT_FILENO, "  -m NUMBER\t\t Number of threads and m = 2k where k >= 1\n");
}

off_t getFileSize(const char *filename)
{
  struct stat st;

  if (stat(filename, &st) == 0)
    return st.st_size;

  return -1;
}