#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
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

char *readFile(char *fileName, int *fileSize)
{

  int fileDescriptor = 0;
  char *fileContent = NULL;
  int fileSizeTemp = 0;
  int readSize = 0;
  int readCount = 0;
  int readTotal = 0;
  char readBuffer[BUFFER_SIZE];

  if ((fileDescriptor = open(fileName, O_RDONLY, fileSize)) < 0)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  fileSizeTemp = lseek(fileDescriptor, 0, SEEK_END);
  if (fileSizeTemp < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }

  fileContent = (char *)malloc(fileSizeTemp + 1);
  if (fileContent == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  if (lseek(fileDescriptor, 0, SEEK_SET) < 0)
  {
    GLOBAL_ERROR = FILE_SEEK_ERROR;
    return NULL;
  }
  while ((readSize = read(fileDescriptor, readBuffer, BUFFER_SIZE)))
  {
    if (readSize < 0)
    {
      GLOBAL_ERROR = FILE_READ_ERROR;
      return NULL;
    }
    readCount = 0;
    while (readCount < readSize)
    {
      fileContent[readTotal] = readBuffer[readCount];
      readCount++;
      readTotal++;
    }
  }

  fileContent[readTotal] = '\0';

  *fileSize = readTotal;
  // close
  int closeRes = close(fileDescriptor);
  if (closeRes < 0)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return fileContent;
}

Matrix *convertToMatrix(const char *content, const int contentSize)
{
  Matrix *matrix = malloc(sizeof(Matrix));
  if (matrix == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }
  matrix->column = 0;
  matrix->row = 0;
  matrix->data = NULL;

  // detect column and row counts
  for (int i = 0; i < contentSize; i++)
  {
    if (content[i] == '\n')
      matrix->column++;
    else if (content[i] == ',' && matrix->column == 0)
      matrix->row++;
  }
  matrix->row++;
  matrix->column++;

  // if not a square matrix
  if (matrix->row != matrix->column)
  {
    GLOBAL_ERROR = INVALID_MATRIX;
    return NULL;
  }

  // allocate memory for data
  matrix->data = malloc(sizeof(int) * matrix->row * matrix->column);
  if (matrix->data == NULL)
  {
    GLOBAL_ERROR = INVALID_MALLOC;
    return NULL;
  }

  int dataIndex = 0;
  int tempNum = 0;
  for (int i = 0; i < contentSize; i++)
  {

    if (content[i] == '\n' || content[i] == ',')
    {
      matrix->data[dataIndex] = tempNum;
      tempNum = 0;
      dataIndex++;
    }
    else
      tempNum = tempNum * 10 + (content[i] - '0');
  }
  matrix->data[dataIndex] = tempNum;

  return matrix;
}

void freeAndExit(char *content, Matrix *matrix, int exit_status)
{
  if (content != NULL)
    free(content);
  if (matrix != NULL)
  {
    if (matrix->data != NULL)
      free(matrix->data);
    free(matrix);
  }
  exit(exit_status);
}

void invalidUsage()
{
  write(STDERR_FILENO, "Usage: ./client -s <server fifo path> -o <data file path>\n",
        strlen("Usage: ./client -s <server fifo path> -o <data file path>\n"));
}
