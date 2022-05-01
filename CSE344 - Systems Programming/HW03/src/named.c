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
#include <semaphore.h>
#include "../include/common.h"
#include "../include/named.h"

// m = Milk
// f = Flour
// w = Walnut
// s = Sugar

static sem_t *pusherWorking, *m, *f, *w, *s;
static sem_t *wf, *ws, *wm, *ms, *mf, *fs;

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

// initialize semaphores with names
int initializeSemaphores(char **names)
{
  if ((pusherWorking = sem_open(names[0], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((m = sem_open(names[1], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((f = sem_open(names[2], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((w = sem_open(names[3], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((s = sem_open(names[4], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((wf = sem_open(names[5], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((ws = sem_open(names[6], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((wm = sem_open(names[7], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((ms = sem_open(names[8], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((mf = sem_open(names[9], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((fs = sem_open(names[10], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }

  return 1;
}

int freeSemaphores(char **names)
{
  for (int i = 0; i < SEMAPHORE_COUNT; i++)
  {
    if (sem_unlink(names[i]) == -1)
    {
      GLOBAL_ERROR = SEMAPHORE_UNLINK_ERROR;
      return -1;
    }
  }
  // close semaphores
  if (sem_close(pusherWorking) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(m) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(f) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(w) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(s) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(wf) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(ws) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(wm) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(ms) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(mf) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(fs) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  return 1;
}

void *createSharedMemory(char ingredient1, char ingredient2)
{
  // create shared memory for 2 size char array
  char ingredients[2];
  ingredients[0] = ingredient1;
  ingredients[1] = ingredient2;

  int shm_fd = shm_open(SHARED_MEMORY_NAME, O_RDWR | O_CREAT, 0777);
  if (shm_fd == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  if (ftruncate(shm_fd, sizeof(char) * 2) == -1)
  {
    GLOBAL_ERROR = FILE_TRUNCATE_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(char) * 2, PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  memcpy(sharedMemory, ingredients, sizeof(char) * 2);

  if (close(shm_fd) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

int runNamed(WholesalerBag wholesalerBag, char **names)
{
  if (initializeSemaphores(names) == -1)
    return -1;

  void *sharedIngredientArray = createSharedMemory(wholesalerBag.ingredients[0].ingredient1, wholesalerBag.ingredients[0].ingredient2);
  if (sharedIngredientArray == NULL)
    return -1;

  char *sharedChar = (char *)sharedIngredientArray;
  dprintf(STDOUT_FILENO, "Shared char: %c %c\n", sharedChar[0], sharedChar[1]);

  if (freeSemaphores(names) == -1)
    return -1;
  shm_unlink(SHARED_MEMORY_NAME);

  return 1;
}
