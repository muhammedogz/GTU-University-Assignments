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
#include "../include/unnamed.h"

int totalDesertOfChef = 0;

void sigintHandlerForChief(int signum)
{
  if (signum == SIGINT)
  {
    exit(totalDesertOfChef);
  }
}

int detectArguments(int argc, char *argv[], char **inputFilePath)
{
  if (argc != 3)
  {
    GLOBAL_ERROR = INVALID_ARGUMENTS;
    return -1;
  }

  // use getopt to parse arguments
  int opt;
  int i_found = 0;
  while ((opt = getopt(argc, argv, "i:")) != -1)
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
    default:
      GLOBAL_ERROR = INVALID_ARGUMENTS;
      return -1;
    }
  }

  return 1;
}

void *createUnnamedSharedMemory(char *sharedName, char ingredient1, char ingredient2)
{
  UnnamedShared unnamedShared;
  unnamedShared.ingredient1 = ingredient1;
  unnamedShared.ingredient2 = ingredient2;

  int sharedMemoryId = shm_open(sharedName, O_CREAT | O_RDWR, 0777);
  if (sharedMemoryId == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  if (ftruncate(sharedMemoryId, sizeof(UnnamedShared)) == -1)
  {
    GLOBAL_ERROR = FILE_TRUNCATE_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(UnnamedShared), PROT_READ | PROT_WRITE, MAP_SHARED, sharedMemoryId, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  memcpy(sharedMemory, &unnamedShared, sizeof(UnnamedShared));

  if (close(sharedMemoryId) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }

  return sharedMemory;
}

void *getUnnamedSharedMemory(char *sharedName)
{
  int sharedMemoryId = shm_open(sharedName, O_RDWR, 0777);
  if (sharedMemoryId == -1)
  {
    GLOBAL_ERROR = FILE_OPEN_ERROR;
    return NULL;
  }

  void *sharedMemory = mmap(NULL, sizeof(UnnamedShared), PROT_READ | PROT_WRITE, MAP_SHARED, sharedMemoryId, 0);
  if (sharedMemory == MAP_FAILED)
  {
    GLOBAL_ERROR = FILE_MMAP_ERROR;
    return NULL;
  }

  if (close(sharedMemoryId) == -1)
  {
    GLOBAL_ERROR = FILE_CLOSE_ERROR;
    return NULL;
  }
  return sharedMemory;
}

int initializeSemaphores(UnnamedShared *unnamedShared)
{

  if ((sem_init(&unnamedShared->pusherWorking, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->chefDelivered, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->m, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->f, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->w, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->s, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->ws, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->fw, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->sf, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->mf, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->mw, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }
  if ((sem_init(&unnamedShared->sm, 1, 0)) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_INIT_FAILED;
    return -1;
  }

  return 1;
}

int runUnNamed(WholesalerBag wholesalerBag)
{
  void *sharedIngredientArray = createSharedMemory(SHARED_MEMORY_UNNAMED_NAME, wholesalerBag.ingredients[0].ingredient1, wholesalerBag.ingredients[0].ingredient2);
  if (sharedIngredientArray == NULL)
    return -1;

  UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredientArray;

  if (initializeSemaphores(unnamedShared) == -1)
    return -1;

  pid_t pids[CHIEF_COUNT + PUSHER_COUNT];
  pid_t pid;

  // create pushers
  for (int i = 0; i < PUSHER_COUNT; i++)
  {
    pid = fork();
    if (pid == -1)
    {
      GLOBAL_ERROR = FORK_ERROR;
      return -1;
    }
    else if (pid == 0)
    {
      // child processes
      if (i == 0)
        pusherM();
      else if (i == 1)
        pusherF();
      else if (i == 2)
        pusherW();
      else if (i == 3)
        pusherS();

      exit(0);
    }
    else
    {
      // parent process
      pids[i] = pid;
    }
  }

  // create chief
  for (int i = 0; i < CHIEF_COUNT; i++)
  {
    pid = fork();
    if (pid == -1)
    {
      GLOBAL_ERROR = FORK_ERROR;
      return -1;
    }
    else if (pid == 0)
    {
      // child processes
      if (i == 0)
        chefWS();
      else if (i == 1)
        chefFW();
      else if (i == 2)
        chefSF();
      else if (i == 3)
        chefMF();
      else if (i == 4)
        chefMW();
      else if (i == 5)
        chefSM();

      exit(0);
    }
    else
    {
      // parent process
      pids[i + PUSHER_COUNT] = pid;
    }
  }

  // wholesaler process starts here
  pid_t myPid = getpid();
  char sharedChar[2];
  sharedChar[0] = unnamedShared->ingredient1;
  sharedChar[1] = unnamedShared->ingredient2;
  while (1)
  {
    char ingredient1 = sharedChar[0];
    char ingredient2 = sharedChar[1];
    if (ingredient1 == 'S')
    {
      sem_post(&unnamedShared->s);
    }
    else if (ingredient1 == 'F')
    {
      sem_post(&unnamedShared->f);
    }
    else if (ingredient1 == 'W')
    {
      sem_post(&unnamedShared->w);
    }
    else if (ingredient1 == 'M')
    {
      sem_post(&unnamedShared->m);
    }

    sem_post(&unnamedShared->pusherWorking);

    dprintf(STDOUT_FILENO, "the wholesaler (pid %d) delivers %s and %s\n", myPid, convertIngredient(ingredient1), convertIngredient(ingredient2));
    dprintf(STDOUT_FILENO, "The wholesaler (pid %d) is waiting for the dessert.\n", myPid);

    sem_wait(&unnamedShared->pusherWorking); // wait till a chef is done
    dprintf(STDOUT_FILENO, "The wholesaler (pid %d) has obtained the dessert and left.\n", myPid);

    wholesalerBag.deliveredIngredient++;
    if (wholesalerBag.deliveredIngredient == wholesalerBag.totalIngredients)
    {
      // kill all child processes
      for (int i = 0; i < CHIEF_COUNT + PUSHER_COUNT; i++)
      {
        kill(pids[i], SIGINT);
      }

      int totalDesert = 0;
      // get total desert from chefs return value
      for (int i = 0; i < CHIEF_COUNT; i++)
      {
        int desertCount = 0;
        if (waitpid(pids[i + PUSHER_COUNT], &desertCount, 0) == -1)
        {
          GLOBAL_ERROR = WAITPID_ERROR;
          return -1;
        }

        if (WIFEXITED(desertCount))
        {
          totalDesert += WEXITSTATUS(desertCount);
        }
      }
      dprintf(STDOUT_FILENO, "The wholesaler (pid %d) is done.", myPid);
      dprintf(STDOUT_FILENO, "(total desserts: %d).\n", totalDesert);

      break;
    }

    // go to next ingredient with shared memory
    sharedChar[0] = wholesalerBag.ingredients[wholesalerBag.deliveredIngredient].ingredient1;
    sharedChar[1] = wholesalerBag.ingredients[wholesalerBag.deliveredIngredient].ingredient2;
  }

  shm_unlink(SHARED_MEMORY_UNNAMED_NAME);
  return 1;
}

int pusherW()
{
  while (1)
  {
    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
    if (sharedIngredient == NULL)
      return -1;

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    sem_wait(&unnamedShared->w);
    sem_wait(&unnamedShared->pusherWorking);

    // check shared ingredient array
    if (unnamedShared->ingredient1 == 'S')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->ws);
    }
    else if (unnamedShared->ingredient1 == 'F')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->fw);
    }
    else if (unnamedShared->ingredient1 == 'M')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->mw);
    }
  }

  return 1;
}

int pusherF()
{
  while (1)
  {
    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
    if (sharedIngredient == NULL)
      return -1;

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    sem_wait(&unnamedShared->f);
    sem_wait(&unnamedShared->pusherWorking);

    // check shared ingredient array
    if (unnamedShared->ingredient1 == 'S')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->sf);
    }
    else if (unnamedShared->ingredient1 == 'W')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->fw);
    }
    else if (unnamedShared->ingredient1 == 'M')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->mf);
    }
  }

  return 1;
}

int pusherM()
{
  while (1)
  {
    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
    if (sharedIngredient == NULL)
      return -1;

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    sem_wait(&unnamedShared->m);
    sem_wait(&unnamedShared->pusherWorking);

    // check shared ingredient array
    if (unnamedShared->ingredient1 == 'S')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->sm);
    }
    else if (unnamedShared->ingredient1 == 'W')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->mw);
    }
    else if (unnamedShared->ingredient1 == 'F')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->mf);
    }
  }

  return 1;
}

int pusherS()
{
  while (1)
  {
    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
    if (sharedIngredient == NULL)
      return -1;

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    sem_wait(&unnamedShared->s);
    sem_wait(&unnamedShared->pusherWorking);

    // check shared ingredient array
    if (unnamedShared->ingredient1 == 'F')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->sf);
    }
    else if (unnamedShared->ingredient1 == 'W')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->ws);
    }
    else if (unnamedShared->ingredient1 == 'M')
    {
      unnamedShared->ingredient1 = '-';
      unnamedShared->ingredient2 = '-';
      sem_post(&unnamedShared->sm);
    }
  }

  return 1;
}

void chefWS()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    dprintf(STDOUT_FILENO, "Chef0 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('W'), convertIngredient('S'));
    sem_wait(&unnamedShared->ws);

    dprintf(STDOUT_FILENO, "Chef0 (pid %d) has taken the %s\n", myPid, convertIngredient('W'));
    dprintf(STDOUT_FILENO, "Chef0 (pid %d) has taken the %s\n", myPid, convertIngredient('S'));
    dprintf(STDOUT_FILENO, "Chef0 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef0 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(&unnamedShared->chefDelivered);
  }
}

void chefFW()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    dprintf(STDOUT_FILENO, "Chef1 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('F'), convertIngredient('W'));
    sem_wait(&unnamedShared->fw);

    dprintf(STDOUT_FILENO, "Chef1 (pid %d) has taken the %s\n", myPid, convertIngredient('F'));
    dprintf(STDOUT_FILENO, "Chef1 (pid %d) has taken the %s\n", myPid, convertIngredient('W'));
    dprintf(STDOUT_FILENO, "Chef1 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef1 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(&unnamedShared->chefDelivered);
  }
}

void chefSF()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    dprintf(STDOUT_FILENO, "Chef2 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('S'), convertIngredient('F'));
    sem_wait(&unnamedShared->sf);

    dprintf(STDOUT_FILENO, "Chef2 (pid %d) has taken the %s\n", myPid, convertIngredient('S'));
    dprintf(STDOUT_FILENO, "Chef2 (pid %d) has taken the %s\n", myPid, convertIngredient('F'));
    dprintf(STDOUT_FILENO, "Chef2 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef2 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(&unnamedShared->chefDelivered);
  }
}

void chefMF()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    dprintf(STDOUT_FILENO, "Chef3 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('M'), convertIngredient('F'));
    sem_wait(&unnamedShared->mf);

    dprintf(STDOUT_FILENO, "Chef3 (pid %d) has taken the %s\n", myPid, convertIngredient('M'));
    dprintf(STDOUT_FILENO, "Chef3 (pid %d) has taken the %s\n", myPid, convertIngredient('F'));
    dprintf(STDOUT_FILENO, "Chef3 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef3 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(&unnamedShared->chefDelivered);
  }
}

void chefMW()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    dprintf(STDOUT_FILENO, "Chef4 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('M'), convertIngredient('W'));
    sem_wait(&unnamedShared->mw);

    dprintf(STDOUT_FILENO, "Chef4 (pid %d) has taken the %s\n", myPid, convertIngredient('M'));
    dprintf(STDOUT_FILENO, "Chef4 (pid %d) has taken the %s\n", myPid, convertIngredient('W'));
    dprintf(STDOUT_FILENO, "Chef4 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef4 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(&unnamedShared->chefDelivered);
  }
}

void chefSM()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);

    UnnamedShared *unnamedShared = (UnnamedShared *)sharedIngredient;

    dprintf(STDOUT_FILENO, "Chef5 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('S'), convertIngredient('M'));
    sem_wait(&unnamedShared->sm);

    dprintf(STDOUT_FILENO, "Chef5 (pid %d) has taken the %s\n", myPid, convertIngredient('S'));
    dprintf(STDOUT_FILENO, "Chef5 (pid %d) has taken the %s\n", myPid, convertIngredient('M'));
    dprintf(STDOUT_FILENO, "Chef5 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef5 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(&unnamedShared->chefDelivered);
  }
}
