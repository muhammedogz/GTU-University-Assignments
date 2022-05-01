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

static sem_t *pusherWorking, *chefDelivered, *m, *f, *w, *s;
static sem_t *ws, *fw, *sf, *mf, *mw, *sm;

int totalDesertOfChef = 0;

void sigintHandlerForChief(int signum)
{
  if (signum == SIGINT)
  {
    exit(totalDesertOfChef);
  }
}

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
  if ((chefDelivered = sem_open(names[1], O_CREAT, 0644, 0)) == SEM_FAILED)
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
  if ((ws = sem_open(names[5], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((sf = sem_open(names[6], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((mf = sem_open(names[7], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((mw = sem_open(names[8], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((mf = sem_open(names[9], O_CREAT, 0644, 0)) == SEM_FAILED)
  {
    GLOBAL_ERROR = SEMAPHORE_OPEN_ERROR;
    return -1;
  }
  if ((sm = sem_open(names[10], O_CREAT, 0644, 0)) == SEM_FAILED)
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
  if (sem_close(chefDelivered) == -1)
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
  if (sem_close(ws) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(fw) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(sf) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(mf) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(mw) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  if (sem_close(sm) == -1)
  {
    GLOBAL_ERROR = SEMAPHORE_CLOSE_ERROR;
    return -1;
  }
  return 1;
}

int runNamed(WholesalerBag wholesalerBag, char **names)
{
  if (initializeSemaphores(names) == -1)
    return -1;

  void *sharedIngredientArray = createSharedMemory(SHARED_MEMORY_NAMED_NAME, wholesalerBag.ingredients[0].ingredient1, wholesalerBag.ingredients[0].ingredient2);
  if (sharedIngredientArray == NULL)
    return -1;

  char *sharedChar = (char *)sharedIngredientArray;

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
  while (1)
  {
    char ingredient1 = sharedChar[0];
    char ingredient2 = sharedChar[1];
    dprintf(STDOUT_FILENO, "Wholesaler %d: %c %c\n", myPid, ingredient1, ingredient2);
    if (ingredient1 == 'S')
    {
      dprintf(STDOUT_FILENO, "S if working\n");
      sem_post(s);
    }
    else if (ingredient1 == 'F')
    {
      dprintf(STDOUT_FILENO, "F if working\n");
      sem_post(f);
    }
    else if (ingredient1 == 'W')
    {
      dprintf(STDOUT_FILENO, "W if working\n");
      sem_post(w);
    }
    else if (ingredient1 == 'M')
    {
      dprintf(STDOUT_FILENO, "M if working\n");
      sem_post(m);
    }
    sem_post(pusherWorking);

    dprintf(STDOUT_FILENO, "the wholesaler (pid %d) delivers %s and %s\n", myPid, convertIngredient(ingredient1), convertIngredient(ingredient2));
    dprintf(STDOUT_FILENO, "The wholesaler (pid %d) is waiting for the dessert.\n", myPid);

    sem_wait(chefDelivered); // wait till a chef is done
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

  freeSemaphores(names);

  shm_unlink(SHARED_MEMORY_NAMED_NAME);

  return 1;
}

int pusherW()
{
  void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
  if (sharedIngredient == NULL)
    return -1;

  char *sharedIngredientChar = (char *)sharedIngredient;

  while (1)
  {
    sem_wait(w);
    dprintf(STDOUT_FILENO, "PusherW passed w.\n");
    sem_wait(pusherWorking);

    dprintf(STDOUT_FILENO, "The pusherW (pid %d) is working.\n", getpid());

    int whichIndex = sharedIngredientChar[1] == 'W' ? 0 : 1;
    int otherIndex = whichIndex == 0 ? 1 : 0;

    // check shared ingredient array
    if (sharedIngredientChar[whichIndex] == 'S')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(ws);
    }
    else if (sharedIngredientChar[whichIndex] == 'F')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(fw);
    }
    else if (sharedIngredientChar[whichIndex] == 'M')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(mw);
    }
  }

  return 1;
}

int pusherS()
{
  void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
  if (sharedIngredient == NULL)
    return -1;

  char *sharedIngredientChar = (char *)sharedIngredient;

  while (1)
  {
    sem_wait(s);
    dprintf(STDOUT_FILENO, "PusherS passed s.\n");
    sem_wait(pusherWorking);

    dprintf(STDOUT_FILENO, "The pusherS (pid %d) is working.\n", getpid());

    int whichIndex = sharedIngredientChar[1] == 'S' ? 0 : 1;
    int otherIndex = whichIndex == 0 ? 1 : 0;
    // check shared ingredient array
    if (sharedIngredientChar[whichIndex] == 'W')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(ws);
    }
    else if (sharedIngredientChar[whichIndex] == 'F')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(sf);
    }
    else if (sharedIngredientChar[whichIndex] == 'M')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(sm);
    }
  }

  return 1;
}

int pusherM()
{
  void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
  if (sharedIngredient == NULL)
    return -1;

  char *sharedIngredientChar = (char *)sharedIngredient;

  while (1)
  {
    sem_wait(m);
    dprintf(STDOUT_FILENO, "PusherM passed m.\n");
    sem_wait(pusherWorking);

    dprintf(STDOUT_FILENO, "The pusherM (pid %d) is working.\n", getpid());

    int whichIndex = sharedIngredientChar[1] == 'M' ? 0 : 1;
    int otherIndex = whichIndex == 0 ? 1 : 0;
    // check shared ingredient array
    if (sharedIngredientChar[whichIndex] == 'W')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(mw);
    }
    else if (sharedIngredientChar[whichIndex] == 'F')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(mf);
    }
    else if (sharedIngredientChar[whichIndex] == 'S')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(sm);
    }
  }

  return 1;
}

int pusherF()
{
  void *sharedIngredient = getSharedMemory(SHARED_MEMORY_NAMED_NAME);
  if (sharedIngredient == NULL)
    return -1;

  char *sharedIngredientChar = (char *)sharedIngredient;

  while (1)
  {
    sem_wait(f);
    dprintf(STDOUT_FILENO, "PusherF passed f.\n");
    sem_wait(pusherWorking);

    dprintf(STDOUT_FILENO, "The pusherF (pid %d) is working.\n", getpid());

    int whichIndex = sharedIngredientChar[1] == 'F' ? 0 : 1;
    int otherIndex = whichIndex == 0 ? 1 : 0;
    // check shared ingredient array
    if (sharedIngredientChar[whichIndex] == 'W')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(fw);
    }
    else if (sharedIngredientChar[whichIndex] == 'S')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(sf);
    }
    else if (sharedIngredientChar[whichIndex] == 'M')
    {
      sharedIngredientChar[whichIndex] = '-';
      sharedIngredientChar[otherIndex] = '-';
      sem_post(mf);
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

    dprintf(STDOUT_FILENO, "Chef0 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('W'), convertIngredient('S'));
    sem_wait(ws);

    dprintf(STDOUT_FILENO, "Chef0 (pid %d) has taken the %s\n", myPid, convertIngredient('W'));
    dprintf(STDOUT_FILENO, "Chef0 (pid %d) has taken the %s\n", myPid, convertIngredient('S'));
    dprintf(STDOUT_FILENO, "Chef0 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef0 (pid %d) has delivered the desert\n", myPid);
    totalDesertOfChef++;

    sem_post(chefDelivered);
  }
}

void chefFW()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    dprintf(STDOUT_FILENO, "Chef1 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('F'), convertIngredient('W'));
    sem_wait(fw);

    dprintf(STDOUT_FILENO, "Chef1 (pid %d) has taken the %s\n", myPid, convertIngredient('F'));
    dprintf(STDOUT_FILENO, "Chef1 (pid %d) has taken the %s\n", myPid, convertIngredient('W'));
    dprintf(STDOUT_FILENO, "Chef1 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef1 (pid %d) has delivered the desert\n", myPid);

    totalDesertOfChef++;
    sem_post(chefDelivered);
  }
}

void chefSF()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    dprintf(STDOUT_FILENO, "Chef2 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('S'), convertIngredient('F'));
    sem_wait(sf);

    dprintf(STDOUT_FILENO, "Chef2 (pid %d) has taken the %s\n", myPid, convertIngredient('S'));
    dprintf(STDOUT_FILENO, "Chef2 (pid %d) has taken the %s\n", myPid, convertIngredient('F'));
    dprintf(STDOUT_FILENO, "Chef2 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef2 (pid %d) has delivered the desert\n", myPid);

    totalDesertOfChef++;
    sem_post(chefDelivered);
  }
}

void chefMF()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    dprintf(STDOUT_FILENO, "Chef3 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('M'), convertIngredient('F'));
    sem_wait(mf);

    dprintf(STDOUT_FILENO, "Chef3 (pid %d) has taken the %s\n", myPid, convertIngredient('M'));
    dprintf(STDOUT_FILENO, "Chef3 (pid %d) has taken the %s\n", myPid, convertIngredient('F'));
    dprintf(STDOUT_FILENO, "Chef3 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef3 (pid %d) has delivered the desert\n", myPid);

    totalDesertOfChef++;
    sem_post(chefDelivered);
  }
}

void chefMW()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    dprintf(STDOUT_FILENO, "Chef4 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('M'), convertIngredient('W'));
    sem_wait(mw);

    dprintf(STDOUT_FILENO, "Chef4 (pid %d) has taken the %s\n", myPid, convertIngredient('M'));
    dprintf(STDOUT_FILENO, "Chef4 (pid %d) has taken the %s\n", myPid, convertIngredient('W'));
    dprintf(STDOUT_FILENO, "Chef4 (pid %d) is preparing the desert\n", myPid);
    dprintf(STDOUT_FILENO, "Chef4 (pid %d) has delivered the desert\n", myPid);

    totalDesertOfChef++;
    sem_post(chefDelivered);
  }
}

void chefSM()
{
  signal(SIGINT, sigintHandlerForChief);

  pid_t myPid = getpid();
  while (1)
  {

    dprintf(STDOUT_FILENO, "Chef5 (pid %d) is waiting for %s and %s\n", myPid, convertIngredient('S'), convertIngredient('M'));
    sem_wait(sm);

    dprintf(STDOUT_FILENO, "Chef5 (pid %d) has taken the %s\n", myPid, convertIngredient('S'));
    dprintf(STDOUT_FILENO, "Chef5 (pid %d) has taken the %s\n", myPid, convertIngredient('M'));
    dprintf(STDOUT_FILENO, "Chef5 (pid %d) is preparing the desert\n", myPid);
    sem_post(sm);
    dprintf(STDOUT_FILENO, "Chef5 (pid %d) has delivered the desert\n", myPid);

    totalDesertOfChef++;
    sem_post(chefDelivered);
  }
}
