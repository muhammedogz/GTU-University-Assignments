#include <stdio.h>
#include <stdlib.h>

extern char **environ;

int main(int argc, char *argv[])
{
  printf("starting new process...\n");

  // print arguments
  for (int i = 0; i < argc; i++)
  {
    printf("argv[%d]: %s\n", i, argv[i]);
  }

  // print all environment variables
  for (int i = 0; environ[i] != NULL; i++)
  {
    printf("environ[%d] = %s\n", i, environ[i]);
  }

  // free environment variables
  for (int i = 0; environ[i] != NULL; i++)
  {
    free(environ[i]);
  }
  free(environ);

  sleep(1);
}