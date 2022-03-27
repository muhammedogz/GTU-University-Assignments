#include <stdio.h>
#include <unistd.h>

int main()
{

  pid_t pid = fork();
  int a = 5;

  char *argv[] = {"./hw02", "hello", "world", "5", NULL};
  char *env[] = {"PATH=/usr/bin", "/home/user", NULL};

  if (pid == 0)
  {
    execve("./exec", argv, env);
  }
  else 
  {
    wait(NULL);
    printf("a = %d\n", a);
    printf("I am parent\n");
    printf("Parent id: %d\n", getpid());
    printf("I am parent. My parent id %d\n", getppid());
  }
}