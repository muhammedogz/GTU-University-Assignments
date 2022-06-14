
#include <common/types.h>
#include <gdt.h>
#include <hardwarecommunication/interrupts.h>
#include <hardwarecommunication/pci.h>
#include <drivers/driver.h>
#include <drivers/keyboard.h>
#include <drivers/mouse.h>
#include <drivers/vga.h>
#include <gui/desktop.h>
#include <gui/window.h>
#include <multitasking.h>
#include <multithread.h>

// #define GRAPHICSMODE

using namespace myos;
using namespace myos::common;
using namespace myos::drivers;
using namespace myos::hardwarecommunication;
using namespace myos::gui;

void printf(char *str)
{
  static uint16_t *VideoMemory = (uint16_t *)0xb8000;

  static uint8_t x = 0, y = 0;

  for (int i = 0; str[i] != '\0'; ++i)
  {
    switch (str[i])
    {
    case '\n':
      x = 0;
      y++;
      break;
    default:
      VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0xFF00) | str[i];
      x++;
      break;
    }

    if (x >= 80)
    {
      x = 0;
      y++;
    }

    if (y >= 25)
    {
      for (y = 0; y < 25; y++)
        for (x = 0; x < 80; x++)
          VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0xFF00) | ' ';
      x = 0;
      y = 0;
    }
  }
}

void printInteger(int a)
{
  char str[10];
  int i = 0;
  while (a > 0)
  {
    str[i] = a % 10 + '0';
    a /= 10;
    i++;
  }
  str[i] = '\0';
  printf(str);
}

void printfHex(uint8_t key)
{
  char *foo = "00";
  char *hex = "0123456789ABCDEF";
  foo[0] = hex[(key >> 4) & 0xF];
  foo[1] = hex[key & 0xF];
  printf(foo);
}

ThreadManager threadManager;

void yieldHelper(int id)
{
  if (threadManager.yieldModeOpen == true && threadManager.yieldedThread == id)
  {
    while (threadManager.yieldModeOpen == true)
    {
    };
  }
}

int turn;
int flag[2];

void petersonsEnter(int id)
{
  int otherId = id == 0 ? 1 : 0;
  flag[id] = true;
  turn = id;
  while (turn == id && flag[otherId] == true)
    ;
}

void petersonsLeave(int id)
{
  flag[id] = false;
}

void taskA()
{
  while (true)
  {
    yieldHelper(0);
    printf("Task ----------------------------- A --------------\n");
  }
}

void taskB()
{
  while (true)
  {
    yieldHelper(1);
    printf("Task +++++++++++++++ B +++++++++++++++\n");
  }
}

int fish = 0; // this is my product

void producer()
{
  while (true)
  {
    if (fish < 1000000)
      petersonsEnter(0);

    // printf("Producer -----------------------------\n");
    fish++;

    if (fish > 1000000)
    {
      printf("Producer: fish > 1000000\n");
      petersonsLeave(0);
    }
  }
}

void consumer()
{
  while (true)
  {
    if (fish > 0)
      petersonsEnter(1);

    // printf("Consumer -----------------------------\n");
    fish--;

    if (fish < 0)
    {
      printf("Consumer: fish < 0\n");
      petersonsLeave(1);
    }
  }
}

class LRU
{

public:
  int Id;
  int hitTime;
  LRU(int id);
};

LRU::LRU(int id)
{
  Id = id;
  hitTime = 0;
}

class PrintfKeyboardEventHandler : public KeyboardEventHandler
{
public:
  void OnKeyDown(char c)
  {
    char *inputBuffer = " ";
    inputBuffer[0] = c;

    if (inputBuffer[0] == '1')
    {
      printf("\nFIFO (First-In-First-Out)\n");
      fifo();
    }
    else if (inputBuffer[0] == '2')
    {
      printf("\nSecond Chance\n");
      secondChance();
    }
    else if (inputBuffer[0] == '3')
    {
      {
        printf("\nLRU (Least Recently Used)\n");
        lru();
      }
    }
  }
  int fifoMemory[15] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
  int fifoHit = 0, fifoMis = 0, fifoIdx = 0;

  int secondMemory[16] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
  int secondHit = 0, secondMis = 0, secondIdx = 0;
  LRU tmp = LRU(-1);
  LRU *LRUMemory; // = {tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp,tmp};
  int LRUHit = 0, LRUMis = 0, LRUIdx = 0;

  int randValues[1000] = {25, 18, 6, 24, 15, 21, 19, 30, 5, 25, 19, 21, 3, 20, 18, 27, 5, 2, 14, 20, 30, 22, 23, 4, 11, 24, 1, 23, 30, 0, 9, 6, 30, 12, 14, 5, 21, 8, 28, 7, 21, 29, 3, 17, 19, 8, 1, 1, 10, 14, 10, 11, 13, 13, 29, 20, 19, 20, 18, 11, 20, 1, 19, 25, 7, 28, 15, 23, 24, 25, 6, 22, 24, 20, 11, 9, 18, 30, 17, 16, 30, 11, 20, 18, 12, 1, 3, 0, 7, 19, 10, 0, 17, 14, 0, 1, 3, 23, 30, 14, 16, 23, 1, 27, 25, 20, 7, 21, 20, 11, 22, 1, 4, 22, 11, 24, 10, 27, 28, 18, 14, 21, 2, 28, 16, 16, 26, 12, 29, 9, 4, 6, 10, 3, 6, 20, 1, 8, 16, 9, 12, 8, 0, 6, 24, 7, 17, 3, 23, 12, 14, 18, 27, 20, 27, 15, 14, 4, 22, 28, 4, 5, 29, 3, 28, 11, 2, 13, 30, 25, 8, 12, 22, 3, 12, 30, 6, 18, 16, 23, 2, 30, 29, 4, 12, 8, 9, 6, 13, 1, 10, 5, 18, 9, 10, 16, 4, 25, 2, 13, 14, 9, 12, 15, 6, 24, 4, 30, 13, 24, 0, 10, 4, 7, 6, 15, 4, 0, 17, 0, 29, 21, 3, 17, 26, 14, 22, 24, 27, 24, 20, 14, 13, 17, 0, 17, 10, 12, 21, 1, 7, 1, 27, 4, 25, 4, 10, 26, 24, 30, 17, 3, 4, 14, 9, 5, 28, 27, 17, 16, 0, 16, 3, 9, 14, 20, 17, 9, 6, 2, 4, 24, 7, 1, 29, 19, 20, 26, 28, 5, 1, 29, 11, 19, 17, 28, 0, 21, 27, 3, 22, 9, 23, 18, 1, 25, 8, 10, 18, 28, 12, 12, 11, 9, 18, 6, 1, 16, 26, 23, 18, 24, 15, 1, 16, 2, 27, 27, 4, 18, 7, 22, 14, 29, 14, 22, 3, 28, 9, 9, 17, 23, 14, 28, 3, 20, 3, 7, 25, 2, 22, 18, 27, 23, 20, 27, 6, 2, 4, 24, 6, 3, 8, 9, 12, 27, 20, 19, 26, 8, 23, 3, 0, 20, 5, 20, 7, 22, 14, 27, 18, 11, 28, 17, 10, 25, 7, 27, 7, 10, 2, 24, 19, 16, 4, 26, 28, 16, 9, 16, 22, 29, 5, 8, 16, 27, 16, 17, 27, 7, 16, 5, 26, 22, 19, 3, 13, 20, 7, 20, 2, 1, 2, 21, 5, 26, 21, 11, 30, 1, 8, 7, 30, 24, 25, 5, 20, 16, 25, 8, 8, 23, 6, 5, 2, 11, 16, 23, 15, 23, 28, 27, 25, 25, 23, 0, 26, 22, 11, 24, 19, 4, 20, 15, 14, 17, 17, 2, 18, 30, 7, 28, 27, 24, 4, 11, 1, 19, 18, 26, 11, 10, 10, 24, 9, 29, 30, 8, 3, 3, 6, 19, 19, 6, 9, 29, 3, 24, 20, 4, 20, 0, 28, 0, 19, 15, 18, 9, 12, 15, 17, 7, 18, 0, 0, 23, 29, 11, 14, 9, 0, 26, 3, 26, 1, 2, 6, 15, 3, 17, 15, 28, 18, 7, 2, 13, 26, 3, 19, 13, 24, 23, 7, 13, 30, 7, 23, 3, 18, 2, 16, 19, 28, 7, 8, 21, 10, 18, 29, 25, 3, 29, 22, 13, 24, 28, 6, 0, 5, 20, 9, 14, 28, 8, 8, 22, 20, 14, 29, 21, 10, 11, 15, 8, 5, 22, 11, 0, 9, 10, 10, 18, 26, 21, 19, 12, 2, 4, 22, 5, 29, 17, 23, 23, 19, 18, 24, 23, 18, 12, 13, 9, 16, 26, 18, 11, 18, 15, 9, 18, 25, 26, 22, 10, 3, 1, 13, 4, 9, 17, 17, 2, 14, 15, 12, 17, 21, 19, 15, 17, 27, 3, 28, 23, 27, 28, 27, 5, 18, 15, 7, 15, 17, 17, 0, 8, 27, 0, 8, 15, 9, 20, 18, 14, 1, 2, 14, 21, 20, 14, 18, 6, 12, 15, 0, 14, 7, 12, 7, 12, 29, 20, 16, 30, 18, 17, 16, 8, 22, 22, 11, 27, 11, 16, 17, 19, 22, 24, 27, 10, 29, 19, 18, 17, 27, 26, 12, 0, 12, 2, 8, 24, 17, 26, 28, 20, 16, 23, 30, 1, 19, 10, 26, 19, 13, 27, 19, 17, 8, 7, 8, 21, 18, 6, 1, 16, 10, 21, 15, 17, 11, 4, 7, 29, 9, 3, 21, 12, 10, 26, 26, 30, 21, 3, 26, 14, 6, 2, 17, 12, 6, 25, 20, 10, 11, 28, 3, 13, 13, 28, 12, 17, 26, 14, 19, 1, 28, 19, 30, 8, 2, 21, 2, 10, 17, 22, 8, 25, 3, 13, 13, 8, 28, 1, 8, 7, 0, 14, 3, 0, 7, 25, 2, 20, 3, 16, 1, 9, 19, 21, 19, 27, 6, 7, 0, 1, 14, 1, 10, 3, 7, 30, 5, 17, 4, 17, 4, 12, 30, 16, 21, 13, 15, 20, 4, 6, 25, 24, 26, 9, 3, 25, 25, 14, 17, 3, 15, 15, 27, 7, 7, 1, 23, 18, 6, 12, 17, 5, 19, 11, 26, 30, 9, 22, 2, 20, 3, 22, 27, 14, 18, 6, 30, 16, 26, 14, 13, 17, 13, 19, 11, 2, 8, 18, 16, 8, 30, 20, 29, 20, 11, 24, 5, 7, 3, 18, 0, 5, 23, 10, 13, 2, 17, 23, 11, 16, 0, 29, 23, 27, 9, 13, 28, 24, 10, 2, 19, 30, 14, 21, 25, 5, 16, 9, 27, 12, 4, 2, 15, 3, 18, 16, 0, 7, 3, 6, 12, 21, 15, 12, 11, 3, 14, 26, 29, 2, 0, 20, 18, 19, 15, 23, 13, 5, 2, 6, 8, 16, 26, 7, 24, 24, 19, 4, 30, 5, 29, 12, 29, 23, 4, 22, 29, 18, 27, 20, 14, 5, 25, 16, 29, 27, 14, 23, 13, 0, 6, 16, 20, 11, 12, 21, 9, 5, 17, 0, 15, 19, 28, 19, 9, 20, 19, 1, 6, 21, 27, 5, 25, 0};

  bool isHaveFifo(int arr[], int num)
  {
    for (int i = 0; i < 15 && i < fifoIdx; i++)
    {
      if (arr[i] == num)
      {
        int temp = arr[i];
        int j = i + 1;
        while (arr[j] != -1 && j < 15)
        {
          arr[j - 1] = arr[j];
          j++;
        }
        arr[j - 1] = temp;

        return true;
      }
    }

    return false;
  }

  bool isHaveLRU(LRU arr[], int num)
  {
    for (int i = 0; i < 15 && i < LRUIdx; i++)
    {
      if (arr[i].Id == num)
      {
        arr[i].hitTime++;
        return true;
      }
    }

    return false;
  }

  bool isHaveSecond(int arr[], int num)
  {
    for (int i = 0; i < 16 && i < secondIdx; i++)
    {
      if (arr[i] == num)
      {
        int temp = arr[i];
        int j = i + 1;
        while (arr[j] != -1 && j < 16)
        {
          arr[j - 1] = arr[j];
          j++;
        }
        arr[j - 1] = temp;

        return true;
      }
    }

    return false;
  }

  void secondChance()
  {
    for (int i = 0; i < 1000; i++)
    {
      int value = randValues[i];

      if (isHaveSecond(secondMemory, value))
      {
        secondHit++;
      }

      else if (secondIdx < 16)
      {
        secondMis++;
        secondMemory[secondIdx++] = value;
      }
      else
      {
        secondMis++;
        for (int i = 1; i < 16; i++)
          secondMemory[i - 1] = secondMemory[i];
        secondMemory[15] = value;
      }
    }

    printf("\nHit time : ");
    printInteger(secondHit);
    printf("\nmiss time : ");
    printInteger(secondMis);
    printf("\n");
    secondIdx = 0;
    secondMis = 0;
    secondHit = 0;
  }
  void lru()
  {
    for (int i = 0; i < 1000; i++)
    {
      int value = randValues[i];

      if (isHaveLRU(LRUMemory, value))
      {
        LRUHit++;
      }

      else if (LRUIdx < 15)
      {
        LRUMis++;
        LRU temp = LRU(value);
        LRUMemory[LRUIdx++] = temp;
      }
      else
      {
        LRUMis++;
        int min = 100000;
        for (int i = 0; i < 15; i++)
          if (min > LRUMemory[i].hitTime)
          {
            min = LRUMemory[i].hitTime;
          }

        for (int i = 0; i < 15; i++)
          if (min == LRUMemory[i].hitTime)
          {
            LRU temp = LRU(value);
            LRUMemory[i] = temp;
            break;
          }
        for (int i = 0; i < 15; i++)
          if (!(0 == LRUMemory[i].hitTime))
          {
            LRUMemory[i].hitTime -= min;
          }
      }
    }

    printf("\nHit time : ");
    printInteger(LRUHit);
    printf("\nmiss time : ");
    printInteger(LRUMis);
    printf("\n");
    LRUIdx = 0;
    LRUHit = 0;
    LRUMis = 0;
  }
  void fifo()
  {

    for (int i = 0; i < 1000; i++)
    {
      int value = randValues[i];

      if (isHaveFifo(fifoMemory, value))
      {
        fifoHit++;
      }

      else if (fifoIdx < 15)
      {
        fifoMis++;
        fifoMemory[fifoIdx++] = value;
      }
      else
      {
        fifoMis++;
        for (int i = 1; i < 15; i++)
          fifoMemory[i - 1] = fifoMemory[i];
        fifoMemory[14] = value;
      }
    }

    printf("\nHit time : ");
    printInteger(fifoHit);
    printf("\nmiss time : ");
    printInteger(fifoMis);
    printf("\n");
    fifoIdx = 0;

    fifoHit = 0;
    fifoMis = 0;
  }
};

class MouseToConsole : public MouseEventHandler
{
  int8_t x, y;

public:
  MouseToConsole()
  {
    uint16_t *VideoMemory = (uint16_t *)0xb8000;
    x = 40;
    y = 12;
    VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0x0F00) << 4 | (VideoMemory[80 * y + x] & 0xF000) >> 4 | (VideoMemory[80 * y + x] & 0x00FF);
  }

  virtual void OnMouseMove(int xoffset, int yoffset)
  {
    static uint16_t *VideoMemory = (uint16_t *)0xb8000;
    VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0x0F00) << 4 | (VideoMemory[80 * y + x] & 0xF000) >> 4 | (VideoMemory[80 * y + x] & 0x00FF);

    x += xoffset;
    if (x >= 80)
      x = 79;
    if (x < 0)
      x = 0;
    y += yoffset;
    if (y >= 25)
      y = 24;
    if (y < 0)
      y = 0;

    VideoMemory[80 * y + x] = (VideoMemory[80 * y + x] & 0x0F00) << 4 | (VideoMemory[80 * y + x] & 0xF000) >> 4 | (VideoMemory[80 * y + x] & 0x00FF);
  }
};

typedef void (*constructor)();
extern "C" constructor start_ctors;
extern "C" constructor end_ctors;
extern "C" void callConstructors()
{
  for (constructor *i = &start_ctors; i != &end_ctors; i++)
    (*i)();
}

extern "C" void kernelMain(const void *multiDoot_structure, uint32_t /*multiboot_magic*/)
{
  printf("Hello World! --- MUHAMMED OGUZ\n");

  GlobalDescriptorTable gdt;

  // ! You can test yield and join from here hocam
  // Thread task1(&gdt, taskA, 0);
  // Thread task2(&gdt, taskB, 1);
  // threadManager.CreateThread(&task1);
  // threadManager.CreateThread(&task2);
  // threadManager.Yield(0);
  // threadManager.Yield(1);
  // threadManager.Join(0);
  // threadManager.Join(1);

  // consumer producer test
  // Thread task1(&gdt, producer, 0);
  // Thread task2(&gdt, consumer, 1);
  // threadManager.CreateThread(&task1);
  // threadManager.CreateThread(&task2);

  InterruptManager interrupts(0x20, &gdt, &threadManager);

#ifdef GRAPHICSMODE
  Desktop desktop(320, 200, 0x00, 0x00, 0xA8);
#endif

  DriverManager drvManager;

#ifdef GRAPHICSMODE
  KeyboardDriver keyboard(&interrupts, &desktop);
#else
  PrintfKeyboardEventHandler kbhandler;
  KeyboardDriver keyboard(&interrupts, &kbhandler);
#endif
  drvManager.AddDriver(&keyboard);

#ifdef GRAPHICSMODE
  MouseDriver mouse(&interrupts, &desktop);
#else
  MouseToConsole mousehandler;
  MouseDriver mouse(&interrupts, &mousehandler);
#endif
  drvManager.AddDriver(&mouse);

  PeripheralComponentInterconnectController PCIController;
  PCIController.SelectDrivers(&drvManager, &interrupts);

  VideoGraphicsArray vga;

  drvManager.ActivateAll();

#ifdef GRAPHICSMODE
  vga.SetMode(320, 200, 8);
  Window win1(&desktop, 10, 10, 20, 20, 0xA8, 0x00, 0x00);
  desktop.AddChild(&win1);
  Window win2(&desktop, 40, 15, 30, 30, 0x00, 0xA8, 0x00);
  desktop.AddChild(&win2);
#endif

  interrupts.Activate();

  printf("1-FIFO\n");
  printf("2-Second Chance\n");
  printf("3-LRU\n");

  while (1)
  {
#ifdef GRAPHICSMODE
    desktop.Draw(&vga);
#endif
  }
}
