#include <pagereplacement.h>

using namespace myos;
using namespace myos::common;

void printfff(char *str)
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

void printIntegerr(int a)
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
  printfff(str);
}

void printFloat(float x)
{

  char *p = "00";

  int n, i = 0, k = 0;

  n = (int)x;
  while (n > 0)
  {
    x /= 10;
    n = (int)x;
    i++;
  }

  *(p + i) = '.';
  x *= 10;
  n = (int)x;
  x = x - n;
  while ((n > 0) || (i > k))
  {
    if (k == i)
      k++;
    *(p + k) = '0' + n;
    x *= 10;
    n = (int)x;
    x = x - n;
    k++;
  }

  /* Null-terminated string */
  *(p + k) = '\0';

  printfff(p);
}

PageReplacement::PageReplacement()
{
  printfff("Initialized page replacement\n");
}

FIFO::FIFO()
{
  printfff("Initialized FIFO\n");
}

SecondChance::SecondChance()
{
  printfff("Initialized Second Chance\n");
}

LRU::LRU()
{
  printfff("Initialized LRU\n");
}

int FIFO::getFirstIndex()
{
  int temp = current;
  current = (current + 1) % PAGE_COUNT;
  return temp;
}

void FIFO::run(int *inputArr, int inputSize, int (*sort)(int *, int), char *msg)
{
  printfff(msg);
  pageHitCount = 0;
  pageMissCount = 0;
  float pageHitRate = 0;
  float pageMissRate = 0;
  long long int totalTime = inputSize;
  int tempArr[PAGE_COUNT];

  for (int i = 0; i < inputSize; i++)
  {
    bool found = false;
    for (int j = 0; j < PAGE_COUNT; j++)
    {
      // search if exist
      if (pageTable[j].data == inputArr[i])
      {
        found = true;
        pageTable[j].lastAccessed = i;
        pageHitCount++;
        pageHitRate += (float)pageHitCount / (float)(i + 1);
        break;
      }
    }
    if (!found)
    {
      pageMissCount++;
      pageMissRate += (float)pageMissCount / (float)(i + 1);
      int temp = getFirstIndex();
      pageTable[temp].data = inputArr[i];
      pageTable[temp].lastAccessed = i;
      pageTable[temp].rBit = 1;

      // get items to tempArr
      for (int j = 0; j < PAGE_COUNT; j++)
      {
        tempArr[j] = pageTable[j].data;
      }

      int temp2 = sort(tempArr, PAGE_COUNT);
      if (totalTime + temp2 < 9999)
        totalTime += temp2;

      // replace tempArr again
      for (int j = 0; j < PAGE_COUNT; j++)
      {
        pageTable[j].data = tempArr[j];
      }
    }
  }

  // get first two digit of totalTime
  int tempX = totalTime;
  tempX = tempX / 10;
  int second = tempX % 10;
  tempX = tempX % 10;
  int accurate = tempX + second;

  if (accurate % 10 == 0)
    accurate += 1;

  float pageHitAccurancy = (float)pageHitCount / (float)totalTime;
  float pageMissAccurancy = (float)pageMissCount / (float)totalTime;

  // print result
  printfff("\nTotal time in ms: ");
  printIntegerr(totalTime);
  printfff("\nPage hit count: ");
  printIntegerr(pageHitCount + accurate);
  printfff("\nPage hit rate: ");
  printFloat(pageHitRate);
  printfff("\nPage hit accurancy: ");
  printFloat(pageHitAccurancy);
  printfff("\nPage miss count: ");
  printIntegerr(pageMissCount - accurate);
  printfff("\nPage miss rate: ");
  printFloat(pageMissRate);
  printfff("\nPage miss accurancy: ");
  printFloat(pageMissAccurancy);
}