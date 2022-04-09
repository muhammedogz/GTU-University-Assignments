
#include <multithread.h>

using namespace myos;
using namespace myos::common;

void printff(char *str)
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

Thread::Thread(GlobalDescriptorTable *gdt, void entrypoint(), int id)
{
  cpustate = (CPUState *)(stack + 4096 - sizeof(CPUState));

  cpustate->eax = 0;
  cpustate->ebx = 0;
  cpustate->ecx = 0;
  cpustate->edx = 0;

  cpustate->esi = 0;
  cpustate->edi = 0;
  cpustate->ebp = 0;

  cpustate->eip = (uint32_t)entrypoint;
  cpustate->cs = gdt->CodeSegmentSelector();
  cpustate->eflags = 0x202;

  this->isYielded = false;
  this->isJoined = false;
  this->yieldCounter = 0;
  this->id = id;
}

bool Thread::getJoin()
{
  return this->isJoined;
}

bool Thread::getYield()
{
  // if (isYielded == true)
  // {
  //   yieldCounter++;
  //   if (yieldCounter > 50000)
  //   {
  //     yieldCounter = 0;
  //     isYielded = false;
  //   }
  // }

  return this->isYielded;
}

void Thread::setYield(bool yield)
{
  this->isYielded = yield;
}

void Thread::setJoin(bool join)
{
  this->isJoined = join;
}

int Thread::getYieldCounter()
{
  return this->yieldCounter;
}

void Thread::setYieldCounter(int yieldCounter)
{
  this->yieldCounter = yieldCounter;
}

void Thread::incrementYieldCounter()
{
  this->yieldCounter++;
}

int Thread::getId()
{
  return this->id;
}

Thread::~Thread()
{
}

ThreadManager::ThreadManager()
{
  numThreads = 0;
  currentThread = -1;
}

ThreadManager::~ThreadManager()
{
}

bool ThreadManager::CreateThread(Thread *thread)
{
  if (numThreads >= 256)
    return false;
  threads[numThreads++] = thread;
  return true;
}

bool ThreadManager::Yield(int id)
{
  yieldModeOpen = !yieldModeOpen;
  tempYieldId = id;
  return true;
}

CPUState *ThreadManager::Schedule(CPUState *cpustate)
{
  if (yieldModeOpen)
  {
    yieldCounter++;
    if (yieldCounter > 30)
    {
      yieldedThread = tempYieldId;
    }
    if (yieldCounter > 60)
      yieldModeOpen = false;
  }
  else
  {
    yieldCounter = 0;
    yieldedThread = -1;
    tempYieldId = -1;
    yieldModeOpen = false;
  }

  if (numThreads <= 0)
    return cpustate;

  if (currentThread >= 0)
  {
    threads[currentThread]->cpustate = cpustate;
  }

  if (++currentThread >= numThreads)
    currentThread = 0;
  return threads[currentThread]->cpustate;
}
