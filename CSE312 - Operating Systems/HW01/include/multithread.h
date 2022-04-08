
#ifndef __MYOS__MULTITHREAD_H
#define __MYOS__MULTITHREAD_H

#include <common/types.h>
#include <gdt.h>
#include <multitasking.h> // for CPUState

namespace myos
{

  class Thread
  {
    friend class ThreadManager;

  private:
    common::uint8_t stack[4096]; // 4 KiB
    CPUState *cpustate;

  public:
    Thread(GlobalDescriptorTable *gdt, void entrypoint());
    ~Thread();
  };

  class ThreadManager
  {
  private:
    Thread *threads[256];
    int numThreads;
    int currentThread;

  public:
    ThreadManager();
    ~ThreadManager();
    bool createThread(Thread *thread);
    CPUState *Schedule(CPUState *cpustate);
  };

}

#endif