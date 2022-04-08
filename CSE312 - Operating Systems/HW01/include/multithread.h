
#ifndef __MYOS__MULTITHREAD_H
#define __MYOS__MULTITHREAD_H

#include <common/types.h>
#include <gdt.h>

namespace myos
{

  struct CPUState
  {
    common::uint32_t eax;
    common::uint32_t ebx;
    common::uint32_t ecx;
    common::uint32_t edx;

    common::uint32_t esi;
    common::uint32_t edi;
    common::uint32_t ebp;

    common::uint32_t error;

    common::uint32_t eip;
    common::uint32_t cs;
    common::uint32_t eflags;
    common::uint32_t esp;
    common::uint32_t ss;
  } __attribute__((packed));

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