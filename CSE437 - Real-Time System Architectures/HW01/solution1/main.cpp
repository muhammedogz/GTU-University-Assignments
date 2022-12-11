#include <iostream>
#include <string>
#include "include/ThreadSafeSet.hpp"
#include "src/ThreadSafeSet.cpp"
#include <atomic>
#include <cassert>
#include <thread>
#include <vector>

void testFunctionality()
{
  ThreadSafeSet<int> set;

  // Test insert()
  assert(set.insert(1));
  assert(set.insert(2));
  assert(set.insert(3));
  assert(!set.insert(1)); // Already in set

  // Test search()
  assert(set.search(1));
  assert(set.search(2));
  assert(set.search(3));
  assert(!set.search(4)); // Not in set

  // Test remove()
  assert(set.remove(1));
  assert(!set.remove(1)); // Already removed
  assert(!set.search(1)); // Not in set anymore

  // Test copy constructor
  ThreadSafeSet<int> set2(set);
  assert(set2.search(2));
  assert(set2.search(3));
  assert(!set2.search(1)); // Not in set2

  // Test move constructor
  ThreadSafeSet<int> set3(std::move(set2));
  assert(set3.search(2));
  assert(set3.search(3));
  assert(!set3.search(1)); // Not in set3

  // Test copy assignment operator
  set2 = set;
  assert(set2.search(2));
  assert(set2.search(3));
  assert(!set2.search(1)); // Not in set2

  // Test move assignment operator
  set3 = std::move(set2);
  assert(set3.search(2));
  assert(set3.search(3));
  assert(!set3.search(1)); // Not in set3

  std::cout << "All tests passed!" << std::endl;
  cout << "Set:" << set << endl;
}

void testThreadSafe()
{
  ThreadSafeSet<int> set;
  std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  std::thread writer([&]() {
    for (unsigned long int i = 0; i < numbers.size(); ++i)
    {
      set.insert(numbers[i]);
    }
  });

  std::thread reader([&]() {
    for (unsigned long int i = 0; i < numbers.size(); ++i)
    {
      if (numbers[i] % 2 == 0)
      {
        set.remove(numbers[i]);
      }
    }
  });

  writer.join();
  reader.join();

  cout << "Set:" << set << endl;

  bool success = true;
  for (unsigned long int i = 0; i < numbers.size(); ++i)
  {
    if (numbers[i] % 2 == 0)
    {
      if (set.search(numbers[i]))
      {
        success = false;
        break;
      }
    }
    else
    {
      if (!set.search(numbers[i]))
      {
        success = false;
        break;
      }
    }
  }

  if (success)
  {
    std::cout << "The test passed" << std::endl;
  }
  else
  {
    std::cout << "The test failed" << std::endl;
  }
}


int main()
{
  cout << "Testing for threadsafe set class" << endl;
  cout << "Testing functionality of the class" << endl;
  testFunctionality();

  cout << "Testing for thread safety" << endl;
  cout << "One reader and one writer" << endl;
  testThreadSafe();
  return 0;
}