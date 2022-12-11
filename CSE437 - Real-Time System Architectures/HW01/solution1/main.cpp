#include <iostream>
#include <string>
#include "include/ThreadSafeSet.hpp"
#include "src/ThreadSafeSet.cpp"
#include <atomic>
#include <cassert>

void testThreadSafeSet()
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
}

int main()
{
  testThreadSafeSet();
  return 0;
}