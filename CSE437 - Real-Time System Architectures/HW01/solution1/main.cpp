#include <iostream>
#include <string>
#include "include/ThreadSafeSet.hpp"
#include "src/ThreadSafeSet.cpp"
#include <atomic>
struct B
{
  int x, y;
};
using namespace std;

int main()
{
  cout << "Hello World!" << endl;
  ThreadSafeSet<string> set;
  set.insert("1");
  set.insert("2");
  set.insert("3");
  cout << "before print" << endl;
  cout << set << endl;

  atomic<ThreadSafeSet<string> *> set2;
  cout << "is lock free: " << set2.is_lock_free() << endl;

  // check if lock-free
  cout << "check if lock-free" << endl;
  cout << std::boolalpha << "is lock-free: " << atomic<int>{}.is_lock_free() << endl;
}