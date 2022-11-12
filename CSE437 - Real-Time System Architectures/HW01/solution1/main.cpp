#include <iostream>
#include "include/ThreadSafeSet.hpp"

using namespace std;

int main()
{
  cout << "Hello World!" << endl;
  ThreadSafeSet set;
  set.insert(1);
  set.insert(2);
  set.insert(3);
  cout << "before print" << endl;
  cout << set << endl;
}