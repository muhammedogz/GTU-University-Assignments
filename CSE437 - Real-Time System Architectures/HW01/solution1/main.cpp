#include <iostream>
#include <string>
#include "include/ThreadSafeSet.hpp"
#include "src/ThreadSafeSet.cpp"

using namespace std;

int main()
{
  cout << "Hello World!" << endl;
  ThreadSafeSet<int> set;
  set.insert(1);
  set.insert(2);
  set.insert(3);
  cout << "before print" << endl;
  cout << set << endl;
}