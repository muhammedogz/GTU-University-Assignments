#include <ostream>
// ThreadSafeSet.hpp

#pragma once
// ThreadSafeSet should be a thread-safe lock-free set.
class ThreadSafeSet
{
public:
  // Add your implementation here
  struct Node
  {
    int data;
    Node *next;
    Node(int d) : data(d)
    {
    }
  };
  ThreadSafeSet();
  // Inserts an element into the set.
  // Returns true if the element was inserted, false if it was already there.
  bool insert(const int &element);

  // Removes an element from the set.
  // Returns true if the element was removed, false if it was not there.
  bool remove(const int &element);

  // Checks if an element is in the set.
  // Returns true if the element is in the set, false otherwise.
  bool contains(const int &element) const;

  // overload << operator to print the set
  friend std::ostream &operator<<(std::ostream &os, const ThreadSafeSet &set);

private:
  Node *head;
  Node *tail;
  int size;
};
