#include "../include/ThreadSafeSet.hpp"
#include "../include/Node.hpp"
#include <iostream>
#include <ostream>
using namespace std;

ThreadSafeSet::ThreadSafeSet()
{
  cout << "Hello World!" << endl;

  size = 0;
}

std::ostream &operator<<(std::ostream &os, const ThreadSafeSet &set)
{
  Node *temp = set.head;
  while (temp != nullptr)
  {
    os << temp->data << " ";
    temp = temp->next;
  }

  return os;
}

bool ThreadSafeSet::insert(const int &element)
{
  // insert to the end
  cout << "insert begins" << endl;
  if (head == nullptr)
  {
    head = new Node(element);
    tail = head;
    size++;
    return true;
  }
  else
  {
    Node *temp = head;
    while (temp != nullptr)
    {
      if (temp->data == element)
      {
        return false;
      }
      temp = temp->next;
    }
    tail->next = new Node(element);
    tail = tail->next;
    size++;
    return true;
  }

  return false;
}
