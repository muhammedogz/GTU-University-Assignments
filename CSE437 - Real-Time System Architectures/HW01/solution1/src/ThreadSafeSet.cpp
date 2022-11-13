#include "../include/ThreadSafeSet.hpp"
#include "../include/Node.hpp"
#include <iostream>
#include <ostream>
using namespace std;

template <typename T>
ThreadSafeSet<T>::ThreadSafeSet()
{
  cout << "Hello World!" << endl;

  size = 0;
}

template <typename T>
std::ostream &operator<<(std::ostream &os, const ThreadSafeSet<T> &set)
{
  Node<T> *temp = set.getHead();
  while (temp != nullptr)
  {
    os << temp->data << " ";
    temp = temp->next;
  }

  return os;
}

template <typename T>
bool ThreadSafeSet<T>::insert(const T &element)
{
  // insert to the end
  cout << "insert begins" << endl;
  if (head == nullptr)
  {
    head = new Node<T>(element);
    tail = head;
    size++;
    return true;
  }
  else
  {
    Node<T> *temp = head;
    while (temp != nullptr)
    {
      if (temp->data == element)
      {
        return false;
      }
      temp = temp->next;
    }
    tail->next = new Node<T>(element);
    tail = tail->next;
    size++;
    return true;
  }

  return false;
}
