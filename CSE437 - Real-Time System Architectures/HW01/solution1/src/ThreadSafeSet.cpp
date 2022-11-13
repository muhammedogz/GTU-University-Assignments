#include "../include/ThreadSafeSet.hpp"
#include "../include/Node.hpp"
#include <iostream>
#include <ostream>
using namespace std;

template <typename T>
ThreadSafeSet<T>::ThreadSafeSet()
{
  cout << "Hello World!" << endl;
  this->head = nullptr;
  this->tail = nullptr;
  size = 0;
}

template <typename T>
std::ostream &operator<<(std::ostream &os, const ThreadSafeSet<T> &set)
{
  Node<T> *temp = set.getHead();
  while (temp != nullptr)
  {
    os << temp->getData() << " ";
    temp = temp->getNext();
  }

  return os;
}

template <typename T>
bool ThreadSafeSet<T>::insert(const T &element)
{
  // insert to the end
  if (head == nullptr)
  {
    head = new Node<T>(element);
    tail = head;
    size++;
    return true;
  }
  else
  {
    cout << "head is not null" << endl;
    Node<T> *temp = head;
    while (temp != nullptr)
    {
      if (temp->getData() == element)
      {
        return false;
      }
      temp = temp->getNext();
    }
    tail->setNext(new Node<T>(element));
    tail = tail->getNext();
    size++;
    return true;
  }

  return false;
}