#include "../include/ThreadSafeSet.hpp"
#include "../include/Node.hpp"
#include <iostream>
#include <ostream>
#include <atomic>
#include <memory>

using namespace std;

template <typename T>
ThreadSafeSet<T>::ThreadSafeSet()
{
  cout << "Hello World!" << endl;
  this->head = nullptr;
  this->tail = nullptr;
  size = 0;
}

// big five
template <typename T>
ThreadSafeSet<T>::ThreadSafeSet(const ThreadSafeSet &other)
{
  this->head = other.head;
  this->tail = other.tail;
  this->size = other.size;
}

// overload =
template <typename T>
ThreadSafeSet<T> &ThreadSafeSet<T>::operator=(const ThreadSafeSet &other)
{
  this->head = other.head;
  this->tail = other.tail;
  this->size = other.size;
  return *this;
}

// destructor
template <typename T>
ThreadSafeSet<T>::~ThreadSafeSet()
{
  std::shared_ptr<Node<T>> temp = this->head;
  // delete all nodes
  while (temp != nullptr)
  {
    std::shared_ptr<Node<T>> next = temp->next;
    temp = nullptr;
    temp = next;
  }
}

// move constructor
template <typename T>
ThreadSafeSet<T>::ThreadSafeSet(ThreadSafeSet &&other)
{
  this->head = other.head;
  this->tail = other.tail;
  this->size = other.size;
  other.head = nullptr;
  other.tail = nullptr;
  other.size = 0;
}

// move assignment
template <typename T>
ThreadSafeSet<T> &ThreadSafeSet<T>::operator=(ThreadSafeSet &&other)
{
  this->head = other.head;
  this->tail = other.tail;
  this->size = other.size;
  other.head = nullptr;
  other.tail = nullptr;
  other.size = 0;
  return *this;
}

template <typename T>
std::ostream &operator<<(std::ostream &os, const ThreadSafeSet<T> &set)
{
  std::shared_ptr<Node<T>> temp = set.head;

  while (temp != nullptr)
  {
    os << temp->data << " ";
    temp = temp->next;
  }

  return os;
}

// contains
template <typename T>
bool ThreadSafeSet<T>::contains(const T &element) const
{
  std::shared_ptr<Node<T>> temp = this->head;

  while (temp != nullptr)
  {
    if (temp->data == element)
    {
      return true;
    }
    temp = temp->next;
  }

  return false;
}

// atomic insert
template <typename T>
bool ThreadSafeSet<T>::insert(const T &element)
{
  // use make_unique to create a new node without auto
  const std::shared_ptr<Node<T>> newNode = make_shared<Node<T>>(element);

  // check if the head is null
  if (!atomic_load(&this->head))
  {
    while (!std::atomic_compare_exchange_weak(&this->head, &newNode->next, newNode))
      ;
    atomic_store(&this->tail, newNode);
    this->size++;
    return true;
  }
  else
  {
    // check if the element is already in the set
    if (this->contains(element))
    {
      return false;
    }
    // insert the element to the tail
    else
    {
      newNode->prev = this->tail;
      while (!std::atomic_compare_exchange_weak(&this->tail->next, &newNode->next, newNode))
        ;
      atomic_store(&this->tail, newNode);
      this->size++;
      return true;
    }
  }

  return true;
}
