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

template <typename T>
ThreadSafeSet<T>::ThreadSafeSet(const ThreadSafeSet &other)
{
  this->head = other.head;
  this->tail = other.tail;
  this->size = other.size;
}

template <typename T>
ThreadSafeSet<T> &ThreadSafeSet<T>::operator=(const ThreadSafeSet &other)
{
  this->head = other.head;
  this->tail = other.tail;
  this->size = other.size;
  return *this;
}

template <typename T>
ThreadSafeSet<T>::~ThreadSafeSet()
{
  std::shared_ptr<Node<T>> temp = this->head;
  while (temp != nullptr)
  {
    std::shared_ptr<Node<T>> next = temp->next;
    temp = nullptr;
    temp = next;
  }
}

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
bool ThreadSafeSet<T>::operator<(const ThreadSafeSet &other) const
{
  shared_ptr<Node<T>> temp = this->getHead();
  shared_ptr<Node<T>> temp2 = other.head;
  bool result = true;
  while (temp != nullptr && temp2 != nullptr)
  {
    if (temp->data > temp2->data)
    {
      result = false;
      break;
    }
    temp = temp->next;
    temp2 = temp2->next;
  }

  return result;
}

template <typename T>
bool ThreadSafeSet<T>::operator==(const ThreadSafeSet &other) const
{
  shared_ptr<Node<T>> temp = this->getHead();
  shared_ptr<Node<T>> temp2 = other.head;
  bool result = true;
  while (temp != nullptr && temp2 != nullptr)
  {
    if (temp->data != temp2->data)
    {
      result = false;
      break;
    }
    temp = temp->next;
    temp2 = temp2->next;
  }

  return result;
}

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
    if (this->search(element))
    {
      return false;
    }
    // insert the element to the tail
    else
    {
      while (!std::atomic_compare_exchange_weak(&this->tail->next, &newNode->next, newNode))
        ;
      atomic_store(&this->tail, newNode);
      this->size++;
      return true;
    }
  }

  return true;
}

// remove
template <typename T>
bool ThreadSafeSet<T>::remove(const T &element)
{
  // check if the head is null
  if (!atomic_load(&this->head))
  {
    return false;
  }
  else
  {
    // check if the element is in the set
    if (!this->search(element))
    {
      return false;
    }
    else
    {
      // remove the element from the set
      shared_ptr<Node<T>> temp = this->head;
      shared_ptr<Node<T>> prev = nullptr;
      // use atomic functions
      while (temp != nullptr)
      {
        if (temp->data == element)
        {
          if (prev == nullptr)
          {
            while (!std::atomic_compare_exchange_weak(&this->head, &temp, temp->next))
              ;
            temp = nullptr;
            this->size--;
            return true;
          }
          else
          {
            while (!std::atomic_compare_exchange_weak(&prev->next, &temp, temp->next))
              ;
            temp = nullptr;
            this->size--;
            return true;
          }
        }
        prev = temp;
        temp = temp->next;
      }
    }
  }

  return true;
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

template <typename T>
bool ThreadSafeSet<T>::search(const T &element) const
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
