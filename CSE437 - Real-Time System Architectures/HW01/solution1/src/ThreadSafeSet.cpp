#include "../include/ThreadSafeSet.hpp"
#include "../include/Node.hpp"
#include <iostream>
#include <ostream>
#include <atomic>
#include <functional>
#include <memory>

using namespace std;

template <typename T>
ThreadSafeSet<T>::ThreadSafeSet()
{
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
  // Create a new empty set
  ThreadSafeSet<T> set;

  // Iterate over the nodes in the other set and insert them into the new set
  auto current = other.head;
  while (current)
  {
    set.insert(current->data);
    current = current->next;
  }

  // Swap the contents of the new set with the current set
  std::swap(head, set.head);
  std::swap(tail, set.tail);
  std::swap(size, set.size);

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
bool ThreadSafeSet<T>::insert(const T &element)
{

  // cout << "inserting " << element << endl;

  // use make_shared to create a new node without auto
  const std::shared_ptr<Node<T>> newNode = std::make_shared<Node<T>>(element);

  // check if the head is null
  if (!std::atomic_load(&this->head))
  {
    while (!std::atomic_compare_exchange_weak(&this->head, &this->head, newNode))
      ;
    std::atomic_store(&this->tail, newNode);
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
      std::shared_ptr<Node<T>> temp = std::atomic_load(&this->head);
      std::shared_ptr<Node<T>> prev = nullptr;
      // use atomic functions
      while (temp != nullptr)
      {
        if (temp->data > element)
        {
          if (prev == nullptr)
          {
            while (!std::atomic_compare_exchange_weak(&this->head, &temp, newNode))
              ;
            newNode->next = temp;
            this->size++;
            return true;
          }
          else
          {
            while (!std::atomic_compare_exchange_weak(&prev->next, &temp, newNode))
              ;
            newNode->next = temp;
            this->size++;
            return true;
          }
        }
        prev = temp;
        temp = temp->next;
      }
      while (!std::atomic_compare_exchange_weak(&this->tail->next, &this->tail->next, newNode))
        ;
      std::atomic_store(&this->tail, newNode);
      this->size++;
      return true;
    }
  }

  return true;
}

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
  std::shared_ptr<Node<T>> temp = std::atomic_load(&set.head);
  while (temp != nullptr)
  {
    os << temp->data << " ";
    temp = temp->next;
  }
  os << std::endl;
  return os;
}

template <typename T>
bool ThreadSafeSet<T>::search(const T &element) const
{
  std::shared_ptr<Node<T>> temp = std::atomic_load(&this->head);
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

template <typename T>
void ThreadSafeSet<T>::clear()
{
  // Set the head and tail pointers to nullptr
  this->head = nullptr;
  this->tail = nullptr;

  // Set the size to 0
  this->size = 0;
}

template <typename T>
void ThreadSafeSet<T>::iterate(const std::function<void(const T &)> &f)
{
  // Start at the head of the set
  std::shared_ptr<Node<T>> curr = this->head;

  // Iterate over all elements in the set
  while (curr != nullptr)
  {
    // Call the iteration code for the current element
    f(curr->data);

    // Move to the next element
    curr = curr->next;
  }
}