/**
 * @file ThreadSafeSet.hpp
 * @author Muhammed Oğuz (m.oguz2018@gtu.edu.tr)
 * @brief ThreadSafeSet class definitions
 * @date 2022-11-13
 *
 */

#include <ostream>
#include "Node.hpp"
// ThreadSafeSet.hpp

#pragma once
// ThreadSafeSet should be a thread-safe lock-free set.
template <typename T>
class ThreadSafeSet
{
public:
  ThreadSafeSet();
  // big five
  ThreadSafeSet(const ThreadSafeSet &other);
  ThreadSafeSet &operator=(const ThreadSafeSet &other);
  ~ThreadSafeSet();
  ThreadSafeSet(ThreadSafeSet &&other);
  ThreadSafeSet &operator=(ThreadSafeSet &&other);

  // Inserts an element into the set.
  // Returns true if the element was inserted, false if it was already there.
  bool insert(const T &element);

  // Removes an element from the set.
  // Returns true if the element was removed, false if it was not there.
  bool remove(const T &element);

  // Checks if an element is in the set.
  // Returns true if the element is in the set, false otherwise.
  bool contains(const T &element) const;

  // overload << operator to print the set
  template <typename U>
  friend std::ostream &operator<<(std::ostream &os, const ThreadSafeSet &set);

  inline void setHead(Node<T> *h) { this->head = h; }
  inline Node<T> *getHead() const { return this->head; }
  inline void setTail(Node<T> *t) { this->tail = t; }
  inline Node<T> *getTail() const { return this->tail; }
  inline void setSize(int s) { this->size = s; }
  inline int getSize() const { return this->size; }

private:
  Node<T> *head;
  Node<T> *tail;
  int size;
};
