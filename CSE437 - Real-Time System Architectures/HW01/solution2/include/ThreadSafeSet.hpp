/**
 * @file ThreadSafeSet.hpp
 * @author Muhammed Oğuz (m.oguz2018@gtu.edu.tr)
 * @date 2022-11-13
 * @brief ThreadSafeSet class definitions
 * @definition: The code is a definition of a template class called ThreadSafeSet, which is a thread-safe lock-free set. A set is a collection of unique elements, and a lock-free set is a set that can be accessed by multiple threads without the use of locks (i.e., it can be accessed concurrently without the need for synchronization).
 */

#include <ostream>
#include <atomic>
#include <functional>
#include <mutex>
#include "Node.hpp"
// ThreadSafeSet.hpp

using namespace std;

#pragma once
// ThreadSafeSet should be a thread-safe lock-free set.
template <typename T>
class ThreadSafeSet
{
public:
  /**
   * @brief Construct a new Thread Safe Set object
   *
   */
  ThreadSafeSet();

  /* Big Five */

  /**
   * @brief Construct a new Thread Safe Set object
   *
   * @param other
   */
  ThreadSafeSet(const ThreadSafeSet &other);

  /**
   * @brief Copy assignment operator
   *
   * @param other
   * @return ThreadSafeSet&
   */
  ThreadSafeSet &operator=(const ThreadSafeSet &other);

  /**
   * @brief Destroy the Thread Safe Set object
   *
   */
  ~ThreadSafeSet();
  /**
   * @brief Construct a new Thread Safe Set object, Move constructor
   *
   * @param other
   */
  ThreadSafeSet(ThreadSafeSet &&other);

  /**
   * @brief Move assignment operator
   *
   * @param other
   * @return ThreadSafeSet&
   */
  ThreadSafeSet &operator=(ThreadSafeSet &&other);

  /**
   * @brief insert a new element to the set
   *
   * @param element
   * @return true if the element is inserted
   * @return false if the element is already in the set
   */
  bool insert(const T &element);

  /**
   * @brief Delete an element from the set
   *
   * @param element
   * @return true if the element is deleted
   * @return false if the element is not in the set
   */
  bool remove(const T &element);

  /**
   * @brief clear the set
   *
   */
  void clear();

  /**
   * @brief check if the element is in the set
   *
   * @param element
   * @return true if the element is in the set
   * @return false if the element is not in the set
   */
  bool search(const T &element) const;

  /**
   * @brief Iterate  
   * 
   */
  void iterate(const std::function<void(const T &)> &f);

  template <typename U>
  friend std::ostream &operator<<(std::ostream &os, const ThreadSafeSet<U> &set);

  // Inline functions
  inline void setHead(shared_ptr<Node<T>> h) { this->head = h; }
  inline shared_ptr<Node<T>> getHead() const { return this->head; }
  inline void setTail(shared_ptr<Node<T>> t) { this->tail = t; }
  inline shared_ptr<Node<T>> getTail() const { return this->tail; }
  inline void setSize(int s) { this->size = s; }
  inline int getSize() const { return this->size; }

  // Mutex for synchronizing access to the set
  mutable std::mutex mutex;

private:
  shared_ptr<Node<T>> head;
  shared_ptr<Node<T>> tail;
  int size;
};
