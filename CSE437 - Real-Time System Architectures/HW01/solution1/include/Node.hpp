#include <memory>

#pragma once

template <typename T>
class Node
{
public:
  Node(T d) : data(d)
  {
    this->next = nullptr;
    this->prev = nullptr;
  }

  T data;
  std::shared_ptr<Node<T>> next;
  std::shared_ptr<Node<T>> prev;
};