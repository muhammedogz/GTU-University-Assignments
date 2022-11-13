#pragma once

template <typename T>
class Node
{
public:
  Node(T d) : data(d) {}
  inline void setData(T d) { this->data = d; }
  inline T getData() const { return this->data; }
  inline void setNext(Node<T> *n) { this->next = n; }
  inline Node<T> *getNext() const { return this->next; }
  inline void setPrev(Node<T> *p) { this->prev = p; }
  inline Node<T> *getPrev() const { return this->prev; }

private:
  T data;
  Node *next;
  Node *prev;
};