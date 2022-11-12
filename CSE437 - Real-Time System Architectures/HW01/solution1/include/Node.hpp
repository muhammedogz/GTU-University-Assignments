#pragma once

template <typename T>
struct Node
{
  T data;
  Node *next;
  Node(T d) : data(d)
  {
  }
};