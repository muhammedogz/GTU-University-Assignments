#include "../include/list.h"
#include "../include/common.h"

List *initializeList()
{
  List *list = malloc(sizeof(List));
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

void addNode(List *list, void *data)
{
  Node *node = (Node *)malloc(sizeof(Node));
  node->data = data;
  node->next = NULL;
  node->prev = NULL;
  if (list->head == NULL)
  {
    list->head = node;
    list->tail = node;
  }
  else
  {
    list->tail->next = node;
    node->prev = list->tail;
    list->tail = node;
  }
  list->size++;
}

// add list node to the beginning of the list
void addNodeHead(List *list, void *data)
{
  Node *node = (Node *)malloc(sizeof(Node));
  node->data = data;
  node->next = NULL;
  node->prev = NULL;
  if (list->head == NULL)
  {
    list->head = node;
    list->tail = node;
  }
  else
  {
    node->next = list->head;
    list->head->prev = node;
    list->head = node;
  }
  list->size++;
}

void *removeNode(List *list)
{
  if (list->head == NULL)
  {
    return NULL;
  }
  Node *node = list->tail;
  if (list->tail == list->head)
  {
    list->head = NULL;
    list->tail = NULL;
  }
  else
  {
    list->tail = list->tail->prev;
    list->tail->next = NULL;
  }
  list->size--;
  void *data = node->data;
  free(node);
  return data;
}

void *removeHeadNode(List *list)
{
  if (list->head == NULL)
  {
    return NULL;
  }
  Node *node = list->head;
  if (list->tail == list->head)
  {
    list->head = NULL;
    list->tail = NULL;
  }
  else
  {
    list->head = list->head->next;
    list->head->prev = NULL;
  }
  list->size--;
  void *data = node->data;
  free(node);
  return data;
}

void sortList(List *list, int (*compare)(void *, void *))
{
  if (list->head == NULL)
  {
    return;
  }
  Node *node = list->head;
  while (node->next != NULL)
  {
    if (compare(node->data, node->next->data) > 0)
    {
      void *data = node->data;
      node->data = node->next->data;
      node->next->data = data;
      node = list->head;
    }
    else
    {
      node = node->next;
    }
  }
}

void printList(List *list, void (*print)(void *))
{
  if (list->head == NULL)
  {
    return;
  }
  Node *node = list->head;
  while (node != NULL)
  {
    print(node->data);
    node = node->next;
  }
}

List *getListInRangeString(List *list, int start, int end)
{
  if (list->head == NULL)
  {
    return NULL;
  }
  List *newList = initializeList();

  Node *node = list->head;
  int i = 0;
  while (node != NULL)
  {
    if (i >= start && i <= end)
    {
      char *temp = malloc(sizeof(char) * (strlen(node->data) + 1));
      strcpy(temp, node->data);
      addNode(newList, temp);
    }
    node = node->next;
    i++;
  }

  return newList;
}

void freeList(List *list, void (*freeData)(void *))
{
  if (list->head == NULL)
  {
    return;
  }
  Node *node = list->head;
  while (node != NULL)
  {
    Node *next = node->next;
    if (freeData != NULL)
    {
      freeData(node->data);
    }
    if (node != NULL)
    {
      free(node);
    }
    node = next;
  }

  if (list != NULL)
  {
    free(list);
  }
}

void freeListExceptData(List* list)
{
  if (list->head == NULL)
  {
    return;
  }
  Node *node = list->head;
  while (node != NULL)
  {
    Node *next = node->next;
    if (node != NULL)
    {
      free(node);
    }
    node = next;
  }

  if (list != NULL)
  {
    free(list);
  }
}

void *listGetFirst(List *list)
{
  if (list->head == NULL)
  {
    return NULL;
  }
  return list->head->data;
}

void *listGetLast(List *list)
{
  if (list->head == NULL)
  {
    return NULL;
  }
  Node *node = list->tail;
  return node->data;
}

void printString(void *data)
{
  dprintf(STDOUT_FILENO, "%s\n", (char *)data);
}

int compareString(void *data1, void *data2)
{
  return strcmp((char *)data1, (char *)data2);
}

void freeString(void *data)
{
  if (data != NULL)
    free(data);
}