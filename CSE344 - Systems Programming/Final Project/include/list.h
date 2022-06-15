#ifndef LIST_H
#define LIST_H

typedef struct Node
{
  struct Node *next;
  struct Node *prev;
  void *data;
} Node;

typedef struct List
{
  Node *head;
  Node *tail;
  int size;
} List;

/**
 * @brief Initialize of the list
 *
 * @return List*
 */
List *initializeList();

/**
 * @brief Add new element to the end of the list
 *
 * @param list List to add element to
 * @param data Data to add
 */
void addNode(List *list, void *data);

/**
 * @brief Add element to head of the list
 *
 * @param list List to add element to
 * @param data Data to add
 */
void addNodeHead(List *list, void *data);

/**
 * @brief Remove element from the list
 *
 * @param list List to remove element from
 */
void *removeNode(List *list);

/**
 * @brief Remove the element from beginning of the list
 *
 * @param list list to remove element from
 */
void *removeHeadNode(List *list);

/**
 * @brief Sort list by comparing two elements
 *
 * @param list List to sort
 * @param compare Function to compare two elements
 */
void sortList(List *list, int (*compare)(void *, void *));

/**
 * @brief Print the list
 *
 * @param list List to print
 * @param print Function to print element
 */
void printList(List *list, void (*print)(void *));

/**
 * @brief Get the List In Range object
 *
 * @param list List to get range from
 * @param start Start index
 * @param end End index
 * @return List* List in range
 */
List *getListInRangeString(List *list, int start, int end);

/**
 * @brief Free the list
 *
 * @param list List to free
 * @param freeData Function to free data
 */
void freeList(List *list, void (*freeData)(void *));

/**
 * @brief Get the first element of the list
 *
 * @param list List to get element from
 * @return void* First element
 */
void *listGetFirst(List *list);

/**
 * @brief Get the last element of the list
 *
 * @param list List to get element from
 * @return void*
 */
void *listGetLast(List *list);

/**
 * @brief print the given void pointer as string
 *
 * @param data void pointer to print
 */
void printString(void *data);

/**
 * @brief Compare given to string
 *
 * @param data1 void pointer to compare
 * @param data2 void pointer to compare
 * @return int
 */
int compareString(void *data1, void *data2);

/**
 * @brief Free the given void pointer
 *
 * @param data void pointer to free
 */
void freeString(void *data);

#endif