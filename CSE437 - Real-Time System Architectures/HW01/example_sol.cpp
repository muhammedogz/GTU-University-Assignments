#include <iostream>
#include <atomic>
#include <thread>
#include <vector>
#define N 1000
using namespace std;
class lf_queue
{
private:
  struct node
  {
    int data;
    atomic<node *> next;
    node(int d) : data(d)
    {
    }
  };
  atomic<node *> Head;
  atomic<node *> Tail;

public:
  lf_queue()
  {
    node *nnode = new node(-1);
    nnode->next = NULL;
    Head = nnode;
    Tail = nnode;
  }
  void enqueue(int data)
  {
    node *nnode = new node(data);
    nnode->next = NULL;
    node *tail, *next_p;
    while (true)
    {
      tail = Tail.load();
      next_p = tail->next;
      if (tail == Tail.load())
      {
        if (next_p == NULL)
        {
          if ((tail->next).compare_exchange_weak(next_p, nnode))
            break;
        }
        else
        {
          Tail.compare_exchange_weak(tail, next_p);
        }
      }
    }
    Tail.compare_exchange_weak(tail, nnode);
  }
  bool dequeue(int &res)
  {
    while (true)
    {
      node *head, *tail, *next_p;
      head = Head.load();
      tail = Tail.load();
      next_p = head->next;
      if (head == Head.load())
      {
        if (head == tail)
        {
          if (next_p == NULL)
            return false;
          Tail.compare_exchange_weak(tail, next_p);
        }
        else
        {
          res = next_p->data;
          if (Head.compare_exchange_weak(head, next_p))
            break;
        }
      }
    } // end loop
    return true;
  }
};
void producer(lf_queue &q)
{ // cout<<this_thread::get_id()<<"Inside producer\n";
  for (int i = 0; i < N; i++)
  {
    q.enqueue(1);
  }
  // cout<<this_thread::get_id()<<" "<<"Finished producing\n";
}
void consumer(lf_queue &q, atomic<int> &sum)
{ // cout<<this_thread::get_id()<<" "<<"Inside consumer\n";
  for (int i = 0; i < N; i++)
  {
    int res = 0;
    while (!q.dequeue(res))
      ;
    sum += res;
  }
  // cout<<this_thread::get_id()<<" "<<"Finished consuming\n";
}
int main()
{
  lf_queue Q;
  atomic<int> sum;
  sum.store(0);
  vector<thread> thread_pool;
  for (int i = 0; i < 10; i++)
  {
    if (i % 2 == 0)
    {
      thread t(consumer, ref(Q), ref(sum));
      thread_pool.push_back(move(t));
    }
    else
    {
      thread t(producer, ref(Q));
      thread_pool.push_back(move(t));
    }
  }
  for (int i = 0; i < thread_pool.size(); i++)
    thread_pool[i].join();
  cout << "Final sum " << sum.load() << "\n";
  return 0;
}