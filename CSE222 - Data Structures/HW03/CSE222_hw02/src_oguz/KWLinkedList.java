package CSE222_hw02.src_oguz;

import java.util.ListIterator;
import java.util.NoSuchElementException;

import CSE222_hw02.interface_oguz.IKWLinkedList;

public class KWLinkedList<E> implements IKWLinkedList<E> {
    /* Data Fields */
    private Node<E> head = null;
    private Node<E> tail = null;
    private int size = 0;


    @Override
    public void add(int index, E item) throws IndexOutOfBoundsException {
        listIterator(index).add(item);      
    }

    @Override
    public void addFirst(E item){ 
        add(0, item);  
    }
    
    @Override
    public void addLast(E item){ 
        add(size, item);  
    }

    @Override
    public E get(int index) throws IndexOutOfBoundsException {
        return listIterator(index).next(); 
    }

    @Override
    public E getFirst(){ 
        return head.data;  
    }

    @Override
    public E getLast(){ 
        return tail.data;  
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void remove() {
        if (tail == null)
            throw new IllegalStateException();
        KWListIter iter = (KWLinkedList<E>.KWListIter) listIterator(size-1);
        iter.next(); iter.remove();
    }

    @Override
    public void remove(int index) throws IndexOutOfBoundsException {
        KWListIter iter = (KWLinkedList<E>.KWListIter) listIterator(index);
        iter.next(); iter.remove();     
    }

    @Override
    public void remove(E item) {
        if (tail == null)
            throw new IllegalStateException();
        KWListIter iter = (KWLinkedList<E>.KWListIter) listIterator(0);

        while(iter.hasNext())
        {
            if (iter.next().equals(item))
                iter.remove();
        }
    }

    @Override
    public void clear() {
        head = null;
        tail = null;
        size = 0;
    }

    @Override
    public ListIterator<E> listIterator(int index) throws IndexOutOfBoundsException {
        return new KWListIter(index);
    }

    @Override
    public String toString() {
        String str = "[";
        KWListIter iter = (KWLinkedList<E>.KWListIter) listIterator(0);

        while(iter.hasNext())
        {
            str += iter.next().toString() + ",";
        }
        str = str.substring(0, str.length() - 1);
        str += "]";
        return str;
    }


    /** Implement node class */
    private static class Node<E> {
        private Node<E> prev = null;
        private Node<E> next = null;
        private E data;

        /** Create new node
            @param item which item gonna stored
        */
        private Node(E item) {
            data = item;
            next = null;
        }
        /** Create new node with referencing other
        @param item which item gonna stored
        @param nodeRef node ref
        */
        private Node(E item, Node<E> nodeRef) {
            data = item;
            next = nodeRef;
        }
    }

    /** implement the ListIterator interface */
    private class KWListIter implements ListIterator<E> {
        private Node<E> nextItem = null;
        private Node<E> lastItemReturned = null;
        private int index = 0;

        /** ref to indexth item
            @param index which item gonna referenced
        */
        public KWListIter(int index) {
            if (index < 0 || index > size) 
                throw new IndexOutOfBoundsException(index);

            // if last item selected
            if (index == size) 
            {
                index = size;
                nextItem = null;
            }
            else 
            {
                nextItem = head;
                for (this.index = 0; this.index < index; this.index++)
                    nextItem = nextItem.next;
            }
        }

        /** Return previous index */
        public int previousIndex(){
            return index - 1;    
        }
        /** Return index number */
        public int nextIndex(){
            return index;    
        }

        /** Look for next item is null or not
         * @return true if not null
        */
        public boolean hasNext() {
            return nextItem != null;
        }

        /** check if previous node exist
         * @return true if exist
        */
        public boolean hasPrevious() {
            return (nextItem == null && size != 0) || nextItem.prev != null;
        }

        /** Move to next and return this item
            @return return item if exist
            @throws NoSuchElementException if there is no such object
        */
        public E next() {
            if (!hasNext()) 
                throw new NoSuchElementException();

            lastItemReturned = nextItem;
            nextItem = nextItem.next;
            index++;
            return lastItemReturned.data;
        }

        /** Go back iterator 
            @return previous data
            @throws NoSuchElementException if there is no such object
        */
        public E previous() {
            if (!hasPrevious())
                throw new NoSuchElementException();

            if (nextItem == null)
                nextItem = tail;
            else
                nextItem = nextItem.prev;

            lastItemReturned = nextItem;
            index--;
            return lastItemReturned.data;
        }

        /** Add item to returned ref from next or previous 
            @param item which item gonna add
        */
        public void add(E item) {
            if (head == null) // if empty list
            {
                head = new Node<E>(item);
                tail = head;
            } 
            else if (nextItem == head) // if add after head
            {
                Node<E> newNode = new Node<E>(item);
                newNode.next = nextItem;
                nextItem.prev = newNode;
                head = newNode;
            }
            else if (nextItem == null) // if add tail 
            {
                Node<E> newNode = new Node<E>(item);
                tail.next = newNode;
                newNode.prev = tail;
                tail = newNode;
            } 
            else // if add middle
            {
                Node<E> newNode = new Node<E>(item);
                newNode.prev = nextItem.prev;
                nextItem.prev.next = newNode;
                newNode.next = nextItem;
                nextItem.prev = newNode;
            }

            size++;
            index++;
            lastItemReturned = null;
        }

        /** Set data to lastItemReturned from next or previous */
        public void set(E item)
        {
            if (lastItemReturned == null)
                throw new IllegalStateException();
            else
                lastItemReturned.data = item;
        }

        /** Remove item which returned next or previous */
        public void remove(){
            if(lastItemReturned != null)
            {
                lastItemReturned.next.prev = lastItemReturned.prev;
                lastItemReturned.prev.next = lastItemReturned.next;
                lastItemReturned = null;
            }
            else
                tail = tail.prev;
            size--;
        }
    }
    
}
