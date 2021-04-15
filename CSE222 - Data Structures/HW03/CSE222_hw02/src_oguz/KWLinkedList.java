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
    public E get(int index) throws IndexOutOfBoundsException {
        return listIterator(index).next(); 
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void remove() {
        if (tail == null)
            throw new IllegalStateException();
        listIterator(size-1).remove();
    }

    @Override
    public void remove(int index) throws IndexOutOfBoundsException {
        listIterator(index).remove();
        
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
        // TODO Auto-generated method stub
        
    }

    @Override
    public ListIterator<E> listIterator(int index) throws IndexOutOfBoundsException {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String toString() {
        // TODO Auto-generated method stub
        return super.toString();
    }

    private static class Node<E> {
        // Data Fields
        private Node<E> prev = null; /* The reference previous node*/
        private E data;     /** The reference to the data. */
        private Node<E> next; /** The reference to the next node. */

        // Constructors
        /** Creates a new node with a null next field.
            @param dataItem The data stored
        */
        private Node(E dataItem) {
            data = dataItem;
            next = null;
        }
        /** Creates a new node that references another node.
        @param dataItem The data stored
        @param nodeRef The node referenced by new node
        */
        private Node(E dataItem, Node<E> nodeRef) {
            data = dataItem;
            next = nodeRef;
        }
    } //End of Node class.

    /** Inner class to implement the ListIterator interface. */
    private class KWListIter implements ListIterator<E> {
        private Node<E> nextItem;   /** A reference to the next item. */
        private Node<E> lastItemReturned;    /** A reference to the last item returned. */
        private int index = 0;  /** The index of the current item. */

        /** Construct a KWListIter that will reference the ith item.
            @param i The index of the item to be referenced
        */
        public KWListIter(int i) {
            // Validate i parameter.
            if (i < 0 || i > size) {
                throw new IndexOutOfBoundsException("Invalid index " + i);
            }
            lastItemReturned = null; // No item returned yet.
            // Special case of last item.
            if (i == size) {
                index = size;
                nextItem = null;
            }
            else { // Start at the beginning
                nextItem = head;
                for (index = 0; index < i; index++) {
                    nextItem = nextItem.next;
                }
            }
        }

        /**Returns previous index */
        public int previousIndex(){
            return index - 1;    
        }
        /**Returns index number */
        public int nextIndex(){
            return index;    
        }


        /** Indicate whether movement forward is defined.
            @return true if call to next will not throw an exception
        */
        public boolean hasNext() {
            return nextItem != null;
        }

        /** Move the iterator forward and return the next item.
            @return The next item in the list
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

        /** Indicate whether movement backward is defined.
            @return true if call to previous will not throw an exception
        */
        public boolean hasPrevious() {
            return (nextItem == null && size != 0) || nextItem.prev != null;
        }

        /** Move the iterator backward and return the previous item.
            @return The previous item in the list
            @throws NoSuchElementException if there is no such object
        */
        public E previous() {
            if (!hasPrevious()) {
                throw new NoSuchElementException();
            }
            if (nextItem == null) { // Iterator is past the last element
                nextItem = tail;
            }
            else{
                nextItem = nextItem.prev;
            }
            lastItemReturned = nextItem;
            index--;
            return lastItemReturned.data;
        }

        /** Add a new item between the item that will be returned
            by next and the item that will be returned by previous.
            If previous is called after add, the element added is
            returned.
            @param obj The item to be inserted
        */
        public void add(E obj) {
            if (head == null) { // Add to an empty list.
                head = new Node<E>(obj);
                tail = head;
            } 
            else if (nextItem == head) { // Insert at head.
                // Create a new node.
                Node<E> newNode = new Node<E>(obj);
                // Link it to the nextItem.
                newNode.next = nextItem; // Step 1
                // Link nextItem to the new node.
                nextItem.prev = newNode; // Step 2
                // The new node is now the head.
                head = newNode; // Step 3
            }
            else if (nextItem == null) { // Insert at tail.
                // Create a new node.
                Node<E> newNode = new Node<E>(obj);
                // Link the tail to the new node.
                tail.next = newNode; // Step 1
                // Link the new node to the tail.
                newNode.prev = tail; // Step 2
                // The new node is the new tail.
                tail = newNode; // Step 3
            } 
            else { // Insert into the middle.
                // Create a new node.
                Node<E> newNode = new Node<E>(obj);
                // Link it to nextItem.prev.
                newNode.prev = nextItem.prev; // Step 1
                nextItem.prev.next = newNode; // Step 2
                // Link it to the nextItem.
                newNode.next = nextItem; // Step 3
                nextItem.prev = newNode; // Step 4
            }

            // Increase size and index and set lastItemReturned.
            size++;
            index++;
            lastItemReturned = null;
        } // End of method add.

        public void set(E obj){ //throws IllegalStateException
            if (lastItemReturned == null)
                throw new IllegalStateException();
            else
                lastItemReturned.data = obj;
        }

        public void remove(){
            if(lastItemReturned != null){
                lastItemReturned.next.prev = lastItemReturned.prev;
                lastItemReturned.prev.next = lastItemReturned.next;
                lastItemReturned = null;
            }
            else{
                tail = tail.prev;
               // tail.next = null;
                size--;
            }
        }
    }
    
}
