package CSE222_hw03.interface_oguz;

import java.util.ListIterator;

public interface IKWLinkedList<E> {

    /**
     * Add element desired index
     * @param index which index gonna used
     * @param item which item gonna ad
     * @throws IndexOutOfBoundsException out of the box
     */
    void add(int index, E item) throws IndexOutOfBoundsException;
    
    /**
     * Add to first index 
     * @param item which item gonna add
     */
    void addFirst(E item);

    /**
     * Add to last index
     * @param item which item gonna add
     */
    void addLast(E item);

    /**
     * Get specific element 
     * @param index which index gonna use
     * @return desired element
     * @throws IndexOutOfBoundsException out of the box
     */
    E get(int index) throws IndexOutOfBoundsException;

    /**
     * Return first element
     * @return first element
     */
    public E getFirst();

    /**
     * Return last element
     * @return last element
     */
    public E getLast();

    /**
     * Return size
     * @return size
     */
    int size();

    /**
     * Search given item if exist or not
     * @param item which item gonna searched
     * @return true if contains, false otherwise
     */
    boolean contains(E item);

    /** Remove last element */
    void remove();
    
    /**
     * Remove specific element 
     * @param index which element gonna removed
     * @throws IndexOutOfBoundsException out of the box
     */
    void remove(int index) throws IndexOutOfBoundsException;
    
    /**
     * Remove given element
     * @param element return removed element
     */
    void remove(E element);
    
    /** Clear all data */
    void clear();
    
    /**
     * Create iterator
     * @param index which index gonna be start point for iterator
     * @return iterator ref
     * @throws IndexOutOfBoundsException out of the box
     */
    ListIterator<E> listIterator(int index) throws IndexOutOfBoundsException;

}
