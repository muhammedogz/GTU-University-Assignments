package CSE222_hw03.interface_oguz;

import CSE222_hw03.src_oguz.KWArrayList;

public interface IKWArrayList<E> {
    
    /**
     * Add item to list
     * @param item which item gonna end
     * @return true if success
     */
    boolean add(E item);

    /**
     * add element to specific index
     * @param index which index gonna used
     * @param item which item gonna add
     * @throws ArrayIndexOutOfBoundsException if index out of the box
     */
    void add(int index, E item)throws ArrayIndexOutOfBoundsException;

    /**
     * Get element from a specific index
     * @param index which index gonna used
     * @return desired element if exist, null otherwise
     * @throws ArrayIndexOutOfBoundsException if index out of the box
     */
    E get(int index)throws ArrayIndexOutOfBoundsException;

    /**
     * Get desired item
     * @param item which item gonna returned
     * @return item
     */
    E get(E item);

    /**
     * Set new value to given index element
     * @param index which index gonna set
     * @param item which item gonna replaced.
     * @return old element. if success, null otherwise
     * @throws ArrayIndexOutOfBoundsException if index out of the box
     */
    E set(int index, E item)throws ArrayIndexOutOfBoundsException;

    /**
     * Search given item if exist or not
     * @param item which item gonna searched
     * @return true if contains, false otherwise
     */
    boolean contains(E item);

    /**
     * Look bunch of items if they exist
     * @param items which items gonna searched
     * @return true if contains, false otherwise
     */
    boolean containsAll(KWArrayList<E> items);

    /**
     * Remove element from specific index
     * @param index which index gonna used
     * @return removed element if success. null otherwise
     * @throws ArrayIndexOutOfBoundsException if index out of the box
     */
    E remove(int index) throws ArrayIndexOutOfBoundsException;

    /**
     * Remove given element from list
     * @param item which element will be removed
     * @return true if success, false otherwise
     */
    boolean remove(E item);

    /**
     * Remove all elements from array 
     * @param items which elements gonna removed
     * @return true if success, false otherwise
     */
    boolean removeAll(KWArrayList<E> items);

    /** Clear content of the List */
    void clear();

    /**
     * Return size of the list
     * @return size
     */
    int size();

    /**
     * Convert whole list to string
     * @return converted string.
     */
    String toString();



}
