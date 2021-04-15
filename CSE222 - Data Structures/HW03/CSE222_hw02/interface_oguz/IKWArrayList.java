package CSE222_hw02.interface_oguz;

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
     */
    void add(int index, E item);

    /**
     * Get element from a specific index
     * @param index which index gonna used
     * @return desired element if exist, null otherwise
     */
    E get(int index);

    /**
     * Set new value to given index element
     * @param index which index gonna set
     * @param item which item gonna replaced.
     * @return old element. if success, null otherwise
     */
    E set(int index, E item);

    /**
     * Remove element from specific index
     * @param index which index gonna used
     * @return removed element if success. null otherwise
     */
    E remove(int index);

    /**
     * Remove given element from list
     * @param item which element will be removed
     * @return true if success, false otherwise
     */
    boolean remove(E item);

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
