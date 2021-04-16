package CSE222_hw03.interface_oguz;

public interface IHybridList<E> {
    
    /**
     * Add new item
     * @param item which item gonna add
     */
    void add(E item);

    /**
     * Search given item if exist or not
     * @param item which item gonna searched
     * @return true if contains, false otherwise
     */
    boolean contains(E item);

    /**
     * Remove item from given index
     * @param index from which index element gonna removed
     */
    void remove(int index);

    /**
     * Remove given element if exist
     */
    void remove(E item);

    /** 
     * Return element from given index
     * @param index which index gonna use
     * @return return if item exist
     */
    E get(int index);

    /**
     * Return size
     * @return size
     */
    int size();

    /**
     * Clear content
     */
    void clear();

    /**
     * Check if array size equal to max_length
     * @return true if full, false otherwise
     */
    boolean isArrayFull();

}
