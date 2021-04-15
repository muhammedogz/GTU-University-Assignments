package CSE222_hw01.interface_oguz;

public interface IArrayContainer<E> {
    /**
     * Add element to array.
     * @param e
     */
    void add(E e);

    /**
     * Search array, if this element is in the array.
     * @param e Searched element
     * @return true if find. False otherwise.
     */
    boolean contains(E e);

    /**
     * Remove selected element.
     * @param e Selected element.
     * @return true if removing success. False otherwise.
     */
    boolean remove(E e);

    /**
     * @return size of the array.
     */
    int size();

    /**
     * Get element at selected index.
     * @param index Selected index.
     * @return Element at selected index. If not exist. Return null.
     */
    E get(int index);
}
