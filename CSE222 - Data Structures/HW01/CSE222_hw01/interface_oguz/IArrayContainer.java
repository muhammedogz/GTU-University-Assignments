/**
 * @author Muhammed Oğuz
 * Interface for Container which implemented with array.
 */

package CSE222_hw01.interface_oguz;

public interface IArrayContainer<E> {
    /**
     * Add element to array.
     * @param e
     * @return return true if success. False otherwise.
     */
    boolean add(E e);

    /**
     * Search array, if this element is in the array.
     * @param e Searched element
     * @return true if find. False otherwise.
     */
    boolean contains(E e);

    /**
     * Look all array for containing
     * @param c compare every element this C
     * @return true if all found.
     */
    boolean containsAll(IArrayContainer<E> c);

    /**
     * Remove selected element.
     * @param e Selected element.
     * @return true if removing success. False otherwise.
     */
    boolean remove(E e);

    /**
     * Remove all C values if contains
     * @param c specify the values
     * @return true if success
     */
    boolean removeAll(IArrayContainer<E> c);

    /**
     * Give size of array
     * @return size of the array.
     */
    int size();

    /**
     * Get element at selected index.
     * @param index Selected index.
     * @return Element at selected index. If not exist. Return null.
     */
    E get(int index);

    /**
     * Look for specified item and return it.
     * @param item It would be copy item. 
     * @return if exist. If not exist return null
     */
    E getItem(E item);
}
