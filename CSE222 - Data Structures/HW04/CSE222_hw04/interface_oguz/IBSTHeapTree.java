package CSE222_hw04.interface_oguz;

import java.util.NoSuchElementException;

import CSE222_hw04.src_oguz.Heap;

public interface IBSTHeapTree<E extends Comparable<E>> {


    /**
     * Add item to tree due to order
     * @param item The item going to add to tree
     * @return number of occurrence after insertion
     */
    int add(E item);

    /**
     * Remove item
     * @param item The item going to removed
     * @return number of occurrence after remove
     * @throws Exception If not exist
     */
    int remove (E item) throws Exception;

    /**
     * Get given item
     * @param item The item going to get
     * @return item
     */
    Heap<E> getItem(E item);

    /**
     * Find number of occurrence of given item
     * @param item The item going to search
     * @return number of occurrence
     * @throws NoSuchElementException if not exist
     */
    int find(E item) throws NoSuchElementException;

    /**
     * Find max occurrence of all 
     * @return Number of max occurrence 
     * @throws NullPointerException errorW
     */
    int find_mode() throws NullPointerException;


}
