/**
 * 
 * @author Muhammed Oguz
 * 
 * This class is interface for HeapIter class
 */

package CSE222_hw04.interface_oguz;

import java.util.NoSuchElementException;


public interface IHeapIter<E> {

    /**
     * Check if there is a element 
     * @return true if has Next
     */
    public boolean hasNext();

    /**
     * Return next element
     * @return next element
     * @throws NoSuchElementException if exceed bounds or empty throw error
     */
    E next() throws NoSuchElementException;


    /**
     * Set last returned item from next() method to new value
     * @param item The value to assign
     * @return true if success
     * @throws NoSuchElementException if invalid
     */
    boolean set(E item) throws NoSuchElementException;

    
}
