/**
 * 
 * @author Muhammed Oguz
 * 
 * This class is interface for Heap class
 */

package CSE222_hw04.interface_oguz;


import java.util.Iterator;
import java.util.NoSuchElementException;

import CSE222_hw04.src_oguz.*;

/**
 * IHash
 */
public interface IHeap<E extends Comparable<E>> {


    /**
     * Add item to heap
     * @param item The item going to add heap
     * @return true
     */
    boolean add(E item);

    /**
     * Find given element if exist
     * @param item The element going to find
     * @return element if exist, if not throw exception
     * @throws NoSuchElementException IF element not exist, throw exception
     */
    E find(E item) throws NoSuchElementException;
        
    /**
     * Search element. Same as find
     * @param item The element going to searched
     * @return return element if exist
     */
    E search(E item);
    
    /**
     * Merge with another Heap structure 
     * @param other The structure to going to merged
     * @return true 
     */
    boolean merge(Heap<E> other);
    
    /**
     * Return a iterator with Private data
     * @return data.iterator();
     */
    Iterator<E> iterator(); 
    
    /**
     * Return a HeapIterator. A special iterator for this structure
     * @return HeapIterator
     */
    HeapIter<E> heapIter();
    
    /**
     * Sort all values and remove Ith biggest element
     * @param index Determine, which biggest element going to removed
     * @return removed object
     * @throws IndexOutOfBoundsException if index is invalid, throw exception
     */
    Object removeIthBiggestElement(int index) throws IndexOutOfBoundsException;
    
}