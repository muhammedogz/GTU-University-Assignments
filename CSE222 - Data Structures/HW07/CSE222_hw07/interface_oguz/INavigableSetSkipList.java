package CSE222_hw07.interface_oguz;

import java.util.Iterator;

public interface INavigableSetSkipList<E> {
    
    /**
     * Insert item
     * @param e Item that going to insert
     * @return true
     */
    boolean insert(E e);

    /**
     * Delete given element if exist
     * @param e Item going to delete
     * @return deleted item, if not exist, return NULL
     */
    E delete(E e);

    /**
     * Return a iterator with descending sort
     * @return Iterator with descending order
     */
    Iterator<E> descendingIterator();
    
}
