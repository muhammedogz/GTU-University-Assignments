package CSE222_hw07.interface_oguz;

import java.util.Iterator;
import java.util.NavigableSet;

public interface INavigableSetAVL<E> {

    /**
     * Insert specified item to Set
     * @param item The item going to inserted
     * @return true if added, already exist return false
     */
    boolean insert(E item);
    
    /**
     * Delete specified item
     * @param item That going to deleted
     * @return true if success, false otherwise
     */
    boolean delete(E item);
    
    /**
     * Return an iterator
     * @return iterator
     */
    Iterator<E> iterator();

    /**
     * Return a set to specified element
     * @param toElement SPecified element
     * @return set
     */
    NavigableSet<E> headSet(E toElement);

    /**
     * Return a set to specified element included
     * @param arg0 specified element
     * @param arg1 true or false
     * @return a set
     */
    NavigableSet<E> headSet(E arg0, boolean arg1);

    /**
     * Return a set up to specified element
     * @param fromElement specified element
     * @return a set
     */
    NavigableSet<E> tailSet(E fromElement);

    /**
     * Return set to specified element included
     * @param arg0 specified element
     * @param arg1 true or false
     * @return a set
     */
    NavigableSet<E> tailSet(E arg0, boolean arg1);
    
}
