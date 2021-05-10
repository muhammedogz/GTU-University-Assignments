/**
 * @author Muhammed Oğuz
 */

package CSE222_hw05.interface_oguz;

public interface IMapIterator<K,V> {

    /**
     * The function returns the next key in the Map. It returns the first key when there is no 
     * not-iterated key in the Map.
     * @return next key in the map
     */
    K next();


    /**
     * The iterator points to the previous key in the Map. It returns the last key when the 
     * iterator is at the first key.
     * @return returns previous key
     */
    K prev();


    /**
     * The method returns True if there are still not-iterated key/s in the Map, otherwise 
     * returns False.
     * @return true or false
     */
    boolean hasNext();
}
