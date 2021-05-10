/**
 * @author Muhammed Oğuz
 * 
 * This class implements IMapIterator class.
 * This class takes called map class's KeySet.
 * Iterates all key values.
 */

package CSE222_hw05.src_oguz;

import CSE222_hw05.interface_oguz.IMapIterator;
import java.util.Iterator;


public class MapIterator<K,V> implements IMapIterator<K,V>{
    /** Data Fields */
    // Iterator 
    private Iterator<K> it;
    // map is going to used to iterate
    HashMapIterable<K,V> map;
    // how mony element iterated
    private int count;

    public MapIterator(HashMapIterable<K,V> map) {
        this.count = 0;
        this.map = map;
        this.it = map.keySet().iterator();
    }

    public MapIterator(HashMapIterable<K,V> map, K key){
        this.count = 0;
        this.it = map.keySet().iterator();
        
        boolean notFound = true;
        while (hasNext()) {
            if (next().equals(key))
            {
                notFound = false;
                break;
            }
        }

        // if given key is not in this map
        // start from first key
        if (notFound) this.it = this.map.keySet().iterator();
    }

    @Override
    public K next() {
        avoidException();

        // if there is no next key
        // recreate (go back first key)
        if (!hasNext())
        {
            this.it = this.map.keySet().iterator();
            count = 0;
        }

        count++;
        return it.next();
    }

    @Override
    public K prev() {
        this.it = this.map.keySet().iterator();
        this.count--;
        K temp = null;

        // if last position is already first position
        // return next (which is first key)
        if (this.count == 0) return next();
        
        for (int i = 0; i < this.count; i++)
            temp = this.it.next();

        return temp;
    }

    @Override
    public boolean hasNext() {
        if (this.count < this.map.size())
            return true;
        return false;
    }   

    /**
     * When new item added to Map
     * Iterator gives ConcurrentModificationException exception
     * To avoid this @exception, I call this function in my methods
     */
    private void avoidException() {
        this.it = this.map.keySet().iterator();
        for (int i = 0; i < this.count; i++) it.next();
    }
}