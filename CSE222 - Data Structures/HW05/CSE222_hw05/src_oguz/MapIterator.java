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
        
        boolean flag = true;
        while (hasNext()) {
            if (next().equals(key))
            {
                flag = false;
                break;
            }
        }
        if (flag) this.it = this.map.keySet().iterator();
    }

    @Override
    public K next() {
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
        
        this.count--;
        K temp = null;
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
}