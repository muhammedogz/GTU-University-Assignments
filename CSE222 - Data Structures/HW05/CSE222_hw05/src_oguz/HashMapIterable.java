package CSE222_hw05.src_oguz;

import java.util.HashMap;


/**
 * HashIter
 */
public class HashMapIterable<K, V> extends HashMap<K, V> {


    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public MapIterator<K, V> iterator(){
        return new MapIterator<K,V>(this);
    }

    public MapIterator<K,V> iterator(K key){
        return new MapIterator<K,V>(this, key);
    }
    

}