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

    private MapIterator<K,V> iter;

    public HashMapIterable() {
        super();
        this.iter = new MapIterator<K,V>(this);
    }

    public V put(K key, V value){
        V val = super.put(key, value);
        this.iter = new MapIterator<K,V>(this);
        return val;
    }

    public MapIterator<K, V> iterator(){
        return iter;
    }

    public MapIterator<K,V> iterator(K key){
        
        return iter;
    }
    

}