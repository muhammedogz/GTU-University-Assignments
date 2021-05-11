/**
 * @author Muhammed Oğuz
 * 
 * This class uses MapIterator class and implements HashMap class
 * Has all HashMap functionality and has a iterator class named MapIterator
 */

package CSE222_hw05.src_oguz;

import java.util.HashMap;


/**
 * Suppress serial warning (Only showing in VsCode or other editors if no flag activated with Javac)
 */
@SuppressWarnings("serial")
public class HashMapIterable<K, V> extends HashMap<K, V> {

    /**
     * Return iterator with zero parameter
     * @return MapIterator 
     */
    public MapIterator<K, V> iterator(){
        return new MapIterator<K,V>(this);
    }

    /**
     * Return iterator with given key index starting position
     * @param key Specify key
     * @return MapIterator
     */
    public MapIterator<K,V> iterator(K key){
        return new MapIterator<K,V>(this, key);
    }
    

}