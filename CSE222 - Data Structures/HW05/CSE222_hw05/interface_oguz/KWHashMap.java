package CSE222_hw05.interface_oguz;

public interface KWHashMap<K,V> {

    //  
    /**
     * Returns the value associated with the specified key.
     * @param key specified key
     * @return null if the key is not present 
     */
    V get(Object key); 

    /**
     * looks if empty or not
     * @return true if this table contains no key‐value mappings
     */
    boolean isEmpty(); 

    /**
     * Associates the specified value with the specified key. 
     * @param key specified key
     * @param value specified value
     * @return the previous value
     */
    V put(K key, V value);

    /**
     *  Removes the mapping for this key from this table if it is present
     * @param key specified key
     * @return the previous value associated with the specified key
     */
    V remove(Object key);

    /**
     * Returns the size of the table
     * @return the size of the table
     */
    int size();
    
}
