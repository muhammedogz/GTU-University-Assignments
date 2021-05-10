package CSE222_hw05.src_oguz;

import java.util.LinkedList;

import CSE222_hw05.interface_oguz.KWHashMap;

@SuppressWarnings("unused")
public class HashTableChain<K,V> implements KWHashMap<K,V>{

    /** The table */
    private LinkedList<Entry<K, V>>[] table;
    /** The number of keys */
    private int numKeys;
    /** The capacity */
    private static final int CAPACITY = 101;
    /** The maximum load factor */
    private static final double LOAD_THRESHOLD = 3.0;
    // Constructor
    @SuppressWarnings("unchecked")
    public HashTableChain() {
        table = new LinkedList[CAPACITY];
    }

 

    /** Method get for class HashtableChain.
     @param key The key being sought
    @return The value associated with this key if found;
    otherwise, null
    */
    @Override
    public V get(Object key) {
        int index = key.hashCode() % table.length;
        if (index < 0)
        index += table.length;
        if (table[index] == null)
            return null; // key is not in the table.
        // Search the list at table[index] to find the key.
        for (Entry<K, V> nextItem : table[index]) {
            if (nextItem.getKey().equals(key))
                return nextItem.getValue();
        }
        // assert: key is not in the table.
        return null;
    }



    /** Contains key‐value pairs for a hash table. */
    private static class Entry<K, V> {
        /** The key */
        private K key;
        /** The value */
        private V value;
        /** Creates a new key‐value pair.
        @param key The key
        @param value The value
        */
        public Entry(K key, V value) {
            this.key = key;
            this.value = value;
        }
        /** Retrieves the key.
        @return The key
        */
        public K getKey() {
            return key;
        }
        /** Retrieves the value.
        @return The value
        */
        public V getValue() {
            return value;
        }
        /** Sets the value.
        @param val The new value
        @return The old value
        */
        public V setValue(V val) {
            V oldVal = value;
            value = val;
            return oldVal;
        }
    }



    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return false;
    }



    @Override
    public V put(K key, V value) {
        // TODO Auto-generated method stub
        return null;
    }



    @Override
    public V remove(Object key) {
        // TODO Auto-generated method stub
        return null;
    }



    @Override
    public int size() {
        // TODO Auto-generated method stub
        return 0;
    }

 
}
