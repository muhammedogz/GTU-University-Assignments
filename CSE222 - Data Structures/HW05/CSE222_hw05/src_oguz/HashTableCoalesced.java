package CSE222_hw05.src_oguz;


import CSE222_hw05.interface_oguz.KWHashMap;


public class HashTableCoalesced<K,V> implements KWHashMap<K,V>{

    private Entry<K, V>[] table;
    private int numKeys;
    private static final int CAPACITY = 20;
    private static final double LOAD_THRESHOLD = 3.0;

    @SuppressWarnings("unchecked")
    public HashTableCoalesced() {
        table = new Entry[CAPACITY];
    }

    @Override
    public V put(K key, V value) {
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length;
        if (table[index] == null)
        {
            table[index] = new Entry<K,V>(key, value);
            return null;
        }
        
        for (Entry<K, V> nextItem : table) 
        {
            // If the search is successful, replace the old value.
            if (nextItem.getKey().equals(key)) 
            {
                V oldVal = nextItem.getValue();
                nextItem.setValue(value);
                return oldVal; 
            }
        }

        // table[index].addFirst(new Entry<K,V>(key, value));
        numKeys++;
        if (numKeys > (LOAD_THRESHOLD * table.length))
            rehash();

        return null;
    }

    @Override
    public V remove(Object key) {
        V res = null;
        if ((res = get(key)) == null) return null;

        int index = key.hashCode() % table.length;
        if (index < 0) index += table.length;
        // ListIterator<Entry<K,V>> iter = table[index].listIterator();

        // while (iter.hasNext()){
        //     if (iter.next().getKey().equals(key))
        //         iter.remove();
        // }

        return res;
    }

 
    @Override
    public V get(Object key) {
        if (isEmpty()) return null;

        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length;
        if (table[index] == null)
            return null; // key is not in the table.
        // Search the list at table[index] to find the key.
        for (Entry<K, V> nextItem : table) 
        {
            if (nextItem.getKey().equals(key))
                return nextItem.getValue();
        }
        // assert: key is not in the table.
        return null;
    }

    @Override
    public boolean isEmpty() {
        if (numKeys == 0) return true;
        return false;
    }

    @Override
    public int size() {
        return numKeys;
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


    @SuppressWarnings("unchecked")
    private void rehash() {
        Entry<K, V>[] oldTable = table;
        table = new Entry[2 * oldTable.length + 1];
        numKeys = 0;

        // Reinsert all items in oldTable into expanded table.
        for (int i = 0; i < oldTable.length; i++) 
            if (oldTable[i] != null) 
                for (Entry<K,V> entry : oldTable)
                    put(entry.getKey(), entry.getValue());
    }

    @Override
    public String toString() {
        if (isEmpty()) return "Empty";

        StringBuilder str = new StringBuilder("{\n");

        for (int i = 0; i < table.length; i++)
        {
            str.append("Index =" + i + "\t");
            if (table[i] == null) {
                str.append("\n");
                continue;
            }
            for (Entry<K,V> entry : table)
            {
                str.append(entry.getKey() + "->" + entry.getValue() + "\t");
            }
            str.append("\n");
        }
        str.append("}");
        return str.toString();
    }



}
