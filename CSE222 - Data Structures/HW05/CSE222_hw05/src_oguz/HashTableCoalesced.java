package CSE222_hw05.src_oguz;


import CSE222_hw05.interface_oguz.IEntry;
import CSE222_hw05.interface_oguz.KWHashMap;


public class HashTableCoalesced<K,V> implements KWHashMap<K,V>{

    private Entry<K, V>[] table;
    private int numKeys;
    private static final int CAPACITY = 10;
    private static final double LOAD_THRESHOLD = 3.0;

    @SuppressWarnings("unchecked")
    public HashTableCoalesced() {
        table = new Entry[CAPACITY];
    }

    @Override
    public V put(K key, V value) {
        if (numKeys == CAPACITY) 
        {
            throw new OutOfMemoryError("This hashMap is full");
        }
        int currentIndex = key.hashCode() % table.length;
        if (currentIndex < 0)
        currentIndex += table.length;
        
        System.out.println("Key: " + key + " Index:" + currentIndex);
        // if this index is null. Set new value 
        if (table[currentIndex] == null)
        {
            table[currentIndex] = new Entry<K,V>(key, value);
            numKeys++;
            return value;
        }

        // If key already exist, set new value
        for (int i = 0; i < numKeys; i++)
        {
            if (table[i] != null && table[i].getKey().equals(key))
            {
                table[i].setValue(value);
                numKeys++;
                return value;
            }
        }

        Entry<K,V> it = table[currentIndex];
        System.out.println("This is it ->" + it.getKey().toString());
        int newIndex = currentIndex;
        int power = 1;
        newIndex = (currentIndex + (power*power)) % table.length;
        power++;
        // find next null place due to  quadratic probing 
        while(it.next != null) 
        {
            newIndex = (currentIndex + (power*power)) % table.length;
            power++;
            it = it.next;
        }
        while(table[newIndex] != null)
        {
            newIndex = (currentIndex + (power*power)) % table.length;
            power++;
        }
        System.out.println("Current:" + currentIndex + " " + "newIndex:" + newIndex);
        
        // set new founded index area to new value
        table[newIndex] = new Entry<K,V>(key, value);
        // set iterator next value to this value
        it.next = table[newIndex];
        
        numKeys++;

        return value;
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

    @Override
    public String toString() {
        if (isEmpty()) return "Empty";

        StringBuilder str = new StringBuilder("{\n");

        for (int i = 0; i < table.length; i++)
        {
            if (table[i] == null)
                str.append("Index: " + i + "\tKey = Empty\n");
            else
                if (table[i].next != null)
                    str.append("Index: " + i + "\tKey:" + table[i].getKey() + "->" + table[i].next.getKey() + "\n");
                else
                    str.append("Index: " + i + "\tKey:" + table[i].getKey() + "->" + table[i].next + "\n");
        }
        str.append("}");
        return str.toString();
    }

    /** Contains key‐value pairs for a hash table. */
    private static class Entry<K, V> implements IEntry<K,V>{
        /** The key */
        private K key;
        /** The value */
        private V value;
        /** Ref to hold next entry */
        private Entry<K,V> next = null;

        /** Creates a new key‐value pair.
        @param key The key
        @param value The value
        */
        public Entry(K key, V value) {
            this.next = null;
            this.key = key;
            this.value = value;
        }

        /**
         * Constructor for creating with entry to hold next
         * @param entry The entry going to ref
         * @param key The key
         */
        public Entry(Entry<K,V> entry, K key, V value) {
            this.next = entry;
            this.key = key;
            this.value = value;
        }
        
        @Override
        public K getKey() {
            return key;
        }
        
        @Override
        public V getValue() {
            return value;
        }

        @Override
        public V setValue(V val) {
            V oldVal = value;
            value = val;
            return oldVal;
        }
    }

}
