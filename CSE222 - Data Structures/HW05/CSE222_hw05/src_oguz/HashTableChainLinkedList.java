/**
 * @author Muhammed Oğuz
 * This class implements KWHashMap interface with LinkedList data structure
 * This class also implements IEntry interface with Entry class
 */

package CSE222_hw05.src_oguz;

import java.util.LinkedList;
import java.util.ListIterator;

import CSE222_hw05.interface_oguz.IEntry;
import CSE222_hw05.interface_oguz.KWHashMap;


public class HashTableChainLinkedList<K,V> implements KWHashMap<K,V>{

    private LinkedList<Entry<K, V>>[] table;
    private int numKeys;
    private static final int CAPACITY = 5;
    private static final double LOAD_THRESHOLD = 3.0;

    /**
     * This warning @unchecked can be avoidable because
     * No need to check type for this
     */
    @SuppressWarnings("unchecked")
    public HashTableChainLinkedList() {
        table = new LinkedList[CAPACITY];
    }

    @Override
    public V put(K key, V value) {
        int index = key.hashCode() % table.length;
        if (index < 0)
            index += table.length;
        if (table[index] == null)
        {
            table[index] = new LinkedList<Entry<K,V>>();
        }
        
        for (Entry<K, V> nextItem : table[index]) 
        {
            // If the search is successful, replace the old value.
            if (nextItem.getKey().equals(key)) 
            {
                V oldVal = nextItem.getValue();
                nextItem.setValue(value);
                return oldVal; 
            }
        }

        table[index].addFirst(new Entry<K,V>(key, value));
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
        ListIterator<Entry<K,V>> iter = table[index].listIterator();

        while (iter.hasNext()){
            if (iter.next().getKey().equals(key))
                iter.remove();
        }

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
        for (Entry<K, V> nextItem : table[index]) 
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

    /**
     * rehash this hash table
     */
    @SuppressWarnings("unchecked")
    private void rehash() {
        LinkedList<Entry<K, V>>[] oldTable = table;
        table = new LinkedList[2 * oldTable.length + 1];
        numKeys = 0;

        // Reinsert all items in oldTable into expanded table.
        for (int i = 0; i < oldTable.length; i++) 
            if (oldTable[i] != null) 
                for (Entry<K,V> entry : oldTable[i])
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
            for (Entry<K,V> entry : table[i])
            {
                str.append("| Key:"+entry.getKey() + " Val:" + entry.getValue() + " |\t");
            }
            str.append("\n");
        }
        str.append("}");
        return str.toString();
    }


    private static class Entry<K, V> implements IEntry<K,V>{
        /** The key */
        private K key;
        /** The value */
        private V value;

        /** Creates a new key‐value pair.
        * @param key The key
        * @param value The value
        */
        public Entry(K key, V value) {
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
