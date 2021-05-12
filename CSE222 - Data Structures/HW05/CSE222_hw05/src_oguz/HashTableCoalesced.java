/**
 * @author Muhammed Oğuz
 * This class implements KWHashMap interface with Coalesced HashMap idea
 * This class also implements IEntry interface with Entry class
 */

package CSE222_hw05.src_oguz;

import java.util.ArrayList;
import java.util.Collections;


import CSE222_hw05.interface_oguz.IEntry;
import CSE222_hw05.interface_oguz.KWHashMap;

public class HashTableCoalesced<K,V> implements KWHashMap<K,V>{

    private ArrayList<Entry<K, V>> table;
    private int numKeys;
    private static final int CAPACITY = 10;
    private static final double LOAD_THRESHOLD = 0.25;


    public HashTableCoalesced() {
        table = new ArrayList<Entry<K,V>>(Collections.nCopies(CAPACITY, null));
    }

    @Override
    public V put(K key, V value) {
        int currentIndex = key.hashCode() % 10;
        if (currentIndex < 0)
            currentIndex += table.size();
            
        // if this index is null. Set new value 
        if (table.get(currentIndex) == null)
        {
            table.set(currentIndex, new Entry<K,V>(key, value));
            numKeys++;
            double loadFactor = (double) (numKeys) / table.size();
            if (loadFactor > LOAD_THRESHOLD)
                rehash();
            return value;
        }

        // If key already exist, set new value
        for (int i = 0; i < numKeys; i++)
        {
            if (table.get(i) != null && table.get(i).getKey().equals(key))
            {
                table.get(i).setValue(value);
                return value;
            }
        }

        Entry<K,V> it = table.get(currentIndex);

        int newIndex = currentIndex;
        int power = 1;
        newIndex = generateIndex(currentIndex, power);
        power++;

        // find next null place due to  quadratic probing 
        while(it.next != null) 
        {
            newIndex = generateIndex(currentIndex, power);
            power++;
            it = it.next;
        }
        while(table.get(newIndex) != null)
        {
            newIndex = generateIndex(currentIndex, power);
            power++;
        }
        
        // set new founded index area to new value
        table.set(newIndex, new Entry<K,V>(key, value)); 
        // set iterator next value to this value
        it.next = table.get(newIndex);
        
        numKeys++;

        return value;
    }

    @SuppressWarnings("unchecked")
    @Override
    public V remove(Object key) {
        V res = null;
        if ((res = get(key)) == null) return null;

        // keep for later use
        int keepIndex = getIndex((K) key);

        // set index due to hashing
        int index = key.hashCode() % 10;
        
        Entry<K,V> it = table.get(index);
        
        if (it.next == null)
            table.set(index, null);

        // go over to find last right place to inserted
        while (it.next != null)
        {
            // if equal, set next value to next.next element
            if (it.getKey().equals(key)) 
            {
                index = keepIndex;
                table.get(index).key = it.next.getKey();
                if (it.next.next != null)
                    table.get(index).next = it.next.next;
                else
                    table.get(index).next = null;
            }
            // if upper if block worked and new next value is null, break the loop
            if (it.next == null)
            {
                break;
            }
            it = it.next;
        }


        numKeys--;
        return res;
    }

    @Override
    public V get(Object key) {
        if (isEmpty()) return null;
        
        int index = key.hashCode() % 10;
        if (index < 0)
            index += table.size();
        if (table.get(index) == null)
            return null; 

        // Search the list at table[index] to find the key.
        for (Entry<K, V> nextItem : table) 
        {
            if (nextItem != null && nextItem.getKey().equals(key))
                return nextItem.getValue();
        }

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

        StringBuilder str = new StringBuilder("Hash\tKey\tNext\nValue\n");

        int val = 0;
        int diff = table.size() - findLastFullIndex();
        if (diff > 1) val = diff;
        if (val > 3) val = 3;

        for (int i = 0; i < findLastFullIndex() + val; i++)
        {
            if (table.get(i) == null)
                str.append(i + "\t-\tnull\n");
            else
                if (table.get(i).next != null)
                {
                    str.append(i + "\tKey:" + table.get(i).getKey() + "\t");
                    if (getIndex(table.get(i).next.getKey()) == -2)
                        str.append("null" + "\n");
                    else 
                        str.append(getIndex(table.get(i).next.getKey()) + "\n");
                }
                else
                    str.append(i + "\tKey:" + table.get(i).getKey() + "\t" + table.get(i).next + "\n");
        }
        str.append("....................\n");
        str.append("....................\n");
        for (int i = val - 1; i >= 0; i--) str.append(table.size() - i + "\t-\tnull\n");
        return str.toString();
    }

    /**
     * get index position of given key
     * @param key specify key
     * @return int index
     */
    private int getIndex(K key){
        for (int i = 0; i < table.size(); i++)
        {
            if (table.get(i) != null && table.get(i).getKey().equals(key))
                return i;
        }
        return -2;
    }

    /**
     * rehash using arrayList
     */
    private void rehash() {
        ArrayList<Entry<K, V>> newTable = table;
        
        table = new ArrayList<Entry<K,V>>(Collections.nCopies(table.size() * 2, null));

        // Reinsert all items in oldTable into expanded table.
        for (int i = 0; i < newTable.size(); i++) 
        {
            Entry<K,V> temp = newTable.get(i);
            if (temp  != null)
            {
                //System.out.println(temp.getKey());
                put(temp.getKey(), temp.getValue());
            }
        }

    }

    /**
     * Find last full place for printing nicely
     * @return last full index
     */
    private int findLastFullIndex() {
        for (int i = table.size() - 1; i >= 0; i--){
            try {
                if (table.get(i) != null) return i;
            } catch (Exception e) {
                e.getMessage();
            }
        }
        return -1;
    }

    /**
     * Use this when generating new index due to pdf
     * @param currentIndex current index
     * @param power last used power info
     * @return int index
     */
    private int generateIndex(int currentIndex, int power) {
        int val = currentIndex + (power*power);
        if (val > table.size())
        {
            rehash();
            generateIndex(currentIndex, power);
        }
        return val;
    }


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
