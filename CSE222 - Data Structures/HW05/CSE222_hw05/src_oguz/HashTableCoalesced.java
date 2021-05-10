package CSE222_hw05.src_oguz;


import java.util.ArrayList;
import java.util.Collections;


import CSE222_hw05.interface_oguz.IEntry;
import CSE222_hw05.interface_oguz.KWHashMap;

@SuppressWarnings("unused")
public class HashTableCoalesced<K,V> implements KWHashMap<K,V>{

    private ArrayList<Entry<K, V>> table;
    private int numKeys;
    private static final int CAPACITY = 10;

    public HashTableCoalesced() {
        table = new ArrayList<Entry<K,V>>(Collections.nCopies(CAPACITY, null));
    }

    @Override
    public V put(K key, V value) {
        if (numKeys == CAPACITY) 
        {
            throw new OutOfMemoryError("This hashMap is full");
        }
        int currentIndex = key.hashCode() % table.size();
        if (currentIndex < 0)
            currentIndex += table.size();
        
        // if this index is null. Set new value 
        if (table.get(currentIndex) == null)
        {
            table.set(currentIndex, new Entry<K,V>(key, value));
            numKeys++;
            return value;
        }

        // If key already exist, set new value
        for (int i = 0; i < numKeys; i++)
        {
            if (table.get(i) != null && table.get(i).getKey().equals(key))
            {
                table.get(i).setValue(value);
                numKeys++;
                return value;
            }
        }

        Entry<K,V> it = table.get(currentIndex);

        int newIndex = currentIndex;
        int power = 1;
        newIndex = (currentIndex + (power*power)) % table.size();
        power++;
        // find next null place due to  quadratic probing 
        while(it.next != null) 
        {
            newIndex = (currentIndex + (power*power)) % table.size();
            power++;
            it = it.next;
        }
        while(table.get(newIndex) != null)
        {
            newIndex = (currentIndex + (power*power)) % table.size();
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

        int otherIndex = getIndex((K) key);
        
        int index = key.hashCode() % table.size();
        

        Entry<K,V> it = table.get(index);
        Entry<K,V> removed = null;



        if (index < 0)
            index += table.size();

        while (it.next != null)
        {
            System.out.println("While index = " + getIndex(it.getKey()));
            if (it.next.getKey().equals(key))
            {
                it.next = it.next.next;
            }
            it = it.next;
        }
        index = otherIndex;
        it = table.get(index);
        
        int temp_index= -1;
        while(it.next != null) 
        {
            temp_index = getIndex(it.next.getKey());
            Entry<K,V> temp = new Entry<K,V>(table.get(temp_index));
            table.set(temp_index, table.get(index));
            table.set(index, temp);

            System.out.println("temp index "+ temp_index + " val " + table.get(temp_index).getKey().toString());
            System.out.println("index "+ index + " val " + table.get(index).getKey().toString());

            it = it.next;
        }

        System.out.println("Temp index ==========" + temp_index);
        if (temp_index == -1)
            table.set(index, null);
        else
        {
            // table.get(temp_index - 1).next = null;
            table.set(temp_index, null);
        }


        numKeys--;


        return res;
    }

    // private void remove(int index){
    //     for (int i = index + 1; i < table.length; i++)
    //         table[i - 1] = table[i];
    // }
    
// Search for key position and it's predecessor pp
//     if key is found at position p
//         if pp != NIL then 
//              next[pp] = NIL  
//         d[p] = NIL           //deletes the key
//         p = next[p]          //move position to next value in the chain
//         UpdateFirstEmpty()
//         while d[p] != NIL do
//             temp = d[p]      //save value
//             d[p] = NIL       //delete value 
//             p = next[p]      //move position to next value in chain
//             UpdateFirstEmpty()
//             Insert(temp)     //insert the value in the list again

//    endif

 
    @Override
    public V get(Object key) {
        if (isEmpty()) return null;
        
        int index = key.hashCode() % table.size();
        if (index < 0)
            index += table.size();
        if (table.get(index) == null)
            return null; // key is not in the table.
        // Search the list at table[index] to find the key.
        for (Entry<K, V> nextItem : table) 
        {
            if (nextItem != null && nextItem.getKey().equals(key))
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

        StringBuilder str = new StringBuilder("Hash\tKey\tNext\nValue\n");

        for (int i = 0; i < table.size(); i++)
        {
            if (table.get(i) == null)
                str.append(i + "\t-\tnull\n");
            else
                if (table.get(i).next != null)
                    str.append(i + "\tKey:" + table.get(i).getKey() + "\t" + getIndex(table.get(i).next.getKey()) + "\n");
                else
                    str.append(i + "\tKey:" + table.get(i).getKey() + "\t" + table.get(i).next + "\n");
        }
        return str.toString();
    }

    private int getIndex(K key){
        for (int i = 0; i < table.size(); i++)
        {
            if (table.get(i) != null && table.get(i).getKey().equals(key))
                return i;
        }
        return -1;
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
        public Entry(Entry<K,V> entry) {
            this.next = entry.next;
            this.key = entry.getKey();
            this.value = entry.getValue();
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
