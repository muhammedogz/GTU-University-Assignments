package CSE222_hw04.src_oguz;


import java.util.Collections;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;

import CSE222_hw04.interface_oguz.IBSTHeapTree;

public class BSTHeapTree<E extends Comparable<E>> implements IBSTHeapTree<E>{
    PriorityQueue<Heap<E>> data;

    public BSTHeapTree(){
        data = new PriorityQueue<>(Collections.reverseOrder());
    }

    public int add(E item){
        if (data.size() == 0)
            data.add(new Heap<>());

        Heap<E> heap = getItem(item);
        if (heap != null)
        {
            heap.addToTree(item);
            HeapData<E> val = heap.getItem(item);
            return val.getCount();
        }

        Iterator<Heap<E>> it = data.iterator();
        while (it.hasNext())
        {

            Heap<E> temp = it.next();
            if (temp.getSize() < 7)
            {
                temp.addToTree(item);
                HeapData<E> val = temp.getItem(item);
                return val.getCount();
            }
        }

        Heap<E> temp = new Heap<E>();
        temp.addToTree(item);
        data.add(temp);
        HeapData<E> val = temp.getItem(item);
        return val.getCount();

    }

    public int remove (E item) throws Exception {
        Heap<E> get = getItem(item);
        if (get == null)
            throw new NoSuchElementException("There is no with this element\n");


        HeapData<E> hData = get.getItem(item);
        if (hData.getCount() == 1)
        {
            get.remove(hData);
            return 0;
        }
        else
            hData.decreaseCount();
        
        return hData.getCount();
    }

    public Heap<E> getItem(E item) {
        Iterator<Heap<E>> it = data.iterator();

        while (it.hasNext())
        {
            Heap<E> temp = it.next(); 
            if (temp.getItem(item) != null)
            {
                return temp;
            }
        }
        return null;
    }

    public int find(E item) {
        HeapData<E> get = getItem(item).getItem(item);

        if (get == null)
            throw new NoSuchElementException();
        
        return get.getCount();

    }

    public int find_mode() {
        if (data.size() == 0)
            throw new NullPointerException();

        Iterator<Heap<E>> it = data.iterator();

        int max = it.next().mostCount();

        while(it.hasNext())
        {
            int temp = it.next().mostCount();
            if (temp > max)
                max = temp;
        }
        return max;
    }

    @Override
    public String toString() {
        String str = "[";
        Iterator<Heap<E>> it = data.iterator();
        int size = 0;
        while (it.hasNext())
        {
            str += it.next().toString();
            if (size != data.size() - 1)
                str += "\n";
            size++;
        }
        str += "]";

        return str;
    }


}
