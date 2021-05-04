package CSE222_hw04.src_oguz;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;

import CSE222_hw04.interface_oguz.IHeap;



public class Heap<E>  implements IHeap<E>  {
    private PriorityQueue<E> data;
    

    public Heap() {
        data = new PriorityQueue<E>();
    }

    @Override
    public boolean add(E item) {
        return data.add(item);
    }

    @Override
    public E find(E item) throws NoSuchElementException {
        if (!data.contains(item))
            throw new NoSuchElementException("This element not in this heap");
            
        HeapIter<E> it = heapIter();
        while (it.hasNext())
        {
            E temp = it.next();
            if (temp.equals(item))
                return temp;
        }

        return null;
    }

    @Override
    public E search(E item) {
        return find(item);
    }

    @Override
    public boolean merge(Heap<E> other) {
        HeapIter<E> it = other.heapIter();

        while(it.hasNext())
        {
            data.add(it.next());
        }
        return true;
    }
    
    @Override
    public Iterator<E> iterator() {
        return data.iterator();
    }

    @Override
    public HeapIter<E> heapIter() {
        return new HeapIter<E>(this);
    }

    @Override
    public Object removeIthBiggestElement(int index) {
        if (index < 0 || index > data.size())  
            throw new IndexOutOfBoundsException();

        Object[] arr = data.toArray();
        Arrays.sort(arr);

        Object temp = arr[index];
        remove(temp);
        return temp;
    }


    @Override 
    public String toString() {
        return data.toString();
    }
    
    private boolean remove(Object item) {   
        return data.remove(item);
    }
}
