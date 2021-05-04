package CSE222_hw04.src_oguz;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;

import CSE222_hw04.interface_oguz.IHeap;



public class Heap<E extends Comparable<E>> implements IHeap<E>  {
    private PriorityQueue<HeapData<E>> data;
    private int size = 0;
    

    public Heap() {
        data = new PriorityQueue<HeapData<E>>();
    }

    @Override
    public boolean add(E item) {
        HeapData<E> temp = new HeapData<E>(item);
        size++;
        return data.add(temp);
    }

    @Override
    public E find(E item) throws NoSuchElementException {
   
        HeapIter<E> it = heapIter();
        while (it.hasNext())
        {
            E temp = it.next();
            if (temp.equals(item))
                return temp;
        }

        throw new NoSuchElementException();
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
            HeapData<E> temp = new HeapData<E>(it.next());
            data.add(temp);
        }
        return true;
    }
    
    @Override
    public Iterator<HeapData<E>> iterator() {
        return data.iterator();
    }

    @Override
    public HeapIter<E> heapIter() {
        return new HeapIter<E>(this);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object removeIthBiggestElement(int index) {
        if (index < 0 || index > data.size())  
            throw new IndexOutOfBoundsException();

        Object[] arr = data.toArray();
        Arrays.sort(arr);

        HeapData<E> temp =  (HeapData<E>) arr[index];
        remove(temp);
        return temp;
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object remove(int index) {
        if (index < 0 || index > data.size())  
            throw new IndexOutOfBoundsException();

        Object[] arr = data.toArray();

        HeapData<E> temp =  (HeapData<E>) arr[index];
        remove(temp);
        return temp;
    }

    @Override
    public int getSize() {
        return size;
    }


    @Override 
    public String toString() {
        return data.toString();
    }

    private boolean remove(HeapData<E> item) {
        size--;
        return data.remove(item);
    }
    
    
}
