package CSE222_hw04.src_oguz;

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;

// import CSE222_hw04.interface_oguz.IHash;



public class Heap<E>  /* implements IHeap<E> */ {
    private PriorityQueue<E> data;
    

    public Heap() {
        data = new PriorityQueue<E>();
    }

    public boolean add(E item) {
        return data.add(item);
    }

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

    public E search(E item) {
        return find(item);
    }

    public boolean merge(Heap<E> other) {
        HeapIter<E> it = other.heapIter();

        while(it.hasNext())
        {
            data.add(it.next());
        }
        return true;
    }
    
    
    public Iterator<E> iterator() {
        return data.iterator();
    }

    public HeapIter<E> heapIter() {
        return new HeapIter<E>(this);
    }

    private boolean remove(Object item) {   
        return data.remove(item);
    }

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


    public static void main(String[] args) {
        Heap<Integer> heap = new Heap<Integer>();
        Heap<Integer> heap2 = new Heap<Integer>();
        

        heap.add(15);
        heap.add(2);
        heap.add(8);
        heap.add(5);
        heap.removeIthBiggestElement(2);
        heap.add(4);

        heap2.add(12);
        heap2.add(1);
        heap2.add(7);
        heap2.add(8);

        System.out.println("Heap1 = " + heap.toString());
        HeapIter<Integer> iter = heap.heapIter();
        iter.next();
        iter.set(100);
        System.out.println("Heap1 = " + heap.toString());
        System.out.println("Heap2 = " + heap2.toString());
        heap.merge(heap2);
        System.out.println("Merged = " + heap.toString());
    }
}
