package CSE222_hw04.src_oguz;

import java.util.Iterator;
import java.util.NoSuchElementException;

import CSE222_hw04.interface_oguz.IHeapIter;

public class HeapIter<E> implements Iterator<E>, IHeapIter<E> {
    private E lastItemReturned = null;
    private Heap<E> heap;
    private Iterator<E> it;

    public HeapIter(Heap<E> heap) {
        this.heap = heap;
        it = heap.iterator();
    }

    @Override
    public boolean hasNext() {
        return it.hasNext();
    }

    @Override
    public E next() {
        if (hasNext())
        {
            lastItemReturned = it.next();
            return lastItemReturned;
        }
        throw new NoSuchElementException();
    }

    @Override
    public boolean set(E item) {
        if (lastItemReturned != null)
        {
            it.remove();
            lastItemReturned = item;
            heap.add(item);
            return true;
        }
        throw new NoSuchElementException("There is no value for lastItemReturned, first use next method.");
    }
}
