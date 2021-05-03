package CSE222_hw04.src_oguz;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class HeapIter<E> implements Iterator<E> {
    private E lastItemReturned = null;
    Heap<E> heap;
    Iterator<E> it;

    public HeapIter(Heap<E> heap) {
        this.heap = heap;
        it = heap.iterator();
    }


    public boolean hasNext() {
        return it.hasNext();
    }

    public E next() {
        if (hasNext())
        {
            lastItemReturned = it.next();
            return lastItemReturned;
        }
        throw new NoSuchElementException();
    }

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
