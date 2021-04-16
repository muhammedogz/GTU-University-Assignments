package CSE222_hw03.src_oguz;



import java.util.Arrays;
import java.util.NoSuchElementException;

import CSE222_hw03.interface_oguz.IKWArrayList;

public class KWArrayList<E> implements IKWArrayList<E> {
    /**  Data Fields */
    /** The default initial capacity */
    private static final int INITIAL_CAPACITY = 10;
    /** The underlying data array */
    private E[] data;
    /** The current size */
    private int size = 0;
    /** The current capacity */
    private int capacity = 0;

    @SuppressWarnings("unchecked") // for unchecked type warning. 
    public KWArrayList(){
        capacity = INITIAL_CAPACITY;
        data = (E[]) new Object[capacity];
    }

    @Override
    public boolean add(E item) {
        if (size == capacity)
            reallocate();
        data[size++] = item;
        return true;
    }

    @Override
    public void add(int index, E item) {
        checkBound(index);

        if (size == capacity)
            reallocate();
 
        for (int i = size; i > index; i--)
            data[i] = data[i-1];

        data[index] = item;
        size++;
    }

    @Override
    public E get(int index) {
        checkBound(index);

        return data[index];
    }

    @Override
    public E get(E item){
        if (!contains(item))
            throw new NoSuchElementException();
            
        for (int i = 0; i < size; i++)
            if (data[i].equals(item))
                return data[i];
        return null;
    }

    @Override
    public E set(int index, E item) {
        checkBound(index);

        E old = data[index];
        data[index] = item;
        return old;
    }

    @Override
    public boolean contains(E item){
        for (int i = 0; i < size; i++)
            if (data[i].equals(item))
                return true;
        return false;
    }

    @Override
    public boolean containsAll(KWArrayList<E> items) {
        for (int i = 0; i < items.size(); i++)
        {
            if (!contains(items.get(i)))
                return false;
        }
        return true;
    }

    @Override
    public E remove(int index) {
        checkBound(index);

        E removed = data[index];
        for (int i = index + 1; i < size; i++)
            data[i-1] = data[i];
        size--;
        return removed;
    }

    @Override
    public boolean remove(E item) {
        for (int i = 0; i < size; i++)
        {
            if (data[i].equals(item))
            {
                remove(i);
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean removeAll(KWArrayList<E> items) {
        if (!containsAll(items))
            return false;

        for (int i = 0; i < items.size(); i++)
        {
            remove(items.get(i));
        }
        return true;
    }

    @Override
    public void clear() {
        data = null;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public String toString() {
        String str = "[";
        for (int i = 0; i < size; i++)
        {
            str += data[i].toString() + ","; 
        }
        str = str.substring(0, str.length() - 1);
        str += "]";
        return str;
    }

    /** Reallocate due to capacity */
    private void reallocate() {
        capacity = 2 * capacity;
        data = Arrays.copyOf(data, capacity);
    }

    /**
     * Check index if out of bound
     * @param index
     * @throws ArrayIndexOutOfBoundsException if index is out of the box
     */
    private void checkBound(int index) throws ArrayIndexOutOfBoundsException {
        if (index < 0 || index >= size)
            throw new ArrayIndexOutOfBoundsException(index);
    }

    
}
