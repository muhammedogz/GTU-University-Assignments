package CSE222_hw01.src_oguz;

import CSE222_hw01.interface_oguz.IArrayContainer;

public class ArrayContainer<E> implements IArrayContainer<E> {
    private E []array;
    private int currentSize;

    public ArrayContainer() {
        currentSize = 0;
    }

    @Override
    @SuppressWarnings("unchecked") // for unchecked type warning. 
    public boolean add(E e)
    {
        if (contains(e))
            return false;
        
        E[] foo = (E[]) new Object[++currentSize];
        for (int i = 0; i < currentSize - 1; i++)
            foo[i] = array[i];

        foo[currentSize - 1] = e;
        array = (E[]) new Object[currentSize];
        for (int i = 0; i < currentSize; i++)
        array[i] = foo[i];
        return true;
    }

    @Override
    public boolean contains(E e)
    {
        for (int i = 0; i < currentSize; i++)
        {
            if (array[i].equals(e))
            {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean containsAll(IArrayContainer<E> c) {
        for (int i = 0; i < c.size(); i++)
            if (!contains(c.get(i)))
                return false;
        return true;
    }

    @Override
    public boolean remove(E e) 
    {
        // if this element is not in array. return false.
        if (!contains(e))
        {
            System.err.println("This item not in the array.");
            return false;
        }
        else if (currentSize < 0)
        {
            System.err.println("This array has no elements. Could not remove anything.");
            return false;
        }
        else
        {
            // first, find the index. Which element will be removed.                 
            int i;
            for (i = 0; i < currentSize; i++)
            {
                if (array[i].equals(e))
                    break;    
            }

            // quick remove algorithm.
            for (int j = i + 1; j < currentSize; j++)
                array[j - 1] = array[j];

            array[--currentSize] = null;

            return true;
        }
    }

    @Override
    public boolean removeAll(IArrayContainer<E> c) {
        if (!containsAll(c))
            return false;
        else
        {
            for (int i = 0; i < c.size(); i++)
                remove(c.get(i));
        }
        return true;
    }

    @Override
    public int size() {
        return this.currentSize;
    }

    @Override
    public E get(int index) {
        if (index >= 0 && index < currentSize)
            return array[index];
        else
        {
            System.err.println("Invalid index");
            return null;
        }        
    }

    @Override
    public E getItem(E item) {
        if(!contains(item))
            return null;
        else
        {
            for (int i = 0; i < size(); i++)
                if (array[i].equals(item))
                    return array[i];
        }
        return null;
    }

    @Override
    public String toString() {
        String r = "";
        for (int i = 0; i < currentSize; i++)
            r += array[i] + "\n";

        return r;
    }

}
