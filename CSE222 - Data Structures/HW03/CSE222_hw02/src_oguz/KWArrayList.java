package CSE222_hw02.src_oguz;



import java.util.Arrays;

import CSE222_hw02.interface_oguz.IKWArrayList;

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
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void add(int index, E item) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public E get(int index) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E set(int index, E item) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E remove(int index) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean remove(E item) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public int size() {
        // TODO Auto-generated method stub
        return 0;
    }

    
}
