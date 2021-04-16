package CSE222_hw02.src_oguz;

import CSE222_hw02.interface_oguz.IHybridList;

public class HybridList<E> implements IHybridList<E> {
    public static final int MAX_NUMBER = 5;
    KWLinkedList<KWArrayList<E>> data = null;
    int size;


    public HybridList(){
        data = new KWLinkedList<KWArrayList<E>>();
        size = 0;
    }

    @Override
    public void add(E item) {
        if (isArrayFull())
        {
            data.addLast(new KWArrayList<E>());
        }
        
    }

    @Override
    public void remove(int index) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void remove(E item) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public E get(int index) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int size() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean isArrayFull() {
        if (data.get(data.size() - 1).size() == MAX_NUMBER)
            return true; 
        return false;
    }
    
}
