package CSE222_hw03.src_oguz;

import java.util.NoSuchElementException;

import CSE222_hw03.interface_oguz.IHybridList;

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
        if (size == 0)
        {
            data.addFirst(new KWArrayList<E>());
            data.getFirst().add(item);
        }
        else if (isArrayFull())
        {
            data.addLast(new KWArrayList<E>());
            data.getLast().add(item);
        }
        else
            data.getLast().add(item); 
        size++;
    }

    @Override
    public void remove(int index) {
        checkBound(index);

        for (int i = 0; i < data.size(); i++)
        {
            for (int j = 0; j < data.get(i).size(); j++)
            {
                if (i + j == index)
                {
                    data.get(i).remove(j);
                    size--;
                    cleanEmptyArray();
                    return;
                }
            }
        }
    }

    @Override
    public void remove(E item) {

        for (int i = 0; i < data.size(); i++)
        {
            for (int j = 0; j < data.get(i).size(); j++)
            {
                if (data.get(i).get(j).equals(item))
                {
                    data.get(i).remove(j);
                    size--;
                    cleanEmptyArray();
                    return;
                }
            }
        }
        throw new NoSuchElementException();
    }

    @Override
    public E get(E item){

        for (int i = 0; i < data.size(); i++)
            if (data.get(i).contains(item))
                return data.get(i).get(item);

        return null;
    }

    @Override
    public boolean contains(E item){
        for (int i = 0; i < data.size(); i++)
            if (data.get(i).contains(item))
                return true;
        return false;
    }

    @Override
    public E get(int index) {
        checkBound(index);

        for (int i = 0; i < data.size(); i++)
            for (int j = 0; j < data.get(i).size(); j++)
                if (i + j == index)
                    return data.get(i).get(j);

        return null;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void clear() {
        data = null;
        size = 0;
    }

    @Override
    public boolean isArrayFull() {
        if (size != 0 && data.getLast().size() == MAX_NUMBER)
            return true; 
        return false;
    }

    @Override
    public String toString() {
        String str = "[";

        for (int i = 0; i < data.size(); i++)
        {
            for (int j = 0; j < data.get(i).size(); j++)
            {
                str += data.get(i).get(j).toString() + ",";
            }
        }
        str = str.substring(0, str.length() - 1);
        str += "]";
        return str;
    }

    /**
     * Check given index is bound
     * @param index which index gonna check
     * @throws ArrayIndexOutOfBoundsException if out of bound
     */
    private void checkBound(int index) throws ArrayIndexOutOfBoundsException {
        if (index < 0 || index >= size)
            throw new ArrayIndexOutOfBoundsException(index);
    }
    
    /**
     * if any ArrayList value is zero, remove it.
     */
    private void cleanEmptyArray(){
        for (int i = 0; i < data.size(); i++)
            if (data.get(i).size() == 0)
                data.remove(i);
    }
}
