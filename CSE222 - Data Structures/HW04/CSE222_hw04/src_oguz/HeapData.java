package CSE222_hw04.src_oguz;

public class HeapData<E extends Comparable<E>> implements Comparable<E> {
    
    private E data;
    private int count;

    public HeapData(E item) {
        data = item;
        count = 0;
    }

    public void increaseCount() {
        count++;
    }

    public void decreaseCount() throws Exception {
        if (count != 0)
            count--;
        else
            throw new Exception("Count is already zero");
    }

    public E getData() {
        return data;
    }

    public int getCount() {
        return count;
    }

    @Override
    public String toString() {
        if (count != 0)
            return data.toString() + "," + count;
        else 
            return data.toString();
    }

    @Override
    public int compareTo(E arg0) {
        return arg0.compareTo(data);
    }

}
