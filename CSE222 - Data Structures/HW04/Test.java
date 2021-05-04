
import CSE222_hw04.src_oguz.*;

public class Test {
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
