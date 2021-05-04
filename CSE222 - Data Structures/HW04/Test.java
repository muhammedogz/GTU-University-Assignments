

import CSE222_hw04.src_oguz.*;

public class Test {
    public static void main(String[] args) {
        BSTHeapTree<Integer> tree = new BSTHeapTree<>();

        try {
            
        
        tree.add(37);
        tree.add(23);
        tree.add(10);
        tree.add(10);
        tree.add(16);
        tree.add(19);
        tree.add(9);
        tree.add(9);
        tree.add(9);
        tree.add(3);

        tree.add(31);
        tree.add(15);
        tree.add(13);
        tree.add(13);
        tree.add(13);
        tree.add(13);
        tree.add(29);


        tree.add(124);
        tree.add(52);
        tree.add(98);
        tree.add(52);
        tree.add(51);
        tree.add(51);
        tree.add(51);
        tree.add(38);
        tree.add(98);
        tree.add(87);
        tree.add(80);
        tree.add(80);

        tree.add(60);
        tree.add(57);
        tree.add(54);
        tree.add(39);
        tree.add(39);
        tree.add(43);
        tree.add(43);
        tree.add(43);

        tree.remove(98);
        tree.remove(98);

        tree.add(98);

        System.out.println(tree.find(124));

        System.out.println(tree.find_mode());

        } catch (Exception e) {
            System.out.println(e);
        }
        

        System.out.println(tree);

        //testHeap();
    }

    public static void testHeap() {
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
        HeapIter<Integer> it = heap.heapIter();


        System.out.println(heap);
        
        
        System.out.println(heap.find(100));


        while(it.hasNext()) {
            System.out.println(it.next());
        }
        System.out.println(heap);

        System.out.println("find" + heap.find(100));
    }
}
