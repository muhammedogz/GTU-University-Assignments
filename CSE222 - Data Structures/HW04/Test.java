

import CSE222_hw04.src_oguz.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;    

public class Test {
    public static void main(String[] args) {
        System.out.println("Test all heap and tree functions");
        testHeap();

        testBST();

        System.out.println("Calling a random tree example");
        testBST2();
    }
    
    public static void testBST() {
        Random rand = new Random();
        ArrayList<Integer> arr = new ArrayList<>();
        BSTHeapTree<Integer> tree = new BSTHeapTree<>();

        System.out.println("Created an ArrayList and a BSTHeapTree for comparing results");

        System.out.println("Generating random 5000 number");

        for (int i = 0; i < 3000; i++) {
            Integer temp = rand.nextInt(5001);

            arr.add(temp);
            tree.add(temp);
        }

        Collections.sort(arr);
        // int occurrences = Collections.frequency(animals, "bat");
        
        System.out.println("5000 Number generated. ArrayList sorted. Writing first 50 Elements in arrayList");
        for (int i = 0; i < 50; i++)
        {
            System.out.print(arr.get(i)+ " ");
        }
        System.out.println();

        System.out.println("Find method tree.find(arr.get(0)) = " + tree.find(arr.get(0)));
        System.out.println("Find method tree.find(arr.get(5)) = " + tree.find(arr.get(5)));
        System.out.println("Find method tree.find(arr.get(10)) = " + tree.find(arr.get(10)));
        System.out.println("Find method tree.find(arr.get(15)) = " + tree.find(arr.get(15)));

        System.out.println("Compare 100 elements occurrences with ArrayList and Tree. If one of them is not correct. Warn.");
        boolean flag = false;
        for (int i = 0; i < 100; i++)
        {
            int temp = arr.get(0);
            int occurrenceCollection = Collections.frequency(arr, temp);
            int occurrenceTree = tree.find(temp);
            if (occurrenceCollection != occurrenceTree)
                flag = true;
        }
        if (flag)
            System.out.println("Failed to test. Some occurrences not matches");
        else
            System.out.println("Passed test successfully");

        System.out.println("Call tree.find() with negative values (which those values not generated)");
        for (int i = -10; i < -1; i++)
        {
            System.out.print("Calling tree.find(negative number) = ");
            try {
                System.out.println(tree.find(i));
            } catch (Exception e) {
                System.out.print("Not in the tree\n");
            }
        }

        System.out.println("ArrayList most occurrence count = " + find_mode_array(arr));
        System.out.println("Tree most occurrence count = " + tree.find_mode());

        flag = false;
        System.out.println("Remove 100 values.");
        for (int i = 0; i < 100; i++)
        {
            try {
                tree.remove(arr.get(i));
            } catch (Exception e) {
                flag = true;
                e.printStackTrace();
            }
            arr.remove(arr.get(i));
        }
        if (flag)
            System.out.println("Failed to test. Some occurrences not matches");
        else
            System.out.println("Passed test successfully");

        System.out.println("Call tree.remove() with negative values (which those values not generated)");
        for (int i = -10; i < -1; i++)
        {
            System.out.print("Calling tree.remove(negative number) = ");
            try {
                System.out.println(tree.remove(i));
            } catch (Exception e) {
                System.out.print("Not in the tree\n");
            }
        }
        
    }

    public static void testBST2() {
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
    
            System.out.println(tree);
    
            } catch (Exception e) {
                System.out.println(e);
            }
    }

    public static void testHeap() {
        Heap<Integer> heap = new Heap<Integer>();
        Heap<Integer> heap2 = new Heap<Integer>();
        

        heap.add(15);
        heap.add(2);
        heap.add(8);
        heap.add(5);
        heap.add(4);
        
        heap2.add(12);
        heap2.add(1);
        heap2.add(7);
        heap2.add(8);
        
        heap.removeIthBiggestElement(2);
        System.out.println("Heap1 = " + heap.toString());
        HeapIter<Integer> iter = heap.heapIter();
        iter.next();
        iter.set(100);
        System.out.println("Heap1 after removeithBiggest and set with iterator");
        System.out.println("Heap1 = " + heap.toString());
        System.out.println("Heap2 = " + heap2.toString());
        heap.merge(heap2);
        System.out.println("Merged = " + heap.toString());
        System.out.println(heap);

        System.out.println("heap.find(100) = " + heap.find(100));
    }

    public static int find_mode_array(ArrayList<Integer> arr) {
        int max = 0;
        int temp = 1;
        for (int i = 0; i < arr.size() - 1; i++)
        {
            if ( arr.get(i).equals(arr.get(i + 1)) )
            {
                temp++;
            }
            else
            {
                if (max < temp)
                    max = temp;
                temp = 1;
            }
        }
        return max;
    }
}
