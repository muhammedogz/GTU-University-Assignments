package CSE222_hw07.src_oguz;

import java.util.Random;

import CSE222_hw07.book_implementation.*;


public class ComparePerformance {

    private static Random random = new Random();
    private static final int K10 = 1000000;

    public static void add_X_10K(int x, BinarySearchTree<Integer> tree) {
        int max = x * 10000;

        for (int i = 0; i < max; i++)  
            tree.add(random.nextInt(K10));

    }

    public static void add_X_10K_B(int x, BTree<Integer> tree) {
        int max = x * 10000;

        for (int i = 0; i < max; i++)  
            tree.add(random.nextInt(K10));

    }

    public static void add_X_10K_S(int x, SkipList<Integer> tree) {
        int max = x * 10000;

        for (int i = 0; i < max; i++)  
            tree.add(random.nextInt(K10 * x));

    }

    public static long calculateTime(BinarySearchTree<Integer> tree) {
        long time = System.nanoTime();

        for (int i = 0; i < 100; i++)
            tree.add(random.nextInt(K10));

         return System.nanoTime() - time; 
    }

    public static long calculateTime_B(BTree<Integer> tree) {
        long time = System.nanoTime();

        for (int i = 0; i < 100; i++)
            tree.add(random.nextInt(K10));

        return System.nanoTime() - time; 
    }

    public static long calculateTime_S(SkipList<Integer> tree) {
        long time = System.nanoTime();

        for (int i = 0; i < 100; i++)
            tree.add(random.nextInt(K10));

        return System.nanoTime() - time; 
    }
    
}
