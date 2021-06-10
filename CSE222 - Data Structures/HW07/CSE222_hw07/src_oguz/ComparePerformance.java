/**
 * @author Muhammed Oguz
 * 
 * This class used for testing speeds of data structures (for part 3)
 */

package CSE222_hw07.src_oguz;

import java.util.Random;

import CSE222_hw07.book_implementation.*;


public class ComparePerformance {

    /** Data Fields */
    // Random generator object
    private static Random random = new Random();
    // A constant variable, Chose over 1 million values
    private static final int RAND_BOUND = 1000000;

    /**
     * Add X0K item to given tree
     * @param x How much item going to added x * 10000
     * @param tree The tree that going to added item
     */
    public static void add_X_10K(int x, BinarySearchTree<Integer> tree) {
        int max = x * 10000;

        for (int i = 0; i < max; i++)  
            tree.add(random.nextInt(RAND_BOUND));

    }

    /**
     * Add X0K item to given tree for BTree
     * @param x How much item going to added x * 10000
     * @param tree The tree that going to added item
     */
    public static void add_X_10K_B(int x, BTree<Integer> tree) {
        int max = x * 10000;

        for (int i = 0; i < max; i++)  
            tree.add(random.nextInt(RAND_BOUND));

    }

    /**
     * Add X0K item to given tree for SkipList
     * @param x How much item going to added x * 10000
     * @param tree The tree that going to added item
     */
    public static void add_X_10K_S(int x, SkipList<Integer> tree) {
        int max = x * 10000;

        for (int i = 0; i < max; i++)  
            tree.add(random.nextInt(RAND_BOUND * x));

    }

    /**
     * Add new 100 item to given tree and calculate adding time
     * @param tree The tree that going to added values
     * @return time in ms
     */
    public static long calculateTime(BinarySearchTree<Integer> tree) {
        long time = System.nanoTime();

        for (int i = 0; i < 100; i++)
            tree.add(random.nextInt(RAND_BOUND));

         return System.nanoTime() - time; 
    }

    /**
     * Add new 100 item to given tree and calculate adding time for BTree
     * @param tree The tree that going to added values
     * @return time in ms
     */
    public static long calculateTime_B(BTree<Integer> tree) {
        long time = System.nanoTime();

        for (int i = 0; i < 100; i++)
            tree.add(random.nextInt(RAND_BOUND));

        return System.nanoTime() - time; 
    }

    /**
     * Add new 100 item to given tree and calculate adding time for SkipList
     * @param tree The tree that going to added values
     * @return time in ms
     */
    public static long calculateTime_S(SkipList<Integer> tree) {
        long time = System.nanoTime();

        for (int i = 0; i < 100; i++)
            tree.add(random.nextInt(RAND_BOUND));

        return System.nanoTime() - time; 
    }
    
}
