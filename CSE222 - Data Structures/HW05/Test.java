/**
 * @author Muhammed Oğuz
 * This class tests all class in CSE222_hw05/src_oguz/ classes implementations
 */

import CSE222_hw05.interface_oguz.KWHashMap;
import CSE222_hw05.src_oguz.*;

public class Test {
    public static void main(String[] args) {
        System.out.println("\n\n----------Welcome to world best test class----------\n\nPart_1 Test and Part_2 Test will called\n\n");

        part1_test();
        part2_tests();

    }

    public static void part1_test() {
        System.out.println("Testing Custom Class HashMapIterable which extended from HashMap\nContains MapIterator");
        System.out.println("Insert following entries.");
        System.out.println("{foo=0, bar=1, YSA=2, Erdogan Hoca=3, Burak Hoca=4, John=5, Doe=6, Peace=7, Lennon=8, Beatles=9}");

        HashMapIterable<String, Integer> map = new HashMapIterable<>();

        map.put("foo", 0);      map.put("bar", 1);
        map.put("YSA", 2);      map.put("Erdogan", 3);
        map.put("Burak", 4);    map.put("John", 5);
        map.put("Doe", 6);      map.put("Peace", 7);
        map.put("Lennon", 8);   map.put("Beatles", 9);

        System.out.println("\nPrint map\n");
        System.out.println(map);
        
        System.out.println("\nCreate zero-parameter iterator -> map.iterator()");
        System.out.println("Use while(it.hashNext()) and print map with iterator");
        System.out.println("Printing Key-Value Pair");
        MapIterator<String, Integer> it = map.iterator();
        printMap(map, it);

        System.out.println("\nAfter calling while loop with it.hasNext() method");
        System.out.println("Call next method 1000 times and prev method 1000 times to show there is no error happening");

        for (int i = 0; i < 1000; i++) it.next();
        for (int i = 0; i < 1000; i++) it.prev();

        System.out.println("\nAdd new following pairs");
        System.out.println("{stone=10, queens=11, forever=12, covid=13, foo=14, bar=15} (inserted already existed keys)");

        map.put("stone", 10);   map.put("queens", 11);
        map.put("forever", 12); map.put("covid", 13);
        map.put("foo", 14);     map.put("bar", 15);

        System.out.println("\nAfter putting new entries. Call 1000 times next and 1000 times prev methods again");
        for (int i = 0; i < 1000; i++) it.next();
        for (int i = 0; i < 1000; i++) it.prev();

        System.out.println("\nPrint map without iterator");
        System.out.println(map);
        System.out.println("Print map with iterator (Remember that prints from last position after 1000 next() and prev() methods)");
        printMap(map, it);
        

        System.out.println("\nStart iterator with given key's position");
        System.out.println("it = map.iterator('YSA')");
        it = map.iterator("YSA");
        System.out.println("Use next() method and prev() method respectively -> " + it.next() + " " + it.prev());
        System.err.println("\nTry with non existing value. (it = map.iterator('Not exist')");
        it = map.iterator("Not exist");
        System.out.println("Use next() method and prev() method respectively -> " + it.next() + " " + it.prev());

        System.out.println("\nPart 1 Test Finished\nThanks for Testing <3");
    }

    public static void printMap(HashMapIterable<String, Integer> map ,MapIterator<String, Integer> it){
        int i = 0;
        while (it.hasNext())
        {
            String key = it.next();
            System.out.print(key + "\t" +map.get(key) +  "\t");
            if (i % 2 == 1) System.out.println();
            i++;
        }
    }

    public static void part2_tests() {
        
        System.out.println("\nPart2 Tests are starting.\n");
        System.out.println("\n-----------Linked List Implementation-----------");
        System.out.println("\nFirst, test HashTableChainLinkedList implementation");
        System.out.println("CAPACITY = 3, HOLD_THRESHOLD= 3.0\n");
        System.out.println("Test with string keys");
        HashTableChainLinkedList<String,Integer> linkedHash = new HashTableChainLinkedList<>();
        testString(linkedHash);
        System.out.println("\nAfter String keys, Test with 10k Integer keys\n");
        HashTableChainLinkedList<Integer,Integer> linkedHash2 = new HashTableChainLinkedList<>();
        long linked_time = testPerformance(linkedHash2);

        System.out.println("\n-----------TreeSet Implementation-----------");
        System.out.println("\nSecond, test HashTableChainTreeSet implementation");
        System.out.println("CAPACITY = 3, HOLD_THRESHOLD= 3.0\n");
        System.out.println("Test with string keys");
        HashTableChainTreeSet<String,Integer> treeSetHash = new HashTableChainTreeSet<>();
        testString(treeSetHash);
        System.out.println("\nAfter String keys, Test with 10k Integer keys\n");
        HashTableChainTreeSet<Integer,Integer> treeSetHash2 = new HashTableChainTreeSet<>();
        long treeSet_time = testPerformance(treeSetHash2);

        System.out.println("\n-----------Coalesced Hash Implementation-----------");
        HashTableCoalesced<Integer, Integer> table = new HashTableCoalesced<>();
        TestCoalescedHash_example();
        System.out.println("\nAfter example keys, Test with 10k Integer keys\n");
        long coalesced_time = testPerformance(table);

        System.out.println("All tests finished.\nPrint performance table");

        System.out.println("HashTableChainLinkedList took \t" + linked_time + " nanosecond");
        System.out.println("HashTableChainTreeSet took \t" + treeSet_time + " nanosecond");
        System.out.println("HashTableCoalesced took \t" + coalesced_time + " nanosecond");


    }

    public static void testString(KWHashMap<String,Integer> map) {

        System.out.println("Insert following entries.");
        System.out.println("{foo=0, bar=1, YSA=2, Erdogan Hoca=3, Burak Hoca=4, John=5, Doe=6, Peace=7, Lennon=8, Beatles=9}");

        map.put("foo", 0);      map.put("bar", 1);
        map.put("YSA", 2);      map.put("Erdogan", 3);
        map.put("Burak", 4);    map.put("John", 5);
        map.put("Doe", 6);      map.put("Peace", 7);
        map.put("Lennon", 8);   map.put("Beatles", 9);

        System.out.println("\nPrint map");
        System.out.println(map);

        System.out.println("Strings working fine");
        

    } 

    public static long testPerformance(KWHashMap<Integer,Integer> map) {

        boolean flag = false;
        
        long time_start = System.nanoTime();
        
        System.out.println("First, Test with a loop that iterates 30 items and print");
        
        for (int i = 0; i < 30; i++) map.put(i, i*i*i);
        System.out.println(map);

        System.out.println("\nNow, Add 10k elements and remove 10k elements respectively.");

        try {
            for(int i = 0; i < 10000; i++) map.put(i, i*i);
        } catch (Exception e) {
            flag = true;
            System.err.println("Error when putting 10k elements");
        }

        try {
            for(int i = 0; i < 10000; i++) map.remove(i);
        } catch (Exception e) {
            flag = true;
            System.err.println("Error when removing 10k elements");
        }

        if (flag) System.out.println("There is some error. Probably my table can not handle 10k elements :(");
        else System.out.println("Everything seems fine!");

        long time_end = System.nanoTime();

        System.out.println("\nTest Finished. Thanks for testing <3\n");

        return time_end - time_start;
    }

    public static void TestCoalescedHash_example() {
        HashTableCoalesced<Integer, Integer> table = new HashTableCoalesced<>();

        System.out.println("CoalescedHashMap Testing Starting");
        System.out.println("Load same example in pdf");
        System.out.println("Input = {3, 12, 13, 25, 23, 51, 42}");
        System.out.println("Print table");

        table.put(3, 0);    table.put(12, 0);
        table.put(13, 0);   table.put(25, 0);
        table.put(23, 0);   table.put(51, 0);
        table.put(42, 0);   

        System.out.println(table);

        System.out.println("\nDelete 13 and print table again");
        table.remove(13);
        System.out.println(table);

        System.out.println("\nDo some other testings with this data");
        System.out.println("Add new data to this example to show hashing working fine");
        System.out.println("New Inputs: {17,18,24,59,96,146,5,4,66,18,70,71}\n");

        table.put(17, 0);   table.put(18, 0);
        table.put(24, 0);   table.put(59, 0);
        table.put(96, 0);   table.put(146, 0);
        table.put(5, 0);    table.put(4, 0);
        table.put(66, 0);   table.put(18, 0);
        table.put(70, 0);   table.put(71, 0);

        System.out.println(table);

        System.out.println("Remove 3 and print again");
        table.remove(3);

        System.out.println(table);
        System.out.println("Add existing element. Remove an element which not in the list.");
        table.put(51, 1); table.remove(999);

        System.err.println("Test finished.");
        
    }

}