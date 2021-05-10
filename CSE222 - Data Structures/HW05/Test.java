

import CSE222_hw05.src_oguz.*;

public class Test {
    public static void main(String[] args) {
        part1_test();
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

        System.out.println("Print map");
        System.out.println(map);
        
        System.out.println("Create zero-parameter iterator -> map.iterator()");
        System.out.println("Use while(it.hashNext()) and print map with iterator");
        System.out.println("Printing Key-Value Pair");
        MapIterator<String, Integer> it = map.iterator();
        printMap(map, it);

        System.out.println("After calling while loop with it.hasNext() method");
        System.out.println("Call next method 1000 times and prev method 1000 times to show there is no error happening");

        for (int i = 0; i < 1000; i++) it.next();
        for (int i = 0; i < 1000; i++) it.prev();

        System.out.println("Add new following pairs");
        System.out.println("{stone=10, queens=11, forever=12, covid=13, foo=14, bar=15} (inserted already existed keys)");

        map.put("stone", 10);   map.put("queens", 11);
        map.put("forever", 12); map.put("covid", 13);
        map.put("foo", 14);     map.put("bar", 15);

        System.out.println("After putting new entries. Call 1000 times next and prev methods again");
        for (int i = 0; i < 1000; i++) it.next();
        for (int i = 0; i < 1000; i++) it.prev();

        System.out.println("Print map without iterator");
        System.out.println(map);
        System.out.println("Finally write map with iterator \n(skips first pair probably, because last operation is calling 1000 times prev method)");
        printMap(map, it);

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
}
