import java.util.Iterator;

import CSE222_hw07.src_oguz.*;

public class Test {
    
    public static void main(String[] args) {
        System.out.println("\n**********Welcome to perfect HW07 implementations**********\n");
        System.out.println("Test will going to run");
        testNavigableSetSkipList();
    }

    public static void testNavigableSetSkipList() {
        System.out.println("\n-----------Testing NavigableSetSkipList class-----------\n");
        System.out.println("This class implemented NavigableSet with skipList data structure");

        System.out.println("Create a NavigableSet with following values");
        System.out.println("5-51-15-4-57-12");
        NavigableSetSkipList<Integer> navigableSetSkipList = new NavigableSetSkipList<>();

        navigableSetSkipList.insert(5);
        navigableSetSkipList.insert(51);
        navigableSetSkipList.insert(15);
        navigableSetSkipList.insert(4);
        navigableSetSkipList.insert(57);
        navigableSetSkipList.insert(12);

        System.out.println("Print whole list");
        System.out.println(navigableSetSkipList);

        System.out.println("Delete 4 from set. Print removed item");
        System.out.println(navigableSetSkipList.delete(4));
        
        System.out.println("Delete 40 (non exist). Print removed item (if not exist print null)");
        System.out.println(navigableSetSkipList.delete(40));

        System.out.println("Create descendingIterator and print");
        Iterator<Integer> reverseIt = navigableSetSkipList.descendingIterator();

        while (reverseIt.hasNext())
            System.out.println(reverseIt.next());

        System.out.println("Delete 5 and Insert -10 and reCreate descendingIterator and print");
        navigableSetSkipList.delete(5);
        navigableSetSkipList.insert(-10);

        reverseIt = navigableSetSkipList.descendingIterator();

        while (reverseIt.hasNext())
            System.out.println(reverseIt.next());


        System.out.println("Lets try with some String values");
        System.out.println("Insert following values");
        System.out.println("Basak Hoca - Erdogan Hoca - Muhammed - Burak Hoca - '20' - '10' - HW08 - HW07");
        NavigableSetSkipList<String> navigableSetSkipListString = new NavigableSetSkipList<>();
        
        navigableSetSkipListString.insert("Basak Hoca");
        navigableSetSkipListString.insert("Erdogan Hoca");
        navigableSetSkipListString.insert("Muhammed");
        navigableSetSkipListString.insert("Burak Hoca");
        navigableSetSkipListString.insert("20");
        navigableSetSkipListString.insert("10");
        navigableSetSkipListString.insert("HW08");
        navigableSetSkipListString.insert("HW07");

        System.out.println("Print new NavigableSet");
        System.out.println(navigableSetSkipListString);

        System.out.println("Delete Muhammed and Print removed item");
        System.out.println(navigableSetSkipListString.delete("Muhammed"));
        System.out.println("Delete CSE222 (not exit) and Print removed item (if not exit print null)");
        System.out.println(navigableSetSkipListString.delete("CSE222"));

        System.out.println("Create descendingIterator and print");
        Iterator<String> reverseStringIterator = navigableSetSkipListString.descendingIterator();

        while (reverseStringIterator.hasNext())
            System.out.println(reverseStringIterator.next());

        System.out.println("End of NavigableSetSkipList. Thanks for Testing :)");

    }
}
