import java.util.Iterator;


import CSE222_hw07.src_oguz.*;

public class Test {
    
    public static void main(String[] args) {
        System.out.println("\n**********Welcome to perfect HW07 implementations**********\n");
        System.out.println("Test will going to run");
        System.out.println(">>>>Test For PART-1<<<<");
        testNavigableSetSkipList();
        testNavigableSetAVL();
        System.out.println("\n<<<<End Of Part-1 Tests>>>>\n");
        
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

    public static void testNavigableSetAVL() {
        System.out.println("\n---------Testing NavigableSetAVL class---------");
        System.out.println("Insert following values to NavigableSetAVL Class");
        System.out.println("10-102-15-11-20-11 (try to add twice) -12");
        NavigableSetAVL<Integer> avlNavigable = new NavigableSetAVL<>();

        avlNavigable.insert(10);
        avlNavigable.insert(102);
        avlNavigable.insert(15);
        avlNavigable.insert(11);
        avlNavigable.insert(20);
        avlNavigable.insert(11);
        avlNavigable.insert(12);

        System.out.println("Print with AVL toString Style. Balance value with following data");
        System.out.println(avlNavigable);

        System.out.println("Create a iterator and print all values");

        Iterator<Integer> it = avlNavigable.iterator();

        while (it.hasNext())
            System.out.println(it.next());

        System.out.println("Delete 11 and 399 (not exist) and print their boolean return values");
        System.out.println(avlNavigable.delete(11));
        System.out.println(avlNavigable.delete(399));

        System.out.println("Insert new values (40-45-50-53-56-59-60) and print tailSet(50, true) and headSet(60) with AVLTree's toString style");
        avlNavigable.insert(40);
        avlNavigable.insert(45);
        avlNavigable.insert(50);
        avlNavigable.insert(53);
        avlNavigable.insert(56);
        avlNavigable.insert(59);
        avlNavigable.insert(60);

        System.out.println(avlNavigable.tailSet(50, true));
        System.out.println(avlNavigable.headSet(60));

        System.out.println("Lets test with String Values");
        System.out.println("Insert Following Values");
        System.out.println("Basak Hoca - Erdogan Hoca - Burak Hoca - Muhammed - CSE222");
        NavigableSetAVL<String> avlNavigableString = new NavigableSetAVL<>();
        avlNavigableString.insert("Basak Hoca");
        avlNavigableString.insert("Erdogan Hoca");
        avlNavigableString.insert("Burak Hoca");
        avlNavigableString.insert("Muhammed");
        avlNavigableString.insert("CSE222");

        System.out.println("Delete Muhammed and HW07 (not exist) and print boolean results");
        System.out.println(avlNavigableString.delete("Muhammed"));
        System.out.println(avlNavigableString.delete("HW07"));

        System.out.println("Print with iterator");
    
        Iterator<String> itString = avlNavigableString.iterator();

        while (itString.hasNext())
            System.out.println(itString.next());

        System.out.println("End of NavigableSetAVL. Thanks for Testing :)");
    
    }

}
