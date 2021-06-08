import java.util.ArrayList;
import java.util.Iterator;

import CSE222_hw07.book_implementation.*;

public class Test {

    private static ArrayList<Integer> reverse = new ArrayList<>();
    
    public static void main(String[] args) {
        SkipList<Integer> skipList = new SkipList<Integer>();

        skipList.add(4);
        skipList.add(1);
        skipList.add(5);
        skipList.add(2);
        skipList.add(6);
        skipList.add(3);

        System.out.println(skipList);
        //System.out.println(skipList.levelView());
        System.out.println(skipList.contains(2));
        System.out.println(skipList.size());
        System.out.println(skipList.remove(3));
        System.out.println(skipList.remove(124));
        System.out.println(skipList);

        Iterator<Integer> it = skipList.iterator();
        reverseIterator(it, -1);
        System.out.println(reverse);
        Iterator<Integer> reverseIt = reverse.iterator();

        while(reverseIt.hasNext())
            System.out.println(reverseIt.next());

    }

    public static void reverseIterator(Iterator<Integer> it, Integer item)
    {
        Integer temp = it.next();
        if (it.hasNext())
        {

            reverseIterator(it, temp);
        }
        reverse.add(temp);

        
        
    }
}
