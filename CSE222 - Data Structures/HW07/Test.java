import java.util.Iterator;

import CSE222_hw07.src_oguz.*;

public class Test {
    
    public static void main(String[] args) {
        NavigableSetSkipList<Integer> skipList = new NavigableSetSkipList<>();

        skipList.add(5);
        skipList.add(3);
        skipList.add(2);
        skipList.add(1);
        skipList.add(15);

        System.out.println(skipList);

    }
}
