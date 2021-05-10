

import CSE222_hw05.src_oguz.*;

public class Test {
    public static void main(String[] args) {
        HashMapIterable<String, Integer> map = new HashMapIterable<>();

        map.put("key", 1);
        map.put("domates", 2);
        map.put("patates", 3);
        map.put("hi", 4);
        map.put("foo", 5);

        System.out.println(map);
        

        MapIterator<String, Integer> it = map.iterator();

        while (it.hasNext())
        {
            String key = it.next();
            System.out.println(key + " " +map.get(key));
            
        }

        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        map.put("joe", 4);
        map.put("doe", 5);
        it = map.iterator();
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());
        System.out.println(it.next());




    }
}
