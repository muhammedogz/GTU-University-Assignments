/**
 * @author Muhammed Oguz
 * 
 * This class implements NavigableSet interface with AVL Tree.
 */

package CSE222_hw07.src_oguz;

import java.util.*;
import CSE222_hw07.book_implementation.AVLTree;
import CSE222_hw07.interface_oguz.INavigableSetAVL;

public class NavigableSetAVL<E extends Comparable<E>> 
                implements NavigableSet<E>, INavigableSetAVL<E> {


    /* Data fields */

    // Keep data in AVLTree data structure
    private AVLTree<E> data;

    public NavigableSetAVL() {
        data = new AVLTree<>();
    }

    @Override
    public boolean insert(E item) {
        return add(item);
    }

    @Override
    public Iterator<E> iterator() {
        return data.iterator();
    }

    @Override
    public boolean delete(E item) {
        return data.remove(item);
    }

    @Override
    public NavigableSetAVL<E> headSet(E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public NavigableSetAVL<E> headSet(E arg0, boolean arg1) {
        Iterator<E> it = iterator();

        NavigableSetAVL<E> head = new NavigableSetAVL<>();

        while (it.hasNext())
        {
            E temp = it.next();
            if (arg1 == true)
            {
                if (temp.compareTo(arg0) <= 0)
                    head.insert(temp);
            }
            else
            {
                if (temp.compareTo(arg0) < 0)
                    head.insert(temp);
            }
        }

        return head;
    }

    @Override
    public NavigableSetAVL<E> tailSet(E fromElement) {
        return tailSet(fromElement, false);
    }

    @Override
    public NavigableSetAVL<E> tailSet(E arg0, boolean arg1) {
        Iterator<E> it = iterator();

        NavigableSetAVL<E> tail = new NavigableSetAVL<>();

        while (it.hasNext())
        {
            E temp = it.next();
            if (arg1 == true)
            {
                if (temp.compareTo(arg0) >= 0)
                    tail.insert(temp);
            }
            else
            {
                if (temp.compareTo(arg0) > 0)
                    tail.insert(temp);
            }
        }

        return tail;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean remove(Object arg0) {
        return data.remove((E) arg0);
    }

    @Override
    public String toString() {
        return data.toString();
    }

    @Override
    public Comparator<? super E> comparator() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E first() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E last() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean add(E arg0) {
        return data.add(arg0);
    }

    @Override
    public boolean addAll(Collection<? extends E> arg0) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean contains(Object arg0) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean containsAll(Collection<?> arg0) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean removeAll(Collection<?> arg0) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean retainAll(Collection<?> arg0) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public int size() {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public Object[] toArray() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public <T> T[] toArray(T[] arg0) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E ceiling(E e) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Iterator<E> descendingIterator() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public NavigableSet<E> descendingSet() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E floor(E e) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E higher(E e) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E lower(E e) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E pollFirst() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E pollLast() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public SortedSet<E> subSet(E arg0, E arg1) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public NavigableSet<E> subSet(E arg0, boolean arg1, E arg2, boolean arg3) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
