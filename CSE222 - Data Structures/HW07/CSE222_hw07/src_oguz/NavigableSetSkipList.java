package CSE222_hw07.src_oguz;


import CSE222_hw07.interface_oguz.INavigableSetSkipList;
import CSE222_hw07.book_implementation.SkipList;
import java.util.*;


public class NavigableSetSkipList<E extends Comparable<E>> 
        implements NavigableSet<E>, INavigableSetSkipList<E> {

    /* Data fields */
    // Keep regular data.
    private SkipList<E> data;
    // Keep reverse data (used for DescendingIterator)
    private ArrayList<E> reverseData;

    public NavigableSetSkipList() {
        data = new SkipList<>();
        reverseData = new ArrayList<>();
    }

    @Override
    public boolean insert(E e) {
        return add(e);
    }

    @Override
    public E delete(E e) {
        return data.remove(e);
    }

    @Override
    public Iterator<E> descendingIterator() {
        reverseData.clear();
        descendingIteratorHelper(iterator());    
        return reverseData.iterator();
    }

    @Override
    public boolean add(E e) {
        data.add(e);
        return true;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean remove(Object o) {
        return (data.remove((E) o) != null);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(Object o) {
        return data.contains((E) o);
    }

    @Override
    public Iterator<E> iterator() {
        return data.iterator();
    }
    
    private void descendingIteratorHelper(Iterator<E> it) {
        E temp;

        if (it.hasNext())
        {
            temp = it.next();
            descendingIteratorHelper(it);
            reverseData.add(temp);
        }
    }

    @Override
    public int size() {
        return data.size();
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
    public boolean addAll(Collection<? extends E> c) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Object[] toArray() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public <T> T[] toArray(T[] a) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public E ceiling(E e) {
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
    public SortedSet<E> headSet(E toElement) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public NavigableSet<E> headSet(E arg0, boolean arg1) {
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

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public NavigableSet<E> tailSet(E arg0, boolean arg1) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
