/**
 * @author Muhammed Oğuz
 * Contains key‐value pairs for a hash table.
 */

package CSE222_hw05.interface_oguz;


public interface IEntry<K, V> {

    /** Retrieves the key.
    * @return The key
    */
    public K getKey();

    /** Retrieves the value.
    * @return The value
    */
    public V getValue();

    /** Sets the value.
    * @param val The new value
    * @return The old value
    */
    public V setValue(V val);
}
