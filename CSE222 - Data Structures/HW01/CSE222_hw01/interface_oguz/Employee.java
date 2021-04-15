/**
 * @author Muhammed Oguz
 * @version 1.0
 */

package CSE222_hw01.interface_oguz;

import CSE222_hw01.src_oguz.ArrayContainer;
import CSE222_hw01.src_oguz.Customer;
import CSE222_hw01.src_oguz.Product;

public interface Employee {
    
    /**
     * Add new product
     * @param product 
     * @throws Exception if can not add.
     */
    void addProduct(Product product) throws Exception;
    /**
     * Remove product if exist
     * @param product
     * @throws Exception if not match
     */
    void removeProduct(Product product) throws Exception;
    /**
     * Add new Customer
     * @param customer
     * @throws Exception if already exist
     */
    void addCustomer(Customer customer) throws Exception;
    /**
     * Remove Customer if exist
     * @param customer
     * @throws Exception if no match
     */
    void removeCustomer(Customer customer) throws Exception;
   
    /**
     * Get products of a customer
     * @param id customer id
     * @return return products
     * @throws Exception if not match
     */
    ArrayContainer<Product> customerProducts(int id) throws Exception;

}
