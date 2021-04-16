package CSE222_hw03.interface_oguz;

import CSE222_hw03.src_oguz.HybridList;

public interface IEmployee {
    
    /**
     * Add new product
     * @param product 
     * @throws Exception if can not add.
     */
    void addProduct(IProduct product) throws Exception;
    /**
     * Remove product if exist
     * @param product
     * @throws Exception if not match
     */
    void removeProduct(IProduct product) throws Exception;
    /**
     * Add new Customer
     * @param customer
     * @throws Exception if already exist
     */
    void addCustomer(ICustomer customer) throws Exception;
    /**
     * Remove Customer if exist
     * @param customer
     * @throws Exception if no match
     */
    void removeCustomer(ICustomer customer) throws Exception;
   
    /**
     * Get products of a customer
     * @param id customer id
     * @return return products
     * @throws Exception if not match
     */
    HybridList<IProduct> customerProducts(int id) throws Exception;

    
    IBranch getBranch();
    
}
