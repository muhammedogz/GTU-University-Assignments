package CSE222_hw03.interface_oguz;

import CSE222_hw03.src_oguz.*;

public interface ICustomer {
    /**
     * Buy product online
     * @param company from which company, will be used to buy
     * @param product which product gonna buy
     * @return true if purchase success, false otherwise
     */
    boolean buyOnline(Company company, Product product);

    /**
     * Buy product from Branch
     * @param company specify which company
     * @param Branch specify which Branch
     * @param product specify which product
     * @return true if success, false otherwise
     */
    boolean buyOffline(Company company, Branch Branch, Product product);

    /**
     * See all purchased products
     * @return all purchased products 
     */
    public IHybridList<Product> getProducts();
}
