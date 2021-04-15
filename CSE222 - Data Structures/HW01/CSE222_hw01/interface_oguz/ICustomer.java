package CSE222_hw01.interface_oguz;

import CSE222_hw01.src_oguz.ArrayContainer;
import CSE222_hw01.src_oguz.Company;
import CSE222_hw01.src_oguz.CompanyBranch;
import CSE222_hw01.src_oguz.Product;

public interface ICustomer {
    /**
     * Buy product online
     * @param company from which company, will be used to buy
     * @param product which product gonna buy
     * @return true if purchase success, false otherwise
     */
    boolean buyOnline(Company company, Product product);

    /**
     * Buy product from branch
     * @param company specify which company
     * @param branch specify which branch
     * @param product specify which product
     * @return true if success, false otherwise
     */
    boolean buyOffline(Company company, CompanyBranch branch, Product product);

    /**
     * See all purchased products
     * @return all purchased products 
     */
    public ArrayContainer<Product> getProducts();
}
