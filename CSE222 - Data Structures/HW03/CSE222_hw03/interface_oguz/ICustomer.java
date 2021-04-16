package CSE222_hw03.interface_oguz;

public interface ICustomer {
    /**
     * Buy product online
     * @param company from which company, will be used to buy
     * @param product which product gonna buy
     * @return true if purchase success, false otherwise
     */
    boolean buyOnline(ICompany company, IProduct product);

    /**
     * Buy product from branch
     * @param company specify which company
     * @param branch specify which branch
     * @param product specify which product
     * @return true if success, false otherwise
     */
    boolean buyOffline(ICompany company, IBranch branch, IProduct product);

    /**
     * See all purchased products
     * @return all purchased products 
     */
    public IHybridList<IProduct> getProducts();
}
