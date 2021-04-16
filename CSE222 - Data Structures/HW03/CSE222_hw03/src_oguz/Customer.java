package CSE222_hw03.src_oguz;

import CSE222_hw03.interface_oguz.*;

public class Customer extends Person implements ICustomer{
    private int id;
    private String mail;
    private HybridList<Product> products;
    private String phone = null;
    private String address = null;

    public Customer(String name, String surname, String password, String mail) {
        super(name, surname, password);
        this.mail = mail;
        this.products = new HybridList<Product>();
    }

    public void setMail(String mail) {
        this.mail = mail;
    }

    public void setPhone(String phone) {
        this.phone = phone;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getId() {
        return id;
    }

    public String getMail() {
        return mail;
    }

    public String getAddress() {
        return address;
    }

    public String getPhone() {
        return phone;
    }

    // Search all Branches till found desired product.
    @Override
    public boolean buyOnline(Company company, Product product) {
        for (int i = 0; i < company.getBranches().size(); i++) {
            // buyOffline function looks a Branch. For loop looks for all Branches.
            if (buyOffline(company, company.getBranches().get(i), product))
                return true;
        }
        return false;
    }

    @Override
    public boolean buyOffline(Company company, Branch Branch, Product product) {

        if (company.getBranches().get(Branch).getProducts().contains(product)) {
            if (company.getBranches().get(Branch).getEmployees().size() == 0) {
                System.out.println("This Branch has no employee");
                return false;
            }

            try {
                company.getBranches().get(Branch).getEmployees().get(0).removeProduct(product);
            } catch (Exception e) {
                e.printStackTrace();
                return false;
            }

            this.add(product);
            return true;
        }
        
        return false;
    }

    /**
     * Helper function
     * @param product
     */
    public void add(Product product){
        if (products.contains(product))
        {
            Product temp = products.get(product);
            temp.setStock(temp.getStock() + product.getStock());
        }
        else
            products.add(product);
    }

    /**
     * See all products of company
     * @param company specify which company
     * @return return all products as a string.
     */
    public String allProducts(Company company){
        return company.allProducts();
    }

    @Override
    public HybridList<Product> getProducts() {
        return products;
    }

    @Override
    public boolean equals(Object obj) {
        if (super.equals(obj))
            return true;
        return false;
    }

    @Override
    public String toString() { 
        if (address == null && phone == null)
            return new String("Customer:" + getName() + " " +  getSurname() +"\t" +  " ID: " + getId() + " Mail:" + getMail());
        else
            return new String("Customer:" + getName() + " " +  getSurname() +"\t" + " ID: " + getId() + " Mail:" + getMail() +
                                " Address: " + getAddress() + " Phone: " + getPhone());
    }
    
}
