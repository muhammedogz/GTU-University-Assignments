package CSE222_hw03.src_oguz;

import CSE222_hw03.interface_oguz.*;


public class Employee extends Person implements IEmployee{
    private Branch Branch;
    private Company company;

    /**
     * Default Constructor
     * @param company company info
     * @param name
     * @param surname
     * @param password
     * @param Branch Branch info
     */
    public Employee(Company company, String name, String surname, String password, Branch Branch) {
        super(name, surname, password);
        this.Branch = Branch;
        this.company = company;
    }
 
    public void setBranch(Branch Branch) {
        this.Branch = Branch;
    }

    public Branch getBranch() {
        return Branch;
    }

    @Override
    public void addCustomer(Customer customer) throws Exception {
        int temp = company.getUniqueCustomerId();
        customer.setId(temp);
        company.setUniqueCustomerId(++temp);
        if(!company.getCustomers().add(customer))
            throw new Exception("Adding Customer Failed. Same Customer already exist");
    }

    @Override
    public void removeCustomer(Customer customer) throws Exception {
        if (!company.getCustomers().remove(customer))
            throw new Exception("Remove Customer Failed, There is no match");
        return;
    }

    @Override
    public void addProduct(Product product) throws Exception {
        if (product.getStock() <= 0)
        {
            System.out.println("Product stock can not be 0 or lower");
            throw new Exception("Product adding failed.");
        }
        Product temp = getBranch().getProducts().get(product);
        if (temp != null)
        {
            temp.setStock(product.getStock() + temp.getStock());
        }
        else
        {
            getBranch().getProducts().add(product);
        }
    }

    @Override
    public void removeProduct(Product product) throws Exception {
        Product temp = getBranch().getProducts().get(product);

        if (temp != null)
        {
            int check = temp.getStock() - product.getStock();
            if (check < 0)
            {
                company.getAdmins().get(0).informedProducts(getBranch(), temp);
                throw new Exception("Removing Product stock Failed");
            }
            else
            {
                temp.setStock(temp.getStock() - product.getStock());
                if (check == 0)
                    company.getAdmins().get(0).informedProducts(getBranch(), temp);
                return;
            }
        }
        throw new Exception("Removing Product stock Failed");
    }

    /**
     * Get All customers
     * @return customer list
     */
    public IKWArrayList<Customer> customerList(){
        return company.getCustomers();
    }


    @Override
    public HybridList<Product> customerProducts(int id) throws Exception {
        for (int i = 0; i < company.getCustomers().size(); i++)
        {
            if (company.getCustomers().get(i).getId() == id)
                return company.getCustomers().get(i).getProducts();
        }
        throw new Exception("There is no customer with this id");
    }

    @Override
    public boolean equals(Object obj) {
        if (super.equals(obj) /*&& this.Branch.getId() == ((Employee) obj).getBranch().getId() */)
            return true;
        return false;
    }

    @Override
    public String toString() { 
        return new String("Employee:" + getName() + " " +  getSurname()+ " Branch Id: " + getBranch().getId());
    }

    
    
}
