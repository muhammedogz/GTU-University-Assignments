package CSE222_hw01.src_oguz;

import CSE222_hw01.interface_oguz.ICompany;

public class Company implements ICompany{
    private String companyName;
    private ArrayContainer<CompanyAdministrator> admins;
    private ArrayContainer<CompanyEmployee> employees;
    private ArrayContainer<CompanyBranch> branches;
    private ArrayContainer<Customer> customers;
    private int uniqueCustomerId = 0;

    public Company(String companyName){
        this.companyName = companyName;
        this.admins = new ArrayContainer<CompanyAdministrator>();
        this.employees = new ArrayContainer<CompanyEmployee>();
        this.branches = new ArrayContainer<CompanyBranch>();
        this.customers = new ArrayContainer<Customer>();
        this.uniqueCustomerId = 0;
    }

    public void setUniqueCustomerId(int uniqueCustomerId) {
        this.uniqueCustomerId = uniqueCustomerId;
    }
    public int getUniqueCustomerId() {
        return uniqueCustomerId;
    }

    /**
     * Add new admin to company
     * @param admin which admin gonna add
     * @return false if already exist. true otherwise
     */
    public boolean addAdmin(CompanyAdministrator admin){
        if(admins.add(admin))
            return true;
        return false;
    }

    public String getCompanyName() {
        return companyName;
    }
    public ArrayContainer<CompanyAdministrator> getAdmins() {
        return admins;
    }
    public ArrayContainer<CompanyEmployee> getEmployees() {
        return employees;
    }
    public ArrayContainer<CompanyBranch> getBranches() {
        return branches;
    }
    public ArrayContainer<Customer> getCustomers() {
        return customers;
    }

    /**
     * See all products
     * @return all products as a String
     */
    public String allProducts(){
        String r = "";
        for (int i = 0; i < getBranches().size(); i++)
        {
            r += "Branch Id:" + getBranches().get(i).getId() + " Product List\n" + getBranches().get(i).getProducts() + "\n";
        }
        return r;
    }

    @Override
    public CompanyAdministrator loginAdmin(CompanyAdministrator admin) {
        if (admins.contains(admin))
        {
            //System.out.println("Admin contains.");
            if(admins.getItem(admin).getPassword().equals(admin.getPassword()))
            {
                return admins.getItem(admin);
            }
        }
        return null;
    }

    @Override
    public CompanyEmployee loginEmployee(CompanyEmployee employee) {
        if (employees.contains(employee))
        {
            //System.out.println("Employee contains.");
            if(employees.getItem(employee).getPassword().equals(employee.getPassword()))
            {
                return employees.getItem(employee);
            }
        }
        return null;
    }

    @Override
    public Customer loginCustomer(Customer customer) {
        if (customers.contains(customer))
        {
            //System.out.println("customers contains");
            if (customers.getItem(customer).getPassword().equals(customer.getPassword()) && 
                customers.getItem(customer).getMail().equals(customer.getMail()))
            {
                return customers.getItem(customer);
            }
        }
        return null;
    }


    @Override
    public String toString() {
        String r =  "\nInformation of Company -> " + getCompanyName() + "\n" +
                    "Admins     \n----------\n" + admins.toString()     + "\n" +
                    "Branches and Products   \n----------\n" + branches.toString()   + "\n" +
                    "Employees  \n----------\n" + employees.toString()  + "\n" +
                    "Customers  \n----------\n" + customers.toString();
        return r;
    }
    
}
