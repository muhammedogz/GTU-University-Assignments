package CSE222_hw03.src_oguz;

import CSE222_hw03.interface_oguz.*;

public class Company implements ICompany{
    private String companyName;
    private KWArrayList<Administrator> admins;
    private KWArrayList<Employee> employees;
    private KWArrayList<Customer> customers;
    private KWLinkedList<Branch> branches;
    private int uniqueCustomerId = 0;

    public Company(String companyName){
        this.companyName = companyName;
        this.admins = new KWArrayList<Administrator>();
        this.employees = new KWArrayList<Employee>();
        this.customers = new KWArrayList<Customer>();
        this.branches = new KWLinkedList<Branch>();
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
    public boolean addAdmin(Administrator admin){
        if(admins.add(admin))
            return true;
        return false;
    }

    public String getCompanyName() {
        return companyName;
    }
    public KWArrayList<Administrator> getAdmins() {
        return admins;
    }
    public KWArrayList<Employee> getEmployees() {
        return employees;
    }
    public IKWArrayList<Customer> getCustomers() {
        return customers;
    }
    public KWLinkedList<Branch> getBranches() {
        return branches;
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
    public Administrator loginAdmin(Administrator admin) {
        if (admins.contains(admin))
        {
            //System.out.println("Admin contains.");
            if(admins.get(admin).getPassword().equals(admin.getPassword()))
            {
                return admins.get(admin);
            }
        }
        return null;
    }

    @Override
    public Employee loginEmployee(Employee employee) {
        if (employees.contains(employee))
        {
            //System.out.println("Employee contains.");
            if(employees.get(employee).getPassword().equals(employee.getPassword()))
            {
                return employees.get(employee);
            }
        }
        return null;
    }

    @Override
    public Customer loginCustomer(Customer customer) {
        if (customers.contains(customer))
        {
            //System.out.println("customers contains");
            if (customers.get(customer).getPassword().equals(customer.getPassword()) && 
                customers.get(customer).getMail().equals(customer.getMail()))
            {
                return customers.get(customer);
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
