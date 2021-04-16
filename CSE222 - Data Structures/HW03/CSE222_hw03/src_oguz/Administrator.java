package CSE222_hw03.src_oguz;

import CSE222_hw03.interface_oguz.*;


public class Administrator extends Person implements IAdministrator {
    private Company company;

    /**
     * Default Constructor
     * @param company company info
     * @param name 
     * @param surname
     * @param password
     */
    public Administrator(Company company, String name, String surname, String password){
        super(name, surname, password);
        this.company = company;
    }

    @Override
    public void addBranch(Branch Branch) throws Exception {
        if(company.getBranches().contains(Branch))
            throw new Exception("Branch adding failed. Same Branch already exist");    
        company.getBranches().addLast(Branch);
        return;
    }

    @Override
    public void removeBranch(Branch Branch) throws Exception {
        if (!company.getBranches().contains(Branch))
            throw new Exception("Branch removing failed. There is no Branch with this id");
        
        Branch temp = company.getBranches().get(Branch);
        company.getBranches().remove(Branch);
        // remove all employees from removed Branch.
        if (company.getEmployees().removeAll(temp.getEmployees()))
            System.out.println("Success removeAll");
        else
            System.out.println("RemoveAll failed"); 


        return;
    }

    @Override
    public void addBranchEmployee(Employee employee) throws Exception {
        // add to Branch employees and all employees.
        if (employee == null)
            throw new Exception("Employee adding failed");
        if (company.getBranches().get(employee.getBranch()) == null)
            throw new Exception("Employee adding failed. Same employee already exist or wrong Branch id");
        if(!(company.getEmployees().add(employee) && company.getBranches().get(employee.getBranch()).getEmployees().add(employee)))
            throw new Exception("Employee adding failed. Same employee already exist or wrong Branch id");
        return; 
    }

    @Override
    public void removeBranchEmployee(Employee employee) throws Exception {
        Employee temp;
        if ((temp = company.getEmployees().get(employee)) != null)
        {
            company.getEmployees().remove(temp);
            company.getBranches().get(temp.getBranch()).getEmployees().remove(temp);
            return;
        }

        throw new Exception("Remove Employee Failed. Employee does not match");
    }

    @Override
    public void informedProducts(Branch Branch, Product temp) {
        if (temp.getStock() == 0)
        {
            System.out.println("Stock is finished. Manager informed. \nProduct removed from product list.");
            Branch.getProducts().remove(temp);
        }
        else
        {
            System.out.println("Not enough stock for this. Manager Informed.");
        }
    }

    /**
     * Overload equals function
     */
    @Override
    public boolean equals(Object obj) {
        return super.equals(obj);
    }

    /**
     * Overload toString Function.
     */
    @Override
    public String toString() { 
        return new String("Admin:" + getName() + " " +  getSurname());
    }
    
}
