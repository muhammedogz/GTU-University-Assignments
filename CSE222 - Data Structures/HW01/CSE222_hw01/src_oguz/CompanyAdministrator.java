package CSE222_hw01.src_oguz;

import CSE222_hw01.interface_oguz.Administrator;
import CSE222_hw01.interface_oguz.Person;

public class CompanyAdministrator extends Person implements Administrator {
    private Company company;

    /**
     * Default Constructor
     * @param company company info
     * @param name 
     * @param surname
     * @param password
     */
    public CompanyAdministrator(Company company, String name, String surname, String password){
        super(name, surname, password);
        this.company = company;
    }

    @Override
    public void addBranch(CompanyBranch branch) throws Exception {
        if(!company.getBranches().add(branch))
            throw new Exception("Branch adding failed. Same branch already exist");    
        return;
    }

    @Override
    public void removeBranch(CompanyBranch branch) throws Exception {
        if (!company.getBranches().contains(branch))
            throw new Exception("Branch removing failed. There is no branch with this id");
        
        // remove all employees from removed branch.
        if (company.getEmployees().removeAll(company.getBranches().getItem(branch).getEmployees()))
            System.out.println("Success removeAll");
        else
            System.out.println("RemoveAll failed"); 

        if(!company.getBranches().remove(branch))
            throw new Exception("Branch removing failed. There is no branch with this id");
        return;
    }

    @Override
    public void addBranchEmployee(CompanyEmployee employee) throws Exception {
        // add to branch employees and all employees.
        if (employee == null)
            throw new Exception("Employee adding failed");
        if (company.getBranches().getItem(employee.getBranch()) == null)
            throw new Exception("Employee adding failed. Same employee already exist or wrong branch id");
        if(!(company.getEmployees().add(employee) && company.getBranches().getItem(employee.getBranch()).getEmployees().add(employee)))
            throw new Exception("Employee adding failed. Same employee already exist or wrong branch id");
        return; 
    }

    @Override
    public void removeBranchEmployee(CompanyEmployee employee) throws Exception {
        CompanyEmployee temp;
        if ((temp = company.getEmployees().getItem(employee)) != null)
        {
            company.getEmployees().remove(temp);
            company.getBranches().getItem(temp.getBranch()).getEmployees().remove(temp);
            return;
        }

        throw new Exception("Remove Employee Failed. Employee does not match");
    }

    @Override
    public void informedProducts(CompanyBranch branch, Product product) {
        if (product.getStock() == 0)
        {
            System.out.println("Stock is finished. Manager informed. \nProduct removed from product list.");
            branch.getProducts().remove(product);
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
