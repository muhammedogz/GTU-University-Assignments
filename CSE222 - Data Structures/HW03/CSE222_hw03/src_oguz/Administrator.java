package CSE222_hw03.src_oguz;

import CSE222_hw03.interface_oguz.*;


public class Administrator extends Person implements IAdministrator {
    private ICompany company;

    /**
     * Default Constructor
     * @param company company info
     * @param name 
     * @param surname
     * @param password
     */
    public Administrator(ICompany company, String name, String surname, String password){
        super(name, surname, password);
        this.company = company;
    }

    @Override
    public void addBranch(IBranch branch) throws Exception {
        if(!company.getBranches().contains(branch))
            throw new Exception("Branch adding failed. Same branch already exist");    
        company.getBranches().addLast(branch);
        return;
    }

    @Override
    public void removeBranch(IBranch branch) throws Exception {
        if (!company.getBranches().contains(branch))
            throw new Exception("Branch removing failed. There is no branch with this id");
        
        IBranch temp = company.getBranches().get(branch);
        company.getBranches().remove(branch);
        // remove all employees from removed branch.
        if (company.getEmployees().removeAll(temp.getEmployees()))
            System.out.println("Success removeAll");
        else
            System.out.println("RemoveAll failed"); 


        return;
    }

    @Override
    public void addBranchEmployee(IEmployee employee) throws Exception {
        // add to branch employees and all employees.
        if (employee == null)
            throw new Exception("Employee adding failed");
        if (company.getBranches().get(employee.getBranch()) == null)
            throw new Exception("Employee adding failed. Same employee already exist or wrong branch id");
        if(!(company.getEmployees().add(employee) && company.getBranches().get(employee.getBranch()).getEmployees().add(employee)))
            throw new Exception("Employee adding failed. Same employee already exist or wrong branch id");
        return; 
    }

    @Override
    public void removeBranchEmployee(IEmployee employee) throws Exception {
        IEmployee temp;
        if ((temp = company.getEmployees().get(employee)) != null)
        {
            company.getEmployees().remove(temp);
            company.getBranches().get(temp.getBranch()).getEmployees().remove(temp);
            return;
        }

        throw new Exception("Remove Employee Failed. Employee does not match");
    }

    @Override
    public void informedProducts(IBranch branch, IProduct product) {
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
