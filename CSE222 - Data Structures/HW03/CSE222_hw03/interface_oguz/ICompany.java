package CSE222_hw03.interface_oguz;


public interface ICompany {
    /**
     * Login as a admin
     * @param admin try to use this info to login
     * @return admin if success, null otherwise
     */
    IAdministrator loginAdmin(IAdministrator admin);
    
    /**
     * Login as a employee
     * @param employee try to use this info to login
     * @return employee if success, null otherwise
     */
    IEmployee loginEmployee(IEmployee employee);

    /**
     * Login as a customer
     * @param customer try to use this info to login
     * @return customer if success, null otherwise
     */
    ICustomer loginCustomer(ICustomer customer);

    IKWLinkedList<IBranch> getBranches();

    IKWArrayList<IEmployee> getEmployees();
}
