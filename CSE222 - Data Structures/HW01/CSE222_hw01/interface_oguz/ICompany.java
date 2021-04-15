package CSE222_hw01.interface_oguz;

import CSE222_hw01.src_oguz.*;

public interface ICompany {
    /**
     * Login as a admin
     * @param admin try to use this info to login
     * @return admin if success, null otherwise
     */
    CompanyAdministrator loginAdmin(CompanyAdministrator admin);
    
    /**
     * Login as a employee
     * @param employee try to use this info to login
     * @return employee if success, null otherwise
     */
    CompanyEmployee loginEmployee(CompanyEmployee employee);

    /**
     * Login as a customer
     * @param customer try to use this info to login
     * @return customer if success, null otherwise
     */
    Customer loginCustomer(Customer customer);
}
