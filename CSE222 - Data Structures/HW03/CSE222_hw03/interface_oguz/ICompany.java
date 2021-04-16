package CSE222_hw03.interface_oguz;

import CSE222_hw03.src_oguz.*;

public interface ICompany {
    /**
     * Login as a admin
     * @param admin try to use this info to login
     * @return admin if success, null otherwise
     */
    Administrator loginAdmin(Administrator admin);
    
    /**
     * Login as a employee
     * @param employee try to use this info to login
     * @return employee if success, null otherwise
     */
    Employee loginEmployee(Employee employee);

    /**
     * Login as a customer
     * @param customer try to use this info to login
     * @return customer if success, null otherwise
     */
    Customer loginCustomer(Customer customer);

}
