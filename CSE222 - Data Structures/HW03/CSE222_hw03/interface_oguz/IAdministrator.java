/**
 * @author Muhammed Oguz
 * @version 1.0
 */

package CSE222_hw03.interface_oguz;

import CSE222_hw03.src_oguz.*;

public interface IAdministrator {
    /**
     * Add new Branch
     * @param Branch specify Branch.
     * @throws Exception if invalid
     */
    void addBranch(Branch Branch) throws Exception;

    /**
     * Remove Branch, if id matches.
     * @param Branch specify, which Branch to remove
     * @throws Exception if not match
     */
    void removeBranch(Branch Branch) throws Exception;

    /**
     * Add new employee to Branch
     * @param Branch specify, which Branch gonna add.
     * @throws Exception if already exist
     */
    void addBranchEmployee(Employee Branch) throws Exception;

    /**
     * Remove employee from Branch.
     * @param employee specify which employee selected.
     * @throws Exception if not match
     */
    void removeBranchEmployee(Employee employee) throws Exception;

    /**
     * Employee calls this function when a product is not enough or finished
     * @param Branch specify in which Branch a help needed
     * @param product specify which product is out of stock.
     */
    void informedProducts(Branch Branch, Product product);
}
