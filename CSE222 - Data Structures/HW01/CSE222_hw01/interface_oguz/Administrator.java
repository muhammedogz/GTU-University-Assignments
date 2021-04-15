/**
 * @author Muhammed Oguz
 * @version 1.0
 */

package CSE222_hw01.interface_oguz;

import CSE222_hw01.src_oguz.CompanyBranch;
import CSE222_hw01.src_oguz.CompanyEmployee;
import CSE222_hw01.src_oguz.Product;

public interface Administrator {
    /**
     * Add new branch
     * @param branch specify branch.
     * @throws Exception if invalid
     */
    void addBranch(CompanyBranch branch) throws Exception;

    /**
     * Remove Branch, if id matches.
     * @param branch specify, which branch to remove
     * @throws Exception if not match
     */
    void removeBranch(CompanyBranch branch) throws Exception;

    /**
     * Add new employee to Branch
     * @param branch specify, which branch gonna add.
     * @throws Exception if already exist
     */
    void addBranchEmployee(CompanyEmployee branch) throws Exception;

    /**
     * Remove employee from branch.
     * @param employee specify which employee selected.
     * @throws Exception if not match
     */
    void removeBranchEmployee(CompanyEmployee employee) throws Exception;

    /**
     * Employee calls this function when a product is not enough or finished
     * @param branch specify in which branch a help needed
     * @param product specify which product is out of stock.
     */
    void informedProducts(CompanyBranch branch, Product product);
}
