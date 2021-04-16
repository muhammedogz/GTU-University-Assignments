/**
 * @author Muhammed Oguz
 * @version 1.0
 */

package CSE222_hw03.interface_oguz;

public interface IAdministrator {
    /**
     * Add new branch
     * @param branch specify branch.
     * @throws Exception if invalid
     */
    void addBranch(IBranch branch) throws Exception;

    /**
     * Remove Branch, if id matches.
     * @param branch specify, which branch to remove
     * @throws Exception if not match
     */
    void removeBranch(IBranch branch) throws Exception;

    /**
     * Add new employee to Branch
     * @param branch specify, which branch gonna add.
     * @throws Exception if already exist
     */
    void addBranchEmployee(IEmployee branch) throws Exception;

    /**
     * Remove employee from branch.
     * @param employee specify which employee selected.
     * @throws Exception if not match
     */
    void removeBranchEmployee(IEmployee employee) throws Exception;

    /**
     * Employee calls this function when a product is not enough or finished
     * @param branch specify in which branch a help needed
     * @param product specify which product is out of stock.
     */
    void informedProducts(IBranch branch, IProduct product);
}