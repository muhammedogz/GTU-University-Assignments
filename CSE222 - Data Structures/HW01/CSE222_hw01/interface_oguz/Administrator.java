package CSE222_hw01.interface_oguz;

import CSE222_hw01.src_oguz.CompanyEmployee;

public interface Administrator {
    /**
     * Add new branch
     * @param branch specify branch.
     */
    void addBranch(Branch branch);

    /**
     * Remove Branch, if id matches.
     * @param branch specify, which branch to remove
     * @return true if successful. false otherwise.
     */
    boolean removeBranch(Branch branch);

    /**
     * Add new employee to Branch
     * @param branch specify, which branch gonna add.
     * @return true if successful. false otherwise.
     */
    boolean addBranchEmployee(Branch branch);

    /**
     * Remove employee from branch.
     * @param branch specify which branch selected
     * @param employee specify which employee selected.
     * @return true if successful, false otherwise.
     */
    boolean removeBranchEmployee(Branch branch, CompanyEmployee employee);
}
