package CSE222_hw01.src_oguz;

import CSE222_hw01.interface_oguz.Person;

public class CompanyEmployee extends Person {
    private int branchId;

    public CompanyEmployee(String name, String surname, String password) {
        super(name, surname, password);
    }
 
    void setBranchId(int id){
        this.branchId = id;
    }

    public int getBranchId() {
        return branchId;
    }
    
}
