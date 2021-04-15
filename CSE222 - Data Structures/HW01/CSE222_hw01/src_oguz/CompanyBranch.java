package CSE222_hw01.src_oguz;


import CSE222_hw01.interface_oguz.Branch;

public class CompanyBranch implements Branch {
    private int id;
    private ArrayContainer<Product> products;
    private ArrayContainer<CompanyEmployee> employees;

    public CompanyBranch(int id){
        this.id = id;
        this.products = new ArrayContainer<Product>();
        this.employees = new ArrayContainer<CompanyEmployee>();
    }

    public int getId() {
        return id;
    }

    public ArrayContainer<Product> getProducts() {
        return products;
    }
    public ArrayContainer<CompanyEmployee> getEmployees() {
        return employees;
    }
    public void setId(int id) {
        this.id = id;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (getId() == ((CompanyBranch) obj).getId() )
            return true;
        return false;
    }


    @Override
    public String toString() {
        String r = new String("Branch Id:" + getId());
        if (employees.size() == 0)
            r += "\nThere is no employee in this branch";
        else
            r += " \nEmployee List\n" + employees;
        if (products.size() == 0)
            r += "\nThere is no product in this branch";
        else
            r += "Product List\n" + products;
        return r;
    }
}
