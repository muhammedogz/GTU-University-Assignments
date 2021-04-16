package CSE222_hw03.src_oguz;

import CSE222_hw03.interface_oguz.*;

public class Branch implements IBranch {
    private int id;
    private HybridList<Product> products;
    private KWArrayList<Employee> employees;

    public Branch(int id){
        this.id = id;
        this.products = new HybridList<Product>();
        this.employees = new KWArrayList<Employee>();
    }

    public int getId() {
        return id;
    }

    public HybridList<Product> getProducts() {
        return products;
    }
    public KWArrayList<Employee> getEmployees() {
        return employees;
    }
    public void setId(int id) {
        this.id = id;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (getId() == ((Branch) obj).getId() )
            return true;
        return false;
    }


    @Override
    public String toString() {
        String r = new String("Branch Id:" + getId());
        if (employees.size() == 0)
            r += "\nThere is no employee in this Branch";
        else
            r += " \nEmployee List\n" + employees;
        if (products.size() == 0)
            r += "\nThere is no product in this Branch";
        else
            r += "Product List\n" + products;
        return r;
    }
}
