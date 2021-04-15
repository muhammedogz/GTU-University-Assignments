package CSE222_hw01.src_oguz;

import CSE222_hw01.interface_oguz.Branch;
import CSE222_hw01.interface_oguz.Employee;

public class CompanyBranch implements Branch {
    ArrayContainer<Employee> employees;
    ArrayContainer<Product> products;


    @Override
    public void addProduct(Product product) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean removeProduct(Product product) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void addEmployee(CompanyEmployee employee) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean removeEmployee(CompanyEmployee employee) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Product getProduct(int index) {
        // TODO Auto-generated method stub
        return null;
    }
    
}
