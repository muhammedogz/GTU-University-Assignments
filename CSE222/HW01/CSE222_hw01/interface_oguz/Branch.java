package CSE222_hw01.interface_oguz;

import CSE222_hw01.src_oguz.CompanyEmployee;
import CSE222_hw01.src_oguz.Product;

public interface Branch {
    
    void addProduct(Product product);

    boolean removeProduct(Product product);

    void addEmployee(CompanyEmployee employee);

    boolean removeEmployee(CompanyEmployee employee);

    Product getProduct(int index);
}
