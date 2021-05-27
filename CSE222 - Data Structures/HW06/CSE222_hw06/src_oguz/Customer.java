package CSE222_hw06.src_oguz;

import java.util.ArrayList;

public class Customer {
    private ArrayList<Product> products = new ArrayList<>();
    private String name;
    private String pass;

    public Customer(String name, String pass)
    {
        this.name = name;
        this.pass = pass;
    }

    public void setName(String name) {
        this.name = name;
    }
    public void setPass(String pass) {
        this.pass = pass;
    }

    public void orderProduct(Product product) {
        products.add(product);
        return;
    }

    public String getName() {
        return name;
    }
    public String getPass() {
        return pass;
    }
    
}
