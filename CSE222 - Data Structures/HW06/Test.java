/**
 * @author Muhammed Oğuz
 * 
 * This class Test all methods and provides a menu for testing CSE222_hw06 classes.
 */

import CSE222_hw06.src_oguz.*;

import java.io.IOException;
import java.util.*;

public class Test {
    public static void main(String[] args) {
        Company co = new Company();
        co.readFile("Data/e-commerce-samples.csv");


        Trader trader = new Trader("@ev", "1234", true);
        ArrayList<String> tempArr = new ArrayList<String>();
        
        tempArr.add("Clothes");
        tempArr.add("Important Sub Category");
        Product tempPro = new Product("213", "product", "150", "150", "very important", "name");
        tempPro.setCategory(tempArr);
        trader.addProduct(tempPro);

        String old = tempPro.getStringFormat();
        
        co.signUpTrader(trader);
        
        
        
        Trader temp = co.getcTraders().get(0);

        temp.setShowCategory(true);
        temp.setShowDescription(true);
        Product tempProduct = trader.getProducts().get(0);
        tempProduct.setName("DOMATESSSSSSSS");

        String str_new = tempProduct.getStringFormat();

        try {
            co.updateProducts(old, str_new);
        } catch (IOException e) {
            e.printStackTrace();
        }

        temp.printProducts();
    }
}
