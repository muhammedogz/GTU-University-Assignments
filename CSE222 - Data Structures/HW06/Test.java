/**
 * @author Muhammed Oğuz
 * 
 * This class Test all methods and provides a menu for testing CSE222_hw06 classes.
 */

import CSE222_hw06.src_oguz.*;
import java.util.*;

public class Test {
    public static void main(String[] args) {
        Company co = new Company();
        co.readFile("Data/e-commerce-samples.csv");
        System.out.println("Login state: " + co.loginTrader("@home", "1234"));

        Trader trader = co.getcTraders().get(0);
        trader.setShowDescription(true);
        trader.setShowCategory(true);
        trader.printProducts();
    }
}
