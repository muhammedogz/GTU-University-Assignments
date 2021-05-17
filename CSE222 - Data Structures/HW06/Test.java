
import CSE222_hw06.src_oguz.*;
import java.util.*;

public class Test {
    public static void main(String[] args) {
        Company co = new Company();
        co.readFile("Data/e-commerce-samples.csv");

        Trader trader = new Trader("@home");

        Iterator<Product> it = trader.getProducts().iterator();

        while(it.hasNext())
        {
            System.out.println(it.next());
        }
    }
}
