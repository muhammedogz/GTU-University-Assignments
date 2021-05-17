
import CSE222_hw06.src_oguz.*;

public class Test {
    public static void main(String[] args) {
        Company co = new Company();
        try {
            co.read_file("makefile");
        } catch (Exception e) {
            
            e.printStackTrace();
        }
    }
}
