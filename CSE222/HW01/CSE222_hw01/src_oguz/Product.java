package CSE222_hw01.src_oguz;


public class Product {
    private String name;
    private int type;
    private int color = 0;

    public Product(String name, int type, int color){
        this.name = name;
        this.type = type;
        this.color = color;
    }
    public void setType(int type) {
        this.type = type;
    }
    public void setColor(int color) {
        this.color = color;
    }
    public void setName(String name) {
        this.name = name;
    }
    public int getColor() {
        return color;
    }
    public String getName() {
        return name;
    }
    public int getType() {
        return type;
    }
}
