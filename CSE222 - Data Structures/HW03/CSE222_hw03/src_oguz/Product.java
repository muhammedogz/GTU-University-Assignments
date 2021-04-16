package CSE222_hw03.src_oguz;


public class Product {
    private String name;
    private int type;
    private int color = 0;
    private int stock = 0;

    public Product(String name, int type, int color, int stock){
        this.name = name;
        this.type = type;
        this.color = color;
        this.stock = stock;
    }

    public Product(Product item) {
        this.color = item.getColor();
        this.name = item.getName();
        this.type = item.getType();
        this.stock = item.getStock();
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
    public void setStock(int stock) {
        this.stock = stock;
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
    public int getStock() {
        return stock;
    }

    @Override
    public boolean equals(Object obj) {
        if (getName().equals(((Product) obj).getName()) &&
            getColor() == ((Product) obj).getColor() &&
            getType() == ((Product) obj).getType() 
            //&& getStock() == ((Product) obj).getStock()
            )
            return true;
        return false;
    }

    @Override
    public String toString() {
        String r = "";
        if (color == 0)
            r += "Product Name:" + name +"\t" + " Type:" + type + " Stock:" + stock;
        else
            r += "Product Name:" + name +"\t" + " Type:" + type + " Color:" + color + " Stock:" + stock; 
        return r;
    }

}
