/**
 * This class is upper class but not an interface. Still it is in interface folder for code readability purpose
 */

package CSE222_hw01.interface_oguz;


public class Person {
    private String name;
    private String surname;
    private String password;

    public Person(String name, String surname, String password)
    {
        this.name = name;
        this.surname = surname;
        this.password = password;
    }

    public void setName(String name) {
        this.name = name;
    }
    public void setSurname(String surname) {
        this.surname = surname;
    }
    public void setPassword(String password) {
        this.password = password;
    }

    public String getName() {
        return name;
    }
    public String getPassword() {
        return password;
    }
    public String getSurname() {
        return surname;
    }
    
}
