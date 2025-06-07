package lab13b;

public class Salesperson implements IAdder<Salesperson> {
    private final String name;
    private double salary;
    private final int numSales;

    public Salesperson(String name, double salary, int numSales) {
        this.name = name;
        this.salary = salary;
        this.numSales = numSales;
    }

    public void addBonus(double amount) {
        salary += amount;
    }

    public int getNumSales() {
        return numSales;
    }

    public double getSalary() {
        return salary;
    }

    public String getName() {
        return name;
    }

    @Override
    public Salesperson add(Salesperson op1, Salesperson op2) {
        int sumSales = op1.getNumSales() + op2.getNumSales();
        return new Salesperson(op1.getName(), op2.getSalary(), sumSales);
    }

    @Override
    public String printNumSales(Salesperson obj) {
        return String.format("Num Sales: %s", IAdder.super.printNumSales(obj));
    }

    //    public void printNumSales(Salesperson obj) {
//        System.out.println(obj.getNumSales());
//    }

    @Override
    public String toString() {
        return String.format("name: %s, salary: %.2f numsales: %d ", name, salary, numSales);
    }
}
