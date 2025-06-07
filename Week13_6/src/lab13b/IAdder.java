package lab13b;

@FunctionalInterface
interface IAdder<T extends Salesperson> {
    T add(T op1, T op2);

    default String printNumSales(T obj) {
        return String.valueOf((obj.getNumSales()));
    }

    static void printSales(Salesperson s) {
        System.out.println(s.getNumSales());
    }
}