public class NewTest {

    private int i;
    private String s;

    public NewTest(int i, String s) {
        this.i = i;
        this.s = s;
    }

    public static NewTest t1() {
        return new NewTest(9, "Foo");
    }

    public static int[] t2() {
        return new int[6];
    }

    public static NewTest[] t3() {
        return new NewTest[7];
    }

    public static int[][] t4() {
        return new int[9][8];
    }

    public static NewTest[][] t5() {
        return new NewTest[10][8];
    }

    public static int[][][] t6() {
        return new int[9][8][7];
    }

    public static int[][][][][] t8() {
        return new int[9][8][7][0][5];
    }
}