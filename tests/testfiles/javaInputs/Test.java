public class Test {
    public static long intLongAdd(int i, long l){
        return i + l;
    }

    public Object stringID(String s) {
        return s;
    }

    public void voidFn(String s, Test t) { }

    public static class InnerTest {
        public static long intLongAdd(int i, long l){
            return i + l;
        }

        public Object stringID(String s) {
            return s;
        }

        public void voidFn(String s, Test t) { }

        public void innerClassParam(InnerTest t) { }
    }
}
