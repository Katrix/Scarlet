public class GenericTest<A, B extends Integer> {
	private A a;

	public <A1, B1 extends Integer> A1 foo(A1 a, B1 b) {
		return null;
	}

	public void bar(java.util.List<? extends String> a) {}

	public void bin(java.util.List<? super String> a) {}

	class Inner<D> {

		public void bar(java.util.List<? extends String> a) {}

		public void bin(java.util.List<? super String> a) {}
	}
}