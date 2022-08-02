package moon.sean;

public class Chap2Exercise {
    interface Num<X> {
        X add(Num<X> a);
        X multiply(Num<X> a);
    }

    <X extends Num<X>> X mTh(X x, X y, X z) {
        return x.multiply(y.multiply(z));
    }


}
