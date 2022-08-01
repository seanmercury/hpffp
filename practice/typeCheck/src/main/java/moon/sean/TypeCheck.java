package moon.sean;

import java.util.List;
import java.util.function.Function;

public class TypeCheck implements ITypeCheck {
    // average ns = sum ns `div` length ns
    @Override
    public int average(List<Integer> e) {
        int sum = e.stream().reduce(0, Integer::sum);
        return sum / e.size();
    }

    // f a b c =
    // (((-b) + sqrt(b^2 - 4*a*c)) / 2 * a, ((-b) + sqrt(b^2 + 4*a*c)) / 2 * a)
    @Override
    public Tuple<Double, Double> f(double a, double b, double c) {
        double v = b * b - 4 * a * c;
        double v1 = b * b + 4 * a * c;

        if (v < 0 || v1 < 0) {
            throw new IllegalArgumentException("invalid...");
        }
        return new Tuple(((-b) + Math.sqrt(v)) / 2 * a,
                ((-b) + Math.sqrt(v1)) / 2 * a);
    }

    // flip f a b = f b a
    @Override
    public <E, F> E flip(Function<F, E> f, E a, F b) {
        return f.apply(b);
    }

    // flip2 :: (a -> b -> c) -> b -> a -> c
    @Override
    public <A, B, C> C flip2(Function<A, Function<B, C>> fabc, B b, A a) {
        return fabc.apply(a).apply(b);
    }

    // munge :: (x -> y) -> (y -> (w, z)) ->   x   -> w
    @Override
    public <A, B, C, D> C munge(Function<A, B> fxy, Function<B, Tuple<C, D>> fywz, A x) {
        return fywz.apply(fxy.apply(x)).fst;
    }

    // myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
    @Override
    public <X, Y, Z, C, A> Tuple<A, Z> myFunc(Function<X, Y> fxy, Function<Y, Z> fyz, C c, Tuple<A, X> tax) {
        return new Tuple<>(tax.fst, fyz.apply(fxy.apply(tax.snd)));
    }

    // co :: (b -> c) -> (a -> b) -> a -> c
    @Override
    public <A, B, C> C co(Function<B, C> fbc, Function<A, B> fab, A a) {
        return fbc.apply(fab.apply(a));
    }

    // a :: (a -> c) -> a -> a
    @Override
    public <A, C> A a(Function<A, C> fac, A a) {
        fac.apply(a);

        return a; // TODO ?????
    }

    // b :: (a -> b) -> a -> b
    @Override
    public <A, B> B b(Function<A, B> fab, A a) {
        return fab.apply(a);
    }





    interface X {}
    interface Y {}
    interface Z {}

    // xz :: X -> Z
    // yz :: Y -> Z
    // xform :: (X, Y) -> (Z, Z)
    public Z xz(X a) {
        return null;
    }

    public Z yz(Y a) {
        return null;
    }

    // xform :: (X, Y) -> (Z, Z)
    public Tuple<Z, Z> xform(Tuple<X, Y> tab) {
        return new Tuple<>(xz(tab.fst), yz(tab.snd));
    }


    // funcT :: ((t1, t2) -> t) -> t1 -> t2 -> t
    @Override
    public <A, B, C> C funcT(Function<Tuple<A, B>, C> abc, A a, B b) {
        return abc.apply(new Tuple<>(a, b));
    }

    // f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    @Override
    public <A, B, C, D> Tuple<Tuple<B, D>, Tuple<A, C>> f(Tuple<A, B> tab, Tuple<C, D> tcd) {
        return new Tuple<>(new Tuple<>(tab.snd, tcd.snd), new Tuple<>(tab.fst, tcd.fst));
    }

}
