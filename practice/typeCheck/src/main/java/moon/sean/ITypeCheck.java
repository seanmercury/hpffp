package moon.sean;

import java.util.List;
import java.util.function.Function;

public interface ITypeCheck {

    // average ns = sum ns `div` length ns
    int average(List<Integer> e);

    // f a b c =
    // (((-b) + sqrt(b^2 - 4*a*c)) / 2 * a, ((-b) + sqrt(b^2 + 4*a*c)) / 2 * a)
    Tuple<Double, Double> f(double a, double b, double c);

    // flip f a b = f b a
    <E, F> E flip(Function<F, E> f, E a, F b);

    // flip2 :: (a -> b -> c) -> b -> a -> c
    <A, B, C> C flip2(Function<A, Function<B, C>> fabc, B b, A a);

    // munge :: (x -> y) -> (y -> (w, z)) ->   x   -> w
    <A, B, C, D> C munge(Function<A, B> fxy, Function<B, Tuple<C, D>> fbcd, A x);

    // myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
    <X, Y, Z, C, A> Tuple<A, Z> myFunc(Function<X, Y> fxy, Function<Y, Z> fyz, C c, Tuple<A, X> tax);

    // co :: (b -> c) -> (a -> b) -> a -> c
    <A, B, C> C co(Function<B, C> fbc, Function<A, B> fab, A a);

    // a :: (a -> c) -> a -> a
    <A, C> A a(Function<A, C> fac, A a);

    // b :: (a -> b) -> a -> b
    <A, B> B b(Function<A, B> fab, A a);

    // funcT :: ((t1, t2) -> t) -> t1 -> t2 -> t
    <A, B, C> C funcT(Function<Tuple<A, B>, C> abc, A a, B b);

    // f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    <A, B, C, D> Tuple<Tuple<B, D>, Tuple<A, C>> f(Tuple<A, B> tab, Tuple<C, D> tcd);

}
