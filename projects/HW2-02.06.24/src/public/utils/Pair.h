#pragma once

template<class T, class E>
struct Pair
{
    Pair(T fst, E snd) : first(fst), second(snd) {}
    Pair() : first(), second() {};

    T first;
    E second;
};
