function calculations(planet_data, t, T)
    a0 = planet_data(1);
    a1 = planet_data(2);
    e0 = planet_data(3);
    e1 = planet_data(4);
    i0 = planet_data(5);
    i1 = planet_data(6);
    Omega0 = planet_data(7);
    Omega1 = planet_data(8);
    w0 = planet_data(9);
    w1 = planet_data(10);
    L0 = planet_data(11);
    L1 = planet_data(12);
    mu = planet_data(13);

    % Стъпка 2: Пресмятаме стойността на всеки орбитален елемент
    a = a0 + a1 * T;
    e = e0 + e1 * T;
    i = i0 + (i1 / 3600) * T;
    Omega = Omega0 + (Omega1 / 3600) * T;
    w = w0 + (w1 / 3600) * T; % Това е \varpi
    L = L0 + L1 * t;

    % Поддържаме ъглите в [0, 360)
    L = mod(L, 360);
    
    kepler_elements(a, e, i, Omega, w, L, mu);
end
