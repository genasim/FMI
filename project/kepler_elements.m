function kepler_elements(a, e, i_deg, Omega_deg, w_deg, L_deg, mu)
    % Конвертиране в радиани
    deg2rad = pi / 180;
    
    i = i_deg * deg2rad;
    Omega = Omega_deg * deg2rad;
    
    % Стъпка 3: Средна и ексцентрична аномалии
    M_deg = L_deg - w_deg;
    M_deg = mod(M_deg, 360);
    M = M_deg * deg2rad;
    
    % Итеративно решение на уравнението на Кеплер (по формулата с 5 вложени синуса)
    E = M;
    for iter = 1:5
        E = M + e * sin(E);
    end
    
    % Стъпка 4: Траектории в Декартови координати
    omega_deg = w_deg - Omega_deg; % \omega = \varpi - \Omega
    omega = mod(omega_deg, 360) * deg2rad;
    
    x_kep = a * (cos(E) - e);
    y_kep = a * sqrt(1 - e^2) * sin(E);
    z_kep = 0;
    
    x = (cos(omega)*cos(Omega) - sin(omega)*sin(Omega)*cos(i)) * x_kep + ...
        (-sin(omega)*cos(Omega) - cos(omega)*sin(Omega)*cos(i)) * y_kep;
        
    y = (cos(omega)*sin(Omega) + sin(omega)*cos(Omega)*cos(i)) * x_kep + ...
        (-sin(omega)*sin(Omega) + cos(omega)*cos(Omega)*cos(i)) * y_kep;
        
    z = (sin(omega)*sin(i)) * x_kep + (cos(omega)*sin(i)) * y_kep;
    
    r = [x; y; z];
    
    % Стъпка 5: Скорости в Декартови координати
    n = sqrt((1 + mu) / a^3);
    
    x_dot_kep = -(a * n * sin(E)) / (1 - e * cos(E));
    y_dot_kep = (a * n * sqrt(1 - e^2) * cos(E)) / (1 - e * cos(E));
    z_dot_kep = 0;
    
    x_dot = (cos(omega)*cos(Omega) - sin(omega)*sin(Omega)*cos(i)) * x_dot_kep + ...
            (-sin(omega)*cos(Omega) - cos(omega)*sin(Omega)*cos(i)) * y_dot_kep;
            
    y_dot = (cos(omega)*sin(Omega) + sin(omega)*cos(Omega)*cos(i)) * x_dot_kep + ...
            (-sin(omega)*sin(Omega) + cos(omega)*cos(Omega)*cos(i)) * y_dot_kep;
            
    z_dot = (sin(omega)*sin(i)) * x_dot_kep + (cos(omega)*sin(i)) * y_dot_kep;
    
    v = [x_dot; y_dot; z_dot];
    
    disp('Coordinates (r)')
    disp(num2str(r))
    disp(['|r| = ', num2str(norm(r))])
   
    disp('Speed (v)')
    disp(num2str(v))
    disp(['|v| = ', num2str(norm(v))])
    disp(' ')
end