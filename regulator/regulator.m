pump_test_voltages = [12 10 8 5 4 3];
pump_test_volumes  = [1.3 1.4 1.1 0.68 0.49 0.3] .* 1000;
pump_test_times    = [45 60.0003 59.72 60.0026 60.0025 59.99];

sensor_heights = [4   5   6   7   8   9  10  11  12   13  14  15 16  17]
sensor_values  = [75 116 160 205 245 288 342 380 420 462 519 580 610 669]

sensor_constants = sensor_heights ./ sensor_values

pump_test_flow = pump_test_volumes ./ pump_test_times;
pump_test_kp = pump_test_flow ./ pump_test_voltages;

nosle_radius = 3 / 20;          % cm
tank_radius  = 80 / 20;         % cm

g  = 983.2;                     % gravitational acceleration              - cm/s²
h0 = 10;                        % operating point                         - cm
A  = (tank_radius ^ 2) * pi;    % bottom area of the water tank           - cm²
a  = (nosle_radius ^ 2) * pi;   % cross sectional area of the output pipe - cm²
Ts = 1 / 10                     % sample time                             - s

kp = mean(pump_test_kp)         % pump constant                           - (cm³/s) / V

k1 = kp / A
k2 = (a * sqrt(2 * g)) / (A * 2 * sqrt(h0))

Gs = tf(k1, [1 k2])            % transfer function of the system         - cm / V

Gd = c2d(Gs, Ts)
