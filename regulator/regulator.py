#!/usr/bin/env python3

import pyfirmata
from time import sleep


class Board():
    def __init__(self, path):
        self.board = pyfirmata.Arduino(path)

        self.pwm_pin = self.board.get_pin('d:3:o')
        self.pwm_pin.mode = pyfirmata.PWM

        self.chip_enable = self.board.get_pin('d:2:o')

        self.sensor_pin = self.board.get_pin('a:0:i')

    def start(self, pwm_value):
        self.pwm_pin.write(pwm_value)
        self.chip_enable.write(True)

    def stop(self):
        self.chip_enable.write(False)

def control_loop(board, ref):
    board.board.sp.flushInput()
    board.board.sp.flushOutput()

    dt = 0.001
    Kp = 4
    Ki = 0.5
    Kd = 0.1
    reference = ref

    integral = 0

    sensor = board.sensor_pin

    board.board.iterate()
    height_voltage = sensor.value
    previous_error = height_voltage
    try:
        while 1:
            height_voltage = sensor.value * 5
            height =  (height_voltage / 4.6) * 21.3
            error = reference - height
            integral = integral + error * dt
            derivative = (height_voltage - previous_error) / dt
            previous_error = height_voltage
#            output = Kp * err # P
#            output = Kp * error + Ki * integral # PI
            output = Kp * error + Ki * integral + Kd * derivative # PID
#            output = Kp * err

            output = output / 12

            if output < 0.25:
                output = 0.25
                integral = integral - error * dt
            elif output > 1.0:
                integral = integral - error * dt
                output = 1.0

            board.start(output)
            print('output:', output, 'v:', height_voltage, 'height', height, 'cm')

            sleep(dt)
            board.board.iterate()

    except KeyboardInterrupt:
        board.stop()
        board.board.sp.flushInput()
        board.board.sp.flushOutput()
