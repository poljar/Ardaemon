#!/usr/bin/env python3

import pyfirmata
from time import sleep, monotonic
from math import pi

class Board():
    def __init__(self, path):
        self.board = pyfirmata.Arduino(path)

        self.pwm_pin = self.board.get_pin('d:3:o')
        self.pwm_pin.mode = pyfirmata.PWM

        self.chip_enable = self.board.get_pin('d:2:o')

        self.sensor_pin = self.board.get_pin('d:4:i')
        self.board.iterate()

    def start(self, pwm_value):
        self.pwm_pin.write(pwm_value)
        self.chip_enable.write(True)

    def stop(self):
        self.chip_enable.write(False)

def pulse_to_flow(pulses, dt):
    impulses_per_liter = 2500
#    impulses_per_liter = 10500
    pulses_per_sec = (pulses / dt)

    return (pulses_per_sec / impulses_per_liter) * 1000

def low_to_heigth(flow):
    pipe_area = ((1.5 / 10) ** 2) * pi
    g = 983.2

    heigth = (flow ** 2) / ((pipe_area ** 2) * (2 * g))
    return heigth + 1.5

def control_loop(board):
    board.board.sp.flushInput()
    board.board.sp.flushOutput()
    board.start(0.25)

    sensor = board.sensor_pin

    board.board.iterate()
    pulse = sensor.value
    start = monotonic()
    try:
        while 1:

            if pulse != sensor.value and sensor.value == False:
                end = monotonic()
                pulse_time = end - start
                flow = pulse_to_flow(1, pulse_time)
                print(sensor.value, pulse_time, start, end)
                print("flow: " + str((flow * 60) / 1000) + " l/min")
                print("height: " + str(low_to_heigth(flow)) + " cm")
                start = monotonic()

            pulse = sensor.value

            sleep(0.001)
            board.board.iterate()

    except KeyboardInterrupt:
        board.stop()
        board.board.sp.flushInput()
        board.board.sp.flushOutput()

def init_board(path):
    board = pyfirmata.Arduino(path)

    pwm_pin = board.get_pin('d:3:o')
    pwm_pin.mode = pyfirmata.PWM

    chip_enable = board.get_pin('d:2:o')

    sensor_pin = board.get_pin('d:4:i')
    board.iterate()

    return board

def stop(board):
    board.digital[2].write(False)

def start(board):
    board.digital[3].write(0.3)
    board.digital[2].write(True)

def loop(board):
    board.iterate()

    try:
        while 1:
            print(board.digital[4].read())
            sleep(0.1)
            board.iterate()
    except KeyboardInterrupt:
        return
