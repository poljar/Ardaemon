#!/usr/bin/env python3
import locale
import curses
import curses.ascii
import random
import math

class Tank:
    def __init__(self, height, bottom_area):
        self.height = height
        self.bottom_area = bottom_area
        self.water_height = 0.5

    def set_water_height(self, height):
        self.water_height = height

class Widget:
    def __init__(self, win, x=0, y=0):
        if win is not None:
            self.screen = win
            self.win = win.subwin(1, 1, y, x)

    def draw(self):
        self.win.erase()

    def resize(self, y, x, height, width):
        try:
            self.win.resize(height, width)
        except:
            self.win.mvderwin(0, 0)
            self.win.resize(height, width)

        self.win.erase()

        try:
            self.win.mvderwin(y, x)
        except:
            pass

    def refresh(self):
        self.win.refresh()

class TankWidget(Widget, Tank):
    def __init__(self, win, height, bottom_area):
        Widget.__init__(self, win)
        self.tank = Tank(height, bottom_area)

    def draw(self):
        Widget.draw(self)
        self.draw_tank(self.tank)

    def redraw(self):
        self.draw_tank(self.tank)
        self.win.refresh()

    def getrandspacer(self):
        if random.randint(1, 10) < 2:
            return "~"
        else:
            return " "

    def draw_tank(self, tank):
        height, width = self.win.getmaxyx()
        win = self.win
        height -= 10
        width -= 5
        i = 10 // 2

        air = " "
        water = "~"
        tank_width = width - 15
        tank_height = height
        fillLevel = tank_height * tank.water_height

        flow_pos = random.randint(1, 8)
        flow_line_in = "." * flow_pos + " " + "." * (7 - flow_pos)
        flow_pos = random.randint(1, 12)
        flow_line_out = "." * flow_pos + " " + "." * (11 - flow_pos)

        in_pipe = ["     |",
                   "_____|",
                   flow_line_in,
                   "-----i  .",
                   "     |  .."]

        out_pipe = ["|_______",
                    flow_line_out,
                    "i--------   ."]

        for line in in_pipe:
            spacer = air
            spacern = tank_width - len(line)
            try:
                win.addstr(i, 5, line + spacer * spacern + '|')
            except:
                pass
            i += 1

        for j in range(0, tank_height - len(in_pipe)):
            line = " " * 5 + "|"
            spacern = tank_width - len(line)
            spacer = ""
            currentPosition = tank_height - j

            if (currentPosition - 1) == math.ceil(fillLevel):
                spacer = water * spacern
            elif (currentPosition) > math.floor(fillLevel):
                spacer = air * spacern
            else:
                for k in range(0, spacern):
                    spacer += self.getrandspacer()

            line += spacer + '|'
            try:
                win.addstr(i, 5, line)
            except:
                pass
            i += 1

        for end in out_pipe:
            spacer = ""
            line = " " * 5 + "|"
            spacern = tank_width - len(line)

            if tank.water_height == 0:
                spacer = air * spacern
            else:
                for k in range(0, spacern):
                    spacer += self.getrandspacer()
            line += spacer + end
            try:
                win.addstr(i, 5, line)
            except:
                pass
            i += 1

        line = " " * 5 + "+" + "-" * (tank_width - 6) + "+" + 11 * " " + ".."
        try:
            win.addstr(i, 5, line)
        except:
            pass

class InputWidget(Widget):
    def __init__(self, win, x, y):
        Widget.__init__(self, win, x, y)
        self.ps = "# "
        self.psLength = len(self.ps)
        self.buf = ""
        self.cursorPosition = self.psLength

    def draw(self):
        Widget.draw(self)
        self.win.addstr(0, 0, self.ps + self.buf)

    def _addch(self, c):
        (backy, backx) = self.win.getyx()
        oldch = self.win.inch()
        self.win.addch(c)

        self._addch(oldch)
        self.win.move(backy, backx)

    def addch(self, c):
        self._addchToBuf(c)
        self.cursorPosition += 1
        self._addch(c)

    def delch(self, i):
        self._delchFromBuf(i - 1)
        self.win.delch(0, i - 1)

    def _addchToBuf(self, c):
        self.buf = self.buf[:self.cursorPosition - self.psLength] + str(c) + self.buf[self.cursorPosition - self.psLength:]

    def _delchFromBuf(self, i):
        self.buf = self.buf[:i - self.psLength] + self.buf[(i + 1) - self.psLength:]

    def getBufLen(self):
        return len(self.buf) + self.psLength

    def clear(self):
        self.win.move(0, 2)
        self.win.clrtoeol()
        self.cursorPosition = self.psLength
        self.buf = ""

    def getCommnad(self):
        return self.buf

    def refresh(self):
        (y, x) = self.screen.getmaxyx()
        self.screen.move(y - 1, self.cursorPosition)
        self.win.move(0, self.cursorPosition)
        self.win.refresh()

    def handle_key(self, key):
        (y, x) = self.win.getyx()

        if type(key) == int:
            if key in (curses.KEY_BACKSPACE, curses.KEY_LEFT):
                if x > self.psLength:
                    self.cursorPosition -= 1
                    if key == curses.KEY_BACKSPACE:
                        self.delch(x)

            elif key == curses.KEY_RIGHT:
                (maxY, maxX) = self.screen.getmaxyx()
                if x < maxX - 1 and x < self.getBufLen():
                    self.cursorPosition += 1

        elif type(key) == str:
            if ord(key) == curses.ascii.ETB:
                for i in range (x, self.psLength, -1):
                    self.delch(i)

                self.cursorPosition = self.psLength

            elif ord(key) == curses.ascii.SOH:
                self.cursorPosition = self.psLength

            elif ord(key) == curses.ascii.ENQ:
                self.cursorPosition = self.getBufLen()

            elif ord(key) == curses.ascii.FF:
                self.win.refresh()
                self.screen.refresh()

            elif key.isprintable():
                (maxY, maxX) = self.screen.getmaxyx()
                if x < maxX - 1:
                    self.addch(key)
        self.refresh()

class StatusWidget(Widget):
    def __init__(self, win, x, y):
        Widget.__init__(self, win, x, y)
        self.message = ""

    def draw(self):
        Widget.draw(self)
        height, wid = self.win.getmaxyx()
        self.win.attron(curses.color_pair(1))
        self.win.hline(0, 0, " ", wid)
        self.win.addstr(0, 0, self.message)
        self.win.attroff(curses.color_pair(1))

    def setMessage(self, msg):
        self.message = "last command -> '" + msg + "'"
        self.draw()

class UI():
    def __init__(self, screen):
        locale.setlocale(locale.LC_ALL, '')

        self.win = screen

        curses.use_default_colors()
        curses.init_pair(1, curses.COLOR_WHITE, curses.COLOR_BLUE)
        curses.init_pair(2, curses.COLOR_WHITE, curses.COLOR_MAGENTA)

        self.win.keypad(1)
        self.win.timeout(200)

        height, width = self.win.getmaxyx()

        self.inwidget = InputWidget(self.win, 0, height - 1)
        self.status = StatusWidget(self.win, 0, height - 2)
        self.tank = TankWidget(self.win, 10, 2)
        self.tank.set_water_height(1.0)
        self.quit = False

    def handle_command(self, command):
        if command in ("/quit", "/q"):
            self.quit = True
        else:
            pass

    def handle_key(self, key):
        if key == curses.KEY_RESIZE:
            self.resize()
            self.draw()

        elif key in (curses.ascii.NL, curses.ascii.LF, curses.KEY_ENTER, '\n'):
            command = self.inwidget.getCommnad()
            self.inwidget.clear()
            self.status.setMessage(command)
            self.refresh()
            self.handle_command(command)

        else:
            self.inwidget.handle_key(key)

    def run(self):
        self.resize()
        self.draw()

        while not self.quit:
            self.refresh()
            try:
                c = self.win.get_wch()
                self.handle_key(c)
            except curses.error:
                self.tank.redraw()

        self.end()

    def end(self):
        curses.nocbreak()
        self.win.keypad(0)
        curses.echo()
        curses.endwin()

    def resize(self):
        height, width = self.win.getmaxyx()
        self.status.resize(height - 2, 0, 1, width)
        self.inwidget.resize(height - 1, 0, 1, width)
        self.tank.resize(0, 0, height - 2, width)

    def draw(self):
        self.tank.draw()
        self.status.draw()
        self.inwidget.draw()

    def refresh(self):
        try:
# FIXME this needs serious fixing, touchwin is wrong here
            self.status.refresh()
            self.tank.refresh()
            self.win.refresh()
            self.inwidget.refresh()
        except:
            pass

def main(screen):
    ui = UI(screen)
    ui.run()

curses.wrapper(main)