#!/usr/bin/env python3
# Copyright (c) 2015 Damir Jelić
# Use of this source code is governed by a BSD-style license that can be
# found in the LICENSE file.

import math
import json
import urwid
import socket
import asyncio
import drawille


class TankWidget(urwid.Widget):
    _sizing = frozenset([urwid.BOX])
    ignore_focus = True

    frame = 0
    fill = 0.0

    margin = 15

    canvas = drawille.Canvas()

    # TODO this is a hack
    def adjust_size(self, size):
        max_x, max_y = size

        max_y *= 4
        max_x *= 2

        max_x -= self.margin * 3
        max_y -= 15

        return (max_x, max_y)

    def draw_water_line(self, canvas, size):
        max_x, max_y = self.adjust_size(size)
        margin = self.margin

        water_height = math.floor((1 - self.fill) * max_y)

        # Sine wave of water level
        [canvas.set(x/4, water_height + math.sin(math.radians(x + self.frame) * 12))
            for x in range(margin * 4, max_x * 4 + margin * 4, 2)]


    def draw_tank(self, s, size):
        max_x, max_y = self.adjust_size(size)

        margin = self.margin

        water_height = math.floor((1 - self.fill) * max_y)

        s.clear()

        # Tank
        [s.set(x,y) for x,y in drawille.line(margin, 0, margin, max_y)]
        [s.set(x,y) for x,y in drawille.line(margin, max_y,
            max_x + margin, max_y)]
        [s.set(x,y) for x,y in drawille.line(max_x + margin, 0,
            max_x + margin, max_y)]

        self.draw_water_line(s, size)

        # Rest of the water
        [[s.set(x, y) for x in range(margin, max_x + margin)]
            for y in range(water_height + 2, max_y)]

    def set_fill_level(self, fill):
        fill /= 21

        if fill > 1.0 or fill < 0:
            return

        if fill != self.fill:
            self.fill = fill
            self._invalidate()

    def update(self):
        self.frame += 1
        self._invalidate()

    def rows(self, size, focus=False):
        col, row = size
        return row

    def render(self, size, focus=False):
        col, row = size

        if col < 17:
            ws = '\n' * (row - 1)
            return urwid.Text(ws).render((col,))
        elif col < 30 or row < 10:
            ws = '\n' * (row - 1)
            return urwid.Text('Not enough space!' + ws, align='center').render((col,))

        self.draw_tank(self.canvas, size)

        return urwid.Text('\n\n' + self.canvas.frame() + '\n', align='center').render((col,))


class CommandLine(urwid.Pile):
    """Command line widget for urwid.

    The command line widgt consists of a input line and a status
    line.

    The input line has emacs like keybindings.

    Attributes:
        commands: A dictionary containing command names and fucntions.
        remote_cmds: A dictionary containing remote commands.
        sock: A socket for the remote commands.
        history: Hisotry of last 20 entered commands.
        hist_pos: Currently selected position in the history.
        cmd: The input line widget.
        status: The status line widget.
        attr_map: Attribution map for the colored status line.
    """

    def __init__(self, commands, remote_cmds, sock):
        """Inits CommandLine widget with a dict of commands."""

        self.loop = None
        self.commands = commands
        self.clear_count = 0

        self.sock = sock
        self.remote_cmds = remote_cmds

        self.history = []
        self.hist_pos = None

        self.cmd = urwid.Edit(caption='λ: ')
        self.status = urwid.Text('')
        self.attr_map = urwid.AttrMap(self.status, 'status')

        super(CommandLine, self).__init__([self.attr_map, self.cmd])

    def set_loop(self, loop):
        self.loop = loop

    def set_status(self, txt):
        self.status.set_text(txt)
        self.clear_count += 1

        def clear_status(loop, data):
            self.clear_count -= 1

            if self.clear_count == 0:
                self.status.set_text('')

        self.loop.set_alarm_in(5, clear_status)

    def keypress(self, size, key):
        """Handle keypresses and add emacs like keybindings."""

        def enter_handler():
            cmd = self.cmd.edit_text.split()
            self.hist_pos = None

            if cmd:
                self.history.append(self.cmd.edit_text)

                self.handle_command(size, cmd[0], cmd[1:])
                self.cmd.set_edit_text('')

        def hist_up():
            if self.history:
                if self.hist_pos is None:
                    self.hist_pos = len(self.history) - 1
                    self.cmd.set_edit_text(self.history[self.hist_pos])
                    mov_end()

                elif self.hist_pos > 0:
                    self.hist_pos = self.hist_pos - 1
                    self.cmd.set_edit_text(self.history[self.hist_pos])
                    mov_end()

        def hist_down():
            if self.history:
                if self.hist_pos < len(self.history) - 1:
                    self.hist_pos = self.hist_pos + 1
                    self.cmd.set_edit_text(self.history[self.hist_pos])
                    mov_end()

        def del_line():
            self.cmd.set_edit_text('')

        def del_word():
            cur_pos = self.cmd.edit_pos
            text = self.cmd.edit_text
            new_cur_pos = cur_pos - len(text)

            text = text.split(' ')
            new_cur_pos = cur_pos - len(text[-1])
            text = ' '.join(text[:-1])

            self.cmd.set_edit_text(text)
            self.cmd.set_edit_pos(new_cur_pos)

        def mov_begin():
            self.cmd.set_edit_pos(0)

        def mov_end():
            self.cmd.set_edit_pos(len(self.cmd.edit_text))

        def mov_back():
            self.cmd.set_edit_pos(self.cmd.edit_pos - 1)

        def mov_fwd():
            self.cmd.set_edit_pos(self.cmd.edit_pos + 1)

        def complete_cmd():
            filter_func = lambda x: x.startswith(self.cmd.edit_text)

            commands = list(self.commands.keys()) + list(self.remote_cmds.keys())
            completions = list(filter(filter_func, commands))

            if len(completions) == 0:
                pass
            elif len(completions) == 1:
                self.cmd.edit_text = str(completions[0]) + ' '
                mov_end()
            else:
                status_txt = [x + ', ' for x in completions]
                self.set_status(status_txt)

        bindings = {
            'enter'  : enter_handler,
            'ctrl a' : mov_begin,
            'ctrl e' : mov_end,
            'ctrl f' : mov_fwd,
            'ctrl b' : mov_back,
            'ctrl p' : hist_up,
            'up'     : hist_up,
            'down'   : hist_down,
            'ctrl u' : del_line,
            'ctrl w' : del_word,
            'ctrl j' : enter_handler,
            'tab'    : complete_cmd,
        }

        if key in bindings:
            bindings[key]()
        else:
            return super(CommandLine, self).keypress(size, key)

    def handle_command(self, size, command, arguments):
        """Handle and dispatch commands."""

        if command in self.commands:
            self.commands[command](arguments)

        elif command in self.remote_cmds:
            f, _ = self.remote_cmds[command]
            request, err = f(command, arguments)

            if err:
                self.set_status(request)
            else:
                self.sock.send(bytes(request, 'utf-8'))

        else:
            self.set_status('Invalid command: ' + command)


def quit_cmd(arguments):
    """Exit the program cleanly"""

    raise urwid.ExitMainLoop()


def pack_json(payload):
    return json.dumps(payload) + '\n'


def seven_cmd(ID, arguments):
    payload = {
        "id": ID,
        "jsonrpc": "2.0",
        "method": "seven",
    }

    return pack_json(payload), None


def add_cmd(ID, arguments):
    try:
        x, y = map(float, arguments)
    except ValueError as e:
        return "Invalid argument(s): " + str(e), -1

    payload = {
        "id": ID,
        "jsonrpc": "2.0",
        "method": "add",
        "params": {
            "x": x,
            "y": y,
        },
    }

    return pack_json(payload), None


def seven_clbk(data, loop, cmd, tank):
    result = data['result']
    cmd.set_status('result: ' + str(result))


def add_clbk(data, loop, cmd, tank):
    result = data['result']
    cmd.set_status('result: ' + str(result))


def ref_cmd(ID, arguments):
    if len(arguments) < 1:
        return "Argument needed!" , -1
    elif len(arguments) > 1:
        return "To many arguments!" , -1


    try:
        ref = float(arguments[0])
    except ValueError as e:
        return "Invalid argument(s): " + str(e), -1

    payload = {
        "id": ID,
        "jsonrpc": "2.0",
        "method": "set-reference",
        "params": {
            "reference": ref,
        },
    }

    return pack_json(payload), None


def ref_clbk(data, loop, cmd, tank):
    try:
        err = data['error']
        cmd.set_status(err['message'])
    except KeyError:
        pass


def lvl_cmd(ID, arguments):
    if len(arguments) != 0:
        return "To many arguments" , -1

    payload = {
        "id": ID,
        "jsonrpc": "2.0",
        "method": "get-level",
    }

    return pack_json(payload), None


def lvl_clbk(data, loop, cmd, tank):
    result = data['result']
    cmd.set_status('level: ' + str(result))


def update_tank_clbk(data, loop, cmd, tank):
    result = data['result']

    try:
        level = float(result)
    except ValueError as e:
        cmd.set_status('Invalid result from server!')

    tank.set_fill_level(level)


def main():
    palette = [
            ('', 'default,bold', 'default', 'bold'),
            ('status', 'white', 'dark blue'),
    ]

    cmd_list = {
            'quit'  : quit_cmd,
            'q'     : quit_cmd,
    }

    remote_cmds = {
            'add'           : (add_cmd, add_clbk),
            'seven'         : (seven_cmd, seven_clbk),
            'set-reference' : (ref_cmd, ref_clbk),
            'get-level'     : (lvl_cmd, lvl_clbk),
            'update-tank'   : (lvl_cmd, update_tank_clbk),
    }

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    try:
        sock.connect(('localhost', 4040))
    except Exception as e:
        print('Error connecting: ' + str(e))
        return
    sock_file = sock.makefile('r')

    command_line = CommandLine(cmd_list, remote_cmds, sock)

    tank = TankWidget()

    top = urwid.Frame(tank, None, command_line, 'footer')

    evl = urwid.AsyncioEventLoop(loop=asyncio.get_event_loop())

    loop = urwid.MainLoop(top, palette, event_loop=evl)

    command_line.set_loop(loop)

    def read_cb():
        data = sock_file.readline()
        data = json.loads(data)
        _, f = remote_cmds[data['id']]
        f(data, loop, command_line, tank)

    def periodic_tasks(loop, data):
        request , _ = lvl_cmd('update-tank', [])
        sock.send(bytes(request, 'utf-8'))

        tank.update()

        loop.set_alarm_in(0.3, periodic_tasks)

    loop.watch_file(sock.fileno(), read_cb)

    loop.set_alarm_in(0.3, periodic_tasks)

    loop.run()

    sock.close()


if __name__ == "__main__":
    main()
