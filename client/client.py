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
    fill = 0.5
    canvas = drawille.Canvas()

    def draw_tank(self, s, size):
        pipe_length = 15
        max_x, max_y = size

        # TODO this is a hack
        max_y *= 4
        max_x *= 2

        max_x -= pipe_length * 3
        max_y -= 15

        water_height = math.floor((1 - self.fill) * max_y)

        s.clear()

        # Tank
        [s.set(x,y) for x,y in drawille.line(pipe_length, 0, pipe_length, max_y)]
        [s.set(x,y) for x,y in drawille.line(pipe_length, max_y,
            max_x + pipe_length, max_y)]
        [s.set(x,y) for x,y in drawille.line(max_x + pipe_length, 0,
            max_x + pipe_length, max_y)]

        # Sine wave of water level
        [s.set(x/4, water_height + math.sin(math.radians(x + self.frame) * 12))
            for x in range(pipe_length * 4, max_x * 4 + pipe_length * 4, 2)]

        # Rest of the water
        [[s.set(x, y) for x in range(pipe_length, max_x + pipe_length)]
            for y in range(water_height + 2, max_y)]

    def set_fill_level(self, fill):
        if fill > 1.0 or fill < 0:
            return

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

        self.commands = commands

        self.sock = sock
        self.remote_cmds = remote_cmds

        self.history = []
        self.hist_pos = None

        self.cmd = urwid.Edit(caption='λ: ')
        self.status = urwid.Text('')
        self.attr_map = urwid.AttrMap(self.status, 'status')

        super(CommandLine, self).__init__([self.attr_map, self.cmd])

    def keypress(self, size, key):
        """Handle keypresses and add emacs like keybindings."""

        def enter_handler():
            cmd = self.cmd.edit_text.split()

            if cmd:
                self.history.append(self.cmd.edit_text)
                self.hist_pos = None

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

        bindings = {
            'enter'  : enter_handler,
            'ctrl a' : mov_begin,
            'ctrl e' : mov_end,
            'ctrl f' : mov_fwd,
            'ctrl b' : mov_back,
            'ctrl p' : hist_up,
            'ctrl n' : hist_down,
            'ctrl u' : del_line,
            'ctrl w' : del_word,
            'ctrl j' : enter_handler,
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
                self.status.set_text(request)
            else:
                self.sock.send(bytes(request, 'utf-8'))

        else:
            self.status.set_text('Invalid command: ' + command)


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
    cmd.status.set_text('hello world')


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
            'add'   : (add_cmd, seven_clbk),
            'seven' : (seven_cmd, seven_clbk)
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

    def read_cb():
        data = sock_file.readline()
        data = json.loads(data)
        _, f = remote_cmds[data['id']]
        f(data, loop, command_line, tank)

    loop.watch_file(sock.fileno(), read_cb)

    loop.run()

    sock.close()


if __name__ == "__main__":
    main()
