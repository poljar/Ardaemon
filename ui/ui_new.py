#!/usr/bin/env python3

import urwid


class CommandLine(urwid.Pile):
    """Command line widget for urwid.

    The command line widgt consists of a input line and a status
    line.

    The input line has emacs like keybindings.

    Attributes:
        commands: A dictionary containing command names and fucntions.
        history: Hisotry of last 20 entered commands.
        hist_pos: Currently selected position in the history.
        cmd: The input line widget.
        status: The status line widget.
        attr_map: Attribution map for the colored status line.
    """

    def __init__(self, commands):
        """Inits CommandLine widget with a dict of commands."""
        self.commands = commands
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

            self.cmd.set_edit_text(text[cur_pos:])
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
        cmd_names = [x[0] for x in self.commands]

        if command in self.commands:
            self.commands[command](arguments)
        else:
            self.status.set_text('Invalid command: ' + command)


def quit_cmd(arguments):
    """Exit the program cleanly"""
    raise urwid.ExitMainLoop()


def main():
    palette = [('', 'default,bold', 'default', 'bold'),
               ('status', 'white', 'dark blue'),]

    cmd_list = {'quit'  : quit_cmd,
                'q'     : quit_cmd,}

    command_line = CommandLine(cmd_list)

    fill = urwid.SolidFill(u'#')

    top = urwid.Frame(fill, None, command_line, 'footer')

    urwid.MainLoop(top, palette).run()

if __name__ == "__main__":
    main()
