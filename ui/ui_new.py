#!/usr/bin/env python3

import urwid


class CommandLine(urwid.Pile):
    """Command line widget for urwid.

    The command line widgt consists of a input line and a status
    line.

    The input line has emacs like keybindings.

    Attributes:
        commands: A dictionary containing command names and fucntions.
        cmd: The input line widget.
        status: The status line widget.
        attr_map: Attribution map for the colored status line.
    """

    def __init__(self, commands):
        """Inits CommandLine widget with a dict of commands."""
        self.commands = commands

        self.cmd = urwid.Edit(caption='λ: ')
        self.status = urwid.Text('')
        self.attr_map = urwid.AttrMap(self.status, 'status')

        super(CommandLine, self).__init__([self.attr_map, self.cmd])

    def keypress(self, size, key):
        """Handle keypresses and add emacs like keybindings."""
        if key == 'enter':
            cmd = self.cmd.edit_text.split()

            self.handle_command(size, cmd[0], cmd[1:])
            self.cmd.set_edit_text('')

        elif key == 'ctrl u':
            self.cmd.set_edit_text('')

        elif key == 'ctrl w':
            cur_pos = self.cmd.edit_pos
            text = self.cmd.edit_text
            new_cur_pos = cur_pos - len(text)

            self.cmd.set_edit_text(text[cur_pos:])
            self.cmd.set_edit_pos(new_cur_pos)

        elif key == 'ctrl a':
            self.cmd.set_edit_pos(0)

        elif key == 'ctrl e':
            self.cmd.set_edit_pos(len(self.cmd.edit_text))

        elif key == 'ctrl j':
            return self.keypress(size, 'enter')

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

    cmd_list = {'quit' : quit_cmd,}

    command_line = CommandLine(cmd_list)

    fill = urwid.SolidFill(u'#')

    top = urwid.Frame(fill, None, command_line, 'footer')

    urwid.MainLoop(top, palette).run()

if __name__ == "__main__":
    main()
