#!/usr/bin/env python3
"""YATL - Yet Another Todo List (Linux terminal edition).

A re-implementation of the 2009 AutoHotkey YATL.ahk for the terminal,
keeping its keyboard-driven feel and points game. No dependencies
beyond the Python standard library.

Keys:
  Enter / n   add a task below the selection
  e           edit the selected task
  x / Del / . check off task (awards its points, logs to yatl.log)
  i           increase the selected task's points by 5
  a           add 5 points to the running score
  s           archive the task to todo.txt (no points)
  Right/Left  indent / dedent
  J / K       move task down / up
  h           toggle help
  q           quit (saves)

Data lives in ~/.local/share/yatl (override with YATL_DIR):
  tasks.txt   the live list, one task per line
  yatl.log    timestamped record of everything checked off
  score.json  lifetime score (replaces the old points4.me server)
  todo.txt    archived tasks
"""

import curses
import json
import os
import re
import time

ADD_TASK_POINTS = 5

DATA_DIR = os.environ.get(
    "YATL_DIR", os.path.join(os.path.expanduser("~"), ".local", "share", "yatl")
)
TASKS_FILE = os.path.join(DATA_DIR, "tasks.txt")
LOG_FILE = os.path.join(DATA_DIR, "yatl.log")
SCORE_FILE = os.path.join(DATA_DIR, "score.json")
ARCHIVE_FILE = os.path.join(DATA_DIR, "todo.txt")

HELP_TEXT = """\
 Enter/n  add task          e      edit task
 x/Del/.  check off task    i      +5 points on task
 Right    indent            Left   dedent
 K        move up           J      move down
 a        add 5 to score    s      archive to todo.txt
 h        toggle help       q      quit (saves)"""


def load_tasks():
    if not os.path.exists(TASKS_FILE):
        return []
    with open(TASKS_FILE, encoding="utf-8") as f:
        return [line.rstrip("\n") for line in f if line.strip()]


def save_tasks(tasks):
    os.makedirs(DATA_DIR, exist_ok=True)
    with open(TASKS_FILE, "w", encoding="utf-8") as f:
        f.write("\n".join(tasks) + ("\n" if tasks else ""))


def load_score():
    try:
        with open(SCORE_FILE, encoding="utf-8") as f:
            return int(json.load(f).get("score", 0))
    except (OSError, ValueError):
        return 0


def save_score(score):
    os.makedirs(DATA_DIR, exist_ok=True)
    with open(SCORE_FILE, "w", encoding="utf-8") as f:
        json.dump({"score": score}, f)


def append_line(path, line):
    os.makedirs(DATA_DIR, exist_ok=True)
    with open(path, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def task_points(task):
    m = re.match(r"\s*\[(\d+)\]", task)
    return int(m.group(1)) if m else ADD_TASK_POINTS


def split_task(task):
    """Return (prefix like '\t\t[5] ', body text)."""
    m = re.match(r"(\s*\[\d*x?\] ?)(.*)", task)
    if m:
        return m.group(1), m.group(2)
    return "[%d] " % ADD_TASK_POINTS, task


class Yatl:
    def __init__(self, stdscr):
        self.scr = stdscr
        self.tasks = load_tasks()
        self.score = load_score()
        self.session = 0
        self.sel = 0
        self.show_help = False
        self.status = ""

    # --- drawing -------------------------------------------------------

    def draw(self):
        self.scr.erase()
        h, w = self.scr.getmaxyx()
        title = " YATL  session: %d  total: %d  tasks: %d " % (
            self.session,
            self.score + self.session,
            len(self.tasks),
        )
        self.scr.addnstr(0, 0, title.ljust(w - 1), w - 1, curses.A_REVERSE)
        top = 2
        for i, task in enumerate(self.tasks[: h - top - 2]):
            attr = curses.A_REVERSE if i == self.sel else curses.A_NORMAL
            self.scr.addnstr(top + i, 1, task.replace("\t", "    "), w - 2, attr)
        if not self.tasks:
            self.scr.addnstr(top, 1, "(empty - press Enter to add a task)", w - 2, curses.A_DIM)
        if self.show_help:
            for j, line in enumerate(HELP_TEXT.splitlines()):
                if h - 9 + j < h - 1:
                    self.scr.addnstr(h - 9 + j, 1, line, w - 2, curses.A_DIM)
        if self.status:
            self.scr.addnstr(h - 1, 1, self.status, w - 2, curses.A_BOLD)
        self.scr.refresh()

    def prompt(self, initial=""):
        """Single-line editor on the bottom row. Enter accepts, Esc cancels."""
        h, w = self.scr.getmaxyx()
        buf = list(initial)
        curses.curs_set(1)
        try:
            while True:
                self.scr.move(h - 1, 0)
                self.scr.clrtoeol()
                self.scr.addnstr(h - 1, 0, "> " + "".join(buf), w - 1)
                self.scr.refresh()
                ch = self.scr.get_wch()
                if ch in ("\n", "\r"):
                    return "".join(buf).strip()
                if ch == "\x1b":  # Esc
                    return None
                if ch in ("\x7f", "\b") or ch == curses.KEY_BACKSPACE:
                    if buf:
                        buf.pop()
                elif isinstance(ch, str) and ch.isprintable():
                    buf.append(ch)
        finally:
            curses.curs_set(0)

    # --- actions -------------------------------------------------------

    def add_task(self):
        text = self.prompt()
        if not text:
            return
        task = "[%d] %s" % (ADD_TASK_POINTS, text)
        pos = self.sel + 1 if self.tasks else 0
        self.tasks.insert(pos, task)
        self.sel = pos
        self.session += ADD_TASK_POINTS  # the original awarded points just for adding
        self.status = "+%d points for adding" % ADD_TASK_POINTS

    def edit_task(self):
        if not self.tasks:
            return
        prefix, body = split_task(self.tasks[self.sel])
        text = self.prompt(body)
        if text:
            self.tasks[self.sel] = prefix + text

    def check_off(self):
        if not self.tasks:
            return
        task = self.tasks.pop(self.sel)
        pts = task_points(task)
        self.session += pts
        done = re.sub(r"(\s*\[)\d*(\])", r"\1x\2", task, count=1)
        stamp = time.strftime("%Y%m%d:%H:%M")
        append_line(LOG_FILE, stamp + done)
        self.sel = min(self.sel, max(len(self.tasks) - 1, 0))
        self.status = "+%d points" % pts

    def archive(self):
        if not self.tasks:
            return
        task = self.tasks.pop(self.sel)
        append_line(ARCHIVE_FILE, task)
        self.sel = min(self.sel, max(len(self.tasks) - 1, 0))
        self.status = "archived"

    def bump_points(self):
        if not self.tasks:
            return
        task = self.tasks[self.sel]
        self.tasks[self.sel] = re.sub(
            r"\d+", lambda m: str(int(m.group()) + 5), task, count=1
        )

    def indent(self, out=False):
        if not self.tasks:
            return
        t = self.tasks[self.sel]
        self.tasks[self.sel] = t[1:] if out and t.startswith("\t") else t if out else "\t" + t

    def move(self, delta):
        j = self.sel + delta
        if not self.tasks or not (0 <= j < len(self.tasks)):
            return
        self.tasks[self.sel], self.tasks[j] = self.tasks[j], self.tasks[self.sel]
        self.sel = j

    # --- main loop -----------------------------------------------------

    def run(self):
        curses.curs_set(0)
        while True:
            self.draw()
            self.status = ""
            ch = self.scr.getch()
            if ch in (ord("q"), 17):  # q or Ctrl-Q
                break
            elif ch in (curses.KEY_ENTER, 10, 13, ord("n")):
                self.add_task()
            elif ch == ord("e"):
                self.edit_task()
            elif ch in (ord("x"), ord("."), curses.KEY_DC):
                self.check_off()
            elif ch == ord("s"):
                self.archive()
            elif ch == ord("i"):
                self.bump_points()
            elif ch == ord("a"):
                self.session += 5
                self.status = "+5 points"
            elif ch == curses.KEY_UP:
                self.sel = max(self.sel - 1, 0)
            elif ch == curses.KEY_DOWN:
                self.sel = min(self.sel + 1, max(len(self.tasks) - 1, 0))
            elif ch == curses.KEY_RIGHT:
                self.indent()
            elif ch == curses.KEY_LEFT:
                self.indent(out=True)
            elif ch in (ord("K"), curses.KEY_SR):
                self.move(-1)
            elif ch in (ord("J"), curses.KEY_SF):
                self.move(1)
            elif ch == ord("h"):
                self.show_help = not self.show_help
            save_tasks(self.tasks)
            save_score(self.score + self.session)


def main():
    curses.wrapper(lambda scr: Yatl(scr).run())


if __name__ == "__main__":
    main()
