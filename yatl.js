#!/usr/bin/env node
// YATL - Yet Another Todo List (Node terminal edition).
//
// A re-implementation of the 2009 AutoHotkey YATL.ahk for the terminal,
// keeping its keyboard-driven feel and points game. No dependencies
// beyond the Node standard library. Shares its data files with yatl.py,
// so the two versions are interchangeable.
//
// Keys:
//   Enter / n    add a task below the selection
//   e            edit the selected task
//   x / Del / .  check off task (awards its points, logs to yatl.log)
//   i            increase the selected task's points by 5
//   a            add 5 points to the running score
//   s            archive the task to todo.txt (no points)
//   Right/Left   indent / dedent
//   Shift+Up/Down (or K/J)  move task up / down
//   h            toggle help
//   q            quit (saves)
//
// Data lives in ~/.local/share/yatl (override with YATL_DIR):
//   tasks.txt   the live list, one task per line
//   yatl.log    timestamped record of everything checked off
//   score.json  lifetime score (replaces the old points4.me server)
//   todo.txt    archived tasks

import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import readline from 'node:readline'

const ADD_TASK_POINTS = 5

const DATA_DIR = process.env.YATL_DIR ?? path.join(os.homedir(), '.local', 'share', 'yatl')
const TASKS_FILE = path.join(DATA_DIR, 'tasks.txt')
const LOG_FILE = path.join(DATA_DIR, 'yatl.log')
const SCORE_FILE = path.join(DATA_DIR, 'score.json')
const ARCHIVE_FILE = path.join(DATA_DIR, 'todo.txt')

const HELP_TEXT = ` Enter/n  add task          e      edit task
 x/Del/.  check off task    i      +5 points on task
 Right    indent            Left   dedent
 Shift+Up move up           Shift+Down move down
 a        add 5 to score    s      archive to todo.txt
 h        toggle help       q      quit (saves)`

// --- storage ---------------------------------------------------------

function loadTasks () {
  if (!fs.existsSync(TASKS_FILE)) return []
  return fs.readFileSync(TASKS_FILE, 'utf8').split('\n').filter(l => l.trim())
}

function saveTasks (tasks) {
  fs.mkdirSync(DATA_DIR, { recursive: true })
  fs.writeFileSync(TASKS_FILE, tasks.length ? tasks.join('\n') + '\n' : '')
}

function loadScore () {
  try {
    return Number(JSON.parse(fs.readFileSync(SCORE_FILE, 'utf8')).score) || 0
  } catch {
    return 0
  }
}

function saveScore (score) {
  fs.mkdirSync(DATA_DIR, { recursive: true })
  fs.writeFileSync(SCORE_FILE, JSON.stringify({ score }))
}

function appendLine (file, line) {
  fs.mkdirSync(DATA_DIR, { recursive: true })
  fs.appendFileSync(file, line + '\n')
}

// --- task helpers ----------------------------------------------------

function taskPoints (task) {
  const m = task.match(/^\s*\[(\d+)\]/)
  return m ? Number(m[1]) : ADD_TASK_POINTS
}

// Return [prefix like '\t\t[5] ', body text]
function splitTask (task) {
  const m = task.match(/^(\s*\[\d*x?\] ?)(.*)$/)
  return m ? [m[1], m[2]] : [`[${ADD_TASK_POINTS}] `, task]
}

function stamp () {
  const d = new Date()
  const p = n => String(n).padStart(2, '0')
  return `${d.getFullYear()}${p(d.getMonth() + 1)}${p(d.getDate())}:${p(d.getHours())}:${p(d.getMinutes())}`
}

// --- state -----------------------------------------------------------

let tasks = loadTasks()
let score = loadScore()
let session = 0
let sel = 0
let showHelp = false
let status = ''
let mode = 'list' // 'list' | 'edit'
let editBuf = ''
let editPrefix = ''
let editAction = 'add'

// --- terminal --------------------------------------------------------

const out = process.stdout

function restoreTerminal () {
  out.write('\x1b[?25h\x1b[?1049l') // show cursor, leave alternate screen
  if (process.stdin.isTTY) process.stdin.setRawMode(false)
}

function quit () {
  saveTasks(tasks)
  saveScore(score + session)
  restoreTerminal()
  process.exit(0)
}

function draw () {
  const rows = out.rows || 24
  const cols = out.columns || 80
  const lines = []
  const title = ` YATL  session: ${session}  total: ${score + session}  tasks: ${tasks.length} `
  lines.push('\x1b[7m' + title.padEnd(cols - 1).slice(0, cols - 1) + '\x1b[0m')
  lines.push('')
  const visible = tasks.slice(0, rows - 4)
  visible.forEach((task, i) => {
    const text = ' ' + task.replaceAll('\t', '    ').slice(0, cols - 2)
    lines.push(i === sel ? '\x1b[7m' + text + '\x1b[0m' : text)
  })
  if (!tasks.length) lines.push('\x1b[2m (empty - press Enter to add a task)\x1b[0m')
  const body = lines.join('\r\n')

  let bottom = ''
  if (showHelp) {
    const helpRow = rows - 8
    bottom += `\x1b[${helpRow};1H\x1b[2m` +
      HELP_TEXT.split('\n').map(l => l.slice(0, cols - 1)).join('\r\n') + '\x1b[0m'
  }
  if (mode === 'edit') {
    bottom += `\x1b[${rows};1H\x1b[K> ${editBuf.slice(0, cols - 3)}\x1b[?25h`
  } else {
    bottom += '\x1b[?25l'
    if (status) bottom += `\x1b[${rows};1H\x1b[K\x1b[1m ${status.slice(0, cols - 2)}\x1b[0m`
  }
  out.write('\x1b[2J\x1b[H' + body + bottom)
}

// --- actions ---------------------------------------------------------

function clampSel () {
  sel = Math.min(sel, Math.max(tasks.length - 1, 0))
}

function startAdd () {
  mode = 'edit'
  editAction = 'add'
  editBuf = ''
}

function startEdit () {
  if (!tasks.length) return
  const [prefix, body] = splitTask(tasks[sel])
  mode = 'edit'
  editAction = 'edit'
  editPrefix = prefix
  editBuf = body
}

function acceptEdit () {
  const text = editBuf.trim()
  mode = 'list'
  if (!text) return
  if (editAction === 'add') {
    const pos = tasks.length ? sel + 1 : 0
    tasks.splice(pos, 0, `[${ADD_TASK_POINTS}] ${text}`)
    sel = pos
    session += ADD_TASK_POINTS // the original awarded points just for adding
    status = `+${ADD_TASK_POINTS} points for adding`
  } else {
    tasks[sel] = editPrefix + text
  }
}

function checkOff () {
  if (!tasks.length) return
  const task = tasks.splice(sel, 1)[0]
  const pts = taskPoints(task)
  session += pts
  const done = task.replace(/^(\s*\[)\d*(\])/, '$1x$2')
  appendLine(LOG_FILE, stamp() + done)
  clampSel()
  status = `+${pts} points`
}

function archive () {
  if (!tasks.length) return
  appendLine(ARCHIVE_FILE, tasks.splice(sel, 1)[0])
  clampSel()
  status = 'archived'
}

function bumpPoints () {
  if (!tasks.length) return
  tasks[sel] = tasks[sel].replace(/\d+/, n => Number(n) + 5)
}

function indent (outdent = false) {
  if (!tasks.length) return
  const t = tasks[sel]
  tasks[sel] = outdent ? (t.startsWith('\t') ? t.slice(1) : t) : '\t' + t
}

function move (delta) {
  const j = sel + delta
  if (!tasks.length || j < 0 || j >= tasks.length) return
  ;[tasks[sel], tasks[j]] = [tasks[j], tasks[sel]]
  sel = j
}

// --- input -----------------------------------------------------------

function onListKey (str, key) {
  if (str === 'q' || (key.ctrl && key.name === 'q')) return quit()
  else if (key.name === 'return' || str === 'n') startAdd()
  else if (str === 'e') startEdit()
  else if (str === 'x' || str === '.' || key.name === 'delete') checkOff()
  else if (str === 's') archive()
  else if (str === 'i') bumpPoints()
  else if (str === 'a') { session += 5; status = '+5 points' }
  else if (key.name === 'up') key.shift ? move(-1) : (sel = Math.max(sel - 1, 0))
  else if (key.name === 'down') key.shift ? move(1) : (sel = Math.min(sel + 1, Math.max(tasks.length - 1, 0)))
  else if (str === 'K') move(-1)
  else if (str === 'J') move(1)
  else if (key.name === 'right') indent()
  else if (key.name === 'left') indent(true)
  else if (str === 'h') showHelp = !showHelp
}

function onEditKey (str, key) {
  if (key.name === 'return') acceptEdit()
  else if (key.name === 'escape') { mode = 'list'; editBuf = '' }
  else if (key.name === 'backspace') editBuf = editBuf.slice(0, -1)
  else if (str && !key.ctrl && !key.meta && str >= ' ' && str !== '\x7f') editBuf += str
}

// --- main ------------------------------------------------------------

readline.emitKeypressEvents(process.stdin)
if (process.stdin.isTTY) process.stdin.setRawMode(true)
process.stdin.on('keypress', (str, key = {}) => {
  if (key.ctrl && key.name === 'c') return quit()
  if (mode === 'edit') onEditKey(str, key)
  else { status = ''; onListKey(str, key) }
  saveTasks(tasks)
  saveScore(score + session)
  draw()
})
process.on('SIGTERM', quit)
process.stdout.on('resize', draw)
process.on('uncaughtException', err => {
  restoreTerminal()
  console.error(err)
  process.exit(1)
})

out.write('\x1b[?1049h\x1b[?25l') // alternate screen, hide cursor
draw()
