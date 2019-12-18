#!/usr/bin/env node

const fs = require('fs')

//const [source, restore] = ['example.int', false]
//const [source, restore] = ['test.int', false]
const [source, restore] = ['real.int', true]
const program = fs.readFileSync(source, 'utf-8')
  .split('\n')
  .join('')
  .split(',')
  .map(v => Number(v) || 0)

// Restore for the real program
if (restore) {
  //program[1] = 12
  //program[2] = 2
  program[1] = 80
  program[2] = 18
}

function run (p /* program */) {
  let o = 0 // offset
  let op = p[o] // op-code
  let a, b

  while (op != 99) {
    switch (op) {
      case 1:
        p[p[o+3]] = p[p[o+1]] + p[p[o+2]]
        break;
      case 2:
        p[p[o+3]] = p[p[o+1]] * p[p[o+2]]
        break;
      default:
        throw new Error('Bad State!')
    }
    o += 4
    op = p[o]
  }

  return program
}

const result = run(program)
console.info(JSON.stringify(result))
