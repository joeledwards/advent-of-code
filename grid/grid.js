#!/usr/bin/env node

const fs = require('fs')

//const source = 'example.grid'
//const source = 'test1.grid'
//const source = 'test2.grid'
const source = 'real.grid'

const [a, b] = fs.readFileSync(source, 'utf-8')
  .split('\n')
  .map(l => l.split(','))

//console.info({ a })
//console.info({ b })

let location = {x: 0, y: 0}

const dist = () => {
  const { x, y } = location
  const distance = Math.abs(x) + Math.abs(y)

  return {
    coord: [x, y],
    distance
  }
}

const up = () => { location.y += 1; return dist() }
const down = () => { location.y -= 1; return dist() }
const left = () => { location.x -= 1; return dist() }
const right = () => { location.x += 1; return dist() }

const determineAction = direction => {
  switch (direction) {
    case 'U': return up
    case 'D': return down
    case 'L': return left
    case 'R': return right
    default: throw new Error(`Unrecognized direction "${direction}"`)
  }
}

const coordinateSet = {}
let aSteps = 0
for (let cmd of a) {
  const direction = cmd.slice(0, 1)
  const distance = Number(cmd.slice(1))
  const action = determineAction(direction)
  for (let i = 1; i <= distance; i++) {
    aSteps++

    const {
      coord,
      distance
    } = action()

    coordinateSet[coord] = aSteps
  }
}

console.info(`Identified ${aSteps} coordinate(s) for wire A`)
//console.info({ coordinateSet })

const bVisited = {}
const intersectList = []
let bSteps = 0
location.x = 0
location.y = 0
for (let cmd of b) {
  const direction = cmd.slice(0, 1)
  const distance = Number(cmd.slice(1))
  const action = determineAction(direction)
  for (let i = 1; i <= distance; i++) {
    bSteps++
    const {
      coord,
      distance
    } = action()

    if (bVisited[coord] == null) {
      bVisited[coord] = bSteps
      if (coordinateSet[coord] != null) {
        intersectList.push({
          coord,
          distance,
          steps: bSteps + coordinateSet[coord]
        })
      }
    }

    //console.info(coord)
  }
}

console.info(`Identified ${bSteps} coordinate(s) for wire B`)
console.info(`The wires intersect at ${intersectList.length} location(s)`)

console.info({ intersectList })

const minDistance = intersectList.map(i => i.distance).sort((a, b) => a - b)[0]
const minSteps = intersectList.map(i => i.steps).sort((a, b) => a - b)[0]
console.info({ minDistance })
console.info({ minSteps })

