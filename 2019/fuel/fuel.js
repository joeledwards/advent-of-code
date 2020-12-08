#!/usr/bin/env node

const fs = require('fs')

const realModules = fs.readFileSync('./modules.fuel', 'utf-8')
    .split('\n')
    .map(l => l.trim())
    .filter(l => l !== '')
    .map(l => Number(l))

const testModules = [100756]

const modules = realModules

function run () {
  const fuel = modules.map(fuelIntegral)
  const total = fuel.reduce((total, canisters) => total + canisters, 0)

  console.info(`total: ${total}`)
}

function fuelIntegral (mass) {
  let unsupported = mass
  let totalFuel = 0

  while (unsupported > 2) {
    const additional = Math.max(0, Math.floor(unsupported / 3) - 2)
    totalFuel += additional
    unsupported = additional
  }

  return totalFuel
}

run()
