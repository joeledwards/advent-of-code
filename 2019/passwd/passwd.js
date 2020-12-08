#!/usr/bin/env node

const { stopwatch } = require('durations')

//const range = '100000-200000'
//const range = '111111-111122'
//const range = '223450-223456'
//const range = '123789-123800'
//const range = '111133-111144'
const range = '152085-670283'

const [start, end] = range.split('-').map(v => Number(v))

function isPasswordSlow (value) {
  const asText = value.toString()
  const parts = value
    .toString()
    .split('')
    .map(d => Number(d))
    .sort()
  const sorted = parts.join('')

  if (sorted != asText) {
    return false
  }

  const distinct = new Set(parts)

  return distinct.size < 6
}

function isPasswordFast (value) {
  let distinct = 1
  let prev = value % 10
  value = Math.floor(value / 10)

  while (value > 0) {
    const next = value % 10
    const diff = prev - next
    prev = next
    value = Math.floor(value / 10)
    if (diff < 0) {
      return false
    } else if (diff > 0) {
      distinct++
    }
  }

  return distinct < 6
}

function isPasswordWithPair (value) {
  let pairFound = false
  let occurrences = 1
  let prev = value % 10
  value = Math.floor(value / 10)

  while (value > 0) {
    const next = value % 10
    const diff = prev - next
    prev = next
    value = Math.floor(value / 10)
    if (diff < 0) {
      return false
    } else if (diff > 0) {
      if (occurrences == 2) {
        pairFound = true
      }
      occurrences = 1
    } else {
      occurrences++
    }
  }
  
  return pairFound || occurrences == 2
}

const watch = stopwatch()

// simple:fast
watch.reset().start()
let pwCount = 0
for (let i = start; i <= end; i++) {
  pwCount += isPasswordFast(i) ? 1 : 0
}

watch.stop()
console.info(`fast: ${pwCount} in ${watch}`)

// simple:slow
watch.reset().start()
pwCount = 0
for (let i = start; i <= end; i++) {
  pwCount += isPasswordSlow(i) ? 1 : 0
}

watch.stop()
console.info(`slow: ${pwCount} in ${watch}`)

// with-pair
watch.reset().start()
pwCount = 0
for (let i = start; i <= end; i++) {
  pwCount += isPasswordWithPair(i) ? 1 : 0
}

watch.stop()
console.info(`with-pair: ${pwCount} in ${watch}`)
