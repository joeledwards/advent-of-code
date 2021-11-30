#!/bin/sh

sbt assembly
cp target/scala-2.13/advent-2021-assembly-1.0.0.jar advent-2021.jar
