#!/bin/sh

function f {
  echo 3
  date
}

f &

f
