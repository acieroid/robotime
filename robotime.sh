#!/bin/sh
sbcl --noinform --eval '(require :asdf)' --eval '(require :robotime)' \
    --eval '(robotime:run)' --eval '(quit)'
