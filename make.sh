#!/bin/bash
ca65 zeroG.asm -o zeroG.o
ld65 zeroG.o -o zeroG.nes -t nes
