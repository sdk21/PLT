@echo off
lame.exe < %1 > program.cpp
cl.exe /clr program.cpp
