cmake_minimum_required(VERSION 3.15)

project(lista7 VERSION 0.1)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wpedantic --pedantic -pedantic-errors")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_CXX_COMPILER "clang++")

file(GLOB SOURCES1 zad1.cpp)
file(GLOB SOURCES2 zad2.cpp)
file(GLOB SOURCES3 zad3.cpp)
file(GLOB SOURCES4 zad4.cpp)
file(GLOB SOURCES5 zad5.cpp)

add_executable(zad1 ${SOURCES1})
add_executable(zad2 ${SOURCES2})
add_executable(zad3 ${SOURCES3})
add_executable(zad4 ${SOURCES4})
add_executable(zad5 ${SOURCES5})