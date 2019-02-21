#ifndef _LINE
#define _LINE

#include <stdexcept>
#include <cmath>
#include <limits>
#include <iostream>

class Vector
{
    public:

    const double dx = 0;
    const double dy = 0;

    Vector(double dx, double dy);
    Vector(const Vector& v);
    Vector() = default;

    void print();

    auto operator= (const Vector& v) -> Vector = delete;
};

class Point
{
    public:

    const double x = 0;
    const double y = 0;

    Point(double x, double y);
    Point(const Point& p, const Vector& v);
    Point(const Point& p);
    Point() = default;

    void print();

    auto operator= (const Point& p) -> Point = delete;
    auto operator== (const Point& p) const -> bool;
};

class Line
{
    double a;
    double b;
    double c;

    void normalize();

    public:

    Line(const Point& p1, const Point& p2);
    Line(const Vector& v);
    Line(double A, double B, double C);
    Line(const Line& old, const Vector& offset);
    Line();

    auto is_perpendicular(const Vector& v) -> bool;
    auto is_parallel(const Vector& v) -> bool;
    auto does_contain_point(const Point& p) -> bool;
    void print();

    auto getA() const -> double;
    auto getB() const -> double;
    auto getC() const -> double;
};

auto compose_vectors(const Vector& v1, const Vector& v2) -> Vector;

auto are_perpendicular(const Line& first, const Line& second) -> bool;

auto are_parallel(const Line& first, const Line& second) -> bool;

auto intersection(const Line& first, const Line& second) -> Point;

auto compare(double first, double second) -> bool;

#endif