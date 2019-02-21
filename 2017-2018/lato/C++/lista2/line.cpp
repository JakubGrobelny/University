#include "line.hpp"

auto compose_vectors(const Vector& v1, const Vector& v2) -> Vector
{
    return Vector(v1.dx + v2.dx, v1.dy + v2.dy);
}

auto are_perpendicular(const Line& first, const Line& second) -> bool
{
    return compare((first.getA() / first.getB()) * (second.getA() / second.getB()), -1.0);
}

auto are_parallel(const Line& first, const Line& second) -> bool
{
    return compare(first.getA() / first.getB(), second.getA() / second.getB());
}

auto intersection(const Line& first, const Line& second) -> Point
{
    double a1, a2;
    double b1, b2;

    double x;
    double y;

    a1 = -first.getA() / first.getB();
    a2 = -second.getA() / second.getB();

    b1 = -first.getC();
    b2 = -second.getC();

    x = (b2 - b1) / (a1 - a2);
    y = a1 * x + b1;

    return Point(x, y);
}

void Point::print()
{
    std::cout << "(" << x << ", " << y << ")" << std::endl;
}

void Vector::print()
{
    std::cout << "[" << dx << ", " << dy << "]" << std::endl;
}

void Line::print()
{
    std::cout << a << "x + " << b << "y + " << c << " = 0" << std::endl;

}

Line::Line()
{
    this->a = 1;
    this->b = 1;
    this->c = 0;

    normalize();
}

Vector::Vector(double dx, double dy):
    dx(dx), dy(dy) {};

Vector::Vector(const Vector& v):
    dx(v.dx), dy(v.dy) {};

Point::Point(double x, double y): 
    x(x), y(y) {};

Point::Point(const Point& p, const Vector& v): 
    x(p.x + v.dx), y(p.y + v.dy) {};

Point::Point(const Point& p): 
    x(p.x), y(p.y) {};

auto Point::operator== (const Point& p) const -> bool
{
    return (this->x == p.x && this->y == p.y);
}

void Line::normalize()
{
    double factor;
    double sq_root = std::sqrt((a * a) + (b * b));

    if (!sq_root)
    {
        std::invalid_argument exception("Line(): can't create 0x + 0y + C = 0 line!");
        throw exception;
    }

    factor = (c >= 0 ? 1 : -1) / sq_root;

    a *= factor;
    b *= factor;
    c *= factor;
}

Line::Line(const Point& p1, const Point& p2) 
{
    if (p1 == p2)
    {
        std::invalid_argument exception("Line(): can't create a line from one point!");
        throw exception;
    }

    if (p1.x == p2.x) // vertical line
    {
        this->a = 1;
        this->b = 0;
        this->c = p1.x;
    }
    else
    {
        // y = ax + b
        double a = (p2.y - p1.y) / (p2.x - p1.x);
        double b = p1.y - a * (p1.x);

        // y = ax + b --> -ax + y - b = 0 
        this->a = -a;
        this->b = 1; 
        this->c = -b;
        
        normalize();
    }
}

Line::Line(const Vector& v) 
{
    if (v.dx == v.dy && !v.dx)
    {
        std::invalid_argument exception("Line(): can't create a line from 0 vector!");
        throw exception;
    }

    Point temp_point(v.dx, v.dy);

    double coefficient = v.dx / v.dy;
    
    coefficient = -1 / coefficient;

    // temp_point.y = (coefficient)(temp_point.x) + C
    this->c = -(temp_point.y - coefficient * temp_point.x);
    this->b = 1;
    this->a = -coefficient;

    normalize();
}

Line::Line(double A, double B, double C):
    a(A), b(B), c(C)
{
    normalize();
}

Line::Line(const Line& old, const Vector& offset) 
{
    this->a = old.a;
    this->b = old.b;
    this->c = old.c - (old.a * offset.dx) - offset.dy;

    normalize();   
}

double Line::getA() const
{
    return a;
}

double Line::getB() const
{
    return b;
}

double Line::getC() const
{
    return c;
}

auto compare(double first, double second) -> bool
{
    static const double error = 1e-9;
    return std::fabs(first - second) < error;
    //return std::fabs(first - second) < std::numeric_limits<double>::epsilon();
}

auto Line::is_perpendicular(const Vector& v) -> bool
{
    if (v.dx == 0 && v.dy == 0)
        return true;

    double v_coefficient = v.dx / v.dy;
    double l_coefficient = -a / b;

    return compare(v_coefficient * l_coefficient, -1.0);
}

auto Line::is_parallel(const Vector& v) -> bool
{
    if (v.dx == 0 && v.dy == 0)
        return true;

    double v_coefficient = v.dx / v.dy;
    double l_coefficient = -a / b;

    return compare(v_coefficient, l_coefficient);  
}

auto Line::does_contain_point(const Point& p) -> bool
{
    return compare(a * p.x + b * p.y + c, 0.0);
}

