#include "line.hpp"
#include <iostream>

auto main() -> int
{
    // Constructor tests

        // Vector

    Vector vec1(4, 2);
    vec1.print();
    
    Vector vec2(vec1);
    vec2.print();

        // Point

    Point p1(1, 1);
    p1.print();

    Point p2(p1, vec1);
    p2.print();

    Point p3(p1);
    p3.print();

        // Line
    try
    {
        // You can't create a line from one point so it will fail
        Line line(p1, p1);
    }
    catch (std::invalid_argument exc)
    {
        std::cerr << exc.what() << std::endl;
    }

    Line l1(vec1);
    l1.print();

    Line l2(-2, 1, 0);
    l2.print();

    Line l3(l1, vec1);
    l3.print();

    Line l4(p1, p2);
    l4.print();
    std::cout << "Printing using getters: " << l4.getA() << "x + " << l4.getB() << "y + " << l4.getC() << " = 0" << std::endl;

    // Functions tests

        // Global

    {
        Vector vector1(20, 12);
        Vector vector2(22, 30);
        std::cout << "Vector composition: " << std::endl;
        compose_vectors(vector1, vector2).print();
    }

    {
        Vector offset(1, 1);
        Line perpendicular(0.5, 1, 0);
        Line parallel(l2,offset);

        std::cout << are_perpendicular(l2, parallel) << std::endl;
        std::cout << are_perpendicular(l2, perpendicular) << std::endl;

        std::cout << are_parallel(l2, parallel) << std::endl;
        std::cout << are_parallel(l2, perpendicular) << std::endl;

        std::cout << "Intersection: ";
        intersection(perpendicular, parallel).print();
    }

        // Methods

    {
        Line line(-1, 1, 0);
        Vector perpendicular(-1, 1);
        Vector parallel(1, 1);
        Point inside(42, 42);
        Point outside(-123, -124);

        std::cout << line.is_perpendicular(perpendicular) << std::endl;
        std::cout << line.is_perpendicular(parallel) << std::endl;

        std::cout << line.is_parallel(perpendicular) << std::endl;
        std::cout << line.is_parallel(parallel) << std::endl;

        std::cout << line.does_contain_point(inside) << std::endl;
        std::cout << line.does_contain_point(outside) << std::endl;
    }

    return 0;
}