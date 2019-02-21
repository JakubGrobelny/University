#include <iostream>
#include "binary.hpp"
#include "unary.hpp"
#include "values.hpp"

auto main() -> int
{
    Variable::add_variable("x", 3);
    Variable::add_variable("y", 1);

    Expression* e1 = new Div(
        new Mul(
            new Sub(
                new Variable("x"),
                new Number(1)
            ),
            new Variable("x")
        ),
        new Number(2)
    );
    
    Expression* e2 = new Div(
        new Add(
            new Number(3),
            new Number(5)
        ),
        new Add(
            new Number(2),
            new Mul(
                new Variable("x"),
                new Number(7)
            )
        )
    );

    Expression* e3 = new Sub(
        new Add(
            new Number(2),
            new Mul(
                new Variable("x"),
                new Number(7)
            )
        ),
        new Add(
            new Mul(
                new Variable("y"),
                new Number(3)
            ),
            new Number(5)
        )
    );

    Expression* e4 = new Div(
        new Cos(
            new Mul(
                new Add(
                    new Variable("x"),
                    new Number(1)
                ),
                new Variable("x")
            )
        ),
        new Pow(
            new E(),
            new Pow(
                new Variable("x"),
                new Number(2)
            )
        )
    );

    Expression* e5 = new Abs(
        new Add(
            new Number(42),
            new Opposite(
                new Number(68)
            )
        )
    );

    Expression* e6 = new Add(
        new Pow(
            new Sin(
                new Phi()
            ),
            new Number(2)
        ),
        new Pow(
            new Cos(
                new Phi()
            ),
            new Number(2)
        )
    );

        Expression* e7 = new Ln(
        new Exp(
            new Number(3)
        )
    );

    Expression* e8 = new Inverse(
        new Div(
            new Number(10),
            new Pi()
        )
    );

    Expression* e9 = new Mod(
        new Log(
            new Number(3),
            new Mul(
                new Number(9),
                new Number(3)
            )
        ),
        new Number(2)
    );

    for (double i = 0.0; i <= 1.0; i += 0.01) 
    {
        std::cout << "\n x = " << i << "\n";
        Variable::change_value("x", i);
        std::cout << e1->to_string() << " = " << e1->evaluate() << std::endl;
        std::cout << e2->to_string() << " = " << e2->evaluate() << std::endl;
        std::cout << e3->to_string() << " = " << e3->evaluate() << std::endl;
        std::cout << e4->to_string() << " = " << e4->evaluate() << std::endl;
    }
    std::cout << std::endl;

    
    std::cout << e5->to_string() << " = " << e5->evaluate() << std::endl;
    std::cout << e6->to_string() << " = " << e6->evaluate() << std::endl;
    std::cout << e7->to_string() << " = " << e7->evaluate() << std::endl;
    std::cout << e8->to_string() << " = " << e8->evaluate() << std::endl;
    std::cout << e9->to_string() << " = " << e9->evaluate() << std::endl;

    delete e1;
    delete e2;
    delete e3;
    delete e4;
    delete e5;
    delete e6;
    delete e7;
    delete e8;
    delete e9;

    return 0;
}