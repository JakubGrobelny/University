#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#include <stdexcept>

auto is_prime(int64_t number) -> bool
{
    if (number < 0)
    {
        if (number == std::numeric_limits<int64_t>::min())
            return false;
        number *= -1;
    }

    const int64_t sqrt_number = std::sqrt(number);

    if (number % 2 == 0 || number % 3 == 0)
        return false;

    // all primes are 6n - 1 or 6n + 1
    for (int64_t i = 6; i <= sqrt_number; i += 6)
    {
        if (!(number % (i + 1) && number % (i - 1)))
            return false;
    }

    return true;
}

auto string_to_int64(std::string str) -> int64_t
{
    int64_t result = 0;

    int64_t digit_value = 1;

    bool negative = false;

    if (str[0] == '-')
    {
        negative = true;
        str.erase(0, 1);

        if (str.length() == 0)
        {
            std::invalid_argument exc("string_to_int64() -- invalid input!");
            throw exc;
        }
    }

    for (int i = str.length() - 1; i >= 0; i--)
    {
        if ((str[i] < '0' || str[i] > '9'))
        {
            std::invalid_argument exc("string_to_int64() -- invalid character in the number!");
            throw exc;
        }

        if (!negative)
            result += digit_value * (str[i] - '0'); 
        else
            result -= digit_value * (str[i] - '0');

        if ((result < 0 && !negative) || (result > 0 && negative))
        {
            std::invalid_argument exc("string_to_int64() -- integer overflow!");
            throw exc;
        }

        digit_value *= 10;
    }

    return result;
}

auto factorize(int64_t number) -> std::vector <int64_t>
{
    std::vector <int64_t> factors;

    if (number == 0 || number == 1 || number == -1)
    {
        factors.push_back(number);
        return factors;
    }

    int64_t factor = 2;

    if (number < 0)
    {
        factors.push_back(-1);
        factor *= -1;
    }

    while (number % 2 == 0)
    {
        number /= 2;
        factors.push_back(2);
    }

    while (number % 3 == 0)
    {
        number /= 3;
        factors.push_back(3);
    }

    factor *= 3; // starting from 6 (all primes are 6n +- 1)

    int counter = 0;

    while ((number > 0) ? (factor - 1 <= number) : (factor + 1 >= number))
    {        
        if (counter > 10000) // if no factors were found recently then it's likely that the number that's left is prime
        {
            if (is_prime(number))
            {
                factors.push_back(number > 0 ? number : -number);
                return factors;
            }

            counter = 0;
        }

        int64_t factor_minus = factor - 1;
        int64_t factor_plus = factor + 1;

        if (number % factor_plus && number % factor_minus)
        {
            factor += number > 0 ? 6 : -6;
            counter++;
        }
        else
        {
            counter = 0;

            int64_t temp_factor = number % factor_minus ? factor_plus : factor_minus;
    
            if (temp_factor < 0)
               temp_factor *= -1;

            factors.push_back(temp_factor);
            number /= temp_factor;
        }
    }

    return factors;
}

void print_instruction(char* program_name)
{
    std::cerr << "To use the program you need to put the numbers that you want to factorize as command line arguments." << std::endl;
    std::cerr << "Example: " << program_name << " 12 -69420 2137" << std::endl;
}

void print_factors(std::string str)
{
    int64_t number;

    try
    {
        number = string_to_int64(str);
    }
    catch (std::invalid_argument exc)
    {
        std::cerr << exc.what() << std::endl;
        exit(-1);
    }

    auto factors = factorize(number);

    bool first = true;

    std::cout << number << " = ";

    for (int64_t factor : factors)
    {
        if (!first)
            std::cout << " * ";
        else
            first = false;
        
        std::cout << factor;
    }
    
    std::cout << std::endl;
}

auto main(int argc, char* argv[]) -> int
{
    if (argc == 1)
        print_instruction(argv[0]);
    else
        for (int i = 1; i < argc; i++)
            print_factors(std::string(argv[i]));

    return 0;
}