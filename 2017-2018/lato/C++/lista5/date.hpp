#ifndef date_hpp
#define date_hpp

#include <ctime>
#include <stdexcept>
#include <iostream>

typedef int32_t day_t;
typedef int32_t month_t;
typedef int32_t year_t;

class Date
{
    auto days_elapsed(const Date& date) const -> day_t;
    auto is_valid_date() const -> bool;

protected:

    day_t day;
    month_t month;
    year_t year;

    auto is_leap_year() const -> bool;

public:

    auto get_day()   const -> day_t;
    auto get_month() const -> month_t;
    auto get_year()  const -> year_t;

    void set_day(day_t d);
    void set_month(month_t m);
    void set_year(year_t y);

    void print();

    virtual auto operator-(const Date& date) const -> day_t;
    auto operator++() -> Date&;
    auto operator--() -> Date&;
    auto operator+=(day_t days) -> Date&;
    auto operator-=(day_t days) -> Date&;

    Date();
    Date(year_t year, month_t month, day_t day);
    Date(const Date& date) = default;
    auto operator=(const Date& date) -> Date& = default;
};

#endif