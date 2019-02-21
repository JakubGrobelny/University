#include "date.hpp"

static const day_t month_lengths[2][13] = 
{
    {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31},
    {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
};

auto Date::days_elapsed(const Date& date) const -> day_t
{
    static int days_since_beginning[2][13] = 
    {
        {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
        {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}
    };

    int leap_years = (date.year / 4) - (date.year / 100) + (date.year / 400);

    day_t result = leap_years * 366 + (date.year - leap_years) * 365;
    result += days_since_beginning[this->is_leap_year()][this->month];
    result += date.day - 1;

    return result;
}

auto Date::is_leap_year() const -> bool
{
    return ((this->year % 4 == 0) and (this->year % 100 != 0)) or (this->year % 400 == 0);
}

auto Date::operator-(const Date& date) const -> day_t
{
    return this->days_elapsed(*this) - date.days_elapsed(date);
}

auto Date::operator++() -> Date&
{
    *this += 1;
    return *this;
}

auto Date::operator--() -> Date&
{
    *this -= 1;
    return *this;
}

auto Date::operator+=(day_t days) -> Date&
{
    if (days < 0)
    {
        *this -= -days;
        return *this;
    }

    this->day += days;

    while (not this->is_valid_date())
    {
        if (this->day > month_lengths[this->is_leap_year()][this->month])
        {
            this->day -= month_lengths[this->is_leap_year()][this->month];
            this->month++;
        }

        if (this->month > 12)
        {
            this->month = 1;
            this->year++;
        }
    }

    return *this;
}

auto Date::operator-=(day_t days) -> Date&
{
    if (days < 0)
    {
        *this += -days;
        return *this;
    }

    this->day -= days;

    while (not this->is_valid_date())
    {
        if (this->day < 1)
        {
            this->month--;
            this->day += month_lengths[this->is_leap_year()][this->month];
        }

        if (this->month < 1)
        {
            this->month = 12;
            this->year--;
        }
    }

    return *this;
}

auto Date::get_day() const -> day_t
{
    return this->day;
}

auto Date::get_month() const -> month_t
{
    return this->month;
}

auto Date::get_year() const -> month_t
{
    return this->year;
}

auto Date::is_valid_date() const -> bool
{
    if (this->month < 1 or this->month > 12)
        return false;
    if (this->day < 1 or this->day > month_lengths[this->is_leap_year()][this->month])
        return false;
    return true;
}

void Date::set_day(day_t d)
{
    day_t old_day = this->day;
    this->day = d;

    if (not this->is_valid_date())
    {
        this->day = old_day;
        throw std::invalid_argument("set_day(): invalid day!");
    }
}

void Date::set_month(month_t m)
{
    month_t old_month = this->month;
    this->month = m;

    if (not this->is_valid_date())
    {
        this->month = old_month;
        throw std::invalid_argument("set_month(): invalid month!");
    }
}

void Date::set_year(year_t y)
{
    this->year = y;
}

Date::Date(year_t year, month_t month, day_t day)
{
    this->year = year;
    this->month = month;
    this->day = day;

    if (not this->is_valid_date())
        throw std::invalid_argument("Date(): invalid date!");
}

void Date::print()
{
    static const char* months[] = {"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

    const char* suffix = (day % 10 == 1 and day != 11) ? "st" : 
                                        ((day % 10 == 2 and day != 12) ? "nd" : 
                                                        (day % 10 == 3 and day != 13) ? "rd" : "th");
    
    std::cout << day << suffix << " of " << months[month] << " " << year << std::endl;
}

Date::Date()
{
    auto current_time = time(NULL);
    auto* t = localtime(&current_time);

    this->year = t->tm_year + 1900;
    this->month = t->tm_mon + 1;
    this->day = t->tm_mday;
}