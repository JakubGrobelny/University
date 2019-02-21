#include "date_hour.hpp"

auto DateHour::get_hour() const -> hour_t
{
    return this->hour;
}

auto DateHour::get_minutes() const -> minute_t
{
    return this->minutes;
}

auto DateHour::get_seconds() const -> second_t
{
    return this->seconds;
}

void DateHour::set_hour(hour_t h)
{
    if (h < 0 or h >= 24)
        throw std::invalid_argument("set_hour(): invalid hour!");
    else
        this->hour = h; 
}

void DateHour::set_minutes(minute_t m)
{
    if (m < 0 or m >= 60)
        throw std::invalid_argument("set_minutes(): invalid minutes!");
    else
        this->minutes = m;
}

void DateHour::set_seconds(second_t s)
{
    if (s < 0 or s >= 60)
        throw std::invalid_argument("set_seconds(): invalid seconds!");
    else
        this->seconds = s;
}

auto DateHour::operator==(const DateHour& time) const -> bool
{
    return (this->year == time.year) and
           (this->month == time.month) and
           (this->day == time.day) and
           (this->hour == time.hour) and
           (this->minutes == time.minutes) and
           (this->seconds == time.seconds);
}

auto DateHour::operator<(const DateHour& time) const -> bool
{
    return (*this - time) >= 0;
}

auto DateHour::operator<=(const DateHour& time) const -> bool
{
    return (*this < time) or (*this == time);
}

auto DateHour::operator>(const DateHour& time) const -> bool
{
    return not (*this <= time);
}

auto DateHour::operator>=(const DateHour& time) const -> bool
{
    return (*this > time) or (*this == time);
}

auto DateHour::operator++(int) -> DateHour&
{
    *this += 1;
    return *this;
}

auto DateHour::operator--(int) -> DateHour&
{
    *this += -1;
    return *this;
}

auto DateHour::operator+=(second_t sec) -> DateHour&
{
    day_t day_diff = sec / (3600 * 24);
    sec -= day_diff * 3600 * 24;
    this->Date::operator+=(day_diff);

    hour_t hour_diff = sec / 3600;
    sec -= hour_diff * 3600;

    minute_t min_diff = sec / 60;
    sec -= min_diff * 60;

    this->hour += hour_diff;
    this->minutes += min_diff;
    this->seconds += sec;

    return *this;
}

auto DateHour::operator-=(second_t sec) -> DateHour&
{
    *this += -sec;
    return *this;
}

auto operator-(const DateHour& time1, const DateHour& time2) -> second_t
{
    second_t result = (second_t)time1.Date::operator-(time2) * 3600 * 24;
    second_t time_diff = 3600 * (time1.hour - time2.hour) + 60 * (time1.minutes - time2.minutes) + (time1.seconds - time2.seconds);
    result += time_diff;
    return result;
}

void DateHour::print()
{
    static const char* months[] = {"", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

    const char* suffix = (day % 10 == 1 and day != 11) ? "st" : 
                                        ((day % 10 == 2 and day != 12) ? "nd" : 
                                                        (day % 10 == 3 and day != 13) ? "rd" : "th");
    
    std::cout << ((hour >= 10) ? "" : "0") << hour << ":" << ((minutes >= 10) ? "" : "0");
    std::cout << minutes << ":" << ((seconds >= 10) ? "" : "0") << seconds << std::endl;
    std::cout << day << suffix << " of " << months[month] << " " << year << std::endl;
}

DateHour::DateHour()
{
    auto current_time = time(NULL);
    auto* t = localtime(&current_time);

    this->hour = t->tm_hour;
    this->minutes = t->tm_min;
    this->seconds = t->tm_sec;
}

DateHour::DateHour(year_t year = 2000, month_t month = 1, day_t day = 1, hour_t hour = 12, minute_t minutes = 0, second_t seconds = 0)
{
    try
    {
        this->set_month(month);
        this->set_day(day);
    }
    catch (std::invalid_argument exc)
    {
        throw exc;
    }

    this->year = year;

    if (minutes < 0 or minutes >= 60 or seconds < 0 or seconds >= 60 or hour < 0 or hour >= 24)
        throw std::invalid_argument("DateHour(): invalid time!");

    this->hour = hour;
    this->minutes = minutes;
    this->seconds = seconds;
}