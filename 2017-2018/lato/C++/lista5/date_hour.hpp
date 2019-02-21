#ifndef date_hour_hpp
#define date_hour_hpp

#include "date.hpp"
#include <iostream>
#include <cstdint>

typedef int32_t hour_t;
typedef int32_t minute_t;
typedef int64_t second_t;

class DateHour final : public Date
{
protected:

    hour_t hour;
    minute_t minutes;
    second_t seconds;

public:

    auto get_hour()    const -> hour_t;
    auto get_minutes() const -> minute_t;
    auto get_seconds() const -> second_t;

    void set_hour(hour_t h);
    void set_minutes(minute_t m);
    void set_seconds(second_t s);

    void print();

    auto operator==(const DateHour& time) const -> bool;
    auto operator<(const DateHour& time)  const -> bool;
    auto operator<=(const DateHour& time) const -> bool;
    auto operator>(const DateHour& time)  const -> bool;
    auto operator>=(const DateHour& time) const -> bool;

    auto operator++(int) -> DateHour&;
    auto operator--(int) -> DateHour&;
    auto operator+=(second_t sec) -> DateHour&;
    auto operator-=(second_t sec) -> DateHour&;

    friend auto operator-(const DateHour& time1, const DateHour& time2) -> second_t;

    DateHour();
    DateHour(year_t year, month_t month, day_t day, hour_t hour, minute_t minutes, second_t seconds);
    DateHour(const DateHour& time) = default;
    auto operator=(const DateHour& time) -> DateHour& = default;
};

auto operator-(const DateHour& time1, const DateHour& time2) -> second_t;

#endif