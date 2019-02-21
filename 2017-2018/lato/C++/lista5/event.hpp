#ifndef event_hpp
#define event_hpp

#include "date_hour.hpp"
#include <string>

class Event
{
    DateHour date;
    std::string description;

public:

    auto get_description() const -> std::string { return description; };
    auto get_date() const -> DateHour { return date; };

    void print()
    {
        std::cout << "#========================#" << std::endl;
        std::cout << description << std::endl;
        date.print();
        std::cout << "#========================#" << std::endl << std::endl;
    }
    
    Event(year_t year = 2000, month_t month = 1, day_t day = 1, hour_t hour = 12, minute_t minutes = 0, second_t seconds = 0, std::string description = "---"):
        date(year, month, day, hour, minutes, seconds),
        description(description) {};
    
    Event(const DateHour& date, std::string description):
        date(date), description(description) {};
    
    Event(const Event& event) = default;
    auto operator=(const Event& event) -> Event& = default;
    
    Event() = delete;
};

#endif