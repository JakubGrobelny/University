#include "event.hpp"
#include <vector>
#include <algorithm>
#include <cstdlib>

auto parse_event(std::string input) -> Event
{
    try
    {
        Event result(std::stoi(input.substr(0, 4)),
                     std::stoi(input.substr(5, 2)),
                     std::stoi(input.substr(8, 2)),
                     std::stoi(input.substr(11, 2)),
                     std::stoi(input.substr(14, 2)),
                     std::stoi(input.substr(17, 2)),
                     input.substr(20, input.length() - 20));   
    
        return result;
    }
    catch (std::invalid_argument exc)
    {
        throw exc;
    }
}

auto main() -> int
{
    std::vector<Event> events;
    events.push_back(Event(DateHour(), "Current time"));

    int n;
    std::cout << "Enter the number of events: ";
    std::cin >> n;
    std::cout << std::endl;
    std::cin.ignore();
    std::cout << "Enter the date and description in \"yyyy.mm.dd hh:mm:ss [description]\" format" << std::endl;

    for (int i = 0; i < n; i++)
    {
        std::cout << "Event #" << i + 1 << std::endl;
        std::string input;

        std::getline(std::cin, input);

        try
        {
            events.push_back(parse_event(input));
        }
        catch (std::invalid_argument exc)
        {
            std::cerr << exc.what() << std::endl;
            exit(1);
        }
    }

    std::cout << "Events sorted by time: " << std::endl;

    std::sort(events.begin(), events.end(), [](const Event& ev1, const Event& ev2) -> bool 
                                            {
                                                return ev1.get_date() > ev2.get_date(); 
                                            } );

    for (auto& event : events)
        event.print();

    return 0;
}