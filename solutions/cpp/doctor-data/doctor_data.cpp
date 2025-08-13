#include "doctor_data.h"

using namespace std;

namespace heaven
{
    Vessel::Vessel(string name, int gen, System system)
        : bob{name}, generation{gen}, current_system{system} {}

    Vessel Vessel::replicate(string name)
    {
        return {move(name), generation + 1, current_system};
    }

    void Vessel::make_buster()
    {
        busters += 1;
    }

    bool Vessel::shoot_buster()
    {
        if (busters == 0)
        {
            return false;
        }
        busters -= 1;
        return true;
    }

    bool in_the_same_system(Vessel &prime, Vessel &other)
    {
        return (prime.current_system == other.current_system);
    }

    string get_older_bob(Vessel &prime, Vessel &other)
    {
        return (prime.generation <= other.generation) ? prime.bob : other.bob;
    }
} // namespace heaven