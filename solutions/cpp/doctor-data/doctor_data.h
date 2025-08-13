#pragma once
#include <string>

namespace star_map
{
    enum class System
    {
        Sol,
        AlphaCentauri,
        BetaHydri,
        DeltaEridani,
        EpsilonEridani,
        Omicron2Eridani,
    };
} // namespace star_map

namespace heaven
{
    using namespace star_map;

    class Vessel
    {
    public:
        std::string bob;
        int generation{0};
        System current_system;
        int busters{0};

        Vessel(std::string, int, System = System::Sol);
        Vessel replicate(std::string name);
        void make_buster();
        bool shoot_buster();
    };

    bool in_the_same_system(Vessel, Vessel);
    std::string get_older_bob(Vessel, Vessel);
} // namespace heaven