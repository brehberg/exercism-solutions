#include "secret_handshake.h"

#include <algorithm>

namespace secret_handshake
{
    using namespace std;

    vector<string> commands(const int code)
    {
        vector<string> handshake;
        auto secret = [&handshake, code](int mask, string action)
        {
            if ((code & mask) == 0)
                return;
            if (action == "reverse")
                reverse(handshake.begin(), handshake.end());
            else
                handshake.emplace_back(action);
        };

        secret(0b00001, "wink");
        secret(0b00010, "double blink");
        secret(0b00100, "close your eyes");
        secret(0b01000, "jump");
        secret(0b10000, "reverse");
        return handshake;
    }

} // namespace secret_handshake
