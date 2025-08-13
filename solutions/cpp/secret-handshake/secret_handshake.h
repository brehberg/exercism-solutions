#if !defined(SECRET_HANDSHAKE_H)
#define SECRET_HANDSHAKE_H

#include <string>
#include <vector>

namespace secret_handshake
{
    // Determine the actions of a secret handshake based
    // on the binary representation of the given `code`.
    std::vector<std::string> commands(const int code);

} // namespace secret_handshake

#endif // SECRET_HANDSHAKE_H
