#include <string>

using namespace std;

namespace log_line
{
    // Get the message from a log line
    string message(string line)
    {
        int offset = line.find("]: ") + 3;
        return line.substr(offset);
    }

    // Get the log level from a log line
    string log_level(string line)
    {
        int offset = line.find("]: ") - 1;
        return line.substr(1, offset);
    }

    // Reformat a log line
    string reformat(string line)
    {
        string msg = message(line);
        string lvl = log_level(line);
        return msg + " (" + lvl + ")";
    }

} // namespace log_line
