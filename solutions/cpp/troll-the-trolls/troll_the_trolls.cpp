namespace hellmath
{
    // Set up user types and permissions.
    enum class AccountStatus
    {
        troll,
        guest,
        user,
        mod,
    };
    enum class Action
    {
        read,
        write,
        remove,
    };

    // Promote trolls only to other trolls.
    bool display_post(AccountStatus poster, AccountStatus viewer)
    {
        return (poster == AccountStatus::troll) ? (viewer == AccountStatus::troll) : true;
    }

    // Check permissions for different users.
    bool permission_check(Action action, AccountStatus user)
    {
        switch (user)
        {
        case AccountStatus::guest:
            return (action == Action::read);
        case AccountStatus::user:
        case AccountStatus::troll:
            return (action == Action::read || action == Action::write);
        case AccountStatus::mod:
            return true;
        default:
            return false;
        }
    }

    // Grant game access and pair players.
    bool valid_player_combination(AccountStatus player1, AccountStatus player2)
    {
        switch (player1)
        {
        case AccountStatus::guest:
            return false;
        case AccountStatus::troll:
            return (player2 == AccountStatus::troll);
        default:
            return (player2 == AccountStatus::mod || player2 == AccountStatus::user);
        }
    }

    // Build priority queuing.
    bool has_priority(AccountStatus user1, AccountStatus user2)
    {
        switch (user1)
        {
        case AccountStatus::mod:
            return (user2 != AccountStatus::mod);
        case AccountStatus::user:
            return (user2 == AccountStatus::guest || user2 == AccountStatus::troll);
        case AccountStatus::guest:
            return (user2 == AccountStatus::troll);
        default:
            return false;
        }
    }
} // namespace hellmath