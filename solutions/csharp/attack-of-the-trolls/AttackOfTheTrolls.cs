using System;

public enum AccountType
{
    Guest = 0,
    User = 1,
    Moderator = 2,
}

[Flags]
public enum Permission : byte
{
    None = 0b00000000,
    Read = 0b00000010,
    Write = 0b00000100,
    Delete = 0b00001000,
    All = 0b00001110
}

static class Permissions
{
    public static Permission Default(AccountType accountType) =>
        accountType switch
        {
            AccountType.Guest => Permission.Read,
            AccountType.User => Permission.Read | Permission.Write,
            AccountType.Moderator => Permission.All,
            _ => Permission.None,
        };

    public static Permission Grant(Permission current, Permission grant) =>
        current | grant;

    public static Permission Revoke(Permission current, Permission revoke) =>
        current & ~revoke;

    public static bool Check(Permission current, Permission check) =>
        (current & check) == check;
}
