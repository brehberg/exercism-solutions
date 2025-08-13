using System;

public class BankAccount
{
    private enum AccountStatus
    {
        Open,
        Closed,
    }

    private AccountStatus _status = AccountStatus.Closed;
    private decimal _balance = 0m;
    private readonly object balanceLocker = new object();

    public void Open()
    {
        _status = AccountStatus.Open;
        _balance = 0m;
    }

    public void Close()
    {
        _status = AccountStatus.Closed;
    }

    public decimal Balance
    {
        get
        {
            checkAccountNotClosed();
            lock (balanceLocker)
            {
                return _balance;
            }
        }
    }

    public void UpdateBalance(decimal change)
    {
        checkAccountNotClosed();
        lock (balanceLocker)
        {
            _balance += change;
        }

    }

    private void checkAccountNotClosed()
    {
        if (_status == AccountStatus.Closed)
        {
            throw new InvalidOperationException("Account closed");
        }
    }
}
