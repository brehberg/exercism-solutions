class BankAccount {

    enum AccountStatus {
        OPEN,
        CLOSED
    }

    private AccountStatus status = AccountStatus.CLOSED;
    private int balance = 0;

    void open() throws BankAccountActionInvalidException {
        checkAccountNotOpen();
        status = AccountStatus.OPEN;
        balance = 0;
    }

    void close() throws BankAccountActionInvalidException {
        checkAccountIsOpen();
        status = AccountStatus.CLOSED;
    }

    synchronized int getBalance() throws BankAccountActionInvalidException {
        checkAccountNotClosed();
        return balance;
    }

    synchronized void deposit(int amount) throws BankAccountActionInvalidException {
        checkAccountNotClosed();
        checkAmountNotNegative(amount);
        balance += amount;
    }

    synchronized void withdraw(int amount) throws BankAccountActionInvalidException {
        checkAccountNotClosed();
        checkAmountNotNegative(amount);
        checkAmountLessThanBalance(amount);
        balance -= amount;
    }

    private void checkAccountNotOpen() throws BankAccountActionInvalidException {
        if (status == AccountStatus.OPEN) {
            throw new BankAccountActionInvalidException("Account already open");
        }
    }

    private void checkAccountIsOpen() throws BankAccountActionInvalidException {
        if (status != AccountStatus.OPEN) {
            throw new BankAccountActionInvalidException("Account not open");
        }
    }

    private void checkAccountNotClosed() throws BankAccountActionInvalidException {
        if (status == AccountStatus.CLOSED) {
            throw new BankAccountActionInvalidException("Account closed");
        }
    }

    private void checkAmountNotNegative(int amount) throws BankAccountActionInvalidException {
        if (amount < 0) {
            throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");
        }
    }

    private void checkAmountLessThanBalance(int amount) throws BankAccountActionInvalidException {
        if (amount > balance) {
            throw new BankAccountActionInvalidException("Cannot withdraw more money than is currently in the account");
        }
    }
}