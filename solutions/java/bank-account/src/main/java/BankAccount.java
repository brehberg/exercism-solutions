class BankAccount {

    enum AccountStatus {
        OPEN,
        CLOSED
    }

    private AccountStatus status = AccountStatus.CLOSED;
    private int balance = 0;

    void open() throws BankAccountActionInvalidException {
        if (status == AccountStatus.OPEN) {
            throw new BankAccountActionInvalidException("Account already open");
        }
        status = AccountStatus.OPEN;
        balance = 0;
    }

    void close() throws BankAccountActionInvalidException {
        checkAccountNotClosed("Account not open");
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
        if (amount > balance) {
            throw new BankAccountActionInvalidException("Cannot withdraw more money than is currently in the account");
        }
        balance -= amount;
    }

    private void checkAccountNotClosed() throws BankAccountActionInvalidException {
        checkAccountNotClosed("Account closed");
    }

    private void checkAccountNotClosed(String message) throws BankAccountActionInvalidException {
        if (status == AccountStatus.CLOSED) {
            throw new BankAccountActionInvalidException(message);
        }
    }

    private void checkAmountNotNegative(int amount) throws BankAccountActionInvalidException {
        if (amount < 0) {
            throw new BankAccountActionInvalidException("Cannot deposit or withdraw negative amount");
        }
    }
}