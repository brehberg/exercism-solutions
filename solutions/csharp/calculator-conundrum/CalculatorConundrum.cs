using System;

public static class SimpleCalculator
{
    public static string Calculate(int operand1, int operand2, string operation)
    {
        int result = 0;
        switch (operation)
        {
            // Implement the calculator operations
            case "+":
                result = SimpleOperation.Addition(operand1, operand2);
                break;
            case "*":
                result = SimpleOperation.Multiplication(operand1, operand2);
                break;
            case "/":
                // Handle errors when dividing by zero
                try { result = SimpleOperation.Division(operand1, operand2); }
                catch (System.DivideByZeroException) { return "Division by zero is not allowed."; }
                break;

            // Handle illegal operations
            case "":
                throw new ArgumentException();
            case null:
                throw new ArgumentNullException();
            default:
                throw new ArgumentOutOfRangeException();
        }
        return $"{operand1} {operation} {operand2} = {result}";
    }
}
