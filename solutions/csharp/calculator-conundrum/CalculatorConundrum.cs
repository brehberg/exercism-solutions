using System;
using static SimpleOperation;

public static class SimpleCalculator
{
    public static string Calculate(int operand1, int operand2, string operation)
    {
        try
        {
            int result = operation switch
            {
                // Implement the calculator operations
                "+" => Addition(operand1, operand2),
                "*" => Multiplication(operand1, operand2),
                "/" => Division(operand1, operand2),

                // Handle illegal operations
                "" => throw new ArgumentException(),
                null => throw new ArgumentNullException(),
                _ => throw new ArgumentOutOfRangeException(),
            };
            return $"{operand1} {operation} {operand2} = {result}";
        }
        // Handle errors when dividing by zero
        catch (DivideByZeroException)
        {
            return "Division by zero is not allowed.";
        }
    }
}
