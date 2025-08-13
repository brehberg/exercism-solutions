class SimpleCalculator
  ALLOWED_OPERATIONS = ["+", "/", "*"].freeze

  def self.calculate(first_operand, second_operand, operation)
    # Handle illegal operations
    raise UnsupportedOperation unless ALLOWED_OPERATIONS.include?(operation)
    begin
      # Handle the code that may raise errors
      result = case operation
        when "+"
          first_operand + second_operand
        when "*"
          first_operand * second_operand
        when "/"
          first_operand / second_operand
        end
      "#{first_operand} #{operation} #{second_operand} = #{result}"
    rescue ZeroDivisionError
      # Handle DivideByZero exceptions
      return "Division by zero is not allowed."
    rescue
      # Handle invalid arguments
      raise ArgumentError
    end
  end

  class UnsupportedOperation < StandardError
  end
end
