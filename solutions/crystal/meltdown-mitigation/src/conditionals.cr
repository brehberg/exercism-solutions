class Reactor
  # Check for criticality
  def self.criticality_balanced?(temperature, neutrons_emitted)
    temperature < 800 && neutrons_emitted > 500 && temperature * neutrons_emitted < 500_000
  end

  # Determine the Power output range
  def self.reactor_efficiency(voltage, current, theoretical_max_power)
    efficiency_percentage = voltage * current / theoretical_max_power * 100

    if efficiency_percentage >= 80
      "green"
    elsif efficiency_percentage >= 60
      "orange"
    elsif efficiency_percentage >= 30
      "red"
    else
      "black"
    end
  end

  # Fail Safe Mechanism
  def self.fail_safe(temperature, neutrons_produced_per_second, threshold)
    operational_value = temperature * neutrons_produced_per_second

    if operational_value < threshold * 0.9
      "LOW"
    elsif (operational_value - threshold).abs <= threshold * 0.1
      "NORMAL"
    else
      "DANGER"
    end
  end
end
