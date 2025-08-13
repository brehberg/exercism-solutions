using System;

class WeighingMachine
{
    public int Precision { get; }

    private double _weight;
    public double Weight
    {
        get => _weight;
        set
        {
            if (value < 0) throw new ArgumentOutOfRangeException("Weight cannot be negative.");
            _weight = value;
        }
    }

    public double TareAdjustment { get; set; } = 5;

    public string DisplayWeight
    {
        get
        {
            double displayWeight = Math.Round(Weight - TareAdjustment, Precision);
            return $"{displayWeight.ToString($"F{Precision}")} kg";
        }
    }

    public WeighingMachine(int precision) => Precision = precision;
}
