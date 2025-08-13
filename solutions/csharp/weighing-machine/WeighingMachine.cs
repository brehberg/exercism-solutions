using System;

class WeighingMachine
{
    private readonly int _precision;
    private double _weight;
    private double _tareAdjustment = 5;

    public int Precision => _precision;
    public double Weight
    {
        get => _weight;
        set
        {
            if (value < 0) throw new ArgumentOutOfRangeException();
            _weight = value;
        }
    }
    public double TareAdjustment { get => _tareAdjustment; set => _tareAdjustment = value; }
    public string DisplayWeight => String.Format($"{{0:F{Precision}}} kg", Weight - TareAdjustment);

    public WeighingMachine(int precision)
    {
        _precision = precision;
    }
}
