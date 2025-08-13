using System;

class RemoteControlCar
{
    private int _distance = 0;
    private int _battery = 100;
    private int _speed = 20;
    private int _batteryDrain = 1;

    public static RemoteControlCar Buy() => new RemoteControlCar();

    public string DistanceDisplay() => $"Driven {_distance} meters";

    public string BatteryDisplay() => _battery == 0 ? "Battery empty" : $"Battery at {_battery}%";

    public void Drive()
    {
        if (_battery >= _batteryDrain)
        {
            _distance += _speed;
            _battery -= _batteryDrain;
        }
    }
}
