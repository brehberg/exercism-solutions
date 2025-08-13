using System;

class RemoteControlCar
{
    private int _distance = 0;
    private int _battery = 100;
    private int _speed = 20;
    private int _batteryDrain = 1;

    // Creating a remote controlled car
    public RemoteControlCar(int speed, int batteryDrain)
    {
        this._speed = speed;
        this._batteryDrain = batteryDrain;
    }

    // Check for a drained battery
    public bool BatteryDrained() => this._battery < this._batteryDrain;

    public int DistanceDriven() => this._distance;

    // Drive the car
    public void Drive()
    {
        if (!BatteryDrained())
        {
            _distance += _speed;
            _battery -= _batteryDrain;
        }
    }

    // Create the Nitro remote control car
    public static RemoteControlCar Nitro() => new RemoteControlCar(50, 4);
}

class RaceTrack
{
    private int _distance;

    // Creating a race track
    public RaceTrack(int distance) => this._distance = distance;

    // Check if a remote control car can finish a race
    public bool TryFinishTrack(RemoteControlCar car)
    {
        while (!car.BatteryDrained())
        {
            car.Drive();
            if (car.DistanceDriven() >= this._distance)
            {
                return true;
            }
        }
        return false;
    }
}
