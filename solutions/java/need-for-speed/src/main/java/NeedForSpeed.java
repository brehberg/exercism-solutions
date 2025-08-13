class NeedForSpeed {
    private final int speed;
    private final int batteryDrain;

    private int distance;
    private int batteryLevel;

    NeedForSpeed(int speed, int batteryDrain) {
        this.speed = speed;
        this.batteryDrain = batteryDrain;
        this.distance = 0;
        this.batteryLevel = 100;
    }

    public boolean batteryDrained() {
        return batteryLevel < batteryDrain;
    }

    public int distanceDriven() {
        return distance;
    }

    public void drive() {
        if (!batteryDrained()) {
            distance += speed;
            batteryLevel -= batteryDrain;
        }
    }

    public static NeedForSpeed nitro() {
        return new NeedForSpeed(50, 4);
    }
}

class RaceTrack {
    private final int distance;

    RaceTrack(int distance) {
        this.distance = distance;
    }

    public boolean canFinishRace(NeedForSpeed car) {
        while (car.distanceDriven() < distance) {
            if (car.batteryDrained())
                return false;
            car.drive();
        }
        return true;
    }
}
