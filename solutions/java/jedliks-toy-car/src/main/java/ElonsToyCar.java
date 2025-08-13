public class ElonsToyCar {
    private static final int speed = 20;
    private static final int batteryDrain = 1;

    private int distance = 0;
    private int batteryLevel = 100;

    public static ElonsToyCar buy() {
        return new ElonsToyCar();
    }

    public String distanceDisplay() {
        return String.format("Driven %d meters", distance);
    }

    public String batteryDisplay() {
        return batteryLevel == 0
                ? "Battery empty"
                : String.format("Battery at %d%%", batteryLevel);
    }

    public void drive() {
        if (batteryLevel >= batteryDrain) {
            distance += speed;
            batteryLevel -= batteryDrain;
        }
    }
}
