public class CarsAssemble {

    private int baseCarsPerHour = 221;

    private double successRate(int speed) {
        double rate = 0.0;
        if (speed >= 1 && speed <= 4) {
            rate = 1.00;
        } else if (speed >= 5 && speed <= 8) {
            rate = 0.90;
        } else if (speed == 9) {
            rate = 0.80;
        } else if (speed == 10) {
            rate = 0.77;
        }
        return rate;
    }

    public double productionRatePerHour(int speed) {
        return baseCarsPerHour * speed * successRate(speed);
    }

    public int workingItemsPerMinute(int speed) {
        return (int) Math.floor(productionRatePerHour(speed) / 60);
    }
}
