
class BirdWatcher {
    private final int[] birdsPerDay;
    private final int[] birdsLastWeek = { 0, 2, 5, 3, 7, 8, 4 };
    private final int birdsPerBusyDay = 5;

    public BirdWatcher(int[] birdsPerDay) {
        this.birdsPerDay = birdsPerDay.clone();
    }

    public int[] getLastWeek() {
        return birdsLastWeek;
    }

    public int getToday() {
        return birdsPerDay[birdsPerDay.length - 1];
    }

    public void incrementTodaysCount() {
        birdsPerDay[birdsPerDay.length - 1] += 1;
    }

    public boolean hasDayWithoutBirds() {
        for (int birdsSeen : birdsPerDay) {
            if (birdsSeen == 0)
                return true;
        }
        return false;
    }

    public int getCountForFirstDays(int numberOfDays) {
        int totalBirds = 0;
        for (int i = 0; i < Math.min(numberOfDays, birdsPerDay.length); i++) {
            totalBirds += birdsPerDay[i];
        }
        return totalBirds;
    }

    public int getBusyDays() {
        int busyDays = 0;
        for (int birdsSeen : birdsPerDay) {
            if (birdsSeen >= birdsPerBusyDay)
                busyDays += 1;
        }
        return busyDays;
    }
}
