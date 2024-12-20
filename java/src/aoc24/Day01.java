package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Day01 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day01.txt");
        try {
            long totalDistance = calculateTotalDistance(inputPath);
            System.out.println("Total distance: " + totalDistance);
        } catch (InvalidInputFormatException e) {
            System.err.println("Error processing input: " + e.getMessage());
            System.exit(1);
        }
    }

    public static long calculateTotalDistance(Path inputPath) throws IOException, InvalidInputFormatException {
        int[][] pairs = Files.lines(inputPath)
                .map(line -> line.split("\\s+"))
                .map(parts -> new int[]{Integer.parseInt(parts[0]), Integer.parseInt(parts[1])})
                .toArray(int[][]::new);

        if (pairs.length == 0) {
            throw new InvalidInputFormatException("Input file is empty or contains no valid lines.");
        }

        int[] departures = Arrays.stream(pairs).mapToInt(p -> p[0]).sorted().toArray();
        int[] arrivals = Arrays.stream(pairs).mapToInt(p -> p[1]).sorted().toArray();

        if (departures.length != arrivals.length) {
            throw new InvalidInputFormatException("Unequal number of departure and arrival times.");
        }

        return IntStream.range(0, departures.length).boxed()
                .collect(Collectors.summingLong(i -> Math.abs(departures[i] - arrivals[i])));

    }

    static class InvalidInputFormatException extends Exception {

        public InvalidInputFormatException(String message) {
            super(message);
        }
    }
}
