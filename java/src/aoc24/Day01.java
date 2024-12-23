package aoc24;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

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

    List<int[]> pairs = Files.lines(inputPath)
        .map(line -> line.split("\\s+"))
        .filter(parts -> parts.length == 2)
        .map(parts -> {
          try {
            int departure = Integer.parseInt(parts[0]);
            int arrival = Integer.parseInt(parts[1]);
            return new int[] { departure, arrival };
          } catch (NumberFormatException e) {
            throw new RuntimeException("Invalid number format in line", e);
          }
        })
        .toList();
    } catch (RuntimeException e) {
      if (e.getCause() instanceof NumberFormatException) {
        throw new InvalidInputFormatException(e.getCause().getMessage());
      }
      throw e;
    }

    if (pairs.isEmpty()) {
      throw new InvalidInputFormatException("Input file is empty or contains no valid lines.");
    }

    int[] departures = pairs.stream().mapToInt(pair -> pair[0]).toArray();
    int[] arrivals = pairs.stream().mapToInt(pair -> pair[1]).toArray();

    Arrays.sort(departures);
    Arrays.sort(arrivals);

    if (departures.length != arrivals.length) {
      throw new InvalidInputFormatException("Unequal number of departure and arrival times.");
    }

    long totalDistance = 0;
    for (int i = 0; i < departures.length; i++) {
      totalDistance += Math.abs(departures[i] - arrivals[i]);
    }

    return totalDistance;
  }
}

class InvalidInputFormatException extends Exception {

    public InvalidInputFormatException(String message) {
      super(message);
    }
  }
}
