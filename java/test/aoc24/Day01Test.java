package aoc24;

import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

class Day01Test {

  @Test
  void testCalculateTotalDistanceWithValidInput() throws Exception {
    Path inputPath = Files.createTempFile("test", ".txt");
    // Create a test file with the following content:
    // 3 4
    // 4 3
    // 2 5
    // 1 3
    // 3 9
    // 3 3
    Files.writeString(inputPath, "3   4\n4   3\n2   5\n1   3\n3   9\n3   3");
    long expectedDistance = 11;
    long actualDistance = Day01.calculateTotalDistance(inputPath);
    assertEquals(expectedDistance, actualDistance);
  }
}
