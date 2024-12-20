package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class Day02Test {

    @Test
    void testCountSafeReports() throws IOException {
        Path inputPath = Files.createTempFile("test", ".txt");
        Files.writeString(inputPath, "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9");
        assertEquals(2, Day02.countSafeReports(inputPath, false));
        assertEquals(4, Day02.countSafeReports(inputPath, true));
    }
}
