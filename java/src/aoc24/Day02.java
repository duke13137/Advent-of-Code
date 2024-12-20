package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Day02 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day02.txt");
        long safeReports = countSafeReports(inputPath);
        System.out.println("Number of safe reports: " + safeReports);
    }

    public static long countSafeReports(Path inputPath) throws IOException {
        try (Stream<String> lines = Files.lines(inputPath)) {
            return lines.map(Day02::parseReport)
                    .filter(Day02::isSafe)
                    .count();
        }
    }

    private static List<Integer> parseReport(String line) {
        return Arrays.stream(line.split("\\s+"))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
    }

    private static boolean isSafe(List<Integer> report) {
        if (report.size() < 2) {
            return true;
        }

        List<Integer> diffs = IntStream.range(0, report.size() - 1)
                .map(i -> report.get(i + 1) - report.get(i))
                .boxed()
                .collect(Collectors.toList());

        boolean allIncreasing = diffs.stream().allMatch(diff -> diff >= 0);
        boolean allDecreasing = diffs.stream().allMatch(diff -> diff <= 0);

        return (allIncreasing || allDecreasing) && diffs.stream().allMatch(diff -> Math.abs(diff) >= 1 && Math.abs(diff) <= 3);
    }
}
