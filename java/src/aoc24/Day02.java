package aoc24;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class Day02 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day02.txt");
        long safeReports = countSafeReports(inputPath, false);
        System.out.println("Number of safe reports (Part 1): " + safeReports);

        long safeReportsWithDampener = countSafeReports(inputPath, true);
        System.out.println("Number of safe reports (Part 2): " + safeReportsWithDampener);
    }

    public static long countSafeReports(Path inputPath, boolean withDampener) throws IOException {
        long safeCount = 0;
        try (BufferedReader reader = Files.newBufferedReader(inputPath)) {
            for (String line = reader.readLine(); line != null; line = reader.readLine()) {
                List<Integer> report = parseReport(line);
                if (withDampener) {
                    if (isSafeWithDampener(report)) {
                        safeCount++;
                    }
                } else {
                    if (isSafe(report)) {
                        safeCount++;
                    }
                }
            }
        }
        return safeCount;
    }

    private static List<Integer> parseReport(String line) {
        String[] parts = line.split("\\s+");
        List<Integer> report = new ArrayList<>();
        for (String part : parts) {
            report.add(Integer.valueOf(part));
        }
        return report;
    }

    private static boolean isSafe(List<Integer> report) {
        if (report.size() < 2) {
            return true;
        }

        List<Integer> diffs = new ArrayList<>();
        for (int i = 0; i < report.size() - 1; i++) {
            diffs.add(report.get(i + 1) - report.get(i));
        }

        boolean allIncreasing = true;
        for (int diff : diffs) {
            if (diff < 0) {
                allIncreasing = false;
                break;
            }
        }

        boolean allDecreasing = true;
        for (int diff : diffs) {
            if (diff > 0) {
                allDecreasing = false;
                break;
            }
        }

        if (!allIncreasing && !allDecreasing) {
            return false;
        }

        for (int diff : diffs) {
            if (Math.abs(diff) < 1 || Math.abs(diff) > 3) {
                return false;
            }
        }

        return true;
    }

    private static boolean isSafeWithDampener(List<Integer> report) {
        if (isSafe(report)) {
            return true;
        }

        for (int i = 0; i < report.size(); i++) {
            List<Integer> tempReport = new ArrayList<>(report);
            tempReport.remove(i);
            if (isSafe(tempReport)) {
                return true;
            }
        }

        return false;
    }
}
