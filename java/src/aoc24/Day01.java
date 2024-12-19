package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Day01 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day01.txt"); // More concise path creation
        List<String> lines = Files.readAllLines(inputPath);

        List<Integer> leftList = new ArrayList<>();
        List<Integer> rightList = new ArrayList<>();

        for (String line : lines) {
            String[] parts = line.split("\\s+");
            if (parts.length == 2) {
                leftList.add(Integer.parseInt(parts[0]));
                rightList.add(Integer.parseInt(parts[1]));
            } else {
                System.err.println("Skipping invalid line: " + line);
            }
        }

        Collections.sort(leftList);
        Collections.sort(rightList);

        long totalDistance = 0;
        if (leftList.size() == rightList.size()) {
            for (int i = 0; i < leftList.size(); i++) {
                totalDistance += Math.abs(leftList.get(i) - rightList.get(i));
            }
        } else {
            System.err.println("Lists have different sizes. Cannot calculate total distance.");
        }

        System.out.println("Total distance: " + totalDistance);
    }
}
