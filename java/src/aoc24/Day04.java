package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Day04 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day04.txt");
        List<String> input = Files.readAllLines(inputPath);

        long part1Result = countOccurrences(input);
        System.out.println("Part 1: " + part1Result);

        long part2Result = countXMASShapes(input);
        System.out.println("Part 2: " + part2Result);
    }

    public static long countOccurrences(List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();
        String word = "XMAS";
        int wordLen = word.length();
        long count = 0;

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                count += checkDirections(grid, row, col, word, wordLen);
            }
        }

        return count;
    }

    private static int checkDirections(List<String> grid, int row, int col, String word, int wordLen) {
        int count = 0;
        int[][] directions = {{0, 1}, {1, 0}, {1, 1}, {1, -1}}; // right, down, diag-down-right, diag-down-left

        for (int[] dir : directions) {
            int dirRow = dir[0];
            int dirCol = dir[1];
            String forward = extractString(grid, row, col, wordLen, dirRow, dirCol);
            String backward = new StringBuilder(forward).reverse().toString();
            if (forward.equals(word)) count++;
            if (backward.equals(word)) count++;
        }
        return count;
    }

    private static String extractString(List<String> grid, int row, int col, int wordLen, int dirRow, int dirCol) {
        StringBuilder sb = new StringBuilder();
        int rows = grid.size();
        int cols = grid.get(0).length();

        for (int i = 0; i < wordLen; i++) {
            int newRow = row + i * dirRow;
            int newCol = col + i * dirCol;

            if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                sb.append(grid.get(newRow).charAt(newCol));
            } else {
                return "";
            }
        }
        return sb.toString();
    }

    public static long countXMASShapes(List<String> grid) {
        int rows = grid.size();
        int cols = grid.get(0).length();
        long count = 0;

        for (int row = 1; row < rows - 1; row++) {
            for (int col = 1; col < cols - 1; col++) {
                if (isXMASShape(grid, row, col)) {
                    count++;
                }
            }
        }

        return count;
    }

    private static boolean isXMASShape(List<String> grid, int row, int col) {
        // Check for 'A' in the center
        if (grid.get(row).charAt(col) != 'A') {
            return false;
        }
    
        // Define the word and its reverse
        String word = "MAS";
        String reversedWord = new StringBuilder(word).reverse().toString();
    
        // Check diagonal pairs
        boolean topLeftBottomRight = false;
        boolean topRightBottomLeft = false;
    
        // Check top-left to bottom-right
        String diag1 = extractString(grid, row - 1, col - 1, word.length(), -1, -1);
        String diag2 = extractString(grid, row + 1, col + 1, word.length(), 1, 1);
        if ((diag1.equals(word) || diag1.equals(reversedWord)) && (diag2.equals(word) || diag2.equals(reversedWord))) {
            topLeftBottomRight = true;
        }
    
        // Check top-right to bottom-left
        String diag3 = extractString(grid, row - 1, col + 1, word.length(), -1, 1);
        String diag4 = extractString(grid, row + 1, col - 1, word.length(), 1, -1);
        if ((diag3.equals(word) || diag3.equals(reversedWord)) && (diag4.equals(word) || diag4.equals(reversedWord))) {
            topRightBottomLeft = true;
        }
    
        return topLeftBottomRight && topRightBottomLeft;
    }
}
